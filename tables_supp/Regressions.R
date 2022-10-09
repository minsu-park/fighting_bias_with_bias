library(data.table)
library(dplyr)
library(zoo)

# DATA PREPARATION --------------------------------------------------------
### INITIAL REVIEW DATA ###
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rws=fread("../data/rws.all.csv") 

colnames(rws)
rws %>% summarise(host.cnt=n_distinct(host_id),
                  guest.cnt=n_distinct(guest_id),
                  ls.cnt=n_distinct(listing_id))

rws %>% group_by(instant_bookable) %>% 
  summarise(ls.cnt=n_distinct(listing_id),
            host.cnt=n_distinct(host_id),
            rws.cnt=n_distinct(id),
            guest.cnt=n_distinct(guest_id))



### MERGE GUEST/HOST RACIAL IDENTITY WITH REVIEWS ###
guest=fread("../data/guest.attribute.csv")
colnames(guest)[c(5,8)]=c("g.num_face","g.ethnicity")
guest=guest[,c("guest_id","g.num_face","g.ethnicity")]
rws.gh=left_join(rws,guest, by="guest_id")

host=fread("../data/host.attribute.csv")
colnames(host)[c(5,8)]=c("h.num_face","h.ethnicity")
host=host[,c("listing_id","host_id","h.num_face","h.ethnicity")]
rws.gh=left_join(rws.gh,host, by=c("listing_id","host_id"))

rws.gh %>% group_by(instant_bookable) %>% 
  summarise(ls.cnt=n_distinct(listing_id),
            host.cnt=n_distinct(host_id),
            rws.cnt=n_distinct(id),
            guest.cnt=n_distinct(guest_id))

rws.gh.t=rws.gh 
rws.gh.t=rws.gh.t[rws.gh.t$g.ethnicity!="",]
rws.gh.t=rws.gh.t[rws.gh.t$h.ethnicity!="",]

host.ins=rws.gh.t[!duplicated(rws.gh.t$host_id),]



### MERGE WITH LISTING VARS ###
rws.gh.t$listing_id=as.character(rws.gh.t$listing_id)

ls=fread("../data/ls.with.rws.csv")
ls=ls[,c("listing_id","price","review_scores_rating",
         "room_type")]
ls$listing_id=as.character(ls$listing_id)
ls$price=as.numeric(gsub('[$,]', '', ls$price))

rws.gh.t=left_join(rws.gh.t,ls, by="listing_id")
n=which(colnames(rws.gh.t)=="review_scores_rating")
colnames(rws.gh.t)[n]="rating"



### LABEL FRONT-PAGE REVIEWS (THE MOST RECENT SIX REVIEWS) ###
df=rws.gh.t

### 1 ### Count total number of reviews -1, lag g.ethnicity by 1 to 
###   ### prepare for calculating the most recent 6 endorsement!
df = df %>% group_by(listing_id) %>% arrange(date) %>%
  dplyr::mutate(cum.cnt=row_number()-1,
         rws.race=lag(g.ethnicity, n=1, default=NA))

df$rws.race[which(is.na(df$rws.race))]=0

## 1-1 ## Count the guest race of each row
df$g.W.cnt=ifelse(df$rws.race=="WHITE", 1, 0)
df$g.B.cnt=ifelse(df$rws.race=="BLACK", 1, 0)
df$g.A.cnt=ifelse(df$rws.race=="ASIAN", 1, 0)

## 1-2 ## Roll sum the most recent 6 guests by date
df=df %>% group_by(listing_id) %>%arrange(date) %>%
  dplyr::mutate(top6.W=rollsumr(g.W.cnt==1, 6, fill = NA),
         top6.B=rollsumr(g.B.cnt==1, 6, fill = NA),
         top6.A=rollsumr(g.A.cnt==1, 6, fill = NA))

## 1-3 ## For NA values that are 1st - 5th rows, use regular cum.cnt
df.na=df[which(is.na(df$top6.W)|is.na(df$top6.B)|is.na(df$top6.A)),]
df.na$g.W.cnt[which(is.na(df.na$g.W.cnt))]=0
df.na$g.A.cnt[which(is.na(df.na$g.A.cnt))]=0
df.na$g.B.cnt[which(is.na(df.na$g.B.cnt))]=0

df.na = df.na %>% group_by(listing_id) %>% arrange(date) %>%
  dplyr::mutate(top6.W=cumsum(g.W.cnt==1),
         top6.B=cumsum(g.B.cnt==1),
         top6.A=cumsum(g.A.cnt==1))

df2=df[which(!is.na(df$top6.W)&!is.na(df$top6.B)&!is.na(df$top6.A)),]

## 1-4 ## rbind df and df2
df=rbind(df2,df.na)
df=df %>% group_by(listing_id) %>% arrange(date)



### COMPUTE THE OVERALL SRE BY PROPORTION ###
df = df %>% group_by(listing_id) %>% arrange(date) %>%
  dplyr::mutate(g.W.cnt=cumsum(rws.race=="WHITE"),
         g.B.cnt=cumsum(rws.race=="BLACK"),
         g.A.cnt=cumsum(rws.race=="ASIAN"))
df$prop.W=round((df$g.W.cnt/df$cum.cnt),2)
df$prop.B=round((df$g.B.cnt/df$cum.cnt),2)
df$prop.A=round((df$g.A.cnt/df$cum.cnt),2)

df$prop.W[which(df$prop.W=="NaN")]=0
df$prop.B[which(df$prop.B=="NaN")]=0
df$prop.A[which(df$prop.A=="NaN")]=0



### ADDING CONTROL VARIABLES ###

### 1 ### Add zip code level racial composition

## 1-1 ## Get the latitude and longitude
ls=fread("../data/ls.with.rws.csv")
colnames(ls)
ls=ls[,c("listing_id","neighbourhood_cleansed","neighbourhood_group_cleansed",
         "zipcode","latitude","longitude")]
colnames(ls)[2:3]=c("neighbourhood","borough")

df$listing_id=as.character(df$listing_id)
ls$listing_id=as.character(ls$listing_id)
df=left_join(df, ls, by="listing_id")
df.withzip=df[df$zipcode!="",]
df.withoutzip=df[df$zipcode=="",]
df.withoutzip$zipcode=NULL

## 1-2 ## Load retrieved zip code data
df.zip=fread("../data/zip.retreied.csv")
df.zip$listing_id=as.character(df.zip$listing_id)
df.withoutzip=left_join(df.withoutzip, df.zip[,c("listing_id","zipcode")],
                        by="listing_id")
df.withoutzip$zipcode=as.character(df.withoutzip$zipcode)
df.zip$zipcode=as.character(df.zip$zipcode)

df=rbind(df.withzip,df.withoutzip)
df$zipcode[which(df$zipcode=="11249\n11249")]="11249"

## 1-3 ## Add racial composition data from ACS 2020 5-year average
race=fread("../data/ACSDT5Y2020.B02001_data_with_overlays_2022-03-24T004426.csv")
# B02001_001E	Estimate!!Total
# B02001_002E	Estimate!!Total:!!White alone
# B02001_003E	Estimate!!Total:!!Black or African American alone
# B02001_005E	Estimate!!Total:!!Asian alone
race=race[,c("GEO_ID","NAME","B02001_001E","B02001_002E","B02001_003E","B02001_005E")]
colnames(race)=c("GEO_ID","NAME","total","white","black","asian")
race=race[-1,] ## remove the first row of description
race$zipcode=substr(race$NAME, 7,11)

race$total=as.numeric(race$total)
race$white=as.numeric(race$white)
race$black=as.numeric(race$black)
race$asian=as.numeric(race$asian)

race$white.pct=race$white/race$total
race$black.pct=race$black/race$total
race$asian.pct=race$asian/race$total

race$zipcode=as.character(race$zipcode)
race=race[,c("zipcode","white.pct", "black.pct", "asian.pct")]
df=dplyr::left_join(df, race, by="zipcode")

df$zip.srp=NA #-Note: **zip.srp** represents zip code level same-race proportion.
df$zip.srp[which(df$g.ethnicity=="ASIAN")]=df$asian.pct[which(df$g.ethnicity=="ASIAN")]
df$zip.srp[which(df$g.ethnicity=="BLACK")]=df$black.pct[which(df$g.ethnicity=="BLACK")]
df$zip.srp[which(df$g.ethnicity=="WHITE")]=df$white.pct[which(df$g.ethnicity=="WHITE")]



### 2 ### Add amenity variable

## 2-1 ## Read amenities
#-Note: There are 23 listings that really do not have amenity information.
amenity=fread("../data/listing.amenitylevel.csv")
colnames(amenity)
amt.source <- amenity[,c("listing_id","amenities")] %>% 
  tidytext::unnest_tokens(cate, amenities,token = 'regex', pattern=",")

amt.source=amt.source[amt.source$cate!="translation missing: en.hosting_amenity_49",]
amt.source=amt.source[amt.source$cate!="translation missing: en.hosting_amenity_50",]

amt.source=unique(amt.source)
amt.source$cate=trimws(amt.source$cate)

## 2-2 ## Match with amenity scores
wiki.data=fread("../data/wiki_premium.csv")
nrow(wiki.data)

wiki.data=wiki.data[,c("Idea Text","Score")]
colnames(wiki.data)[1]="cate"
amt.source=left_join(amt.source, wiki.data, by="cate")
ls.score=amt.source %>% group_by(listing_id) %>%
  dplyr:: summarise(amenity.sum=sum(Score, na.rm=T),
                    amenity.cnt=n())

hist(ls.score$amenity.sum)
hist(ls.score$amenity.cnt)

ls.score$listing_id=as.character(ls.score$listing_id)
colnames(ls.score)
df=left_join(df, ls.score, by="listing_id")
colnames(df)



### 3 ### Add average price and average rating
df.m=df[!duplicated(df$listing_id),]
df.m$avg.price=log(df.m$price+1)
df.m$avg.rate=log(df.m$rating/20+1)

df=left_join(df, df.m[,c("listing_id","avg.price","avg.rate")], 
             by="listing_id")

### 4 ### Add gender dummy variable
guest=fread("../data/guest.attribute.csv")
guest=guest[,c("guest_id","gender")]
colnames(guest)[2]="g.gender"
host=fread("../data/host.attribute.csv")
host=host[,c("host_id","gender")]
colnames(host)[2]="h.gender"
host=host[!duplicated(host$host_id),]
df=left_join(df, guest, by="guest_id")
df=left_join(df, host, by="host_id")

df$same_gender="0"
df$same_gender[which(df$g.gender==df$h.gender)]="1"
table(df$same_gender)
df$gender.binary="0"
df$gender.binary[which(df$h.gender=="Female")]="1"
table(df$gender.binary)

### 5 ### Add room type as a binary variable
table(df$room_type)
df$room_type2="0"
df$room_type2[which(df$room_type!="Shared room")]="1"
table(df$room_type2)





# SAVE DATA FOR REGRESSIONS -----------------------------------------------
write.csv(df, file="../data/data.aggregate.regression.csv", row.names = F)





# RUNNING REGRESSIONS -----------------------------------------------------
library(data.table)
library(nnet)
library(emmeans)

### READ DATA ###
df=fread("../data/data.aggregate.regression.csv")
table(df$instant_bookable)

### CHOOSE A SAMPLE ###
# 1 # Only instant bookings
df=df[df$instant_bookable=="t",]
outfile_t1="supp_table_s3.csv"
outfile_t2="supp_table_s4"

# 2 # Both booking types
df=df
outfile_t1="supp_table_s5.csv"
outfile_t2="supp_table_s6"

# 3 # Only requested bookings
df=df[df$instant_bookable=="f",]
outfile_t1="supp_table_s7.csv"
outfile_t2="supp_table_s8"

### TUNING VARIABLES ###
df$same_gender=as.factor(df$same_gender)
df$gender.binary=as.factor(df$gender.binary)
df$room_type2=as.factor(df$room_type2)

df$h.ethnicity <- factor(df$h.ethnicity, 
                         levels=c("WHITE","BLACK","ASIAN"))
df$g.ethnicity <- factor(df$g.ethnicity, 
                         levels=c("WHITE","BLACK","ASIAN"))

### RUNNING REGRESSIONS ###
# 1 # Pairing probability without considering SREs
cate=c("Without listing attributes", "With listing attributes")
dt.all=data.frame()
for (x in cate) {
  if(x=="Without listing attributes"){
    mod=multinom(g.ethnicity ~h.ethnicity, data = df)}
  if(x=="With listing attributes"){
    mod=multinom(g.ethnicity ~h.ethnicity+avg.price+avg.rate+
                   room_type2+cum.cnt+zip.srp + log(amenity.sum)+
                   same_gender+gender.binary, data = df)}
  
  dt.mod=as.data.frame(test(emmeans(mod, ~h.ethnicity|g.ethnicity, 
                                    CIs = T, type = "response")))
  dt.mod$cate=x
  dt.mod$prob.p=format(round(dt.mod$prob,digits = 3), nsmall = 3)
  dt.mod$SE.p=format(round(dt.mod$SE,digits = 3), nsmall = 3)
  dt.mod$p.value.p=format(round(dt.mod$p.value,digits = 3), nsmall = 3)
  dt.mod$p.value.p[dt.mod$p.value.p<.001]="< .001"
  dt.mod$p.value.p[dt.mod$p.value.p>=.001&dt.mod$p.value.p<.01]="< .01"
  dt.mod$p.value.p[dt.mod$p.value.p>=.01&dt.mod$p.value.p<.05]="< .05"
  dt.all=rbind(dt.all, dt.mod)
  
  write.csv(dt.all, file=paste0("../output/",outfile_t1), row.names=F)
}

# 2 # Pairing probability based on SREs
df2=df[df$cum.cnt>=6,]
cate=c("Without listing attributes", "With listing attributes")
race=c("WHITE","BLACK","ASIAN")
SRE.type=c("cnt","pct") ##front page & all reviews
SRE.table=function(SRE.type){
  dt.all=data.frame()
  for (x in cate){
    for (y in race){
      
      if(SRE.type=="cnt"){
        if(y=="WHITE"){df2$tmp=df2$top6.W}
        if(y=="BLACK"){df2$tmp=df2$top6.B}
        if(y=="ASIAN"){df2$tmp=df2$top6.A}}
      if(SRE.type=="pct"){
        if(y=="WHITE"){df2$tmp=df2$prop.W}
        if(y=="BLACK"){df2$tmp=df2$prop.B}
        if(y=="ASIAN"){df2$tmp=df2$prop.A}}
      
      if(x=="Without listing attributes"){
        mod2=multinom(g.ethnicity ~tmp*h.ethnicity,data = df2)}
      if(x=="With listing attributes"){
        mod2=multinom(g.ethnicity ~tmp*h.ethnicity+avg.price+avg.rate+
                        room_type2+cum.cnt+zip.srp+log(amenity.sum)+
                        same_gender+gender.binary, data = df2)}
      
      dt.mod=as.data.frame(test(emtrends(mod2, ~h.ethnicity|g.ethnicity, var = "tmp")))
      dt.mod$cate=x
      dt.mod$tmp.trend.p=format(round(dt.mod$tmp.trend,digits = 3), nsmall = 3)
      dt.mod$SE.p=format(round(dt.mod$SE,digits = 3), nsmall = 3)
      dt.mod$p.value.p=format(round(dt.mod$p.value,digits = 3), nsmall = 3)
      dt.mod$p.value.p[dt.mod$p.value.p<.001]="< .001"
      dt.mod$p.value.p[dt.mod$p.value.p>=.001&dt.mod$p.value.p<.01]="< .01"
      dt.mod$p.value.p[dt.mod$p.value.p>=.01&dt.mod$p.value.p<.05]="< .05"
      dt.mod=dt.mod[dt.mod$g.ethnicity==y,]
      dt.all=rbind(dt.all, dt.mod)
    }
  }
  f.name=paste0("../output/",outfile_t2,"_",SRE.type,".csv")
  write.csv(dt.all, file=f.name, row.names=F)
}

SRE.table("cnt")
SRE.table("pct")
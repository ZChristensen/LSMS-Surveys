library(readr)
sect3_plantingw3 <- read_csv("~/GHA 2017/NGA_2015_GHSP-W3_v01_M_CSV/sect3_plantingw3.csv")

unittoduration = function(unit){
  if(is.na(unit)){return(NA)}
  if(unit==1){return(1/24)  }
  if(unit==2){return(1)  }
  if(unit==3){return(7)  }
  if(unit==4){return(14)  }
  if(unit==5){return(30.42)  }
  if(unit==6){return(365/4)  }
  if(unit==7){return(365/2)  }
  if(unit==8){return(365)  }
}

sect3_plantingw3$incomeduration = sapply(sect3_plantingw3$s3q21b,unittoduration)
sect3_plantingw3 = transform(sect3_plantingw3,dailyincome=s3q21a/incomeduration)
sect3_plantingw3$incomeduration2 = sapply(sect3_plantingw3$s3q24b,unittoduration)
sect3_plantingw3 = transform(sect3_plantingw3,dailyincome2=s3q24a/incomeduration2)
sect3_plantingw3$incomeduration3 = sapply(sect3_plantingw3$s3q34b,unittoduration)
sect3_plantingw3 = transform(sect3_plantingw3,dailyincome3=s3q34a/incomeduration3)
sect3_plantingw3$incomeduration4 = sapply(sect3_plantingw3$s3q37b,unittoduration)
sect3_plantingw3 = transform(sect3_plantingw3,dailyincome4=as.numeric(s3q37a)/incomeduration4)

psum <- function(...,na.rm=FALSE) { 
  rowSums(do.call(cbind,list(...)),na.rm=na.rm) }
sect3_plantingw3$wageincome = psum(
  sect3_plantingw3$dailyincome
  , sect3_plantingw3$dailyincome2
  , sect3_plantingw3$dailyincome3
  , sect3_plantingw3$dailyincome4
  ,na.rm= TRUE
  )
sect3_plantingw3$wagemissing = psum(
  is.na(sect3_plantingw3$dailyincome)
  , is.na(sect3_plantingw3$dailyincome2)
  , is.na(sect3_plantingw3$dailyincome3)
  , is.na(sect3_plantingw3$dailyincome4)
  ,na.rm= TRUE
)
sect3_plantingw3$wageincome[which(sect3_plantingw3$wagemissing==4)]=NA
describe(sect3_plantingw3$wageincome)

tab = data.table(sect3_plantingw3)
hhwage.tab = tab[,.(
  mean.income = mean(wageincome, na.rm=TRUE),
  median.income = median(wageincome, na.rm=TRUE),
  count = sum(!is.na(ea))
), by=.(state,hhid)]

mergeddata = merge(sect3_plantingw3,sect13_harvestw3)
mergeddata = merge(mergeddata,sect12_harvestw3)


mergeddata = merge(mergeddata,nga_householdgeovars_y3)
names(mergeddata)
write.csv(mergeddata,"~/GHA 2017/mergeddata.csv", row.names=FALSE, na="")

#add 2010 panel data
statadata2010 <- read.dta("~/GHA 2017/NGA_2010_GHSP_v02_M_STATA/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Post Harvest Wave 1/Household/sect12_harvestw1.dta")
statadatageo <- read.dta("~/GHA 2017/NGA_2010_GHSP_v02_M_STATA/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Geodata/NGA_HouseholdGeovariables_Y1.dta")

mergeddata2010 = merge(statadata2010,statadatageo)
names(mergeddata)
write.csv(mergeddata2010,"~/GHA 2017/mergeddata2010.csv", row.names=FALSE, na="")


#stunting
secta_harvestw3 <- read_csv("~/GHA 2017/NGA_2015_GHSP-W3_v01_M_CSV/secta_harvestw3.csv")
sect1_harvestw3 <- read_csv("~/GHA 2017/NGA_2015_GHSP-W3_v01_M_CSV/sect1_harvestw3.csv")
sect4a_harvestw3 <- read_csv("~/GHA 2017/NGA_2015_GHSP-W3_v01_M_CSV/sect4a_harvestw3.csv")

stuntingdata = merge(sect4a_harvestw3,secta_harvestw3)
stuntingdata = merge(stuntingdata, sect1_harvestw3)
stuntingdata$s1q6_month[which(stuntingdata$s1q6_month>12)] = NA
stuntingdata$age.months = (12 * (stuntingdata$saq13y - stuntingdata$s1q6_year)) + (stuntingdata$saq13m - stuntingdata$s1q6_month)
stuntingdata= subset(stuntingdata,age.months>=0 & age.months<61)
stuntingdata= merge(stuntingdata,hhtrack)
sweights = sum(stuntingdata$wt_w3v1,na.rm= TRUE)
weightratio=nrow(stuntingdata)/sweights
stuntingdata$rweights=stuntingdata$wt_w3v1*weightratio
weightsum = sum(stuntingdata$rweights)
#igrowup
weianthro<-read.table("~/GHA 2017/igrowup_R/weianthro.txt",header=T,sep="",skip=0)
lenanthro<-read.table("~/GHA 2017/igrowup_R/lenanthro.txt",header=T,sep="",skip=0)
bmianthro<-read.table("~/GHA 2017/igrowup_R/bmianthro.txt",header=T,sep="",skip=0)
hcanthro<-read.table("~/GHA 2017/igrowup_R/hcanthro.txt",header=T,sep="",skip=0)
acanthro<-read.table("~/GHA 2017/igrowup_R/acanthro.txt",header=T,sep="",skip=0)
ssanthro<-read.table("~/GHA 2017/igrowup_R/ssanthro.txt",header=T,sep="",skip=0)
tsanthro<-read.table("~/GHA 2017/igrowup_R/tsanthro.txt",header=T,sep="",skip=0)
wflanthro<-read.table("~/GHA 2017/igrowup_R/wflanthro.txt",header=T,sep="",skip=0)
wfhanthro<-read.table("~/GHA 2017/igrowup_R/wfhanthro.txt",header=T,sep="",skip=0)
source("~/GHA 2017/igrowup_R/igrowup_standard.r")
source("~/GHA 2017/igrowup_R/igrowup_restricted.r")

igrowup.restricted(FileLab = "ch", FilePath = "~/GHA 2017/igrowup_R",
                   mydf=stuntingdata, sex = s1q2
                   , age=age.months, age.month = TRUE
                   , weight=s4aq52
                   , lenhei=s4aq53
                   , sw=rweights
    )
z.scores <- read.csv("~/GHA 2017/igrowup_R/ch_z_rc.csv")
z.scores = subset(z.scores,flen==0)
z.scores = merge(z.scores,nga_householdgeovars_y3)
z.scores$stunted = z.scores$zlen<=-2
#Wasting
library(readr)
nga_householdgeovars_y3 <- read_csv("~/GHA 2017/NGA_2015_GHSP-W3_v01_M_CSV/nga_householdgeovars_y3.csv")
z.scores.w <- read.csv("~/GHA 2017/igrowup_R/ch_z_rc.csv")
z.scores.w = subset(z.scores.w,flen==0)
z.scores.w = merge(z.scores.w,nga_householdgeovars_y3)
z.scores.w$wasting = z.scores.w$zwei<=-2
wastingprev.tab = data.table(z.scores.w)[,.(
  mean.wast = mean(wasting, na.rm=TRUE)
),  by=.(LON_DD_MOD,LAT_DD_MOD)]
write.csv(wastingprev.tab,"~/GHA 2017/wasting2016.csv", row.names=FALSE, na="")

stuntingprev.tab = data.table(z.scores)[,.(
  mean.stunt = mean(stunted, na.rm=TRUE))
#), by=.(LON_DD_MOD,LAT_DD_MOD)]
, by =(state)]
stuntingprev.tab$mean.stunt = stuntingprev.tab$mean.stunt * 100
write.csv(stuntingprev.tab,"~/GHA 2017/stunting20162.csv", row.names=FALSE, na="")
#2010 stunting
secta_harvestw1 <- read.dta("~/GHA 2017/NGA_2010_GHSP_v02_M_STATA/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Post Harvest Wave 1/Household/secta_harvestw1.dta")
sect1_harvestw1 <- read.dta("~/GHA 2017/NGA_2010_GHSP_v02_M_STATA/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Post Harvest Wave 1/Household/sect1_harvestw1.dta")
sect4a_harvestw1 <- read.dta("~/GHA 2017/NGA_2010_GHSP_v02_M_STATA/NGA_2010_GHSP_v02_M_STATA/W1 public 2013/Post Harvest Wave 1/Household/sect4a_harvestw1.dta")

stuntingdataw1 = merge(sect4a_harvestw1,secta_harvestw1)
stuntingdataw1 = merge(stuntingdataw1, sect1_harvestw1)
stuntingdataw1$s1q6_month=as.numeric(stuntingdataw1$s1q6_month)
stuntingdataw1$s1q6_month[which(stuntingdataw1$s1q6_month>12)] = NA
stuntingdataw1$age.months = (12 * (stuntingdataw1$saq13y - stuntingdataw1$s1q6_year)) + (stuntingdataw1$saq13m - stuntingdataw1$s1q6_month)
stuntingdataw1= subset(stuntingdataw1,age.months>=0 & age.months<61)

stuntingdataw1= merge(stuntingdataw1,hhtrack)
sweights = sum(stuntingdataw1$wt_w1v1,na.rm= TRUE)
weightratio=nrow(stuntingdataw1)/sweights
stuntingdataw1$rweights=stuntingdataw1$wt_w1v1*weightratio
weightsum = sum(stuntingdataw1$rweights)





nrow(unique(nga_householdgeovars_y3[c("LON_DD_MOD", "LAT_DD_MOD")]))
nrow(unique(nga_householdgeovars_y3["ea"]))
nrow(unique(nga_householdgeovars_y3["hhid"]))
nrow(unique(nga_householdgeovars_y3[c("state", "zone", "sector","lga", "ea")]))

fragileareas = subset(sect3_plantingw3,state %in% c(8,35))
sect3_plantingw3$fragilearea = sect3_plantingw3$state %in% c(8,35)
library(data.table)
tab = data.table(sect3_plantingw3)
fragile.tab = tab[,.(
  mean.income = mean(dailyincome, na.rm=TRUE),
  median.income = median(dailyincome, na.rm=TRUE),
  count = sum(!is.na(ea))
), by=.(fragilearea, hhid)]
length(unique(sect3_plantingw3$hhid))
hist(log(subset(sect3_plantingw3,fragilearea==FALSE)$dailyincome))
hist(log(subset(sect3_plantingw3,fragilearea==TRUE)$dailyincome))
mergeddata = merge(sect3_plantingw3,nga_householdgeovars_y3)
coordinates(mergeddata) = ~LON_DD_MOD + LAT_DD_MOD
plot(mergeddata)
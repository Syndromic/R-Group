##Pull of MMWR Weekly Counts from cdc.data.gov written by Aaron Kite-Powell (akitepowell@gmail.com) and lots of help from stackover flow, etc..
library(scales)
library(plyr)
library(dplyr)
library(ggplot2)
library(RCurl)
library(caTools)
library(zoo)
library(stringr)
setwd("~/R")
source("date code.R")
integer_breaks <- function(n = 5, ...) {
  breaker <- pretty_breaks(n, ...)
  function(x) {
    breaks <- breaker(x)
    breaks[breaks == floor(breaks)]
  }
}
#Infrequently Reported Diseases 
URL2014 <- "https://data.cdc.gov/api/views/wcwi-x3uk/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Disease, MMWR.year, MMWR.week, Current.week)
d2014$Disease <- toupper(d2014$Disease)
d2014 <- arrange(d2014, Disease, MMWR.year, MMWR.week)
URL2015 <- "https://data.cdc.gov/api/views/pb4z-432k/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Disease, MMWR.year, MMWR.week, Current.week)
d2015$Disease <- toupper(d2015$Disease)
d2015 <- arrange(d2015, Disease, MMWR.year, MMWR.week)
URL2016 <- "https://data.cdc.gov/api/views/dwqk-w36f/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Disease, MMWR.year, MMWR.week, Current.week)
d2016$Disease <- toupper(d2016$Disease)
d2016 <- arrange(d2016, Disease, MMWR.year, MMWR.week)
d <- rbind(d2014, d2015, d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
d$c <- as.numeric(d$Current.week)
d <- d %>% mutate(Current.week = ifelse(is.na(Current.week),0,Current.week))
d$newweek <- str_pad(d$MMWR.week, width=2, pad="0")
d$yw <- paste(d$MMWR.year, d$newweek, sep="")

d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Disease, MMWR.year, MMWR.week)
d$Disease <- toupper(d$Disease)
#disease names are different bewteen years, try to clean some disease names up
#Encoding(d$Disease) <- "latin1"
#d$Disease <- iconv(d$Disease, "latin1", "ASCII", sub="")
#d$Disease <- gsub(":","",d$Disease)
#d$Disease <- gsub(",","",d$Disease)
#d$Disease <- gsub("\\*","",d$Disease)
d$Disease <- as.factor(d$Disease)
d$New_Disease[grepl("q fever|qfever",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"Q FEVER TOTAL"
d$New_Disease[grepl("influenza a",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"NOVEL INFLUENZA A"
d$New_Disease[grepl("VANCOMYCIN-INTERMEDIATE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"VISA"
d$New_Disease[grepl("VANCOMYCIN-RESISTANT",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"VIRA"
d$New_Disease[grepl("CRIMEAN-CONGO",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"CCHF"
d$New_Disease[grepl("EBOLA",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"EBOLA-HF"
d$New_Disease[grepl("GUANARITO",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"GUANARITO-HF"
d$New_Disease[grepl("JUNIN",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"JUNIN-HF"
d$New_Disease[grepl("LASSA",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"LASSA-HF"
d$New_Disease[grepl("LUJO",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"LUJO-HF"
d$New_Disease[grepl("MACHUPO",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"MACHUPO-HF"
d$New_Disease[grepl("MARBURG",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"MARBURG-HF"
d$New_Disease[grepl("SABIA-ASSOCIATED",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"SABIA-ASSOCIATED-HF"
d$New_Disease[grepl("VIRAL HEMORRHAGIC FEVER¶¶¶",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"VHF"
d$New_Disease[grepl("TYPHOID FEVER",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"TYPHOID FEVER"
d$New_Disease[grepl("TULAREMIA",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"TULAREMIA"
d$New_Disease[grepl("TRICHINELLOSIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"TRICHINELLOSIS"
d$New_Disease[grepl("TOXIC-SHOCK SYNDROME",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"TOXIC-SHOCK SYNDROME"
d$New_Disease[grepl("TETANUS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"TETANUS"
d$New_Disease[grepl("SYPHILIS, CONGENITAL",d$Disease, perl=TRUE, ignore.case=TRUE)& !grepl("(AGE <1 YR)", d$Disease, perl=TRUE, ignore.case=T)]<-"SYPHILIS, CONGENITAL"
d$New_Disease[grepl("STREPTOCOCCAL TOXIC-SHOCK",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"STREPTOCOCCAL TOXIC-SHOCK"
d$New_Disease[grepl("ST. LOUIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"ST. LOUIS ENCEPHALITIS VIRUS"
d$New_Disease[grepl("SMALLPOX",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"SMALLPOX"
d$New_Disease[grepl("SARS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"SARS-COV"
d$New_Disease[grepl("RUBELLA",d$Disease, perl=TRUE, ignore.case=TRUE)& !grepl("CONGENITAL", d$Disease, perl=TRUE, ignore.case=T)]<-"RUBELLA"
d$New_Disease[grepl("RUBELLA, CONGENITAL SYNDROME",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"RUBELLA, CONGENITAL SYNDROME"
d$New_Disease[grepl("RABIES, HUMAN",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"RABIES, HUMAN"
d$New_Disease[grepl("PSITTACOSIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"PSITTACOSIS"
d$New_Disease[grepl("POWASSAN",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"POWASSAN VIRUS"
d$New_Disease[grepl("POLIO VIRUS INFECTION|POLIOMYELITIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"POLIO"
d$New_Disease[grepl("PLAGUE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"PLAGUE"
d$New_Disease[grepl("MENINGOCOCCAL DISEASE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"MENINGOCOCCAL DISEASE TOTAL"
d$New_Disease[grepl("MEASLES",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"MEASLES"
d$New_Disease[grepl("LISTERIOSIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"LISTERIOSIS"
d$New_Disease[grepl("LEPTOSPIROSIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"LEPTOSPIROSIS"
d$New_Disease[grepl("INFLUENZA-ASSOCIATED",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"INFLUENZA-ASSOCIATED PEDIATRIC MORTALITY"
d$New_Disease[grepl("HEMOLYTIC UREMIC SYNDROME",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"HEMOLYTIC UREMIC SYNDROME"
d$New_Disease[grepl("HANTAVIRUS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"HANTAVIRUS"
d$New_Disease[grepl("HANSEN",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"HANSEN'S DISEASE"
d$New_Disease[grepl("HAEMOPHILUS INFLUENZAE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"HAEMOPHILUS INFLUENZAE INVASIVE DISEASE (AGE <5 YRS)"
d$New_Disease[grepl("FOODBORNE",d$Disease, perl=TRUE, ignore.case=TRUE) & grepl("BOTULISM, FOODBORNE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"BOTULISM, FOODBORNE"
d$New_Disease[grepl("INFANT",d$Disease, perl=TRUE, ignore.case=TRUE) & grepl("BOTULISM, INFANT",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"BOTULISM, INFANT"
d$New_Disease[grepl("BOTULISM",d$Disease, perl=TRUE, ignore.case=TRUE) & grepl("OTHER",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"BOTULISM, OTHER"
d$New_Disease[grepl("BOTULISM, TOTAL",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"BOTULISM, TOTAL"
d$New_Disease[grepl("EASTERN EQUINE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"EEE"
d$New_Disease[grepl("DIPHTHERIA",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"DIPHTHERIA"
d$New_Disease[grepl("CYCLOSPOROSIS|CYCLOSPORIASIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"CYCLOSPORIASIS"
d$New_Disease[grepl("CHOLERA",d$Disease, perl=TRUE, ignore.case=TRUE) & !grepl("NONCHOLERA",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"CHOLERA"
d$New_Disease[grepl("CHIKUANGUNYA|CHIKUNGUNYA",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"CHIKUNGUNYA"
d$New_Disease[grepl("BRUCELLOSIS",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"BRUCELLOSIS"
d$New_Disease[grepl("ZIKA",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"ZIKA VIRUS"
d$New_Disease[grepl("WESTERN EQUINE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"WESTERN EQUINE ENCEPHALITIS VIRUS"
d$New_Disease[grepl("POWASSAN",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"POWASSAN VIRUS"
d$New_Disease[grepl("LA CROSSE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"LA CROSSE VIRUS"
d$New_Disease[grepl("JAMESTOWN",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"JAMESTOWN CANYON VIRUS"
d$New_Disease[grepl("EASTERN EQUINE",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"EASTERN EQUINE ENCEPHALITIS VIRUS"
d$New_Disease[grepl("CALIFORNIA SEROGROUP",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"CALIFORNIA SEROGROUP VIRUS"
d$New_Disease[grepl("ANTHRAX",d$Disease, perl=TRUE, ignore.case=TRUE)]<-"ANTHRAX"

#unique(subset(d$Disease,grepl("JAMESTOWN", d$Disease, perl=TRUE, ignore.case=TRUE)))

newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(New_Disease) %>% mutate(fourteenwk.thresh=newthresh(Current.week,14),
          tenwk.thresh=newthresh(Current.week,10),
          fourteenwk.alert=Current.week>fourteenwk.thresh,
          tenwk.alert=Current.week>tenwk.thresh)
d <- group_by(d, New_Disease, MMWR.year) %>% mutate(cumulate=cumsum(Current.week),
          cumu10=cumulate+(tenwk.thresh-Current.week),
          cumu14=cumulate+(fourteenwk.thresh-Current.week))
p <- ggplot(d, aes(Date_Week, Current.week)) + geom_line() + 
  facet_wrap(~New_Disease, scales="free")+
  scale_y_continuous(breaks=integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Infrequent Disease Reports, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 month"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_infreq_diseases.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Salmonellosis MMWR Data
URL2014 <- "https://data.cdc.gov/api/views/52cr-rw4k/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Salmonellosis..Current.week)
d2014 <- arrange(d2014, Reporting.Area, MMWR.Year, MMWR.Week)
URL2015 <- "https://data.cdc.gov/api/views/d6kj-devz/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Salmonellosis..Current.week)
d2015 <- arrange(d2015, Reporting.Area, MMWR.Year, MMWR.Week)
URL2016 <- "https://data.cdc.gov/api/views/4qb4-rsd8/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Salmonellosis..Current.week)
d2016 <- arrange(d2016, Reporting.Area, MMWR.Year, MMWR.Week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015,d2016)
names(d)[names(d) == 'Salmonellosis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$MMWR.Week <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$MMWR.Week, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Salmonellosis Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("2 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_salmonella_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Campylobacteriosis MMWR Data
URL2015 <- "https://data.cdc.gov/api/views/s5s8-d82d/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Campylobacteriosis..Current.week)
d2015 <- arrange(d2015, Reporting.Area, MMWR.Year, MMWR.Week)
URL2016 <- "https://data.cdc.gov/api/views/4y34-2pku/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Campylobacteriosis..Current.week)
d2016 <- arrange(d2016, Reporting.Area, MMWR.Year, MMWR.Week)
d <- dplyr::bind_rows(d2015,d2016)
rm(nndss2015,nndss2016,d2015,d2016)
names(d)[names(d) == 'Campylobacteriosis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$MMWR.Week <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$MMWR.Week, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Campylobacteriosis Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("2 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_campy_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Babesiosis MMWR Data
URL2015 <- "https://data.cdc.gov/api/views/s5s8-d82d/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Babesiosis..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/4y34-2pku/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Babesiosis..Current.week)
d <- dplyr::bind_rows(d2015,d2016)
rm(nndss2015,nndss2016,d2015,d2016)
names(d)[names(d) == 'Babesiosis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$MMWR.Week <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$MMWR.Week, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Babesiosis Reports by Region/State, 2015-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("2 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_babesiosis_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Meningococcal MMWR Data
URL2014 <- "https://data.cdc.gov/api/views/y6uv-t34t/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Meningococcal.disease..invasive...All.serogroups..Current.week)
d2014 <- arrange(d2014, Reporting.Area, MMWR.Year, MMWR.Week)
URL2015 <- "https://data.cdc.gov/api/views/7pb7-w9us/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Meningococcal.disease..invasive...All.serogroups..Current.week)
d2015 <- arrange(d2015, Reporting.Area, MMWR.Year, MMWR.Week)
URL2016 <- "https://data.cdc.gov/api/views/93k9-hy54/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Meningococcal.disease..invasive...All.serogroups..Current.week)
d2016 <- arrange(d2016, Reporting.Area, MMWR.Year, MMWR.Week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015,d2016)
names(d)[names(d) == 'Meningococcal.disease..invasive...All.serogroups..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                                       tenwk.thresh=newthresh(c,10),
                                                       fourteenwk.alert=c>fourteenwk.thresh,
                                                       tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Meningococcal Reports by Region/State, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("1 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_mening_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#shigellosis MMWR Data
URL2014 <- "https://data.cdc.gov/api/views/52cr-rw4k/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Shigellosis..Current.week)
d2014 <- arrange(d2014, Reporting.Area, MMWR.Year, MMWR.Week)
URL2015 <- "https://data.cdc.gov/api/views/n3wf-wtep/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Shigellosis..Current.week)
d2015 <- arrange(d2015, Reporting.Area, MMWR.Year, MMWR.Week)
URL2016 <- "https://data.cdc.gov/api/views/xv7k-8e7s/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Shigellosis..Current.week)
d2016 <- arrange(d2016, Reporting.Area, MMWR.Year, MMWR.Week)

d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015,d2016)
names(d)[names(d) == 'Shigellosis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Shigellosis Reports by Region/State, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("1 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_shigellosis_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#E coli
URL2014 <- "https://data.cdc.gov/api/views/52cr-rw4k/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Shiga.toxin.producing.E..coli..STEC....Current.week)
d2014 <- arrange(d2014, Reporting.Area, MMWR.Year, MMWR.Week)
URL2015 <- "https://data.cdc.gov/api/views/n3wf-wtep/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Shiga.toxin.producing.E..coli..STEC....Current.week)
d2015 <- arrange(d2015, Reporting.Area, MMWR.Year, MMWR.Week)
URL2016 <- "https://data.cdc.gov/api/views/xv7k-8e7s/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Shiga.toxin.producing.E..coli..STEC....Current.week)
d2016 <- arrange(d2016, Reporting.Area, MMWR.Year, MMWR.Week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015,d2016)
names(d)[names(d) == 'Shiga.toxin.producing.E..coli..STEC....Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Shiga-toxin Producing E coli Reports by Region/State, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("1 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_stec_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Cryptosporidiosis MMWR Data 
URL2014 <- "https://data.cdc.gov/api/views/b36e-ru3r/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWRYear, MMWRWeek, Cryptosporidiosis..Current.week)
d2014 <- arrange(d2014, Reporting.Area, MMWRYear, MMWRWeek)
URL2015 <- "https://data.cdc.gov/api/views/9n3x-apcd/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWRYear, MMWRWeek, Cryptosporidiosis..Current.week)
d2015 <- arrange(d2015, Reporting.Area, MMWRYear, MMWRWeek)
URL2016 <- "https://data.cdc.gov/api/views/kikd-77zw/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Cryptosporidiosis..Current.week)
d2016 <- arrange(d2016, Reporting.Area, MMWR.Year, MMWR.Week)

d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015,d2016)
names(d)[names(d) == 'Cryptosporidiosis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWRWeek, width=2, pad="0")
d$yw <- paste(d$MMWRYear, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Cryptosporidiosis Reports by Region/State, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("1 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_crypto_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Dengue Total
URL2014 <- "https://data.cdc.gov/api/views/b36e-ru3r/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWRYear, MMWRWeek, Dengue.Fever...Current.week, Dengue.Hemorrhagic.Fever...Current.week)
d2014 <- arrange(d2014, Reporting.Area, MMWRYear, MMWRWeek)
URL2015 <- "https://data.cdc.gov/api/views/9n3x-apcd/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWRYear, MMWRWeek, Dengue...Current.week,Dengue.Severe..Current.week)
d2015 <- arrange(d2015, Reporting.Area, MMWRYear, MMWRWeek)
URL2016 <- "https://data.cdc.gov/api/views/kikd-77zw/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Dengue...Current.week,Dengue.Severe..Current.week)
d2016 <- arrange(d2016, Reporting.Area, MMWR.Year, MMWR.Week)
names(d2014)[names(d2014)=="Dengue.Fever...Current.week"] <- "Dengue...Current.week"
names(d2014)[names(d2014)=="Dengue.Hemorrhagic.Fever...Current.week"] <- "Dengue.Severe..Current.week"
names(d2016)[names(d2016)=="MMWR.Year"] <- "MMWRYear"
names(d2016)[names(d2016)=="MMWR.Week"] <- "MMWRWeek"
d <- rbind(d2014,d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015,d2016)
d <- d %>% mutate(Dengue...Current.week = ifelse(is.na(Dengue...Current.week),0,Dengue...Current.week)) %>%
  mutate(Dengue.Severe..Current.week = ifelse(is.na(Dengue.Severe..Current.week),0,Dengue.Severe..Current.week)) %>%
  mutate(Dengue_Total = Dengue...Current.week + Dengue.Severe..Current.week)
names(d)[names(d) == 'Dengue_Total'] <- 'c'
d$c <- as.numeric(d$c)
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWRYear) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWRWeek, width=2, pad="0")
d$yw <- paste(d$MMWRYear, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Dengue Reports by Region/State, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("1 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_dengue_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Pertussis by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/8rkx-vimh/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Pertussis..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/d69q-iyrb/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Pertussis..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/bfe6-2gyq/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Pertussis..Current.week)
d <- dplyr::bind_rows(d2014, d2015, d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015, d2016)
names(d)[names(d) == 'Pertussis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                      cumu10=cumulate+(tenwk.thresh-c),
                                                      cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Pertussis Reports by Region/State, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_pertussis_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Mumps MMWR
URL2014 <- "https://data.cdc.gov/api/views/8rkx-vimh/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Mumps..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/d69q-iyrb/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Mumps..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/bfe6-2gyq/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Mumps..Current.week)
d <- dplyr::bind_rows(d2014, d2015, d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015, d2016)
names(d)[names(d) == 'Mumps..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + facet_wrap(~Reporting.Area, scales="free")+scale_y_continuous(breaks = integer_breaks())+ggtitle("Reporting Varies by State - MMWR Mumps Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")
p+theme(panel.background = element_rect(fill="white"))+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+scale_x_date(breaks=date_breaks("1 months"))+geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_mumps_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Malaria by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/y6uv-t34t/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Malaria..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/7pb7-w9us/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Malaria..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/93k9-hy54/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Malaria..Current.week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
names(d)[names(d) == 'Malaria..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Malaria Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_malaria_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Legionellosis by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/23gt-ssfe/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Legionellosis..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/ydsy-yh5w/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Legionellosis..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/yqwx-bvu7/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Legionellosis..Current.week)
d <- dplyr::bind_rows(d2014,d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
names(d)[names(d) == 'Legionellosis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Legionellosis Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_legionellosis_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Hepatitis A by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/rg4j-6mcc/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Hepatitis..viral..acute...type.A..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/65xe-6neq/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Hepatitis..viral..acute...type.A..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/7vnz-2mjz/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Hepatitis..viral..acute...type.A..Current.week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014, d2015,d2016)
names(d)[names(d) == 'Hepatitis..viral..acute...type.A..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y") 
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Hepatitis A Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_hep_a_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Hepatitis B, Acute by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/rg4j-6mcc/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Hepatitis..viral..acute...type.B..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/65xe-6neq/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Hepatitis..viral..acute...type.B..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/7vnz-2mjz/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week, Hepatitis..viral..acute...type.B..Current.week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
names(d)[names(d) == 'Hepatitis..viral..acute...type.B..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Hepatitis B, Acute Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_hep_bacute_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Hepatitis C, Acute by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/rg4j-6mcc/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Hepatitis..viral..acute...type.C..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/65xe-6neq/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Hepatitis..viral..acute...type.C..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/7vnz-2mjz/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Hepatitis..viral..acute...type.C..Current.week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
names(d)[names(d) == 'Hepatitis..viral..acute...type.C..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Hepatitis c, Acute Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("1 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_hep_cacute_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Giardiasis by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/9ix3-ryt6/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Giardiasis..Current.week)
URL2015 <- "https://data.cdc.gov/api/views/mpdg-hf57/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Giardiasis..Current.week)
URL2016 <- "https://data.cdc.gov/api/views/afja-b25e/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Giardiasis..Current.week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
names(d)[names(d) == 'Giardiasis..Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))
d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + facet_wrap(~Reporting.Area, scales="free")+scale_y_continuous(breaks = integer_breaks())+ggtitle("Reporting Varies by State - MMWR Giardiasis Reports by Region/State, 2014-2016")+labs(x="Week Number", y="Weekly Counts")
p+theme(panel.background = element_rect(fill="white"))+theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+scale_x_date(breaks=date_breaks("1 months"))+geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_giardiasis_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Varicella by jurisdiction
URL2014 <- "https://data.cdc.gov/api/views/ig4m-ub43/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Varicella..chickenpox...Current.week)
URL2015 <- "https://data.cdc.gov/api/views/nf22-99pv/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Varicella..chickenpox...Current.week)
URL2016 <- "https://data.cdc.gov/api/views/tj26-bdgd/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Varicella..chickenpox...Current.week)
d <- dplyr::bind_rows(d2014, d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
names(d)[names(d) == 'Varicella..chickenpox...Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Varicella Reports by Region/State, 2014-2016")+
  labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_varicella_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Vibrios by jurisdiction - only 2015 is available
URL2015 <- "https://data.cdc.gov/api/views/nf22-99pv/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Vibriosis...Current.week)
URL2016 <- "https://data.cdc.gov/api/views/tj26-bdgd/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.Year, MMWR.Week,Vibriosis...Current.week)
d <- rbind(d2015,d2016)
rm(nndss2015,nndss2016,d2015,d2016)
names(d)[names(d) == 'Vibriosis...Current.week'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- group_by(d, Reporting.Area, MMWR.Year) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.Week, width=2, pad="0")
d$yw <- paste(d$MMWR.Year, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
d <- arrange(d, Reporting.Area, MMWR.Year, MMWR.Week)
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Vibrio Reports by Region/State, 2015-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("2 months"))+
  geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_vibrio_bystate.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

#Pneumonia and Influenza Mortalty by City
URL2014 <- "https://data.cdc.gov/api/views/qpap-3u8w/rows.csv?accessType=DOWNLOAD"
x2014 <- getURL(URL2014, ssl.verifypeer = FALSE)
nndss2014 <- read.csv(textConnection(x2014), strip.white=T, stringsAsFactors=F)
d2014 <- nndss2014 %>% select(Reporting.Area, MMWR.YEAR, MMWR.WEEK,P.I..Total)
URL2015 <- "https://data.cdc.gov/api/views/7esm-uptm/rows.csv?accessType=DOWNLOAD"
x2015 <- getURL(URL2015, ssl.verifypeer = FALSE)
nndss2015 <- read.csv(textConnection(x2015), strip.white=T, stringsAsFactors=F)
d2015 <- nndss2015 %>% select(Reporting.Area, MMWR.YEAR, MMWR.WEEK,P.I..Total)
URL2016 <- "https://data.cdc.gov/api/views/rpjd-ejph/rows.csv?accessType=DOWNLOAD"
x2016 <- getURL(URL2016, ssl.verifypeer = FALSE)
nndss2016 <- read.csv(textConnection(x2016), strip.white=T, stringsAsFactors=F)
d2016 <- nndss2016 %>% select(Reporting.Area, MMWR.YEAR, MMWR.WEEK,P.I..Total)
d <- dplyr::bind_rows(d2014,d2015,d2016)
rm(nndss2014,nndss2015,nndss2016,d2014,d2015,d2016)
names(d)[names(d) == 'P.I..Total'] <- 'c'
d$c <- as.numeric(d$c)
d <- d %>% mutate(c = ifelse(is.na(c),0,c))
newthresh <- function(x,days){
  thresh <-runmean(x, days, align="right")+2*runsd(x, days,endrule="sd",align="right")
  thresh[is.na(thresh)]<-x[is.na(thresh)]
  return(thresh)
}
d <- d %>% group_by(Reporting.Area) %>% mutate(fourteenwk.thresh=newthresh(c,14),
                                               tenwk.thresh=newthresh(c,10),
                                               fourteenwk.alert=c>fourteenwk.thresh,
                                               tenwk.alert=c>tenwk.thresh)
d <- d %>% group_by(Reporting.Area, MMWR.YEAR) %>% mutate(cumulate=cumsum(c),
                                                       cumu10=cumulate+(tenwk.thresh-c),
                                                       cumu14=cumulate+(fourteenwk.thresh-c))

d$newweek <- str_pad(d$MMWR.WEEK, width=2, pad="0")
d$yw <- paste(d$MMWR.YEAR, d$newweek, sep="")
d <- merge(d, date_week, by.x="yw", by.y="mmwr_year_week")
d$Date_Week <- as.Date(d$Date_Week, "%m/%d/%Y")
p <- ggplot(d, aes(Date_Week, c)) + geom_line() + 
  facet_wrap(~Reporting.Area, scales="free_y")+
  scale_y_continuous(breaks = integer_breaks())+
  ggtitle("Reporting Varies by State - MMWR Pneumonia and Influenza Mortality Reports by City, 2014-2016")+labs(x="Week Number", y="Weekly Counts")+
  theme(panel.background = element_rect(fill="white"))+
  theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank(),panel.border=element_blank())+
  scale_x_date(breaks=date_breaks("3 months"))+geom_point(data=subset(d,fourteenwk.alert == "TRUE"),colour="red")+
  theme(axis.text.x=element_text(angle=-90,size=rel(0.8),hjust=0))
ggsave(filename="mmwr_P_and_I_mort_bycity.png", path="C:/Users/Aaron/Documents/R/Rplots/cdc mmwr", width=23.2, height=12.1)

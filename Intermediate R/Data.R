setwd("/Users/Harold/Desktop/SHINE")
set.seed(123)

n <- 1367000
Date <- seq.Date(from = as.Date("2013-01-01"), to = as.Date("2016-09-28"), by="day")
Date <- rep(Date, length.out=n)

Zip <- c(46201,46202,46203,46204,46206,46207,46208,46209,46210,46211,46213,46214,46216,46217,46218,46219,46220,46221,46222,46223,46224,46225,46226,46227,46228,46230,46231,46234,46235,46236,46237,46239,46240,46241,46242,46244,46247,46249,46250,46251,46253,46254,46255,46256,46259,46260,46262,46266,46268,46274,46275)
Zip <- rep(Zip, length.out=n)
Sex <- rep(c("Female", "Male"), length.out=n)
Age <- sample(0:80, n, replace=TRUE)

MedRecNo <- sample(0:1000, n, replace=TRUE)
Hospital <- rep(c("HospA", "HospB", "HospC", "HospD", "HospE"), length.out=n)

Chief_Complaint <- rep(c("ALCOHOL POISONING","EXTREMELY TIPSY, ETOH, DEHYDRATED","N/V/D","CRAMP SPASM","DIARRHEA UNSPECIFIED","VOMITING WITHOUT NAUSEA","FEVER, HA","SORE THROAT","FEVER,HA,SORE THROAT","RED SPOTS","RED BLISTER","PAROTITIS","POSSIBLE MUMPS","CUT WRISTS","SUICIDAL"), length.out=n)
Discharge_Diagnosis <- rep(c("T51.91XA",NA,NA,NA,NA,NA,NA,NA,"J11.1",NA,NA,"B26.0",NA,NA,"R45.851"), length.out=n)

df<- data.frame(Date, MedRecNo, Zip, Sex, Age, Chief_Complaint, Discharge_Diagnosis, Hospital)

df_export<-df[sample(nrow(df), 500000), ]

library(readr)

write_delim(df_export, path="/Users/Harold/Desktop/SHINE/temp.csv", delim=",")
new_data<-read_delim("/Users/Harold/Desktop/SHINE/temp.csv", delim=",")

read_excel(path, sheet = 1, col_names = TRUE, col_types = NULL, 
           na = "", skip = 0)




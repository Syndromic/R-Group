setwd("M:/LEEDS/Surveillance Reports/Mardi Gras 2016/R")

library(surveillance)

#read in data
MardiGras<-read.table("MardiGrasReg1_2016.txt",header=TRUE)


#create disProgObj
giDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$gi1,state=MardiGras$State,freq=365)
feverDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$fever1,state=MardiGras$State,freq=365)
iliDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$ili1,state=MardiGras$State,freq=365)
lrtiDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$lrti1,state=MardiGras$State,freq=365)
urtiDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$urti1,state=MardiGras$State,freq=365)
sstiDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$ssti1,state=MardiGras$State,freq=365)
alcoholDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$alcohol1,state=MardiGras$State,freq=365)
drugDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$drug1,state=MardiGras$State,freq=365)
injDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$inj1,state=MardiGras$State,freq=365)
violenceDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$violence1,state=MardiGras$State,freq=365)
mvaDisProg1<-create.disProg(week=MardiGras$Day,observed=MardiGras$mva1,state=MardiGras$State,freq=365)


#convert to sts obj
gists1=disProg2sts(giDisProg1)
feversts1=disProg2sts(feverDisProg1)
ilists1=disProg2sts(iliDisProg1)
lrtists1=disProg2sts(lrtiDisProg1)
urtists1=disProg2sts(urtiDisProg1)
sstists1=disProg2sts(sstiDisProg1)
alcoholsts1=disProg2sts(alcoholDisProg1)
drugsts1=disProg2sts(drugDisProg1)
injsts1=disProg2sts(injDisProg1)
violencests1=disProg2sts(violenceDisProg1)
mvasts1=disProg2sts(mvaDisProg1)


#run ears C2
giears1<-earsC(gists1,control=list(method="C2"))
feverears1<-earsC(feversts1,control=list(method="C2"))
iliears1<-earsC(ilists1,control=list(method="C2"))
lrtiears1<-earsC(lrtists1,control=list(method="C2"))
urtiears1<-earsC(urtists1,control=list(method="C2"))
sstiears1<-earsC(sstists1,control=list(method="C2"))
alcoholears1<-earsC(alcoholsts1,control=list(method="C2"))
drugears1<-earsC(drugsts1,control=list(method="C2"))
injears1<-earsC(injsts1,control=list(method="C2"))
violenceears1<-earsC(violencests1,control=list(method="C2"))
mvaears1<-earsC(mvasts1,control=list(method="C2"))


#plot ears
plot(giears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(feverears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(iliears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(lrtiears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(urtiears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(sstiears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(alcoholears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(drugears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(injears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(violenceears1,legend.opts=list(horiz=TRUE,x="topright"))
#plot(mvaears1,legend.opts=list(horiz=TRUE,x="topright"))


lrtiears1@alarm
lrtiears1@observed
lrtiears1@upperbound

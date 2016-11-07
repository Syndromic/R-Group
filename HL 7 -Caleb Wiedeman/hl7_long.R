#Program (maybe function?) reads in text files from a directory(presumed to be HL7 format) 
#and outputs as long CSV files for selected variables
hl7_long<-function(hl7_file="unknown"){
  
require("tidyverse")
require("sqldf")
require("plyr")
require("reshape2")
  
  if (hl7_file[1]=="unknown"){
    stop("hL7_file not defined in function argument")
  }

#Import HL7 file, specifying all column formats as character, going out to 56 possible columns

hl7_test<-as_tibble(read.delim(file=file.choose(), header=FALSE, sep = "|", 
                      col.names=c("x1","x2","x3","x4","x5","x6","x7","x8","x9","x10","x11","x12","x13",
                                  "x14","x15","x16","x17","x18","x19","x20","x21","x22","x23","x24",
                                  "x25","x26","x27","x28","x29","x30","x31","x32","x33","x34","x35",
                                  "x36","x37","x38","x39","x40","x41","x42","x43","x44","x45","x46",
                                  "x47","x48","x49","x50","x51","x52","x53","x54","x55","x56"),
                      colClasses=list("character"), fill=TRUE))


Z
hl7_test<- tibble::add_column(hl7_test, MSH10_message_control_id = hl7_test$x10)


# Just in case x10 is in use by another HL7 segment 
#cleaning it up for all non-MSH segments   

for( i in 1:length(hl7_test$x1)){
  if(hl7_test$x1[i] != 'MSH'){
  hl7_test$MSH10_message_control_id[i] = NA}
}

#Lagging the message control id to assign a unique ID to all segments

for (i in 2:length(hl7_test$MSH10_message_control_id)){
  if(is.na(hl7_test$MSH10_message_control_id[i])){
         hl7_test$MSH10_message_control_id[i] = hl7_test$MSH10_message_control_id[i-1]}
  else{hl7_test$MSH10_message_control_id[i]=hl7_test$x10[i]}
}

#Working with each segment seperately

MSH<-filter(hl7_test, x1 == "MSH")
EVN<-filter(hl7_test, x1 == "EVN")
PID<-filter(hl7_test, x1 == "PID")
PV1<-filter(hl7_test, x1 == "PV1")  
PV2<-filter(hl7_test, x1 == "PV2")
DG1<-filter(hl7_test, x1 == "DG1")
OBX<-filter(hl7_test, x1 == "OBX")

rm(hl7_test)

#Pulling out MSH fields of interest
MSH<-transmute(MSH, 
               msg_ctrl_id = MSH10_message_control_id,
               MSH3 = x3,
               MSH4 = x4,
               MSH7 = x7,
               MSH9 = x9,
               MSH10 = x10,
               MSH11 = x11,
               MSH12 = x12,
               MSH21 = x21)

# Pulling out EVN fields of interest

EVN<-transmute(EVN,
               msg_ctrl_id = MSH10_message_control_id,
               EVN2 = x3,
               EVN7 = x8)

# Pulling out PID fields of interest, assuming only 1 PID segment
PID<-transmute(PID,
               msg_ctrl_id = MSH10_message_control_id,
               PID1 = x2,
               PID3 = x4,
               PID8 = x9,
               PID10 = x11,
               PID18 = x19,
               PID22 = x23,
               PID29 = x30,
               PID30 = x31)

# Pulling out PV1 fields of interest

PV1<-transmute(PV1,
               msg_ctrl_id = MSH10_message_control_id,
               PV1_2 = x3,
               PV1_4 = x5,
               PV1_14 = x15,
               PV1_18  = x18,
               PV1_19 = x19,
               PV1_36 = x36,
               PV1_44 = x45,
               PV1_45 = x46)

# Pulling out PV2 fields of interest 
PV2<-transmute(PV2, 
               msg_ctrl_id = MSH10_message_control_id,
               PV2_3 = x4,
               PV2_38 = x39)

# Pulling out and transposing DG1 fields
DG1<-transmute(DG1, 
               msg_ctrl_id = MSH10_message_control_id,
               DG1_1 = formatC(x2, width=3, mode="integer", format="d", flag="0"),
               DG1_3 = x4,
               DG1_5 = x6,
               DG1_6 = x7)

DG1<-melt(DG1, id=c("msg_ctrl_id","DG1_1"))

DG1_3<-filter(DG1, variable == "DG1_3")
DG1_5<-filter(DG1, variable == "DG1_5")
DG1_6<-filter(DG1, variable == "DG1_6")


DG1_3t<-dcast(DG1_3, msg_ctrl_id~variable+DG1_1)
DG1_5t<-dcast(DG1_5, msg_ctrl_id~variable+DG1_1)
DG1_6t<-dcast(DG1_6, msg_ctrl_id~variable+DG1_1)

DG1_final<-select(DG1_3t, 1)

for( i in 2:max(DG1$DG1_1)){

  DG1_final<-join(DG1_final, DG1_3t[,c(1,i)], type="left", by = "msg_ctrl_id")
  DG1_final<-join(DG1_final, DG1_5t[,c(1,i)], type="left", by = "msg_ctrl_id")
  DG1_final<-join(DG1_final, DG1_6t[,c(1,i)], type="left", by = "msg_ctrl_id")

}

rm(DG1, DG1_3t, DG1_5t, DG1_6t, DG1_3, DG1_5, DG1_6)
# Pulling out and transposing OBX fields

OBX<-transmute(OBX,
          msg_ctrl_id = MSH10_message_control_id,
          OBX1 = x2,
          OBX2 = x3,
          OBX3 = x4,
          OBX5 = x6,
          OBX6 = x7,
          OBX11 = x12,
          OBX14 = x15)

OBX<-melt(OBX, id=c("msg_ctrl_id","OBX1"))

OBX2<-filter(OBX, variable == "OBX2")
OBX3<-filter(OBX, variable == "OBX3")
OBX5<-filter(OBX, variable == "OBX5")
OBX6<-filter(OBX, variable == "OBX6")
OBX11<-filter(OBX, variable == "OBX11")
OBX14<-filter(OBX, variable == "OBX14")

OBX2_t<-dcast(OBX2, msg_ctrl_id~variable+OBX1)
OBX3_t<-dcast(OBX3, msg_ctrl_id~variable+OBX1)
OBX5_t<-dcast(OBX5, msg_ctrl_id~variable+OBX1)
OBX6_t<-dcast(OBX6, msg_ctrl_id~variable+OBX1)
OBX11_t<-dcast(OBX11, msg_ctrl_id~variable+OBX1)
OBX14_t<-dcast(OBX14, msg_ctrl_id~variable+OBX1)


OBX_final<-select(OBX2_t,1)

for( i in 2:max(OBX$OBX1)){
  
  OBX_final<-join(OBX_final, OBX2_t[,c(1,i)], type="left", by = "msg_ctrl_id")
  OBX_final<-join(OBX_final, OBX3_t[,c(1,i)], type="left", by = "msg_ctrl_id")
  OBX_final<-join(OBX_final, OBX5_t[,c(1,i)], type="left", by = "msg_ctrl_id")
  OBX_final<-join(OBX_final, OBX6_t[,c(1,i)], type="left", by = "msg_ctrl_id")
  OBX_final<-join(OBX_final, OBX11_t[,c(1,i)], type="left", by = "msg_ctrl_id")
  OBX_final<-join(OBX_final, OBX14_t[,c(1,i)], type="left", by = "msg_ctrl_id")
  
}

rm(OBX, OBX2, OBX3, OBX5, OBX6, OBX11, OBX14, OBX2_t, OBX3_t, OBX5_t, OBX6_t, OBX11_t, OBX14_t)

#Can merge all columns together by joining on msg_ctrl_id

final<-as_tibble(join_all(list(MSH,EVN,PID,PV1,PV2,DG1_final,OBX_final), by= "msg_ctrl_id", type="left"))

#Export out as a CSV for analysis in something else

file_name<-strsplit(hl7_file,".", fixed=TRUE)[[1]]

new_txt<-paste(file_name[1],".csv", sep="")

write.csv(final,file = new_txt)

return(new_txt)

rm(DG1_final, EVN, final, i, MSH, OBX_final, PID, PV1, PV2, file_name)

}

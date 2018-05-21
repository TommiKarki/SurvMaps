
setwd("C:/Data/Myproject/SSI2013_2014/R_files/")
haissip<-as.data.table(haissiop)

#Age as numeric (creates NAs for UNK)
system.time(haissiop$Age2<-as.numeric(haissiop$Age))

system.time(haissip$Age2<-as.numeric(haissip$Age))
system.time(haissip$Age3[as.numeric(haissip$Age)])


#Create a new RecordId for the Norwegians 2012, 2013 and 2014
haissi$RecordId<-ifelse(haissi$DataSource=="NO-NOIS" & haissi$DateUsedForStatistics%in%c("2012", "2013", "2014"), paste(haissi$ReportingCountry, haissi$DateUsedForStatistics,
                                                            haissi$HospitalId, sep=""), haissi$RecordId)

#Create a new RecordId for the Norwegians 2011
haissi$RecordId<-ifelse(haissi$DataSource=="NO-NOIS" & haissi$DateUsedForStatistics%in%c("2011"), paste(haissi$HospitalId, haissi$DateUsedForStatistics,
                                                                                                                sep="-"), haissi$RecordId)

#Create a new RecordId for the Dutch
haissi$RecordId<-ifelse(haissi$DataSource=="NL-HAISSI", paste(haissi$DateUsedForStatistics,
                                                              haissi$HospitalId, sep=""), haissi$RecordId)


#Infection date and year
haissiopinf$infyear<-as.numeric(substr(haissiopinf$DateOfOnset, 1, 4))
haissiopinf$infyear<-ifelse(haissiopinf$infyear=="UNK", NA, haissiopinf$infyear)
haissiopinf$DateOfOnset<-as.Date(fastPOSIXct(haissiopinf$DateOfOnset))

# table(haissiopinf$infyear)

#Other dates
haissiop$opyear<-as.numeric(substr(haissiop$DateOfOperation, 1, 4))
haissiop$DateOfOperation<-as.Date(fastPOSIXct(haissiop$DateOfOperation))
# table(haissiop$opyear)

haissiop$DateOfHospitalDischarge<-as.Date(fastPOSIXct(haissiop$DateOfHospitalDischarge))

#Duplicated infections for same patient (by recordid)?
# length(unique(haissiopinf$RecordId))

# length(unique(haissiop$RecordId))

#Duplicated infections for the same patient (by parentid)
# length(unique(haissiopinf$ParentId))

#Duplicated infections for the same patient (by parentid, dateofonset)
#To do a proper selection of the duplicates, add Date of operation to the Infection data including the onset time
haissiopinf$DateOfOperation<-haissiop$DateOfOperation[match(haissiopinf$ParentId, haissiop$RecordId)]
haissiopinf$OnsetTime<-haissiopinf$DateOfOnset-haissiopinf$DateOfOperation
#To order the the file correctly for exlusions, create a numerical for infection type with 4 as unknown
haissiopinf$Type<-ifelse(haissiopinf$SSIType=="Unk", 4,
                         ifelse(haissiopinf$SSIType=="S", 3,
                                ifelse(haissiopinf$SSIType=="D", 2,
                                       ifelse(haissiopinf$SSIType=="O", 1, NA))))
haissiopinf<-haissiopinf[order(haissiopinf$ParentId, haissiopinf$Type),]

#Select from haissiopinfres only those that will be included also in the infection level!!!
haissiopinfres<-haissiopinfres[!haissiopinfres$ParentId%in%haissiopinf$RecordId[duplicated(haissiopinf$ParentId)],]
                    
#Select only the first occurrence (note that might not always work 100% perfectly if negative/false onset times; 
# for 2011-2014 yes works just fine) and drop the "type" variable only used for correct ordering
haissiopinf<-haissiopinf[!duplicated(haissiopinf$ParentId), -11]

# length(unique(paste(haissiopinf$ParentId, haissiopinf$DateOfOnset, sep="")))
# print(haissiopinf$dupl[duplicated(haissiopinf$dupl)])

#Delete the operations and the infections/microorganismism linking to them from <-2011 and 2014->
haissiopinfres<-haissiopinfres[haissiopinfres$ParentId%in%haissiopinf$RecordId[haissiopinf$ParentId%in%haissiop$RecordId[haissiop$opyear>2010 
                                                                                                                         & haissiop$opyear<2015]],]
haissiopinf<-haissiopinf[haissiopinf$ParentId%in%haissiop$RecordId[haissiop$opyear>2010 & haissiop$opyear<2015],]
haissiop<-haissiop[haissiop$opyear>2010,]
haissiop<-haissiop[haissiop$opyear<2015,]


#SSI for each that have an infection record, no if no infection record
haissiop$SSI<-ifelse(haissiop$RecordId%in%haissiopinf$ParentId, 1,0)

#Infection type to haissiop level
haissiop$SSIType<-haissiopinf$SSIType[match(haissiop$RecordId, haissiopinf$ParentId)]

#Infection date to haissiop level
# head(haissiopinf)
haissiop$DateOfOnset<-haissiopinf$DateOfOnset[match(haissiop$RecordId, haissiopinf$ParentId)]
# table(is.na(haissiop$DateOfOnset), haissiop$SSI)

#Infection year to haissiop level
haissiop$infyear<-haissiopinf$infyear[match(haissiop$RecordId, haissiopinf$ParentId)]

#Calculate post-op stay
haissiop$postdays<-haissiop$DateOfHospitalDischarge-haissiop$DateOfOperation

# #Check of infections within 30 days and within one year
# head(haissiop)
haissiop$OnsetTime<-haissiop$DateOfOnset-haissiop$DateOfOperation
# mean(haissiop$OnsetTime, na.rm=T)
# max(haissiop$OnsetTime, na.rm=T)
# min(haissiop$OnsetTime, na.rm=T)
# 
# #Check for the cases where onset before operation
# ssicheck<-subset(haissiop, SSI==1 & OnsetTime<0)
# ssicheck<-subset(haissiop, SSI==1 & is.na(OnsetTime))
# head(ssicheck)
# rm(ssicheck)

# tapply(haissiop$OnsetTime, haissiop$SurgicalSiteInfection, simplify=T, FUN="mean", na.rm=T)

#check for cases by followup time of infections
# tapply(haissiop$OnsetTime, haissiop$OPCode, simplify=T, FUN="mean", na.rm=T)
# tapply(haissiop$OnsetTime, haissiop$OPCode, simplify=T, FUN="median", na.rm=T)
# tapply(haissiop$OnsetTime, haissiop$OPCode, simplify=T, FUN="max", na.rm=T)

# table(haissiop$SSI)
# table(haissiop$SSIType, haissiop$SSI, useNA="always")
#Recode as non-infections those where the followup time too long taking into account 90 DAYS for HPRO/KPRO for the D/O infections using a Custom C++ function:
haissiop$SSI<-ifSSI(haissiop$OnsetTime, haissiop$OPCode, haissiop$SSIType, haissiop$SSI)

#Or use the slower standard with taking notice that for some infections onset time < 0 days
#Recode the cases where case before operation or onset date missing to non-cases
# haissiop$SSI<-ifelse((haissiop$SSI==1 & haissiop$OnsetTime<0) | (haissiop$SSI==1 & is.na(haissiop$OnsetTime)), 0,
# haissiop$SSI)

# haissiop$SSI3<-ifelse((haissiop$OnsetTime>30 & haissiop$OPCode=="CHOL") |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="COLO") |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="CSEC") |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="REC") |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="LAM") |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="CABG" & haissiop$SSIType=="S")|
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="CBGB" & haissiop$SSIType=="S")|
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="CBGC" & haissiop$SSIType=="S")|
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="CABG" & haissiop$SSIType%in%c("D", "O")) |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="CBGB" & haissiop$SSIType%in%c("D", "O")) |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="CBGC" & haissiop$SSIType%in%c("D", "O")) |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="HPRO" & haissiop$SSIType=="S") |
#                        (haissiop$OnsetTime>30 & haissiop$OPCode=="KPRO" & haissiop$SSIType=="S") |
#                        (haissiop$OnsetTime>90 & haissiop$OPCode=="HPRO" & haissiop$SSIType%in%c("D", "O")) |
#                        (haissiop$OnsetTime>90 & haissiop$OPCode=="KPRO" & haissiop$SSIType%in%c("D", "O"))|
#                        haissiop$SSI==0, 0, haissiop$SSI)


# table(haissiop$SSI, haissiop$SSIType, haissiop$OPCode)

#Create a SSI in-hospital, out-hospital and unk-hospital variable
#Consider using the SSIDiagnosis field to correct if the discharge date is missing???
#haissiop$SSIDiagnosis<-haissiopinf$SSIDiagnosis[match(haissiop$RecordId, haissiopinf$ParentId)]

haissiop$SSIInHosp<-ifelse(haissiop$DateOfHospitalDischarge>=haissiop$DateOfOnset & haissiop$SSI==1, 1, 0)
haissiop$SSIOutHosp<-ifelse(haissiop$DateOfHospitalDischarge<haissiop$DateOfOnset & haissiop$SSI==1, 1, 0)
haissiop$SSIUnkHosp<-ifelse(is.na(haissiop$DateOfHospitalDischarge) & haissiop$SSI==1, 1, 0)





#Two letter country codes into one country taking into account UK four countries
haissi$DataSource<-ifelse(haissi$DataSource=="UK-EN-SSI", "UK-EN",
                          ifelse(haissi$DataSource=="UK-NI-SSI", "UK-NI",
                                 ifelse(haissi$DataSource=="UK-SC-SSI", "UK-SC",
                                        ifelse(haissi$DataSource=="UK-WLS-SSI", "UK-WLS",
                                               ifelse(haissi$DataSource=="DE-HAIICU_AND_SSI", "GER",
                                                      substring(haissi$DataSource, 1, 2))))))

#Data source on each level
haissiop$DataSource<-haissi$DataSource[match(haissiop$ParentId, haissi$RecordId)]
haissiopinf$DataSource<-haissiop$DataSource[match(haissiopinf$ParentId, haissiop$RecordId)]
haissiopinfres$DataSource<-haissiopinf$DataSource[match(haissiopinfres$ParentId, haissiopinf$RecordId)]

#Hospital Id on operation and infection level taking into account also the data source
haissiop$Hospital<-paste(haissi$DataSource, haissi$HospitalId)[match(haissiop$ParentId, haissi$RecordId)]
haissiopinf$Hospital<-haissiop$Hospital[match(haissiopinf$ParentId, haissiop$RecordId)]
# haissiopinfres$Hospital<-haissiopinf$Hospital[match(haissiopinfres$ParentId, haissiopinf$RecordId)]

#Each record has equivalent on the upper level
# table(haissiop$ParentId%in%haissi$RecordId)
# table(haissiopinf$ParentId%in%haissiop$RecordId, haissiopinf$DataSource, useNA="always")
# table(haissiopinfres$ParentId%in%haissiopinf$RecordId)

#One operation type for CABG using a custom C++ IFC5
haissiop$OPCode<-ifC5(haissiop$OPCode, "CBGB", "CABG", "CBGC", "CABG")
# haissiop$OPCode<-ifelse(haissiop$OPCode%in%c("CBGB", "CABG", "CBGC"), "CABG", haissiop$OPCode)
#Operation code to the infection and resistance level
haissiopinf$OPCode<-haissiop$OPCode[match(haissiopinf$ParentId, haissiop$RecordId)]
haissiopinfres$OPCode<-haissiopinf$OPCode[match(haissiopinfres$ParentId, haissiopinf$RecordId)]

#Hospital Id on haissiop level
haissiop$HospitalId<-haissi$HospitalId[match(haissiop$ParentId, haissi$RecordId)]

#Positive identified microorganisms only
#Drop the rows where no identified result isolate fron haissiopinfres
haissiopinfres<-haissiopinfres[!haissiopinfres$ResultIsolate%in%c("_NA", "_NOEXA", "_STERI", "_NONID"),]

#Add a marker of positive microbiology to infection level
haissiopinf$mo<-ifelse(haissiopinf$RecordId%in%haissiopinfres$ParentId, 1, 0)
# prop.table(table(haissiopinf$mo))*100
# prop.table(table(haissiopinf$OPCode, haissiopinf$mo),1)*100

# haissiopinf$DataSource<-substring(haissiopinf$DataSource, 1, 2)

#Aggregate the post operative patient-days
haissiop$popdays<-as.numeric(haissiop$DateOfHospitalDischarge-haissiop$DateOfOperation+1)
# min(haissiop$popdays, na.rm=T)
# max(haissiop$popdays, na.rm=T)

#Create the NHSN risk index in three levels and UNK
source("C:/Data/Myproject/SSI2013_2014/R_files/nhsn.R")

# table(haissi$RecordId%in%haissiop$ParentId)
#Drop CARD as only one/two countries reporting
haissiop<-haissiop[!haissiop$OPCode=="CARD",]
haissiopinf<-haissiopinf[!haissiopinf$OPCode=="CARD",]
haissiopinfres<-haissiopinfres[!haissiopinfres$OPCode=="CARD",]

#Create a "new" endoscopic variable of the two endoscopic variables
haissiop$Endoscopic<-with(haissiop, ifelse(EndoscopicProc =="Y" | EndoscopicProcOp =="Y", "Y",
                                                   ifelse(EndoscopicProc =="N" | EndoscopicProcOp =="N", "N", NA)))

#Create a new "table" for CHOL and COLO where endoscopic vs non endoscopic distinction is needed
haissiOpEndo<-haissiop[haissiop$OPCode%in%c("CHOL", "COLO"),]

#Recode the opcodes as endoscopic/open
haissiOpEndo$OPCode<-with(haissiOpEndo, ifelse(Endoscopic =="Y" & OPCode =="COLO", "endoCOLO",
                                               ifelse(Endoscopic =="N" & OPCode =="COLO", "openCOLO",
                                                      ifelse(Endoscopic =="Y" & OPCode =="CHOL", "endoCHOL",
                                                             ifelse(Endoscopic =="N" & OPCode =="CHOL", "openCHOL", NA)))))

#keep only those where endoscopic Y/N
haissiOpEndo<-haissiOpEndo[!is.na(haissiOpEndo$OPCode),]

#Add a variable with value 1 if known discharge date; 0 if unknown discharge date
head(haissiop)
haissiop$KnownDischarge<-ifelse(is.na(haissiop$popdays), 0, 1)



# table(haissiOpEndo$OPCode, haissiOpEndo$EndoscopicProc, useNA="always")
# table(haissiOpEndo$OPCode, haissiOpEndo$EndoscopicProcOp, useNA="always")
# table(haissiOpEndo$OPCode, haissiOpEndo$EndoscopicProc, haissiOpEndo$opyear, useNA="always")
# table(haissiOpEndo$OPCode, haissiOpEndo$EndoscopicProcOp, haissiOpEndo$opyear, useNA="always")
# table(haissiOpEndo$OPCode, haissiOpEndo$Endoscopic, haissiOpEndo$opyear, useNA="always")
# table(haissiOpEndo$OPCode, haissiOpEndo$Endoscopic, haissiOpEndo$DataSource, useNA="always")
# head(haissiOpEndo)
# round(prop.table(table(haissiOpEndo$Endoscopic, haissiOpEndo$OPCode),2)*100, 2)

# table(haissi$RecordId%in%haissiop$ParentId)

#Select a nice folder for the output
# setwd("C:/Data/Myproject/SSI2013_2014/Report/")

#Save the clean dataset to report subfolder
# dir.create("C:/Data/Myproject/SSI2013_2014/Report/Data/")
# save.image("C:/Data/Myproject/SSI2013_2014/Report/Data/All_2013_2014.RData")

#Each record has equivalent on the upper level
# table(haissi$RecordId%in%haissiop$ParentId, haissi$DateUsedForStatistics)
# table(haissi$RecordId%in%haissiop$ParentId, haissi$DataSource)
# table(haissiop$ParentId%in%haissi$RecordId)
# table(haissiopinf$ParentId%in%haissiop$RecordId)
# table(haissiopinfres$ParentId%in%haissiopinf$RecordId)
# 
# haissiop$ParentId[!haissiop$ParentId%in%haissi$RecordId]
# haissi$RecordId[!haissi$RecordId%in%haissiop$ParentId]
# 
# # 
# # table(haissiop$ParentId%in%haissi$RecordId)
# 
# haissino<-subset(haissi, DataSource=="NO-NOIS" & DateUsedForStatistics=="2011")

#NORWAY WITHOUT THE PATIENT REPORTED INFECTIONS!
# haissiop$PDS<-haissiopinf$SSIPDSMethod[match(haissiop$RecordId, haissiopinf$ParentId)]
# # table(haissiop$PDS, haissiop$DataSource)
# # head(haissiop)
# haissiop$SSI<-ifelse(haissiop$PDS=="ICPAT" | haissiop$SSI==0, 0, haissiop$SSI)
# haissiop$SSIInHosp<-ifelse(haissiop$PDS=="ICPAT", 0, haissiop$SSIInHosp)
# haissiop$SSIOutHosp<-ifelse(haissiop$PDS=="ICPAT", 0, haissiop$SSIOutHosp)
# haissiop$SSIUnkHosp<-ifelse(haissiop$PDS=="ICPAT", 0, haissiop$SSIUnkHosp)



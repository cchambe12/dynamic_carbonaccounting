### Import FVS runs from Python/OneDrive
## Started 27 October 2024 by Cat


### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(timeout = 5000)

### Load Libraries
library(RSQLite)
library(dplyr)
library(magrittr)
library(stringr)
library(ggplot2)
library(tidyr)

## Set Working Directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/northeast/fvs/02_carbonaccounting/")

## Set up the destination folder
datafolder <- "~/Documents/git/dynamic_carbonaccounting/analyses/northeast/fvs/02_carbonaccounting/output/"

## Are you assessing Maple / beech / birch or Oak / hickory? If Oak / Hickory then say TRUE
useoak <- FALSE


##################### Saving key files ###################
if(useoak == TRUE){
  i <- "oakcov"
  j <- "oakcov"
}else {
  i <- "mbbcov"
  j <- "mbbcov"
}





## connect to db
con <- dbConnect(drv=RSQLite::SQLite(), 
                 dbname=paste0("~/Documents/git/dynamic_carbonaccounting/analyses/northeast/fvs/01_fvssimulations/output/", i, ".sqlite"))

## list all tables
tables <- dbListTables(con)

#summary table
summary <- dbGetQuery(conn=con, 
                      statement=paste("SELECT * FROM '",
                                      tables[[match("FVS_Summary_East",tables)]],
                                      "'", sep=""))

#cut list
cut <- dbGetQuery(conn=con, 
                  statement=paste("SELECT * FROM '", 
                                  tables[[match("FVS_CutList_East",tables)]],
                                  "'", sep=""))

#Get the lookup for Stand_CN
lookup <- dbGetQuery(conn=con, 
                     statement=paste("SELECT * FROM '",
                                     tables[[match("FVS_Cases",tables)]], 
                                     "'", sep=""))

#Save cubic feet/ac to filter for initial conditions
stats <- dbGetQuery(conn=con, 
                    statement=paste("SELECT * FROM '", 
                                    tables[[match("FVS_Stats_Stand",tables)]], 
                                    "'", sep="")) 
  
#Also save the starting stats by species to compare to my calculations
stats.sp <- dbGetQuery(conn=con, 
                       statement=paste("SELECT * FROM '", 
                                       tables[[match("FVS_Stats_Species",tables)]], 
                                       "'", sep="")) 

#Save carbon df
carb <- dbGetQuery(conn=con, 
                   statement=paste("SELECT * FROM '", 
                                   tables[[match("FVS_Carbon",tables)]], 
                                   "'", sep=""))

#Tree list
tl <- dbGetQuery(conn=con, 
                 statement=paste("SELECT * FROM '",
                                 tables[[match("FVS_TreeList_East",tables)]], 
                                 "'", sep=""))

#Disconnect the database
dbDisconnect(con = con)

#Save summary file to look at volume over time
summary <- left_join(summary, lookup[,c("CaseID","Stand_CN","StandID")])
write.csv(summary, file = paste0(datafolder, "summary_",j,".csv"), row.names=FALSE)


############### Summarizing the tree list ###############################

#Aggregate stand BA (ft2/ac) by species, site, trt, and year
#Not enough memory to merge and do together so I'll do it twice
tl <- tl %>%
  mutate(BA.ft.ac = (pi*((DBH/12)/2)^2)*TPA,
         MCuFt.ac = MCuFt*TPA) %>%
  group_by(CaseID, StandID, Year, SpeciesFVS) %>%
  summarise(BA.ft.ac = sum(BA.ft.ac),
            MCuFt.ac = sum(MCuFt.ac))

#then merge and add in Stand_CN
tl <- tl %>%
  merge(., lookup[,c('CaseID', 'Stand_CN')],
        all.x = TRUE)
write.csv(tl, file = paste0(datafolder, "tl_",j,".csv"), row.names=FALSE)

##################### Carbon prep files #####################
#Add Stand_CN to carbon files
carb <- carb %>%
  merge(., lookup[,c('CaseID', 'Stand_CN')],
        all.x = TRUE) %>%
  dplyr::select(-CaseID) %>%
  #temporal order of plots, regardless of year
  arrange(Stand_CN, StandID, Year) %>%
  group_by(Stand_CN, StandID) %>%
  mutate(time = row_number()) %>%
  mutate(time = time*5-5) %>%
  ungroup() %>%
  mutate(FOR = substr(StandID, 1, 3))

####################### Summarizing the raw cut list ##########################
#Aggregate cut list BA (ft2/ac) by species, site, trt, and year
cut.raw <- cut %>%
  #radius in feet, squared, times pi
  mutate(cut.BA.ft.ac = (pi*((DBH/12)/2)^2)*TPA,
         cut.MCuFt.ac = MCuFt*TPA) %>%
  group_by(CaseID, StandID, Year, SpeciesFVS) %>%
  summarise(cut.BA.ft.ac = sum(cut.BA.ft.ac),
            cut.MCuFt.ac = sum(cut.MCuFt.ac)) %>%
  #this merge takes a long time so I do it last
  merge(., lookup[,c('CaseID', 'Stand_CN')],
            all.x = TRUE) %>%
  dplyr::select(-CaseID)
write.csv(cut.raw, file = paste0(datafolder, "cut.raw_",j,".csv"), row.names=FALSE)

################## Calculate cut HWP by category #########################
#MCuFt is merchantable cubic feed (sawtimber + pulpwood)
#SCuFt is sawtimber cubic foot volume
#So can calculate PCuFt - pulpwood cubic feet
cut$PCuFt <- cut$MCuFt - cut$SCuFt

#Use FIA to classify hardwood/softwood
#300 and above are hardwoods
cut$HS <- "S"
cut[cut$SpeciesFIA>=300,] %<>% mutate(HS = "H")

#Bring in stand codes
cut <- merge(cut, lookup[,c("CaseID","Stand_CN")], all.x = TRUE) %>%
  dplyr::select(-CaseID) #get rid of case ID

#Convert to volume per ac
cut$PCuFt_ac <- cut$PCuFt*cut$TPA
cut$SCuFt_ac <- cut$SCuFt*cut$TPA

#There are no defects recorded
#Now, aggregate volume for S and H in pulpwood and Sawtimber
#By Stand_CN, StandID, and year

cut <- cut %>%
  group_by(Stand_CN, StandID, Year, HS) %>%
  summarize(across(c(PCuFt_ac, SCuFt_ac), list(sum)))
#Clean up names
names(cut) <- str_remove(names(cut),"_1")
#Now put in a wide format
#First, longer
cut <- merge(
  
  pivot_wider(data=cut %>%
                select(-SCuFt_ac), names_from = HS, values_from = PCuFt_ac,
              values_fill = 0) %>%
    rename(H_PCuFt_ac = H,
           S_PCuFt_ac = S),
  
  pivot_wider(data=cut %>%
                select(-PCuFt_ac), names_from = HS, values_from = SCuFt_ac,
              values_fill = 0) %>%
    rename(H_SCuFt_ac = H,
           S_SCuFt_ac = S), 
  
  all = TRUE)

#Now, multiply them by 100 year storage factors
#in-use and landfill
cut <- cut%>%
  mutate(FOR = substr(StandID, 1, 3))

#First, convert from cuft/ac to m3/ha
cut[,4:7] <- cut[,4:7]*(2.47/35.3147)
#Then change the labels
names(cut)[4:7] <- c("hard_pulp","soft_pulp","hard_saw","soft_saw")
cut$FOR <- substr(cut$StandID, 1, 3)
#pivot longer
cut <- pivot_longer(cut, hard_pulp:soft_saw)
cut$ID <- paste(cut$FOR, cut$name, sep="_")
names(cut)[6] <- "m3_ha"


#Bring in the conversion factor
carb_factor <- read.csv("input/carbon_calc.csv")
names(carb_factor)[1] <- "ID"
#put them together
cut <- left_join(cut, carb_factor[,c("ID","TOTAL")])

#Now, multiply volume by conversion factor to get
#Carbon in tonnes per hectare at year 100
cut$tC_ha_100 <- cut$m3_ha*cut$TOTAL

#Make one in ac since the other carbon is in acres
cut$tC_ac_100 <- cut$tC_ha_100/2.47

#Merge wide version of total carbon with carbon dataframe,
#and fill in zero where there is nothing
carb <- carb %>%
  left_join(., cut %>%
              select(Stand_CN, StandID, Year, name, tC_ac_100) %>%
              pivot_wider(data=., names_from = name, values_from = tC_ac_100) %>%
              mutate(tot_tC_ac_100 = hard_pulp + hard_saw + soft_pulp + soft_saw))
#Replace NA with zero
carb[is.na(carb)] <- 0

##################### Save files #################################

#Save key files
write.csv(cut, file = paste0(datafolder, "cut_",j,".csv"), row.names=FALSE)
write.csv(carb, file = paste0(datafolder, "carb_",j,".csv"), row.names=FALSE)




### Calculate Carbon Gains after eligibility requirements
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
library(rFIA)
library(sf)
library(viridis)
library("ggsci")
library(boot)

## Set working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/centralapps/fvs/02_carbonaccounting/")

### Select input/output folder
datafolder <- "~/Documents/git/dynamic_carbonaccounting/analyses/centralapps/fvs/02_carbonaccounting/output/"

## Are you assessing Maple / beech / birch or Oak / hickory? If Oak / Hickory then say TRUE
useoak <- FALSE

#Read in key files
#Save key files
#Setting the year

################################################################################
################## Determine dynamic baseline for HI and HL ####################

mbb <- read.csv("../../output/clean_centralapps_fiadata.csv") %>%
  filter(forestname == "Maple / beech / birch group") %>%
  mutate(timediff = mean(timediff)) %>%
  select(plt_cn, harvest, baac.perc.cut, baac.perc.cut.prev, baac.perc.cut.prev.prev, forestname, 
         harv1, harv2, harv3, timediff) %>%
  ungroup() %>% distinct()

remv1mbb = mean(mbb$baac.perc.cut.prev.prev[mbb$baac.perc.cut.prev.prev>0])
remv3mbb = mean(mbb$baac.perc.cut[mbb$baac.perc.cut>0])

hl1mbb <- unique(round((nrow(mbb[mbb$harv3==1,])/nrow(mbb)) * (20/(mbb$timediff)), digits=2))
hl3mbb <- unique(round((nrow(mbb[mbb$harv1==1,])/nrow(mbb)) * (20/(mbb$timediff)), digits=2))

oh <- read.csv("../../output/clean_centralapps_fiadata.csv") %>%
  filter(forestname == "Oak / hickory group") %>%
  mutate(timediff = mean(timediff)) %>%
  select(plt_cn, harvest, baac.perc.cut, baac.perc.cut.prev, baac.perc.cut.prev.prev, forestname, 
         harv1, harv2, harv3, timediff) %>%
  ungroup() %>% distinct()

remv1oh = mean(oh$baac.perc.cut.prev.prev[oh$baac.perc.cut.prev.prev>0])
remv3oh = mean(oh$baac.perc.cut[oh$baac.perc.cut>0])

hl1oh <- unique(round((nrow(oh[oh$harv3==1,])/nrow(oh)) * (20/(oh$timediff)), digits=2))
hl3oh <- unique(round((nrow(oh[oh$harv1==1,])/nrow(oh)) * (20/(oh$timediff)), digits=2))

#### Select inputs
if(useoak == TRUE){
  i <- "oak"
  ## What is the harvest likelihood over the 20-year period
  oakrate <- 0.28
  
  oakrate1 = hl1oh
  oakrate3 = hl3oh
  
}else{
  i <- "mbb"
  mbbrate <- 0.43
  
  mbbrate1 = hl1mbb
  mbbrate3 = hl3mbb
  
}
timestamp <- 20

################################################################################
#### Get the FIA data from the prefeasibility scoping with eligibility thresholds
if(useoak == TRUE){
  i <- "oak"
  bau <- read.csv( "output/clean_centralapps_fiadata.csv") %>%
    filter(forestname == "Oak / hickory group")
}else {
  i <- "mbb"
  bau <- read.csv( "output/clean_centralapps_fiadata.csv") %>%
    filter(forestname == "Maple / beech / birch group")
}

#Reading in key files
cut <- read.csv(paste0(datafolder, "cut.raw", "_", i, ".csv"))

carb <- read.csv(paste0(datafolder, "carb", "_", i, ".csv")) %>%
  distinct()

tl <- read.csv(paste0(datafolder, "tl", "_", i, ".csv")) %>%
  distinct() %>%
  #bring in time
  left_join(.,carb %>% 
              mutate(StandID = str_remove(StandID,"_ADJ$")) %>%
              select(Stand_CN, StandID, Year, time) %>%
              unique(.))

#### Check on Counties included
## Add in FIPS
fips <- read.csv("~/Documents/git/tnc_baselines/analyses/input/fips.csv")
fips$statecd <- as.numeric(substr(fips$FIPS, 1, nchar(fips$FIPS)-3))
fips$countycd <- as.numeric(substr(fips$FIPS, nchar(fips$FIPS)-2, nchar(fips$FIPS)))


################## Calculating total carbon #######################
#Total biomass equals sum of: 
#1. Live aboveground biomass
#2. Live belowground biomass
#3. Standing dead wood stocks

#Plus 100 year value of removed biomass
#Need breakdown of softwood and hardwood
#In pulpwood and saw log size classes

#Make cumulative wood products
carb <- carb %>%
  arrange(Stand_CN, StandID, Year) %>%
  group_by(Stand_CN, StandID) %>%
  mutate(cum_tC_ac_100 = cumsum(tot_tC_ac_100)) %>%
  ungroup()

tonne.c <- 0.907185 #convert to metric tonnes
carb$TOT_CARB <- carb$Aboveground_Total_Live*tonne.c + 
  carb$Belowground_Live*tonne.c + 
  carb$Standing_Dead*tonne.c + 
  carb$cum_tC_ac_100


###################### Additionality #####################

#Make a column IDing the treatment, independent of forest type
carb <- carb %>%
  left_join(data.frame("StandID" = sort(unique(carb$StandID)),
                       "TRT" = substr(sort(unique(carb$StandID)),5,1000)))

#Make them all start at the same carbon point
carb <- carb %>%
  filter(time !=0) %>% #get rid of the time 0
  #and replace with new df of time zero
  rbind(., #Duplicate the values from GROW at time 0 to be labeled for each 
        #StandID (plus TRT label) for each plot
        carb %>%
          filter(time == 0,
                 TRT == "GROW") %>%
          dplyr::select(-StandID, -TRT) %>%
          left_join(data.frame(unique(carb[,c("TRT","StandID","Stand_CN")])), .))

#Calculate change in carbon from year to year
carb <- carb %>%
  group_by(Stand_CN, StandID) %>% #grouping variables
  arrange(Stand_CN, StandID, time) %>% #make sure it's in the correct order
  mutate(delta = TOT_CARB - lag(TOT_CARB, default = TOT_CARB[1])) %>%
  mutate(delta = delta/5) #divide by number of years to get annual delta

#Now, compare IFM to BAU - need to account for % of plots not harvested
#Multiply appropriate scenario for each plot by weighting factor
#and add together to get scenario carbon

#save a list of standIDs
names <- unique(carb$StandID)



#Bring in initial basal area
#add in the intiial age, tpa, baa, mcuft
carb <- carb %>%
  left_join(read.csv(file = paste0(datafolder,"summary_",i,".csv", sep = "")) %>%
              arrange(Stand_CN, StandID, Year) %>%
              group_by(Stand_CN, StandID) %>%
              mutate(time = row_number()) %>%
              mutate(time = time*5-5) %>%
              ungroup() %>%
              filter(time == 0) %>%
              select(StandID,Age,Tpa,BA,MCuFt,Stand_CN)) %>%
  filter(time <= timestamp)

#Summarize average carbon storage rates
carb %>%
  filter(time <= timestamp) %>% 
  group_by(StandID) %>%
  summarise(mean = mean(delta, na.rm = TRUE)*(44/12))


#Now delta calculations
#wide delta carbon
w.carb <- carb %>%
  select(Stand_CN, FOR, StandID, time, delta) %>%
  #Put delta in wide format
  pivot_wider(data = ., names_from = StandID, values_from = delta) %>%
  #first, calculate the difference between treatments
  filter(time <= timestamp) %>% #just the years we want
  arrange(Stand_CN, time)


#Make variables for the  function
if(useoak == TRUE){
  
  cleancarb <- w.carb %>%
    #Calculate scenarios
    mutate(#calc.oak.gmf = (OAK_GMF*oakrate)*(44/12),
           #calc.oak.gmf.comp = (OAK_GMF*oakrate + OAK_GROW*(1-oakrate))*(44/12), 
           calc.oak.bau = (OAK_BAU*oakrate)*(44/12), 
           calc.oak.bau.comp = (OAK_BAU*oakrate + OAK_GROW*(1-oakrate))*(44/12), #
           calc.oak.bau.comp.blend = ifelse(time<15, OAK_BAU*oakrate1 + OAK_GROW*(1-oakrate1)*(44/12),
                                            OAK_BAU*oakrate3 + OAK_GROW*(1-oakrate3)*(44/12)), #
           calc.oak.grow = OAK_GROW*(44/12),
           #Calculate deltas
           #delta.oak.gmf = calc.oak.gmf - calc.oak.bau,
           delta.oak.grow = calc.oak.grow - calc.oak.bau,
           #delta.oak.gmf.comp = calc.oak.gmf.comp - calc.oak.bau.comp,
           delta.oak.grow.comp = calc.oak.grow - calc.oak.bau.comp,
           delta.oak.grow.comp.blend = calc.oak.grow - calc.oak.bau.comp.blend) %>%
    rename(plt_cn = Stand_CN) %>%
    left_join(bau)
  
  #mean(cleancarb$delta.oak.gmf)
  mean(cleancarb$delta.oak.grow)
  mean(cleancarb$delta.oak.grow.comp)
  mean(cleancarb$delta.oak.grow.comp.blend)
  
  write.csv(cleancarb, "output/clean_fvsoutput_oak.csv", row.names=FALSE)
  
  
}else if(useoak == FALSE) {
  
  cleancarb <- w.carb %>%
    #filter(time == timestamp) %>%
    #Calculate scenarios
    mutate(#calc.mbb.gmf = (MBB_GMF*mbbrate)*(44/12),
           #calc.mbb.gmf.comp = (MBB_GMF*mbbrate + MBB_GROW*(1-mbbrate))*(44/12), 
           calc.mbb.bau = (MBB_BAU*mbbrate)*(44/12), 
           calc.mbb.bau.comp = (MBB_BAU*mbbrate + MBB_GROW*(1-mbbrate))*(44/12), #
           calc.mbb.bau.comp.blend = ifelse(time<15, MBB_BAU*mbbrate1 + MBB_GROW*(1-mbbrate1)*(44/12),
                                            MBB_BAU*mbbrate3 + MBB_GROW*(1-mbbrate3)*(44/12)), #
           calc.mbb.grow = MBB_GROW*(44/12),
           #Calculate deltas
           #delta.mbb.gmf = calc.mbb.gmf - calc.mbb.bau,
           delta.mbb.grow = calc.mbb.grow - calc.mbb.bau,
           #delta.mbb.gmf.comp = calc.mbb.gmf.comp - calc.mbb.bau.comp,
           delta.mbb.grow.comp = calc.mbb.grow - calc.mbb.bau.comp,
           delta.mbb.grow.comp.blend = calc.mbb.grow - calc.mbb.bau.comp.blend) %>%
    rename(plt_cn = Stand_CN) %>%
    left_join(bau)
  
  #mean(cleancarb$delta.mbb.gmf)
  mean(cleancarb$delta.mbb.grow)
  mean(cleancarb$delta.mbb.grow.comp)
  mean(cleancarb$delta.mbb.grow.comp.blend)
  
  write.csv(cleancarb, "output/clean_fvsoutput_mbb.csv", row.names=FALSE)
  
  
}



### Calculate Carbon Gains
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
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/southernapps/fvs/02_carbonaccounting/")

### Select input/output folder
datafolder <- "~/Documents/git/dynamic_carbonaccounting/analyses/southernapps/fvs/02_carbonaccounting/output/"

## Are you assessing Maple / beech / birch or Oak / hickory? If Oak / Hickory then say TRUE
useoak <- FALSE

#Read in key files
#Save key files
#Setting the year

#### Select inputs
if(useoak == TRUE){
  i <- "oak"
  ## What is the harvest likelihood over the 20-year period
  oakrate <- 0.29
}else{
  i <- "mbb"
  mbbrate <- 0.40
}
timestamp <- 20


#### Get FVS names of species
fvscodes <- read.csv("input/specieslookup.csv") 
colnames(fvscodes) <- fvscodes[1,]
fvscodes <- fvscodes[-1, ]

fvscodes <- fvscodes %>%
  select(`TNC code`, Species, `FIA Species Code`, `FVS Southern Variant Species Code`,
         `Vol Table FIA Species Code`, CONFIG_ID, SPECIES_SYMBOL, SPECIES_NUM, 
         CF_MIN_DBH, CF_VOL_EQ, CF_VOL_SP, COEF_TABLE, COEF_TBL_SP) %>%
  filter(`FVS Southern Variant Species Code` != "") %>%
  rename(SpeciesFVS = `FVS Southern Variant Species Code`)

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
  #Make some groups based on initial basal area
  mutate(BA_tier = cut(BA, 
                       breaks = c(49,75,90,130,250),
                       labels = c("t50_75","t75_90","t90_130","t130_"))) %>%
  filter(time <= timestamp)

#Summarize average carbon storage rates
carb %>%
  filter(time <= timestamp) %>% 
  #optional: filter by BA
  #filter(BA > 90) %>%
  group_by(StandID) %>%
  summarise(mean = mean(delta, na.rm = TRUE)*(44/12))


#Now delta calculations
#wide delta carbon
w.carb <- carb %>%
  select(Stand_CN, FOR, StandID, time, delta) %>%
  #Put delta in wide format
  pivot_wider(data = ., names_from = StandID, values_from = delta) %>%
  #first, calculate the difference between treatments
  filter(time %in% c(5,10,15,20)) %>% #just the years we want
  arrange(Stand_CN, time)


#Make variables for the  function
if(useoak == TRUE){
  
  oak.bau <- mean(w.carb$OAK_BAU, na.rm = TRUE)
  oak.gmf <- mean(w.carb$OAK_GMF, na.rm = TRUE)
  oak.grow <- mean(w.carb$OAK_GROW, na.rm = TRUE)
  c(oak.bau, oak.gmf, oak.grow)*(44/12)
  
  carbon.calc <- function(FOR, ifm.hr, bau.hr, r = 0) {
    #Helpful values
    ifm.res <- 1-ifm.hr
    bau.res <- 1-bau.hr
    
    #Calculate scenarios
      calc.oak.gmf <- (oak.gmf*ifm.hr + oak.grow*ifm.res)*(44/12)
      calc.oak.bau <- (oak.bau*bau.hr + oak.grow*bau.res)*(44/12)
      calc.oak.grow <- oak.grow*(44/12)
      #Calculate deltas
      delta.oak.gmf <- calc.oak.gmf - calc.oak.bau
      delta.oak.grow <- calc.oak.grow - calc.oak.bau
      #Return values
      return(data.frame("GROW" = calc.oak.grow,
                        "GMF" = calc.oak.gmf,
                        "BAU" = calc.oak.bau,
                        "delta.gmf" = delta.oak.gmf,
                        "delta.grow" = delta.oak.grow))
  }
  


  #Now, pick your forest type, the harvest rate for ifm and bau,
  #and the reserve size (default 0)
  carbon.calc("OAK", oakrate, oakrate, 0)
  
  
  cleancarb <- carb %>%
    #filter(!StandID %in% c("OAK_GROW")) %>%
    mutate(StandID = substr(StandID, 5, nchar(StandID)))
  
  write.csv(cleancarb, "output/clean_fvsoutput_mbb.csv", row.names=FALSE)
  
  png(paste0("figures/totalcarbon_overtime_oak.png"), 
      width=7, height=5, unit="in", res=200)
  ggplot(cleancarb, aes(y=Total_Stand_Carbon, x=Year, col = StandID, fill=StandID)) + 
    geom_smooth(method="loess", 
                aes(ymax = after_stat(y) + 3,
                    ymin = after_stat(y) - 3)) + 
    scale_color_viridis(discrete = TRUE, name="Practice") +
    scale_fill_viridis(discrete = TRUE, name = "Practice") +
    theme_bw() + xlab("") + ylab("Total Mt CO2 per acre")
  dev.off()


  png(paste0("figures/carbongains_oak.png"), 
      width=7, height=5, unit="in", res=200)
  ggplot(cleancarb %>% filter(StandID != "BAU", time <= 20) %>% 
           group_by(StandID) %>%
           mutate(StandID = ifelse(StandID == "GROW", "Extended Rotation", "25% allowable cut")) %>%
           summarize(meanc = mean(delta, na.rm=TRUE),
                     sec = sd(delta, na.rm=TRUE)/sqrt(length(delta))), 
         aes(y=meanc, x=StandID, col = StandID, fill=StandID)) + 
    geom_col() + geom_errorbar(aes(ymin = meanc - sec, ymax = meanc + sec)) +
    scale_color_d3(name="Practice", palette = "category20b") +
    scale_fill_d3(name = "Practice", palette = "category20b") +
    theme_bw() + xlab("") + ylab("Total Mt CO2 per acre") +
    theme(legend.position = "none") +
    scale_x_discrete(guide = guide_axis(angle=45)) +
    geom_text(aes(label = round(meanc, digits=2)), y=0.08, col="white") 
  dev.off()
  
}else {
  
  mbb.bau <- mean(w.carb$MBB_BAU, na.rm = TRUE)
  mbb.gmf <- mean(w.carb$MBB_GMF, na.rm = TRUE)
  mbb.grow <- mean(w.carb$MBB_GROW, na.rm = TRUE)
  c(mbb.bau, mbb.gmf, mbb.grow)*(44/12)
  
  carbon.calc <- function(FOR, ifm.hr, bau.hr, r = 0) {
    #Helpful values
    ifm.res <- 1-ifm.hr
    bau.res <- 1-bau.hr
    
    #Calculate scenarios
    calc.mbb.gmf <- (mbb.gmf*ifm.hr + mbb.grow*ifm.res)*(44/12)
    calc.mbb.bau <- (mbb.bau*bau.hr + mbb.grow*bau.res)*(44/12)
    calc.mbb.grow <- mbb.grow*(44/12)
    #Calculate deltas
    delta.mbb.gmf <- calc.mbb.gmf - calc.mbb.bau
    delta.mbb.grow <- calc.mbb.grow - calc.mbb.bau
    #Return values
    return(data.frame("GROW" = calc.mbb.grow,
                      "GMF" = calc.mbb.gmf,
                      "BAU" = calc.mbb.bau,
                      "delta.gmf" = delta.mbb.gmf,
                      "delta.grow" = delta.mbb.grow))
  }
  
  
  
  #Now, pick your forest type, the harvest rate for ifm and bau,
  #and the reserve size (default 0)
  carbon.calc("MBB", mbbrate, mbbrate, 0)
  
  
  cleancarb <- carb %>%
    #filter(!StandID %in% c("OAK_GROW")) %>%
    mutate(StandID = substr(StandID, 5, nchar(StandID)))
  
  write.csv(cleancarb, "output/clean_fvsoutput_mbb.csv", row.names=FALSE)
  
  png(paste0("figures/totalcarbon_overtime_mbb.png"), 
      width=7, height=5, unit="in", res=200)
  ggplot(cleancarb, aes(y=Total_Stand_Carbon, x=Year, col = StandID, fill=StandID)) + 
    geom_smooth(method="loess", 
                aes(ymax = after_stat(y) + 3,
                    ymin = after_stat(y) - 3)) + 
    scale_color_viridis(discrete = TRUE, name="Practice") +
    scale_fill_viridis(discrete = TRUE, name = "Practice") +
    theme_bw() + xlab("") + ylab("Total Mt CO2 per acre")
  dev.off()
  
  
  png(paste0("figures/carbongains_mbb.png"), 
      width=7, height=5, unit="in", res=200)
  ggplot(cleancarb %>% filter(StandID != "BAU", time <= 20) %>% 
           group_by(StandID) %>%
           mutate(StandID = ifelse(StandID == "GROW", "Extended Rotation", "25% allowable cut")) %>%
           summarize(meanc = mean(delta, na.rm=TRUE),
                     sec = sd(delta, na.rm=TRUE)/sqrt(length(delta))), 
         aes(y=meanc, x=StandID, col = StandID, fill=StandID)) + 
    geom_col() + geom_errorbar(aes(ymin = meanc - sec, ymax = meanc + sec)) +
    scale_color_d3(name="Practice", palette = "category20b") +
    scale_fill_d3(name = "Practice", palette = "category20b") +
    theme_bw() + xlab("") + ylab("Total Mt CO2 per acre") +
    theme(legend.position = "none") +
    scale_x_discrete(guide = guide_axis(angle=45)) +
    geom_text(aes(label = round(meanc, digits=2)), y=0.08, col="white") 
  dev.off()
  
}






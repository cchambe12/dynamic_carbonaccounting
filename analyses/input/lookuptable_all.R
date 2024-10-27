#### Combine all lookup tables for the prefeasibility tool
# Started 26 Sept 2022 by Cat

if(FALSE){
## Set working directory
  
  # housekeeping
  rm(list=ls()) 
  options(stringsAsFactors = FALSE)
  options(mc.cores = parallel::detectCores())
  rstan::rstan_options(auto_write = TRUE)
  
  setwd("~/Documents/git/prefeasibility/source/")
  
  
  # Load Libraries
  library(dplyr)
  library(tidyr)


ne <- read.csv("lookuptable_newengland_harvestintensity.csv")
ca <- read.csv("lookuptable_centralapps_harvestintensity.csv")
se <- read.csv("lookuptable_southeast_harvestintensity.csv")
lr <- read.csv("lookuptable_lakesregion_harvestintensity.csv")
mw <- read.csv("lookuptable_midwest_harvestintensity.csv")
sw <- read.csv("lookuptable_southwest_harvestintensity.csv")
pnw <- read.csv("lookuptable_pnw_harvestintensity.csv")
pac <- read.csv("lookuptable_pacific_harvestintensity.csv")


lookup <- full_join(ne, ca)
lookup <- full_join(lookup, se)
lookup <- full_join(lookup, lr)
lookup <- full_join(lookup, mw)
lookup <- full_join(lookup, sw)
lookup <- full_join(lookup, pnw)
lookup <- full_join(lookup, pac)


## Check and clean the data
sort(unique(lookup$state)) 
length(unique(lookup$state)) ### Should be 48 - missing HI and NV
#lookup$state <- toupper(lookup$state)

sort(unique(lookup$forestgroup)) ## have an error with 380 showing up

sort(unique(lookup$stock))
#lookup$stock <- plyr::round_any(lookup$stock, 10, f = floor) 

#write.csv(lookup, file="clean_lookuptable_output.csv", row.names = FALSE)
}

#### FCSE function output
if(FALSE){
  
  states <- c("me", "ma")
  stock <- "Fully stocked (60 - 99%)"
  foresttype <- "maple/beech/birch"
  
}

fcseoutput <- function(state, stock, forestgroup, lookup){
  
  stateoutput <- state
  
  if(TRUE){
  stockoutput <- if(stock=="Overstocked (100%)"){c(seq(100, 120, by=10))}else 
    if(stock=="Fully stocked (60 - 99%)"){c(seq(60, 99, by=10))}else 
      if(stock=="Medium stocked (35 - 59%)"){c(seq(35, 59, by=10))}else 
        if(stock=="Poorly stocked (10 - 34%)"){c(seq(10, 34, by=10))}else 
          if(stock=="Nonstocked (0 - 9%)"){c(seq(0, 9, by=10))}
  }
  
  #stockoutput <- as.numeric(stock)
  
  fortype <- forestgroup
  
  
  df <- lookup[(lookup$state%in%stateoutput & lookup$stock%in%stockoutput &
                  lookup$forestgroup%in%fortype),]
  
  return(df)
  
}

#df <- fcseoutput(states, stock, foresttype, lookup)










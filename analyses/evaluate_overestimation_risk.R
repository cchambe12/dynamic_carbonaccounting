### Estimate different in IRA funding potential from over-estimation vs
## rigorous estimation

### Use numbers from Favero et al 2025
# https://pmc.ncbi.nlm.nih.gov/articles/PMC11895230/


# I wonder if you could play out a scenario to really drive this home. 
# Something like - if the IRA investments in climate smart forestry were measured using 
# XYZ they would overestimate mitigation by ###.  
# Just a thought - maybe there  is a better scenario to play out related to the market 
# or another federal program (maybe the USDA Small Landowner Access to Carbon Market Program - 
# since it is in the same region-ish as this study?).

### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(tidyr)
library(dplyr)

## Set Working director
setwd("~/Documents/git/dynamic_carbonaccounting/")

### Load in method comparisons
methods <- read.csv("analyses/output/methodoutputs_comparegains.csv")

### Input values from the Favero paper
dollar_perC = 70 # $/tCO2
C_year = 60 * 1000000 #tCO2/year

dollar_year = dollar_perC * C_year ## $4.2 billion invested

## Area invested in by 2030
ha_year = 8000000 # hectare per year
ac_year = ha_year * 2.47
## Total estimated acres through 2030 - predicted 28% of the forestry sector towards IFM projects
ifm_ac = (ac_year * 5) * 0.28


### Next, convert paper tons per acre per year to tons per year
region_ft = c( "MBB_CApps", "MBB_NE", "MBB_NW", "MBB_SApps",
               "OH_CApps", "OH_NE", "OH_NW", "OH_SApps")
scenario_a = c(methods$average_gains[methods$method=="single model - static"])
scenario_b = c(methods$average_gains[methods$method=="blended model - static"])
scenario_c = c(methods$average_gains[methods$method=="blended model - dynamic"])
scenario_d = c(methods$average_gains[methods$method=="measured blended - dynamic"])


estimate_c <- data.frame(
  region_ft = region_ft,
  scenario_a = scenario_a,
  scenario_b = scenario_b,
  scenario_c = scenario_c,
  scenario_d = scenario_d
)

estimate_c <- estimate_c %>%
  mutate(totalc_scenarioa = ifm_ac * scenario_a,
         totalc_scenariob = ifm_ac * scenario_b,
         prop_scenariob = totalc_scenariob/totalc_scenarioa,
         totalc_scenarioc = ifm_ac * scenario_c,
         prop_scenarioc = totalc_scenarioc/totalc_scenarioa,
         totalc_scenariod = ifm_ac * scenario_d,
         prop_scenariod = totalc_scenariod/totalc_scenarioa)

averages_scenarios <- estimate_c %>%
  summarize(mean_scenarioa = mean(totalc_scenarioa),
            mean_scenariob = mean(totalc_scenariob),
            prop_scenariob = mean(prop_scenariob),
            mean_scenarioc = mean(totalc_scenarioc),
            prop_scenarioc = mean(prop_scenarioc),
            mean_scenariod = mean(totalc_scenariod),
            prop_scenariod = mean(prop_scenariod))

### Okay, not sure if this got us to the right place... need to scale now up to IRA level estimates... 


#### By doing this, the importance of having place-based estimates of 
## land prices is critical for predicting mitigation





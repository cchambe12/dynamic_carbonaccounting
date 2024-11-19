#### Attempt to demonstrate dynamic baseline approach using FIA
### Randomly select 30 FIA plots from 20 years ago for each region and forest type
### Match to the most similar plots and then monitor growth over and harvests over time

### Can we estimate additional emission reductions and removals?

### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(timeout = 5000)

# Load Libraries
library(ggplot2)
library(plotly)
library(viridis)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(sf)
library(gridExtra)
library(lme4)
library("ggsci")
library(rFIA)
library(optmatch)

### Set the working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/southernapps/")


#################################################################################
#################### STEP 1 - Establish input varibles and ROI ##################
## Input variables to standardize approach across regions
states <- c("AL", "GA", "KY", "NC", "SC", "TN", "VA") 
region <- "southernapps"  

ecosubstokeep <- c("221", "223", "231", "M221")


### Organize these in alphabetical order to have consistent and accurate outputs
forestname1 <- "Maple / beech / birch group"  #
forestname2 <- "Oak / hickory group" #

### Add in species level and forest level detail
specieslist <- read.csv("../input/2021_MasterSpecies.csv")
specieslist <- subset(specieslist, select = c("FIA.Code", "Common.Name"))
names(specieslist) <- c("spcd", "species")

forests <- read.csv("../input/REF_FOREST_TYPE.csv")
forests <- subset(forests, select=c(TYPGRPCD, MEANING, VALUE))
names(forests) <- c("fortypgrpcd", "forestsubtype", "fortypcd")

forestgroup <- read.csv("../input/REF_FOREST_TYPE_GROUP.csv") %>%
  dplyr::select(VALUE, MEANING) %>%
  rename(fortypgrpcd = VALUE) %>%
  rename(forestname = MEANING) %>%
  left_join(forests, multiple = "all")

## Prepare FIPS to line up PLOT table to counties we're enrolling
fips <- read.csv("../input/fips.csv")
fips$statecd <- as.numeric(substr(fips$FIPS, 1, nchar(fips$FIPS)-3))
fips$countycd <- as.numeric(substr(fips$FIPS, nchar(fips$FIPS)-2, nchar(fips$FIPS)))

## Prepare Forisk Dataset to get estimated # of mills in county
forisk <- st_read("~/OneDrive - The Nature Conservancy/FORISK/2024_Q1_Forisk_North_American_Ind_Cap_DB_Shape/Forisk_NA_FI_Capacity_DB_2024_Q1.shp") %>%
  filter(State_Prov %in% states, Status == "Open") %>%
  rename(Name = County, State = State_Prov) %>%
  group_by(State, Name) %>%
  summarize(nummills = n()) %>%
  st_drop_geometry()

fips <- left_join(fips, forisk) %>%
  mutate(nummills = ifelse(is.na(nummills), 0, nummills))


#################################################################################
###################### STEP 2 - Clean FIA data for the ROI ######################
## Read in FIA data
fia <- readFIA(paste0("../../fiadata/", region))

## Read in FVS runs
oakfvs <- read.csv("fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") 
mbbfvs <- read.csv("fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") 

## Subset to plots that have 20 years of data
# Combining states' plots into one dataframe
allplot <- fia$PLOT
names(allplot) <- tolower(names(allplot))


### Add in traits to calculate relative density and also for desirability codes
traits = read.csv("../input/Species Traits.csv")


# Combining states' trees into one dataframe
tree = fia$TREE
names(tree) <- tolower(names(tree))

## Remove data from periodic inventories (prior to annualized plot design)
## to reduce file storage size
tree = tree[which(tree$invyr>1997), ]

# Specific Gravity (for RD calculation)
tree$spec.grav = traits[match(tree$spcd, traits$spcd), "spec.grav"]

# Desirability
tree$desire = traits[match(tree$spcd, traits$spcd), "desire"]
tree$desire.fac = traits[match(tree$spcd, traits$spcd), "desire.fac"]

### Clean up BA and Volume metrics
tree <- tree %>%
  mutate(tpa.custom = case_when( is.na(tpa_unadj) & dia < 5 ~ 74.965282,
                                 is.na(tpa_unadj) & dia >= 5 ~ 6.018046,
                                 TRUE ~ tpa_unadj),
         ##### Calculate additional variables  
         # Basal area
         ba = dia^2 * 0.005454 ,
         # Calculate basal area per acre
         baac = ba * tpa.custom,
         # per acre gross cubic-foot volume for total volume
         volcfgrs.ac = volcfgrs * tpa.custom,
         # per acre gross cubic-foot sawlog volume of a sawtimber tree for sawlog metric
         volcsnet.ac = volcsnet * tpa.custom,
         # per acre gross board-foot sawlog volume of a sawtimber tree for sawlog metric in board feet
         volbfgrs.ac = volbfgrs * tpa.custom)


# Lorey Height is the average height of all trees in a stand weighted by tree basal area
# To calculate Lorey Height, see Matt Russell's blog post here: https://arbor-analytics.com/post/2023-04-21-lorey-s-height-the-remote-sensing-way-to-estimate-tree-height/
# Or here: https://doi.org/10.1016/S0378-1127(01)00737-X
tree$ba_part_lorey = ifelse(is.na(tree$actualht), NA, tree$baac)
tree$lorey_ht_part = tree$ba_part_lorey*tree$actualht

# Relative density - calculated from Ducey-Knapp
tree$rd = 2.47 * (0.00015 + (0.00218 * tree$spec.grav)) * ((tree$dia/10)^1.6)     
tree$rd.ac = tree$rd * tree$tpa.custom 

## per acre proxy for stem biomass = Gross volume * specific gravity
# this variable was identified as a desired dependent variable for later regressions
# in 6-2-2020 meeting w/ David Shoch & Ethan Belair. 
tree$stem.bio.ac = tree$volcfgrs * tree$tpa.custom * tree$spec.grav

# Aboveground, belowground, and total C
tree$ag_mtco2eac = tree$carbon_ag * tree$tpa.custom/2204.6*(44/12) ## 44/12 is the molar ratio for converting C to CO2

tree$bg_mtco2eac = tree$carbon_bg * tree$tpa.custom/2204.6*(44/12)

tree$total_mtco2eac = tree$ag_mtco2eac + tree$bg_mtco2eac

# Add the previous plot sequence to each tree from the plot table
# this will be used to link individual trees and plot level summaries to their previous (T2) measurements
tree$prev.plt.cn = allplot[match(tree$plt_cn, allplot$cn), "prev_plt_cn"]

# Add the next previous plot sequence to each tree from the plot table
# this will be used to link individual trees and plot level summaries to their previous (T3) measurements
tree$prev.prev.plt.cn = allplot[match(tree$prev.plt.cn, allplot$cn), "prev_plt_cn"]

# Add the next, next previous plot sequence to each tree from the plot table
# this will be used to link individual trees and plot level summaries to their T4 measurements
tree$prev.prev.prev.plt.cn = allplot[match(tree$prev.prev.plt.cn, allplot$cn), "prev_plt_cn"]

### Add in previous diameters and statuses for the % removal codes
# This loop filters to find those cns that are the prev.cn for a different tree, takes the dia, statuscd, baac, relative density, and tpa for those trees, and then right joins to the original - so if there is a new tree with no prev cn, the dia, statuscd, baac, rd, and tpa are filled in as NAs.
tree = tree %>%
  ## Add in previous measurements for rd.perc.cut and baac.perc.cut
  filter(cn %in% unique(prev_tre_cn)) %>%
  select(cn, dia, statuscd, baac, rd.ac, tpa.custom, volbfgrs.ac, volcsnet.ac) %>%
  rename_with(~paste0(., ".prev")) %>%
  rename(prev_tre_cn = cn.prev) %>%
  right_join(tree)


# Combining states' COND tables into one dataframe
cond = fia$COND
names(cond) = tolower(names(cond))
cond = cond[which(cond$invyr>1997), ]


## Clean up TREE and COND to only include relevant plots
# Subset to remove plots reserved from management for wood products
# Add reservcd to Tree table from Condition table
# Then subset the Condition table to remove plots reserved from management for wood products.
# The reservcd variable codes plots as 1 = reserved or 0 = not reserved.
tree$reservcd = cond[match(tree$plt_cn, cond$plt_cn), "reservcd"]
# Subset to include only land on which timber harvesting is allowed 
tree = tree[which(tree$reservcd == 0), ]
# We only subset in the tree table for now, but this is cleaned when we later join with the cond and plot tables


# Subset to remove plots that span multiple conditions
# Subset the Condition table to remove plots with multiple condition status codes.
# The condrprop_unadj variable shows the proportion of a plot that is in a given condition.
# Thus if condprop_unadj = 1, the plot is entirely within a single condition.
# Remove plots from cond table that have multiple condition status codes (split cover types/owners/landuses)
cond = cond[which(cond$condprop_unadj==1), ]

# Then, subset the tree table, keeping only rows indicating single condition plots
# Add conprop_unadj to tree table
tree$unsplit = cond[match(tree$plt_cn, cond$plt_cn), "condprop_unadj"]

# Subset tree table to keep only trees from intact/unsplit plots
tree = tree[which(tree$unsplit==1), ]  


# Remove trees from plots with plot_status_cd indicating non-sampled plots (plot_status_cd == 3)
# or sampled plots with no accessible forestland (plot_status_cd == 2)
# Add plot_status_cd from plot table to tree table
tree$plot_status_cd = allplot[match(tree$plt_cn, allplot$cn), "plot_status_cd"]
tree = tree[tree$plot_status_cd==1,]

## Clean up COND table to remove nas
cond = cond[!is.na(cond$owncd),]

### Join up cleaned TREE and COND
trees <- left_join(tree, cond, by=c("plt_cn", "invyr", "statecd", "countycd", "unitcd", "plot"),
                   relationship="many-to-many")


allplot = allplot %>%
  filter(invyr > 1997) %>%
  dplyr::select("cn", "ecosubcd", "statecd", "countycd", "unitcd", "lat", "lon", "elev", 
                "measmon", "measday", "measyear", "remper", "kindcd", "p2panel", "qa_status", "designcd") %>% 
  mutate(ecosub = substr(ecosubcd, 1, nchar(ecosubcd)-2),
         countyid = paste(statecd, countycd),
         measdate = as.Date(paste(measmon, measday, measyear, sep="-"), format="%m-%d-%Y")) %>%
  rename(plt_cn = cn) %>%
  filter(ecosub %in% c(ecosubstokeep))

treeplot = left_join(trees, allplot)


################################################################################
################# Next, we need to clean up the treelist #######################
################################################################################
carb_factor <- read.csv("fvs/02_carbonaccounting/input/carbon_calc.csv") %>%
  filter(TYPE == "H", SIZE == "S") %>%
  mutate(forestname = ifelse(FOR == "OAK", "Oak / hickory group",
                             ifelse(FOR == "MBB", "Maple / beech / birch group", FOR)))


### Step 1: Getting plot level summaries
dat_grouped <- treeplot %>%
  left_join(forestgroup) %>%
  left_join(carb_factor %>% select(forestname, TOTAL)) %>%
  dplyr::mutate(current = dia >= 5,
                harv = statuscd.prev == 1 & dia.prev >= 5 & statuscd == 3,
                mort = statuscd.prev == 1 & statuscd == 2,
                prevall = statuscd.prev == 1 & volbfgrs.ac == 0,
                standall = statuscd == 1 & statuscd.prev == 1,
                prev = dia.prev >= 5 &  statuscd.prev == 1,
                stand = statuscd.prev == 1 & statuscd == 1 & dia.prev >= 5) %>%
  group_by(plt_cn) %>%
  summarize(across(c(ecosub, ecosubcd, statecd, countycd, plot, unitcd, lat, lon, fortypcd, owngrpcd, stdorgcd, invyr, remper, measdate, prev.plt.cn, prev.prev.plt.cn, prev.prev.prev.plt.cn, dstrbcd1, 
                     dstrbcd2, dstrbcd3, stdage, siteclcd, spgrpcd, kindcd, designcd, qa_status, TOTAL, siteclcd,
                     gsstk, elev,  slope, aspect, trtcd1, harvest_type1_srs, physclcd), function(x) first(na.omit(x))),
            ## Calculate % BA removed
            baac.perc.cut = (sum(baac.prev[harv], na.rm=TRUE))/sum(baac.prev[prev], na.rm=TRUE),
            ## Calculate HWP
            hwp = ((sum(volcsnet.ac.prev[harv], na.rm=TRUE)*TOTAL)*(2.47/35.3147))*0.907185,
            ## Stand-level total Mt CO2e/acre
            total_mtco2eac = sum(total_mtco2eac, na.rm=TRUE),
            ## Stand-level MBF vol of sawtimber portion gross
            volbfgrs.ac = sum(volbfgrs.ac, na.rm=TRUE),
            ## Stand-level total Mt CO2e/acre + HWP
            totalc= sum(total_mtco2eac, na.rm=TRUE) + hwp,
            ## Calculate relative density for understory and overstory specis
            rd.ac.over = sum(rd.ac[dia >= 5 & statuscd == 1], na.rm = TRUE),
            rd.ac.regen = sum(rd.ac[dia < 5 & statuscd == 1], na.rm = TRUE)) %>%
  distinct(plt_cn, .keep_all = TRUE) 

## Species list is downloaded from here: https://www.fia.fs.usda.gov/library/field-guides-methods-proc/#TreeSpecList
## Click on link FIA Master Tree Species List 9.1 (232 KB) under 'Master Tree Species List' heading
dat_grouped <- left_join(dat_grouped, forestgroup)


datprev = dat_grouped %>%
  ## Add in previous measurements for invyr, measdate, rd.perc.cut, baac.perc.cut, and qmd
  filter(plt_cn %in% unique(prev.plt.cn)) %>%
  select(plt_cn, invyr, measdate, dstrbcd1, dstrbcd2, dstrbcd3, harvest_type1_srs,  baac.perc.cut, hwp,
         trtcd1, total_mtco2eac, totalc, rd.ac.over, rd.ac.regen, volbfgrs.ac) %>%
  rename_with(~paste0(., ".prev")) %>%
  rename(prev.plt.cn = plt_cn.prev) %>%
  right_join(dat_grouped) %>%
  ## Calculate number of years between measurement dates for HL equation
  mutate(timediff = lubridate::time_length(difftime(measdate, measdate.prev), "years")) 
datprevprev = datprev %>%
  ## Add in previous, previous measurements for rd.perc.cut, baac.perc.cut, and qmd
  filter(plt_cn %in% unique(prev.prev.plt.cn)) %>%
  select(plt_cn, invyr, measdate, dstrbcd1, dstrbcd2, dstrbcd3, harvest_type1_srs, total_mtco2eac,  baac.perc.cut, hwp,
         trtcd1, totalc, rd.ac.over, rd.ac.regen, volbfgrs.ac) %>%
  rename_with(~paste0(., ".prev.prev")) %>%
  rename(prev.prev.plt.cn = plt_cn.prev.prev) %>%
  right_join(datprev)
dat = datprevprev %>%
  ## Add in previous, previous measurements for rd.perc.cut, baac.perc.cut, and qmd
  filter(plt_cn %in% unique(prev.prev.prev.plt.cn)) %>%
  select(plt_cn, trtcd1, total_mtco2eac) %>%
  rename_with(~paste0(., ".prev.prev.prev")) %>%
  rename(prev.prev.prev.plt.cn = plt_cn.prev.prev.prev) %>%
  right_join(datprevprev) %>%
  ## Finally, determine when the harvest happened and how many harvests occurred
  mutate(baac.perc.cut = ifelse(is.na(baac.perc.cut) | baac.perc.cut < 0, 0, baac.perc.cut),
         baac.perc.cut.prev = ifelse(is.na(baac.perc.cut.prev) | baac.perc.cut.prev < 0, 0, baac.perc.cut.prev),
         baac.perc.cut.prev.prev = ifelse(is.na(baac.perc.cut.prev.prev) | baac.perc.cut.prev.prev < 0, 0, baac.perc.cut.prev.prev),
         harvest = ifelse((trtcd1 == 10 | baac.perc.cut >= 0.25 & dstrbcd1 == 0 )|
                            (trtcd1.prev == 10 | baac.perc.cut.prev >= 0.25 & dstrbcd1.prev == 0 )| 
                            (trtcd1.prev.prev == 10 | baac.perc.cut.prev.prev >= 0.25 & 
                               dstrbcd1.prev.prev == 0), 1, 0),
         harv1 = ifelse(trtcd1 == 10 | baac.perc.cut >= 0.25 & dstrbcd1 == 0, 1, 0),
         harv2 = ifelse(trtcd1.prev == 10 | baac.perc.cut.prev >= 0.25 & dstrbcd1.prev == 0, 1, 0),
         harv2 = ifelse(is.na(harv2), 0, harv2),
         harv3 = ifelse(trtcd1.prev.prev == 10 | baac.perc.cut.prev.prev >= 0.25 & dstrbcd1.prev.prev == 0, 1, 0),
         harv3 = ifelse(is.na(harv3), 0, harv3),
         numharvs = harv1 + harv2 + harv3) %>% 
  rowwise() %>% 
  mutate(baac.remv = ifelse(harvest==1, max(baac.perc.cut, baac.perc.cut.prev, baac.perc.cut.prev.prev), 0),
         hwptot = ifelse(harvest==1, max(hwp, hwp.prev, hwp.prev.prev), 0)) 

### Step 2: Finding initial measurements for each plot
## We want to know what predictors lead to a harvest in the next 20 years

## The following columns will change over time and we need initial measurements
colstodupe <- c("forestname", "forestsubtype", "stdage", "gsstk", "rd.ac.over", "rd.ac.regen",
                "invyr", "total_mtco2eac", "totalc", "volbfgrs.ac")


## Find initial measurements
prevdf <- dat[(dat$plt_cn %in% unique(dat$prev.plt.cn)),]
prevdf <- prevdf %>%
  select(plt_cn, colstodupe) %>%
  rename_with(~paste0(., ".prev")) %>%
  rename(prev.plt.cn = plt_cn.prev) %>%
  left_join(dat)

prevprevdf <- dat[(dat$plt_cn %in% unique(dat$prev.prev.plt.cn)),]
prevprevdf <- prevprevdf %>%
  select(plt_cn, colstodupe) %>%
  rename_with(~paste0(., ".prev.prev")) %>%
  rename(prev.prev.plt.cn = plt_cn.prev.prev) %>%
  left_join(dat)

dat_initial <- full_join(prevdf, prevprevdf)


prevprevprevdf <- dat[(dat$plt_cn %in% unique(dat$prev.prev.prev.plt.cn)),]
prevprevprevdf <- prevprevprevdf %>%
  select(plt_cn, colstodupe) %>%
  rename_with(~paste0(., ".prev.prev.prev")) %>%
  rename(prev.prev.prev.plt.cn = plt_cn.prev.prev.prev) %>%
  left_join(dat)

dat_initial <- full_join(dat_initial, prevprevprevdf)

dat_initial <- dat_initial %>%
  mutate(volbfgrs.ac.initial = ifelse(!is.na(volbfgrs.ac.prev.prev.prev),
                                      volbfgrs.ac.prev.prev.prev,
                                      ifelse(!is.na(volbfgrs.ac.prev.prev), volbfgrs.ac.prev.prev,
                                             ifelse(!is.na(volbfgrs.ac.prev), volbfgrs.ac.prev,
                                                    volbfgrs.ac))))

getmills <- left_join(fips, dat_initial %>% 
                        select(statecd, countycd, unitcd) %>% distinct()) %>%
  group_by(statecd, unitcd) %>%
  mutate(nummills = sum(nummills, na.rm = TRUE)) %>%
  ungroup()

datclean <- dat_initial %>%
  left_join(getmills) %>%
  filter(forestname %in% c(forestname1, forestname2),
         !is.na(ecosub),
         kindcd == 2,
         designcd == 1, 
         qa_status == 1,
         owngrpcd == 40,
         volbfgrs.ac.initial > 4000,
         siteclcd <= 6,
         stdorgcd == 0,
         (invyr - invyr.prev) <= mean(invyr - invyr.prev, na.rm=TRUE) + 2)


datclean$harvest <- ifelse(datclean$trtcd1 == 10 | 
                             datclean$baac.perc.cut >= 0.25 & datclean$dstrbcd1 == 0 |
                             !is.na(datclean$harvest_type1_srs), 1, 0) 

oakies <- datclean %>%
  filter(forestname == "Oak / hickory group", 
         plt_cn %in% unique(oakfvs$plt_cn))

length(unique(oakies$plt_cn))

#################################################################################
################## STEP 3 - Randomly select GMF and GOF plots ###################
### Randomly select OH plots with 4 measurements to assess Total C over time
### Select both GMF type plots and GOF type plots - or use TC approach with emission reductions
set.seed(11122024)

oaks <- oakies %>%
  filter(!is.na(prev.plt.cn) & forestname == "Oak / hickory group") %>%
  mutate(get_removals = plyr::round_any(round(baac.remv, digits=2)*100, 5, f = floor),
         unitname = as.numeric(paste0(statecd, unitcd)))

table(oaks$harvest)
table(oaks$get_removals)

#### Grow-only plots
getgofc.oaks <- oaks %>%
  filter(get_removals == 0, numharvs==0) %>%
  select(plt_cn, totalc, totalc.prev, totalc.prev.prev, totalc.prev.prev.prev,
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(totalc:totalc.prev.prev.prev), names_to = "time", 
               values_to = "totalc") %>%
  mutate(time = ifelse(time == "totalc", 15, 
                       ifelse(time == "totalc.prev", 10,
                              ifelse(time == "totalc.prev.prev", 5, 0))))

getgoft.oaks <- oaks %>%
  filter(get_removals == 0, numharvs==0) %>%
  select(plt_cn, invyr, invyr.prev, invyr.prev.prev, invyr.prev.prev.prev,
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(invyr:invyr.prev.prev.prev), names_to = "time", 
               values_to = "year") %>%
  mutate(time = ifelse(time == "invyr", 15, 
                       ifelse(time == "invyr.prev", 10,
                              ifelse(time == "invyr.prev.prev", 5, 0))))

getgof.oaks <- left_join(getgofc.oaks, getgoft.oaks) %>%
  #select(-time) %>%
  arrange(plt_cn, year) %>%
  group_by(plt_cn) %>%
  mutate(deltac = (totalc - lag(totalc, default=first(totalc)))/(year-lag(year, default=first(year))),
         deltac = ifelse(is.na(deltac), 0, deltac),
         mean.deltac = mean(deltac, na.rm=TRUE))

randomsubset.gof <- sample(unique(getgof.oaks$plt_cn), 10)


getgof.oaks.final <- getgof.oaks %>%
  filter(plt_cn %in% randomsubset.gof)

#### Finally, entire BAU
donorpoolc.oaks <- oaks %>%
  select(plt_cn, totalc, totalc.prev, totalc.prev.prev, totalc.prev.prev.prev,
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(totalc:totalc.prev.prev.prev), names_to = "time", 
               values_to = "totalc") %>%
  mutate(time = ifelse(time == "totalc", 15, 
                       ifelse(time == "totalc.prev", 10,
                              ifelse(time == "totalc.prev.prev", 5, 0))))
donorpoolt.oaks <- oaks %>%
  select(plt_cn, invyr, invyr.prev, invyr.prev.prev, invyr.prev.prev.prev, 
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(invyr:invyr.prev.prev.prev), names_to = "time", 
               values_to = "year") %>%
  mutate(time = ifelse(time == "invyr", 15, 
                       ifelse(time == "invyr.prev", 10,
                              ifelse(time == "invyr.prev.prev", 5, 0))))

donorpool.oaks <- left_join(donorpoolc.oaks, donorpoolt.oaks) %>%
  arrange(plt_cn, year) %>%
  group_by(plt_cn) %>%
  mutate(deltac = (totalc - lag(totalc, default=first(totalc)))/(year-lag(year, default=first(year))),
         deltac = ifelse(is.na(deltac), 0, deltac),
         mean.deltac = mean(deltac, na.rm=TRUE))

donorpool.oaks.final <- donorpool.oaks %>%
  filter(!plt_cn %in% c(randomsubset.gof))

#### Run matching code
pltstomatch.oaks <- donorpool.oaks.final %>%
  select(-totalc, -year, -mean.deltac, -deltac, -time) %>%
  distinct() %>%
  mutate(tx = 0) 

pltstochoose.oaksgof <- getgof.oaks.final %>%
  select(-totalc, -year, -mean.deltac, -deltac, -time) %>%
  distinct() %>%
  mutate(tx = 1) 

allplots <- rbind(pltstomatch.oaks, pltstochoose.oaksgof) %>%
  na.omit() 

allplots$plotnames = rownames(allplots)


# Use the MatchIt package in R to calculate Mahalanobis distances for each pairwise comparison of treat x control plot
m.dists <- MatchIt::matchit(tx ~  elev + stdage.prev.prev.prev + rd.ac.over.prev.prev.prev + siteclcd + 
                              rd.ac.regen.prev.prev.prev + ecosubcd, data = allplots,
                            method="nearest", 
                            distance="mahalanobis", replace=TRUE, ratio=10)

## Convert to a vetor
matches <- as.data.frame(m.dists$match.matrix) 

## Order m.dists by Mahalanobis Distance
matches = matches[order(matches[,2]), ]

## Build data frame and match to "project" plots
matches <- as.data.frame(matches)

## Make longer
matches <- matches %>%
  pivot_longer(cols = c(V1:V10), names_to = "project", values_to = "matches") %>%
  mutate(project = rep(allplots$plt_cn[allplots$tx==1], each=10))


## Append donor pool plt_cn to match dataframe
matches$matches = allplots$plt_cn[match(matches$matches, allplots$plotnames)]

## calculate weights for each plot
compositebaseline <- matches %>%
  group_by(project) %>%
  mutate(inv.m.dist = (1/matches) * 100,
         weight = inv.m.dist / sum(inv.m.dist, na.rm = TRUE)) %>%
  left_join(donorpool.oaks.final %>% select(plt_cn, deltac, time, baac.remv), by= c("matches" = "plt_cn")) %>%
  group_by(project, time) %>%
  summarize(deltac.bau = weighted.mean(deltac),
            baac.remv.bau = weighted.mean(baac.remv))

project.bau <- compositebaseline %>%
  ungroup() %>%
  left_join(getgof.oaks.final %>% select(plt_cn, deltac, time, baac.remv), by= c("project" = "plt_cn", "time"))

mean(project.bau$baac.remv.bau)
mean(project.bau$baac.remv)


addit_oakgof <- project.bau %>%
  ungroup() %>%
  group_by(project, time) %>%
  reframe(addit = deltac - mean(deltac.bau)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(time) %>%
  summarize(meangrow = mean(addit),
            segrow = sd(addit)/sqrt(length(addit)))

addit_oaks <- addit_oakgof %>%
  mutate(forestname = "Oak / hickory group",
         region = "Southern Apps",
         method = "dynamic") 

######## Compare to a static baseline
fvs_getgmfs <- oakfvs %>%
  select(plt_cn, time, delta.oak.grow) %>% 
  filter(time < 20) %>%
  group_by(time) %>%
  summarize(meangrow = mean(delta.oak.grow),
         segrow = sd(delta.oak.grow)) %>%
  mutate(forestname = "Oak / hickory group",
         region = "Southern Apps",
         method = "static")

addit_oaks = full_join(addit_oaks, fvs_getgmfs)

write.csv(addit_oaks, "output/southernapps_oaks_scenarios.csv", row.names = FALSE)
write.csv(project.bau, "output/southernapps_oaks_projectvbau_fia.csv", row.names = FALSE)


png("figures/dynamicvsstatic_oaks_extendedrotation.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(addit_oaks %>%
         mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
       aes(x=time, y=meangrow, col=method, fill=method)) +
  #geom_ribbon(aes(ymin=meangrow-segrow, ymax=meangrow+segrow), alpha=0.3) +
  geom_line() +
  ggtitle("h) Southern Apps: Oak / Hickory group") +
  theme_bw() + scale_color_d3(palette="category20c", name="Method") + scale_fill_d3(palette="category20c", name="Method") +
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")
dev.off()

###################################################################################################
###################################################################################################
### Randomly select MBB plots with 4 measurements to assess Total C over time
### Select GOF type plots
###################################################################################################
###################################################################################################
set.seed(11122024)
mbbies <- datclean %>%
  filter(forestname == "Maple / beech / birch group", 
         plt_cn %in% unique(mbbfvs$plt_cn))

length(mbbies$plt_cn)

mbbs <- mbbies %>%
  filter(!is.na(prev.plt.cn)) %>%
  mutate(get_removals = plyr::round_any(round(baac.remv, digits=2)*100, 5, f = floor),
         unitname = as.numeric(paste0(statecd, unitcd)))



#### Grow-only plots
getgofc.mbbs <- mbbs %>%
  filter(get_removals == 0, numharvs==0) %>%
  select(plt_cn, totalc, totalc.prev, totalc.prev.prev, totalc.prev.prev.prev,
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(totalc:totalc.prev.prev.prev), names_to = "time", 
               values_to = "totalc") %>%
  mutate(time = ifelse(time == "totalc", 15, 
                       ifelse(time == "totalc.prev", 10,
                              ifelse(time == "totalc.prev.prev", 5, 0))))

getgoft.mbbs <- mbbs %>%
  filter(get_removals == 0, numharvs==0) %>%
  select(plt_cn, invyr, invyr.prev, invyr.prev.prev, invyr.prev.prev.prev,
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(invyr:invyr.prev.prev.prev), names_to = "time", 
               values_to = "year") %>%
  mutate(time = ifelse(time == "invyr", 15, 
                       ifelse(time == "invyr.prev", 10,
                              ifelse(time == "invyr.prev.prev", 5, 0))))

getgof.mbbs <- left_join(getgofc.mbbs, getgoft.mbbs) %>%
  #select(-time) %>%
  arrange(plt_cn, year) %>%
  group_by(plt_cn) %>%
  mutate(deltac = (totalc - lag(totalc, default=first(totalc)))/(year-lag(year, default=first(year))),
         deltac = ifelse(is.na(deltac), 0, deltac),
         mean.deltac = mean(deltac, na.rm=TRUE))

randomsubset.gof <- sample(unique(getgof.mbbs$plt_cn), 10)


getgof.mbbs.final <- getgof.mbbs %>%
  filter(plt_cn %in% randomsubset.gof)


#### Finally, entire BAU
donorpoolc.mbbs <- mbbs %>%
  select(plt_cn, totalc, totalc.prev, totalc.prev.prev, totalc.prev.prev.prev,
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(totalc:totalc.prev.prev.prev), names_to = "time", 
               values_to = "totalc") %>%
  mutate(time = ifelse(time == "totalc", 15, 
                       ifelse(time == "totalc.prev", 10,
                              ifelse(time == "totalc.prev.prev", 5, 0))))
donorpoolt.mbbs <- mbbs %>%
  select(plt_cn, invyr, invyr.prev, invyr.prev.prev, invyr.prev.prev.prev, 
         elev, stdage.prev.prev.prev,
         unitname, baac.remv, harvest, nummills, rd.ac.over.prev.prev.prev,
         rd.ac.regen.prev.prev.prev, siteclcd, ecosubcd, forestname) %>%
  na.omit() %>%
  pivot_longer(cols = c(invyr:invyr.prev.prev.prev), names_to = "time", 
               values_to = "year") %>%
  mutate(time = ifelse(time == "invyr", 15, 
                       ifelse(time == "invyr.prev", 10,
                              ifelse(time == "invyr.prev.prev", 5, 0))))

donorpool.mbbs <- left_join(donorpoolc.mbbs, donorpoolt.mbbs) %>%
  arrange(plt_cn, year) %>%
  group_by(plt_cn) %>%
  mutate(deltac = (totalc - lag(totalc, default=first(totalc)))/(year-lag(year, default=first(year))),
         deltac = ifelse(is.na(deltac), 0, deltac),
         mean.deltac = mean(deltac, na.rm=TRUE))

donorpool.mbbs.final <- donorpool.mbbs %>%
  filter(!plt_cn %in% c(randomsubset.gof))

#### Find Matches to each plot in GOF scenario
### Do some matching
### Match the sites with the potential controls
pltstomatch.mbbs <- donorpool.mbbs.final %>%
  select(-totalc, -year, -mean.deltac, -deltac, -time) %>%
  distinct() %>%
  mutate(tx = 0) 

pltstochoose.mbbsgof <- getgof.mbbs.final %>%
  select(-totalc, -year, -mean.deltac, -deltac, -time) %>%
  distinct() %>%
  mutate(tx = 1) 

allplots <- rbind(pltstomatch.mbbs, pltstochoose.mbbsgof) %>%
  na.omit() 

allplots$plotnames = rownames(allplots)


# Use the MatchIt package in R to calculate Mahalanobis distances for each pairwise comparison of treat x control plot
m.dists <- MatchIt::matchit(tx ~  elev + stdage.prev.prev.prev + rd.ac.over.prev.prev.prev + siteclcd + 
                              rd.ac.regen.prev.prev.prev + ecosubcd, data = allplots,
                            method="nearest", 
                            distance="mahalanobis", replace=TRUE, ratio=10)

## Convert to a vetor
matches <- as.data.frame(m.dists$match.matrix) 

## Order m.dists by Mahalanobis Distance
matches = matches[order(matches[,2]), ]

## Build data frame and match to "project" plots
matches <- as.data.frame(matches)

## Make longer
matches <- matches %>%
  pivot_longer(cols = c(V1:V10), names_to = "project", values_to = "matches") %>%
  mutate(project = rep(allplots$plt_cn[allplots$tx==1], each=10))


## Append donor pool plt_cn to match dataframe
matches$matches = allplots$plt_cn[match(matches$matches, allplots$plotnames)]

## calculate weights for each plot
compositebaseline <- matches %>%
  group_by(project) %>%
  mutate(inv.m.dist = (1/matches) * 100,
         weight = inv.m.dist / sum(inv.m.dist, na.rm = TRUE)) %>%
  left_join(donorpool.mbbs.final %>% select(plt_cn, deltac, time, baac.remv), by= c("matches" = "plt_cn")) %>%
  group_by(project, time) %>%
  summarize(deltac.bau = weighted.mean(deltac),
            baac.remv.bau = weighted.mean(baac.remv))

project.bau <- compositebaseline %>%
  ungroup() %>%
  left_join(getgof.mbbs.final %>% select(plt_cn, deltac, time, baac.remv), by= c("project" = "plt_cn", "time"))

mean(project.bau$baac.remv.bau)
mean(project.bau$baac.remv)


addit_mbbgof <- project.bau %>%
  ungroup() %>%
  group_by(project, time) %>%
  reframe(addit = deltac - mean(deltac.bau)) %>%
  distinct() %>%
  ungroup() %>%
  group_by(time) %>%
  summarize(meangrow = mean(addit),
            segrow = sd(addit)/sqrt(length(addit)))

addit_mbbs <- addit_mbbgof %>%
  mutate(forestname = "Maple / beech / birch group",
         region = "Southern Apps",
         method = "dynamic") 

######## Compare to a static baseline
fvs_getgofs <- mbbfvs %>%
  select(plt_cn, time, delta.mbb.grow) %>% 
  filter(time < 20) %>%
  group_by(time) %>%
  summarize(meangrow = mean(delta.mbb.grow),
            segrow = sd(delta.mbb.grow)) %>%
  mutate(forestname = "Maple / beech / birch group",
         region = "Southern Apps",
         method = "static")

addit_mbbs = full_join(addit_mbbs, fvs_getgofs)


mean(addit_mbbs$meangrow[addit_mbbs$method=="dynamic"]) ### 0.90
mean(addit_mbbs$meangrow[addit_mbbs$method=="static"]) ### 1.08

mean(addit_oaks$meangrow[addit_oaks$method=="dynamic"]) ### 0.09
mean(addit_oaks$meangrow[addit_oaks$method=="static"]) ### 2.28

write.csv(addit_mbbs, "output/southernapps_mbbs_scenarios.csv", row.names = FALSE)
write.csv(project.bau, "output/southernapps_mbbs_projectvbau_fia.csv", row.names = FALSE)


png("figures/dynamicvsstatic_mbbs_extendedrotation.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(addit_mbbs %>%
         mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
       aes(x=time, y=meangrow, col=method, fill=method)) +
  #geom_ribbon(aes(ymin=meangrow-segrow, ymax=meangrow+segrow), alpha=0.3) +
  geom_line() +
  ggtitle("i) Southern Apps: Maple / beech / birch group") +
  theme_bw() + scale_color_d3(palette="category20c", name="Method") + scale_fill_d3(palette="category20c", name="Method") +
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")
dev.off()






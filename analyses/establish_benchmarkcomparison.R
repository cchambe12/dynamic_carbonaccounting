### Evaluate actual growth in the FIA project sites over time
## Maybe this could serve as a benchmark comparison of additionality
## Started 6 May 2026 by Cat

### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

### Load Libraries
library(gridExtra)
library(patchwork)
library(ggsci)
library(viridis)
library(RColorBrewer)
library(ggthemes)
library(brms)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(tidyr)
library(broom.mixed)

## Set working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/")

#################################################################################
################ Load in all data and prepare plots ############################
#################################################################################
cappsoak <- read.csv("centralapps/output/centralapps_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Central Apps") %>%
  filter(!is.na(time))
cappsmbb <- read.csv("centralapps/output/centralapps_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Central Apps") %>%
  filter(!is.na(time))

neoak <- read.csv("northeast/output/northeast_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Northeast") %>%
  filter(!is.na(time))
nembb <- read.csv("northeast/output/northeast_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Northeast") %>%
  filter(!is.na(time))

nwoak <- read.csv("northwoods/output/northwoods_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Northwoods") %>%
  filter(!is.na(time))
nwmbb <- read.csv("northwoods/output/northwoods_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Northwoods") %>%
  filter(!is.na(time))

sappsoak <- read.csv("southernapps/output/southernapps_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Southern Apps") %>%
  filter(!is.na(time))
sappsmbb <- read.csv("southernapps/output/southernapps_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Southern Apps") %>%
  filter(!is.na(time))


### And clean up to make sure consistent PLT_CNs ### 
allmeasured <- full_join(cappsoak, cappsmbb) %>%
  full_join(neoak) %>%
  full_join(nembb) %>%
  full_join(nwoak) %>%
  full_join(nwmbb) %>%
  full_join(sappsoak) %>%
  full_join(sappsmbb) %>%
  ungroup() %>%
  group_by(plt_cn, time, method) %>%
  mutate(addit = mean(deltac)) %>%
  distinct() %>%
  select(plt_cn, time, forestname, region, addit) %>%
  group_by(forestname, region) %>%
  summarize(mean_addit = mean(addit),
            se_addit = sd(addit)/sqrt(length(addit)))
  
  
  

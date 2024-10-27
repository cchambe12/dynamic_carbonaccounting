#### Integrate output from the BAU prefeasibility assessment
## Use this to start looking at a simplified covariate analysis

## Started 27 October 2024 by Cat

### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(tidyr)
library(dplyr)
library(ggplot2)
library(brms)
library(bayesplot)

## Set working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/northwoods/fvs")

### Select input/output folder
datafolder <- "~/Documents/git/dynamic_carbonaccounting/analyses/northwoods/fvs/output/"

## Select columns of interest for covariate analysis
colstokeep <- c("plt_cn", "harvest", "timediff", "baac.remv", "qmd.prev",
                "baac.prev", "lorey_ht.prev", "desire.prev", "statecd", "unitcd")

## Are you assessing Maple / beech / birch or Oak / hickory? If Oak / Hickory then say TRUE
useoak <- TRUE

################################################################################
#### Step 1 - Get the FIA data from the prefeasibility scoping
if(useoak == TRUE){
  bau <- read.csv( "../output/clean_northeast_fiadata.csv") %>%
      filter(forestname == "Oak / hickory group")
}else {
  bau <- read.csv( "../output/clean_northeast_fiadata.csv") %>%
    filter(forestname == "Maple / beech / birch group")
}

################################################################################
#### Step 2 - Get outputs from the FVS cleaning and join to BAU data
if(useoak == TRUE){
  fvs <- read.csv("output/clean_fvsoutput_oak.csv") %>%
      rename(plt_cn = Stand_CN)
}else {
  fvs <- read.csv("output/clean_fvsoutput_mbb.csv") %>%
    rename(plt_cn = Stand_CN)
}


## Subset the FVS runs to get only the FIA plots of interest
cov <- fvs %>%
  filter(plt_cn %in% unique(bau$plt_cn)) %>%
  left_join(bau %>% select(all_of(colstokeep)))

  
#  length(unique(bau$plt_cn)) ## 
#  length(unique(fvs$plt_cn)) ## 
#  length(unique(cov$plt_cn)) ## 


################################################################################
#### Step 3 - Begin cleaning data
## z-score to get everything on the same level in case we want to model

cov <- cov %>%
  mutate(baacz = (baac.prev - mean(baac.prev, na.rm=TRUE)) / (sd(baac.prev, na.rm=TRUE)),
         loreysz = (lorey_ht.prev - mean(lorey_ht.prev, na.rm=TRUE)) / (sd(lorey_ht.prev, na.rm=TRUE)),
         desirez = (desire.prev - mean(desire.prev, na.rm=TRUE)) / (sd(desire.prev, na.rm=TRUE)),
         qmdz = (qmd.prev - mean(qmd.prev, na.rm=TRUE)) / (sd(qmd.prev, na.rm=TRUE)),
         unitname = paste(statecd, unitcd))

covgmf <- cov %>% filter(StandID == "GMF")
harv <- covgmf %>% filter(harvest == 1)



################################################################################
################################################################################
## Step 4 - Begin running brms models
covgmf <- cov %>% filter(StandID == "GMF")
hl.mod <- brm( (harvest*(20/timediff)) ~ baacz + loreysz + desirez + qmdz +
                 (1 | unitname),
               chains=2, data=covgmf, control = list(adapt_delta=0.99))


## Plot model output to see what variable are predictive for initial carbon
hl.p <- mcmc_intervals(hl.mod, pars=c("b_baacz", 
                                      "b_qmdz",
                                      "b_loreysz",
                                      "b_desirez"), 
                       prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-0.15, 0.35), ylim=c(1, 4.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(100*x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.01, xend = 0.32, y = 4.9, yend = 4.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.01, xend = -0.12, y = 4.9, yend = 4.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.18, y = 5.05, colour = "black", size=3.5, label=bquote("Increased likelihood")) + 
  annotate("text", x = -0.06, y = 5.05, colour = "black", size=3.5, label=bquote("Decreased likelihood")) +
  scale_y_discrete(limits=rev(c("b_baacz", 
                                "b_qmdz",
                                "b_loreysz",
                                "b_desirez")), 
                   labels=rev(c("Previous BAAC", "Previous QMD",
                                "Previous Lorey's Height", 
                                "Previous Species Desireability"))) +
  xlab("Model estimate of change in harvest likelihood (%)") 

############################ Harvest Intensity Model ###########################
## Run the model
harv <- covgmf %>% filter(harvest == 1)
hi.mod <- brm( baac.remv ~ baacz + loreysz + desirez + qmdz +
                 (1 | unitname),
               chains=2, data=harv, control = list(adapt_delta=0.99))


## Plot model output to see what variable are predictive for initial carbon
hi.p <- mcmc_intervals(hi.mod, pars=c("b_baacz", 
                                      "b_qmdz",
                                      "b_loreysz",
                                      "b_desirez"), 
                       prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-.18, .25), ylim=c(1, 4.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(100*x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.01, xend = 0.23, y = 4.9, yend = 4.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.01, xend = -0.15, y = 4.9, yend = 4.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.15, y = 5.05, colour = "black", size=3.5, label=bquote("Increased intensity")) + 
  annotate("text", x = -0.08, y = 5.05, colour = "black", size=3.5, label=bquote("Decreased intensity")) +
  scale_y_discrete(limits=rev(c("b_baacz", 
                                "b_qmdz",
                                "b_loreysz",
                                "b_desirez")), 
                   labels=rev(c("Previous BAAC", "Previous QMD",
                                "Previous Lorey's Height", 
                                "Previous Species Desireability"))) +
  xlab("Model estimate of change in harvest intensity (% BAAC removed)") 


################################################################################
################################################################################
### Step 5 - Save the outputs
png(paste0("figures/modeloutput_covariateanalysis_", i,".png"), 
    width=18, height=6, unit="in", res=200)
grid.arrange(hl.p, hi.p, addit.p, ncol=3)
dev.off()





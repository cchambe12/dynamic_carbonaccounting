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
library(sjPlot)
library(sjlabelled)
library(sjmisc)

## Set working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/centralapps/fvs/02_carbonaccounting/")

### Select input/output folder
datafolder <- "~/Documents/git/dynamic_carbonaccounting/analyses/centralapps/fvs/02_carbonaccounting/output/"

## Select columns of interest for covariate analysis
colstokeep <- c("plt_cn", "harvest", "timediff", "baac.remv", "qmdchange", "qmd.prev", "volbfgrs.ac.prev",
                "baac.prev", "lorey_ht.prev", "desire.prev", "statecd", "unitcd")

## Are you assessing Maple / beech / birch or Oak / hickory? If Oak / Hickory then say TRUE
useoak <- TRUE

################################################################################
#### Step 1 - Get the FIA data from the prefeasibility scoping
if(useoak == TRUE){
  i <- "oak"
  bau <- read.csv( "../../output/clean_centralapps_fiadata.csv") %>%
      filter(forestname == "Oak / hickory group")
}else {
  i <- "mbb"
  bau <- read.csv( "../../output/clean_centralapps_fiadata.csv") %>%
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
#cov <- fvs %>%
#  filter(plt_cn %in% unique(bau$plt_cn)) %>%
#  left_join(bau %>% select(all_of(colstokeep)))

  
#  length(unique(bau$plt_cn)) ## 
#  length(unique(fvs$plt_cn)) ## 
#  length(unique(cov$plt_cn)) ## 


################################################################################
#### Step 3 - Begin cleaning data
## z-score to get everything on the same level in case we want to model

cov <- bau %>%
  mutate(PrevBA = (baac.prev - mean(baac.prev, na.rm=TRUE)) / (sd(baac.prev, na.rm=TRUE)),
         PrevBFVol = (volbfgrs.ac.prev - mean(volbfgrs.ac.prev, na.rm=TRUE)) / (sd(volbfgrs.ac.prev, na.rm=TRUE)),
         PrevLoreyHT = (lorey_ht.prev - mean(lorey_ht.prev, na.rm=TRUE)) / (sd(lorey_ht.prev, na.rm=TRUE)),
         PrevSpeciesDesireability = (desire.prev - mean(desire.prev, na.rm=TRUE)) / (sd(desire.prev, na.rm=TRUE)),
         PrevQMD = (qmd.prev - mean(qmd.prev, na.rm=TRUE)) / (sd(qmd.prev, na.rm=TRUE)),
         PrevStdAge = (stdage.prev - mean(stdage.prev, na.rm=TRUE)) / (sd(stdage.prev, na.rm=TRUE)),
         unitname = paste(statecd, unitcd)) %>%
  select(plt_cn, PrevBA, baac.prev, PrevBFVol, volbfgrs.ac.prev, PrevLoreyHT, lorey_ht.prev, PrevSpeciesDesireability,
         desire.prev, PrevQMD, qmd.prev, PrevStdAge, stdage.prev, unitname, ecosub, forestname, statecd, countycd, unitcd,
         baac.remv, harvest, qmdchange) %>%
  na.omit()


harv <- cov %>% filter(harvest == 1)



################################################################################
################################################################################
### Check out the raw data first
ggplot(cov, aes(x=baac.prev, y=harvest)) + geom_point() + geom_smooth()

## Including for colinearity 
ggplot(cov, aes(x=baac.prev, y=lorey_ht.prev)) + geom_point() + theme_bw()
ggplot(cov, aes(x=baac.prev, y=volbfgrs.ac.prev)) + geom_point() + theme_bw() ## colinearity issues
ggplot(cov, aes(x=baac.prev, y=desire.prev)) + geom_point() + theme_bw()
ggplot(cov, aes(x=baac.prev, y=qmd.prev)) + geom_point() + theme_bw()
ggplot(cov, aes(x=baac.prev, y=stdage.prev)) + geom_point() + theme_bw()

ggplot(cov, aes(x=lorey_ht.prev, y=desire.prev)) + geom_point() + theme_bw()
ggplot(cov, aes(x=lorey_ht.prev, y=qmd.prev)) + geom_point() + theme_bw() ## colinearity issues
ggplot(cov, aes(x=lorey_ht.prev, y=stdage.prev)) + geom_point() + theme_bw()

## Step 4 - Begin running brms models
cov$totremv <- cov$harvest * cov$baac.remv
harv.mod <- brm( totremv ~ PrevBA + PrevLoreyHT + PrevSpeciesDesireability + PrevStdAge + 
                 (PrevBA + PrevLoreyHT + PrevSpeciesDesireability + PrevStdAge | unitname), 
               cores=2, chains = 2, 
               iter = 3000, warmup = 2000,
               prior = prior(normal(0,1), class = "b"), 
               data=cov, control = list(adapt_delta=0.99))


### Rhats and ESS look great!

## Check posteriors
color_scheme_set("red")
ppc_dens_overlay(y = cov$totremv,
                 yrep = posterior_predict(harv.mod, draws = 50))


## Plot model output to see what variable are predictive for harvest likelihood
theme_set(theme_sjplot())

hlplot <- sjPlot::plot_model(hl.mod, vline.color = "black", 
                             title = paste0("Harvest likelihood")) 
  

hl.p <- mcmc_intervals(hl.mod, pars=c("b_baacz", 
                                      "b_volbfz",
                                      "b_qmdz",
                                      "b_loreysz",
                                      "b_desirez"), 
                       prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-0.15, 0.35), ylim=c(1, 5.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(100*x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.01, xend = 0.32, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.01, xend = -0.12, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.18, y = 6.05, colour = "black", size=3.5, label=bquote("Increased likelihood")) + 
  annotate("text", x = -0.06, y = 6.05, colour = "black", size=3.5, label=bquote("Decreased likelihood")) +
  scale_y_discrete(limits=rev(c("b_baacz", 
                                "b_volbfz",
                                "b_qmdz",
                                "b_loreysz",
                                "b_desirez")), 
                   labels=rev(c("Previous BAAC", "Previous BF Vol", "Previous QMD",
                                "Previous Lorey's Height", 
                                "Previous Species Desireability"))) +
  xlab("Model estimate of change in harvest likelihood (%)") 

############################## QMD Change Model ################################
## Run the model
qmd.mod <- brm( qmdchange ~ baacz + volbfz + loreysz + desirez + qmdz +
                 (1 | unitname),
               chains=2, data=harv, control = list(adapt_delta=0.99))


## Plot model output to see what variable are predictive for initial carbon
qmd.p <- mcmc_intervals(qmd.mod, pars=c("b_baacz", 
                                      "b_volbfz",
                                      "b_qmdz",
                                      "b_loreysz",
                                      "b_desirez"), 
                       prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-1.5, 1), ylim=c(1, 5.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.01, xend = 0.9, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.01, xend = -1.4, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.5, y = 6.05, colour = "black", size=3.5, label=bquote("Increased intensity")) + 
  annotate("text", x = -0.8, y = 6.05, colour = "black", size=3.5, label=bquote("Decreased intensity")) +
  scale_y_discrete(limits=rev(c("b_baacz", 
                                "b_volbfz",
                                "b_qmdz",
                                "b_loreysz",
                                "b_desirez")), 
                   labels=rev(c("Previous BAAC", "Previous BF Vol", "Previous QMD",
                                "Previous Lorey's Height", 
                                "Previous Species Desireability"))) +
  xlab("Model estimate of change in harvest intensity (% BAAC removed)") 

############################## Harvest Intensity Model ################################
hi.mod <- brm( baac.remv ~ baacz + volbfz + loreysz + desirez + qmdz +
                 (1 | unitname),
               chains=2, data=harv, control = list(adapt_delta=0.99))


## Plot model output to see what variable are predictive for initial carbon
hi.p <- mcmc_intervals(hi.mod, pars=c("b_baacz", 
                                      "b_volbfz",
                                      "b_qmdz",
                                      "b_loreysz",
                                      "b_desirez"), 
                       prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-0.15, 0.35), ylim=c(1, 5.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(100*x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.01, xend = 0.32, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.01, xend = -0.12, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.18, y = 6.05, colour = "black", size=3.5, label=bquote("Increased likelihood")) + 
  annotate("text", x = -0.06, y = 6.05, colour = "black", size=3.5, label=bquote("Decreased likelihood")) +
  scale_y_discrete(limits=rev(c("b_baacz", 
                                "b_volbfz",
                                "b_qmdz",
                                "b_loreysz",
                                "b_desirez")), 
                   labels=rev(c("Previous BAAC", "Previous BF Vol", "Previous QMD",
                                "Previous Lorey's Height", 
                                "Previous Species Desireability"))) +
  xlab("Model estimate of change in harvest likelihood (%)") 


################################################################################
################################################################################
### Step 5 - Save the outputs
png(paste0("figures/modeloutput_covariateanalysis_", i,".png"), 
    width=18, height=6, unit="in", res=200)
grid.arrange(hl.p, hi.p, qmd.p, ncol=3)
dev.off()

################################################################################
################################################################################
### Step 6 - Adjust thresholds

if(useoak == TRUE){
  
  ### Potentially a signal with species... 
  hist(cov$baac.prev)
  hist(harv$baac.prev)
  
  hist(cov$lorey_ht.prev)
  hist(harv$lorey_ht.prev)
  
  hist(cov$volbfgrs.ac.prev)
  hist(harv$volbfgrs.ac.prev)
  
  
  
  covsub <- cov %>%
    filter(baac.prev >= 90 & lorey_ht.prev >= 60 & volbfgrs.ac.prev >= 5000)
  
  harvsub <- covsub %>%
    filter(harvest == 1)
  
  mean(harvsub$baac.remv, na.rm=TRUE)
  mean(harv$baac.remv, na.rm=TRUE)
  
  ### Build the figures for output
  
  
  
  
  ggplot(cov, aes(x=baac.prev, y=baac.remv)) + geom_point() +
    geom_smooth()
  
} else {
  
  ### Looks like there might be a relationship with Lorey's Height
  hist(cov$qmd.prev) ## Let's try 60 as minimum
  hist(harv$qmd.prev)
  
  mean(harv$baac.remv, na.rm=TRUE) ## Average is 25% removal... 
  
  mean(harv$baac.remv); sd(harv$baac.remv)/sqrt(length(harv$baac.remv))
  mean(harv$baac.remv[harv$qmd.prev>=8]); sd(harv$baac.remv[harv$qmd.prev>=8])/sqrt(length(harv$baac.remv[harv$qmd.prev>=8]))
  mean(harv$baac.remv[harv$qmd.prev>=10]); sd(harv$baac.remv[harv$qmd.prev>=10])/sqrt(length(harv$baac.remv[harv$qmd.prev>=10]))
  mean(harv$baac.remv[harv$qmd.prev>=12]); sd(harv$baac.remv[harv$qmd.prev>=12])/sqrt(length(harv$baac.remv[harv$qmd.prev>=12]))
  
  ggplot(harv, aes(x=qmd.prev, y=baac.remv*100)) + 
    geom_point() + geom_smooth(method="lm") +
    theme_bw() + xlab("Previous QMD") + ylab("Harvest intensity (%)")
  
  mean(cov$delta[cov$StandID=="GMF"], na.rm=TRUE); sd(cov$delta[cov$StandID=="GMF"], na.rm=TRUE)
  mean(cov$delta[cov$StandID=="GROW"]); sd(cov$delta[cov$StandID=="GROW"])
  
  covsub <- cov %>%
    filter(lorey_ht.prev >= 60)
  
  mean(covsub$baac.remv[covsub$harvest==1], na.rm=TRUE)
  
  mean(covsub$delta[covsub$StandID=="GMF"]); sd(covsub$delta[covsub$StandID=="GMF"])
  
  ### Okay actually gains decrease a bit then... 
  
  
  png(paste0("figures/eligible_carbongains_mbb.png"), 
      width=7, height=5, unit="in", res=200)
  ggplot(covsub %>% filter(StandID != "BAU", time <= 20) %>% 
           group_by(StandID) %>%
           mutate(StandID = ifelse(StandID == "GROW", "GOF", "GMF")) %>%
           summarize(meanc = mean(delta, na.rm=TRUE),
                     sec = sd(delta, na.rm=TRUE)/4), 
         aes(y=meanc, x=StandID, col = StandID, fill=StandID)) + 
    geom_col() + geom_errorbar(aes(ymin = meanc - sec, ymax = meanc + sec)) +
    scale_color_d3(name="Practice", palette = "category20b") +
    scale_fill_d3(name = "Practice", palette = "category20b") +
    theme_bw() + xlab("") + ylab("Total Mt CO2 per acre") +
    theme(legend.position = "none") +
    geom_text(aes(label = round(meanc, digits=2)), y=0.08, col="white") 
  dev.off()
  
}




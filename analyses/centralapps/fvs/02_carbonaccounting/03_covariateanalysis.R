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
library(patchwork)

## Set working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/centralapps/fvs/02_carbonaccounting/")

### Select input/output folder
datafolder <- "~/Documents/git/dynamic_carbonaccounting/analyses/centralapps/fvs/02_carbonaccounting/output/"

## Select columns of interest for covariate analysis
colstokeep <- c("plt_cn", "harvest", "timediff", "baac.remv", "qmdchange", "qmd.prev", "volbfgrs.ac.prev",
                "baac.prev", "lorey_ht.prev", "desire.prev", "statecd", "unitcd", "nummills")

## Are you assessing Maple / beech / birch or Oak / hickory? If Oak / Hickory then say TRUE
useoak <- TRUE

#### Set up plot themes
color_scheme_set("red")
theme_set(theme_sjplot())

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
#### Step 2 - Begin cleaning data
## z-score to get everything on the same level in case we want to model

cov <- bau %>%
  mutate(PrevBA = (baac.prev - mean(baac.prev, na.rm=TRUE)) / (sd(baac.prev, na.rm=TRUE)),
         PrevBFVol = (volbfgrs.ac.prev - mean(volbfgrs.ac.prev, na.rm=TRUE)) / (sd(volbfgrs.ac.prev, na.rm=TRUE)),
         PrevLoreyHT = (lorey_ht.prev - mean(lorey_ht.prev, na.rm=TRUE)) / (sd(lorey_ht.prev, na.rm=TRUE)),
         PrevSpeciesDesireability = (desire.prev - mean(desire.prev, na.rm=TRUE)) / (sd(desire.prev, na.rm=TRUE)),
         PrevQMD = (qmd.prev - mean(qmd.prev, na.rm=TRUE)) / (sd(qmd.prev, na.rm=TRUE)),
         PrevStdAge = (stdage.prev - mean(stdage.prev, na.rm=TRUE)) / (sd(stdage.prev, na.rm=TRUE)),
         NumMills = (nummills - mean(nummills, na.rm=TRUE)) / (sd(nummills, na.rm=TRUE)),
         unitname = paste(statecd, unitcd)) %>%
  select(plt_cn, PrevBA, baac.prev, PrevBFVol, volbfgrs.ac.prev, PrevLoreyHT, lorey_ht.prev, PrevSpeciesDesireability,
         desire.prev, PrevQMD, qmd.prev, PrevStdAge, stdage.prev, unitname, ecosub, forestname, statecd, countycd, unitcd,
         baac.remv, harvest, qmdchange, NumMills, nummills) %>%
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

## Step 3 - Begin running brms models
zip_prior <- c(set_prior("normal(0,1)", class = "Intercept"),
              set_prior("normal(0,0.5)", class = "b"))

hl.mod <- brm( harvest | trials(1) ~ PrevBA + PrevLoreyHT + PrevSpeciesDesireability + PrevStdAge + NumMills +
                 (1 | unitname / ecosub), 
               cores=2, chains = 2, 
               iter = 3000, warmup = 2000,
               family = zero_inflated_binomial(link = "logit", link_zi = "logit"),
               prior = zip_prior, 
               data=cov, control = list(adapt_delta=0.99))


### Rhats and ESS look great!

## Check posteriors
ppc_dens_overlay(y = cov$harvest,
                 yrep = posterior_predict(hl.mod, draws = 50))


## Plot model output to see what variable are predictive for harvest likelihood
sjPlot::plot_model(hl.mod, vline.color = "black", ci.style = "whisker",
                             title = paste0("Harvest Likelihood (%)")) #+ ylim(-0.5, 0.5)
  

hl.p <- mcmc_intervals(hl.mod, pars=c("b_PrevBA", 
                                      "b_PrevLoreyHT",
                                      "b_PrevSpeciesDesireability",
                                      "b_PrevStdAge", "b_NumMills"), 
                       prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-0.75, 1.25), ylim=c(1, 5.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(100*x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.01, xend = 1.2, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.01, xend = -0.7, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.6, y = 6.05, colour = "black", size=3.5, label=bquote("Increased likelihood")) + 
  annotate("text", x = -0.35, y = 6.05, colour = "black", size=3.5, label=bquote("Decreased likelihood")) +
  scale_y_discrete(limits=rev(c("b_PrevBA", 
                                "b_PrevLoreyHT",
                                "b_PrevSpeciesDesireability",
                                "b_PrevStdAge", "b_NumMills")), 
                   labels=rev(c("Previous BAAC", "Previous Lorey's HT", "Previous Desireability",
                                "Previous Stand Age", "Number of Mills"))) +
  xlab("Model estimate of change in harvest likelihood (%)") 


############################## Harvest Intensity Model ################################

hi.mod <- brm( baac.remv ~ PrevBA + PrevLoreyHT + PrevSpeciesDesireability + PrevStdAge + NumMills +
                 (1 | unitname / ecosub), 
               cores=2, chains = 2, 
               iter = 3000, warmup = 2000,
               prior = zip_prior, 
               data=harv, control = list(adapt_delta=0.99))


### Rhats and ESS look great!

## Check posteriors
ppc_dens_overlay(y = harv$baac.remv,
                 yrep = posterior_predict(hi.mod, draws = 50))


## Plot model output to see what variable are predictive for harvest likelihood
sjPlot::plot_model(hi.mod, vline.color = "black", ci.style = "whisker",
                   title = paste0("Harvest Intensity"))


hi.p <- mcmc_intervals(hi.mod, pars=c("b_PrevBA", 
                                      "b_PrevLoreyHT",
                                      "b_PrevSpeciesDesireability",
                                      "b_PrevStdAge", "b_NumMills"), 
                       prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-0.1, 0.15), ylim=c(1, 5.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(100*x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.001, xend = 0.14, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.001, xend = -0.09, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.07, y = 6.05, colour = "black", size=3.5, label=bquote("Increased intensity")) + 
  annotate("text", x = -0.045, y = 6.05, colour = "black", size=3.5, label=bquote("Decreased intensity")) +
  scale_y_discrete(limits=rev(c("b_PrevBA", 
                                "b_PrevLoreyHT",
                                "b_PrevSpeciesDesireability",
                                "b_PrevStdAge", "b_NumMills")), 
                   labels=rev(c("Previous BAAC", "Previous Lorey's HT", "Previous Desireability",
                                "Previous Stand Age", "Number of Mills"))) +
  xlab("Model estimate of change in % BA removed in harvest") 

############################## QMD Change Model ################################
## Run the model
qmd.mod <- brm( qmdchange ~ PrevBA + PrevLoreyHT + PrevSpeciesDesireability + PrevStdAge + NumMills +
                   (1 | unitname / ecosub), 
                 cores=2, chains = 2, 
                 iter = 3000, warmup = 2000,
                 data=harv, control = list(adapt_delta=0.99))

### Rhats and ESS look great!

## Check posteriors
ppc_dens_overlay(y = harv$qmdchange,
                 yrep = posterior_predict(qmd.mod, draws = 50))


## Plot model output to see what variable are predictive for initial carbon
sjPlot::plot_model(qmd.mod, vline.color = "black", ci.style = "whisker",
                               title = paste0("Change in QMD")) 


qmd.p <- mcmc_intervals(qmd.mod, pars=c("b_PrevBA", 
                                          "b_PrevLoreyHT",
                                          "b_PrevSpeciesDesireability",
                                          "b_PrevStdAge", "b_NumMills"), 
                         prob=0.5, prob_outer = 0.89) +
  coord_cartesian(xlim=c(-1, 1), ylim=c(1, 5.15), clip = 'off') +
  scale_x_continuous(labels = function(x) {format(x, digits = 2)}) +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.1, xend = 0.9, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.1, xend = -0.9, y = 5.9, yend = 5.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.5, y = 6.05, colour = "black", size=3.5, label=bquote("Thin from above")) + 
  annotate("text", x = -0.5, y = 6.05, colour = "black", size=3.5, label=bquote("Thin from below")) +
  scale_y_discrete(limits=rev(c("b_PrevBA", 
                                "b_PrevLoreyHT",
                                "b_PrevSpeciesDesireability",
                                "b_PrevStdAge", "b_NumMills")), 
                   labels=rev(c("Previous BAAC", "Previous Lorey's HT", "Previous Desireability",
                                "Previous Stand Age", "Number of Mills"))) +
  xlab("Model estimate of change in QMD") 


################################################################################
################################################################################
### Step 4 - Save the outputs
png(paste0("figures/modeloutput_covariateanalysis_", i,".png"), 
    width=18, height=6, unit="in", res=200)
grid.arrange(hl.p, hi.p, qmd.p, ncol=3)
dev.off()

################################################################################
################################################################################
### Step 6 - Adjust thresholds

if(useoak == TRUE){
  
  ### Build figures for manuscript
  #### Assess HL
  cov$hl <- as.data.frame(predict(hl.mod))$Estimate
  ggplot(cov, aes(x=lorey_ht.prev, y=hl)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(cov$hl[cov$lorey_ht.prev>=70]) ### This is the key winner
  mean(cov$hl)
  
  ggplot(cov, aes(x=baac.prev, y=hl)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(cov$hl[cov$baac.prev>=100])
  mean(cov$hl)
  
  #### Assess HI
  harv$hi <- as.data.frame(predict(hi.mod))$Estimate
  ggplot(harv, aes(x=lorey_ht.prev, y=hi)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$hi[harv$lorey_ht.prev>=70]) ### This is the key winner
  mean(harv$hi)
  
  ggplot(harv, aes(x=baac.prev, y=hi)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$hi[harv$baac.prev>=100])
  mean(harv$hi)
  
  #### Assess QMD
  harv$qmdest <- as.data.frame(predict(qmd.mod))$Estimate
  ggplot(harv, aes(x=lorey_ht.prev, y=qmdest)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$qmdest[harv$lorey_ht.prev>=70])
  mean(harv$qmdest)
  
  ggplot(harv, aes(x=baac.prev, y=qmdest)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$qmdest[harv$baac.prev>=100]) ### slightly less negative
  mean(harv$qmdest)
  
  
  covsub <- cov %>%
    filter(lorey_ht.prev >= 70, nummills > 0)
  
  harvsub <- harv %>%
    filter(harvest == 1, lorey_ht.prev >= 70, nummills > 0)
  
  allremv <- ggplot(harv, aes(y=hi*100, x=lorey_ht.prev)) + 
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    ggtitle("Harvest Intensity (% BA removed)") +
    xlab("Previous Lorey's Height (ft)") + ylab("% BA removed") + coord_cartesian(ylim=c(0,100)) +
    geom_text(aes(label = paste0("mean = ",round(mean(hi*100), digits=0), "%")), y=90, x=80, col="red4")
  
  subremv <- ggplot(harvsub, aes(y=hi*100, x=lorey_ht.prev)) + 
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    xlab("Previous Lorey's Height (ft)") + ylab("% BA removed") + coord_cartesian(ylim=c(0,100))+
    geom_text(aes(label = paste0("mean = ",round(mean(hi*100), digits=0), "%")), y=90, x=90, col="red4")
  
  allharv <- ggplot(cov, aes(y=hl*100, x=lorey_ht.prev)) + 
    ggtitle("Harvest Likelihood (%)") +
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    xlab("Previous Lorey's Height (ft)") + ylab("% harvest likelihood") + coord_cartesian(ylim=c(0,100))+
    geom_text(aes(label = paste0("mean = ",round(mean(hl*100), digits=0), "%")), y=90, x=80, col="red4")
  
  subharv <- ggplot(covsub, aes(y=hl*100, x=lorey_ht.prev)) + 
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    xlab("Previous Lorey's Height (ft)") + ylab("% harvest likelihood") + coord_cartesian(ylim=c(0,100))+
    geom_text(aes(label = paste0("mean = ",round(mean(hl*100), digits=0), "%")), y=90, x=90, col="red4")

  
  ### Save for draft
  png("figures/eligibility_thresholds_oak.png", res=200, width=8, height=6, unit="in")
  allharv + subharv + allremv + subremv +
  plot_layout(ncol = 2, nrow = 2, guides = "collect")
  dev.off()
  
} else {
  
  ### Build figures for manuscript
  #### Assess HL
  cov$hl <- as.data.frame(predict(hl.mod))$Estimate
  ggplot(cov, aes(x=lorey_ht.prev, y=hl)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(cov$hl[cov$lorey_ht.prev>=70]) ### This is the key winner
  mean(cov$hl)
  
  ggplot(cov, aes(x=baac.prev, y=hl)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(cov$hl[cov$baac.prev>=100])
  mean(cov$hl)
  
  #### Assess HI
  harv$hi <- as.data.frame(predict(hi.mod))$Estimate
  ggplot(harv, aes(x=lorey_ht.prev, y=hi)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$hi[harv$lorey_ht.prev>=70]) ### This is the key winner
  mean(harv$hi)
  
  ggplot(harv, aes(x=baac.prev, y=hi)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$hi[harv$baac.prev>=100])
  mean(harv$hi)
  
  #### Assess QMD
  harv$qmdest <- as.data.frame(predict(qmd.mod))$Estimate
  ggplot(harv, aes(x=lorey_ht.prev, y=qmdest)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$qmdest[harv$lorey_ht.prev>=70])
  mean(harv$qmdest)
  
  ggplot(harv, aes(x=baac.prev, y=qmdest)) + geom_point() +
    geom_smooth(method="lm", formula=y~poly(x,2))
  mean(harv$qmdest[harv$baac.prev>=100]) ### slightly less negative
  mean(harv$qmdest)
  
  
  covsub <- cov %>%
    filter(lorey_ht.prev >= 70, nummills > 0)
  
  harvsub <- harv %>%
    filter(harvest == 1, lorey_ht.prev >= 70, nummills >0)
  
  allremv <- ggplot(harv, aes(y=hi*100, x=lorey_ht.prev)) + 
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    ggtitle("Harvest Intensity (% BA removed)") +
    xlab("Previous Lorey's Height (ft)") + ylab("% BA removed") + coord_cartesian(ylim=c(0,100)) +
    geom_text(aes(label = paste0("mean = ",round(mean(hi*100), digits=0), "%")), y=90, x=80, col="red4")
  
  subremv <- ggplot(harvsub, aes(y=hi*100, x=lorey_ht.prev)) + 
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    xlab("Previous Lorey's Height (ft)") + ylab("% BA removed") + coord_cartesian(ylim=c(0,100))+
    geom_text(aes(label = paste0("mean = ",round(mean(hi*100), digits=0), "%")), y=90, x=90, col="red4")
  
  allharv <- ggplot(cov, aes(y=hl*100, x=lorey_ht.prev)) + 
    ggtitle("Harvest Likelihood (%)") +
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    xlab("Previous Lorey's Height (ft)") + ylab("% harvest likelihood") + coord_cartesian(ylim=c(0,100))+
    geom_text(aes(label = paste0("mean = ",round(mean(hl*100), digits=0), "%")), y=90, x=80, col="red4")
  
  subharv <- ggplot(covsub, aes(y=hl*100, x=lorey_ht.prev)) + 
    geom_smooth(method="lm", col="red", linetype="dashed") + geom_point(alpha=0.3) +
    xlab("Previous Lorey's Height (ft)") + ylab("% harvest likelihood") + coord_cartesian(ylim=c(0,100))+
    geom_text(aes(label = paste0("mean = ",round(mean(hl*100), digits=0), "%")), y=90, x=90, col="red4")
  
  
  ### Save for draft
  png("figures/eligibility_thresholds_mbb.png", res=200, width=8, height=6, unit="in")
  allharv + subharv + allremv + subremv +
    plot_layout(ncol = 2, nrow = 2, guides = "collect")
  dev.off()
  
}




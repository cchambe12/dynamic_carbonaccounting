##### Very quick script to plot all additionality figures in one place
## Started 12 November 2024 by Cat

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

## Set working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/")

#################################################################################
################ Load in all data and prepare plots ############################
#################################################################################
cappsoak <- read.csv("centralapps/output/centralapps_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Central Apps")
cappsmbb <- read.csv("centralapps/output/centralapps_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Central Apps")

neoak <- read.csv("northeast/output/northeast_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Northeast")
nembb <- read.csv("northeast/output/northeast_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Northeast")

nwoak <- read.csv("northwoods/output/northwoods_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Northwoods")
nwmbb <- read.csv("northwoods/output/northwoods_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Northwoods")

sappsoak <- read.csv("southernapps/output/southernapps_oaks_projectvbau_fia.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Southern Apps")
sappsmbb <- read.csv("southernapps/output/southernapps_mbbs_projectvbau_fia.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Southern Apps")

allplots <- full_join(cappsoak, cappsmbb) %>%
  full_join(neoak) %>%
  full_join(nembb) %>%
  full_join(nwoak) %>%
  full_join(nwmbb) %>%
  full_join(sappsoak) %>%
  full_join(sappsmbb) %>%
  ungroup() %>%
  group_by(project, time) %>%
  mutate(dynamic = deltac - mean(deltac.bau)) %>%
  distinct() %>%
  select(project, time, forestname, region, baac.remv.bau, dynamic) %>%
  rename(fiabau = baac.remv.bau,
         plt_cn = project)

################################################################################
#### Get FVS outputs
cappsoakfvs <- read.csv("centralapps/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Central Apps", baac.remv = 0.34)
cappsmbbfvs <- read.csv("centralapps/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Central Apps", baac.remv = 0.34)

neoakfvs <- read.csv("northeast/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Northeast", baac.remv = 0.37)
nembbfvs <- read.csv("northeast/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Northeast", baac.remv = 0.52)

nwoakfvs <- read.csv("northwoods/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Northwoods", ecosub = as.character(ecosub), baac.remv = 0.36)
nwmbbfvs <- read.csv("northwoods/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Northwoods", ecosub = as.character(ecosub), baac.remv = 0.39)

sappsoakfvs <- read.csv("southernapps/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  mutate(forestname = "Oak / hickory group", region = "Southern Apps", baac.remv = 0.53)
sappsmbbfvs <- read.csv("southernapps/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  mutate(forestname = "Maple / beech / birch group", region = "Southern Apps", baac.remv = 0.40)

allfvs <- full_join(cappsoakfvs, cappsmbbfvs) %>%
  full_join(neoakfvs) %>%
  full_join(nembbfvs) %>%
  full_join(nwoakfvs) %>%
  full_join(nwmbbfvs) %>%
  full_join(sappsoakfvs) %>%
  full_join(sappsmbbfvs) 

fvs <- allfvs %>%
  ungroup() %>%
  select(plt_cn, time, delta.oak.grow, delta.mbb.grow, delta.oak.grow.comp, delta.mbb.grow.comp, delta.oak.grow.comp.blend, delta.mbb.grow.comp.blend,
         region, forestname, baac.remv) %>% 
  filter(time < 20) %>%
  pivot_longer(cols = c(delta.oak.grow:delta.mbb.grow.comp.blend), names_to = "method", values_to = "deltac") %>%
  na.omit() %>%
  mutate(method = ifelse(substr(method, nchar(method)-4, nchar(method))==".comp", "static.comp", 
                         ifelse(substr(method, nchar(method)-5, nchar(method))==".blend", "static.blend", "static"))) %>%
  pivot_wider(names_from=method, values_from=deltac) %>%
  select(plt_cn, time, forestname, region, baac.remv, static, static.comp, static.blend) %>%
  rename(fvsbau = baac.remv)


#### Bring static and dynamic together
## First try merging by plt_cn if possible
dynamstat <- left_join(allplots, fvs) %>%
  ungroup() %>%
  group_by(plt_cn, forestname, region) %>%
  mutate(staticmean = ifelse(time > 0, mean(static, na.rm=TRUE), static),
         staticmean.comp = ifelse(time > 0, mean(static.comp, na.rm=TRUE), static.comp),
         diff = staticmean - dynamic,
         diff.comp = staticmean.comp - dynamic) %>%
  ungroup() %>%
  group_by(forestname, region) %>%
  mutate(staticmean.grouped = mean(staticmean, na.rm=TRUE),
         staticmean.grouped.comp = mean(staticmean.comp, na.rm=TRUE),
         dynamic.grouped = mean(dynamic, na.rm=TRUE),
         diff.grouped = staticmean.grouped - dynamic.grouped,
         diff.grouped.comp = staticmean.grouped.comp - dynamic.grouped)


#################################################################################
################### Plotting differences in averages ############################
#################################################################################
mbb.p <- ggplot(dynamstat %>% filter(forestname == "Maple / beech / birch group") %>%
                  select(forestname, region, dynamic, staticmean, staticmean.comp, static.blend) %>%
                  pivot_longer(cols=c(dynamic:static.blend), names_to = "method", values_to = "meangrow") %>%
                  group_by(forestname, region, method) %>%
                  summarize(meangrow = mean(meangrow, na.rm=TRUE)) %>%
                  mutate(method = ifelse(method=="staticmean", "single model - static", 
                                         ifelse(method=="staticmean.comp", "blended model - static",
                                                ifelse(method=="static.blend", "blended model - dynamic", "measured composite - dynamic"))),
                         order = factor(method, levels=c('single model - static', 'blended model - static', 'blended model - dynamic', 'measured composite - dynamic'))), 
                aes(x=order, y=meangrow, group=order, fill=order)) +
  ggtitle("a) Maple / beech / birch group") +
  geom_col(position = position_dodge()) + theme_bw() + theme(legend.position="none") +
  scale_fill_colorblind() + facet_wrap(~region) + xlab("") +
  ylab("Avg additional Mt CO2e/ac/yr") + coord_cartesian(ylim=c(0,5)) +
  geom_text(aes(label = round(meangrow, digits=2)), vjust=-0.25) + scale_x_discrete(guide = guide_axis(angle=45))

oak.p <- ggplot(dynamstat %>% filter(forestname == "Oak / hickory group") %>%
                  select(forestname, region, dynamic, staticmean, staticmean.comp, static.blend) %>%
                  pivot_longer(cols=c(dynamic:static.blend), names_to = "method", values_to = "meangrow") %>%
                  group_by(forestname, region, method) %>%
                  summarize(meangrow = mean(meangrow, na.rm=TRUE)) %>%
                  mutate(method = ifelse(method=="staticmean", "single model - static", 
                                         ifelse(method=="staticmean.comp", "blended model - static",
                                                ifelse(method=="static.blend", "blended model - dynamic", "measured composite - dynamic"))),
                         order = factor(method, levels=c('single model - static', 'blended model - static', 'blended model - dynamic', 'measured composite - dynamic'))), 
                aes(x=order, y=meangrow, group=order, fill=order)) +
  ggtitle("b) Oak / hickory group") +
  geom_col(position = position_dodge()) + theme_bw() + theme(legend.position="none") +
  scale_fill_colorblind() + facet_wrap(~region) + xlab("") +
  ylab("Avg additional Mt CO2e/ac/yr") + coord_cartesian(ylim=c(0,5)) +
  geom_text(aes(label = round(meangrow, digits=2)), vjust=-0.25) + scale_x_discrete(guide = guide_axis(angle=45))

png("figures/method_compare.png", 
    width=10,
    height=8, units="in", res = 350 )
mbb.p + oak.p +
  plot_layout(ncol = 2, nrow = 1, guides = "collect")
dev.off()

################################################################################
################### Look at differences over time ##############################
mbbtime.p <- ggplot(dynamstat %>% filter(forestname == "Maple / beech / birch group") %>%
                      select(forestname, time, region, dynamic, staticmean, staticmean.comp, static.blend) %>%
                      pivot_longer(cols=c(dynamic:static.blend), names_to = "method", values_to = "meangrow") %>%
                      ungroup() %>%
                      group_by(time, method, region, forestname) %>%
                      summarize(meangrow = mean(meangrow, na.rm=TRUE)) %>%
                      ungroup() %>%
                      group_by(method, region, forestname) %>%
                      mutate(method = ifelse(method=="staticmean", "single model - static", 
                                             ifelse(method=="staticmean.comp", "blended model - static",
                                                    ifelse(method=="static.blend", "blended model - dynamic", "measured composite - dynamic"))),
                             order = factor(method, levels=c('single model - static', 'blended model - static', 'blended model - dynamic', 'measured composite - dynamic'))), 
                    aes(x=time, y=meangrow, col=order, fill=order)) +
  geom_line(size=1.2) +
  ggtitle("a) Maple / beech / birch group") +
  theme_bw() + scale_color_colorblind(name="Method") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr") + facet_wrap(~region)

oaktime.p <- ggplot(dynamstat %>% filter(forestname == "Oak / hickory group") %>%
                      select(forestname, time, region, dynamic, staticmean, staticmean.comp, static.blend) %>%
                      pivot_longer(cols=c(dynamic:static.blend), names_to = "method", values_to = "meangrow") %>%
                      ungroup() %>%
                      group_by(time, method, region, forestname) %>%
                      summarize(meangrow = mean(meangrow, na.rm=TRUE)) %>%
                      ungroup() %>%
                      group_by(method, region, forestname) %>%
                      mutate(method = ifelse(method=="staticmean", "single model - static", 
                                             ifelse(method=="staticmean.comp", "blended model - static",
                                                    ifelse(method=="static.blend", "blended model - dynamic", "measured composite - dynamic"))),
                             order = factor(method, levels=c('single model - static', 'blended model - static', 'blended model - dynamic', 'measured composite - dynamic'))), 
                    aes(x=time, y=meangrow, col=order, fill=order)) +
  geom_line(size=1.2) +
  ggtitle("b) Oak / hickory group") +
  theme_bw() + scale_color_colorblind(name="Method") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr") + facet_wrap(~region)


png("figures/dynamicvsstatic.png", 
    width=10,
    height=8, units="in", res = 350 )
mbbtime.p + oaktime.p +
  plot_layout(ncol = 2, nrow = 1, guides = "collect")
dev.off()

################################################################################
############ Prepare covariates for Bayesian Hierarchical Model ################

caqmd <- read.csv("centralapps/output/clean_centralapps_fiadata.csv") %>%
  mutate(region = "Central Apps", variant = "southern") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant,
         siteclcd, elev, slope, rd.ac.over, rd.ac.regen, rddistcd) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()
neqmd <- read.csv("northeast/output/clean_northeast_fiadata.csv") %>%
  mutate(region = "Northeast", variant = "northeast") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant,
         siteclcd, elev, slope, rd.ac.over, rd.ac.regen, rddistcd) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()
nwqmd <- read.csv("northwoods/output/clean_northwoods_fiadata.csv") %>%
  mutate(region = "Northwoods", variant = "lakestates") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant,
         siteclcd, elev, slope, rd.ac.over, rd.ac.regen, rddistcd) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()
saqmd <- read.csv("southernapps/output/clean_southernapps_fiadata.csv") %>%
  mutate(region = "Southern Apps", variant = "southern") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant,
         siteclcd, elev, slope, rd.ac.over, rd.ac.regen, rddistcd) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()


qmd <- full_join(caqmd, neqmd) %>%
  full_join(nwqmd) %>%
  full_join(saqmd) %>%
  right_join(dynamstat %>% select(plt_cn, forestname, region, dynamic, staticmean, staticmean.comp, 
                                  static.blend, fiabau) %>%
               pivot_longer(cols=c(dynamic:static.blend), names_to = "method", values_to = "meangrow") %>%
               ungroup() %>%
               group_by(plt_cn, method, region, forestname, fiabau) %>%
               summarize(meangrow = mean(meangrow))) %>%
  ungroup() %>%
  mutate(BAAC = (baac - mean(baac, na.rm=TRUE)) / (sd(baac, na.rm=TRUE)),
         BAremv = (fiabau - mean(fiabau, na.rm=TRUE)) / (sd(fiabau, na.rm=TRUE)),
         ChangeQMD = (qmdchange - mean(qmdchange, na.rm=TRUE)) / (sd(qmdchange, na.rm=TRUE)),
         SpDiversity = (spdiv - mean(spdiv, na.rm=TRUE)) / (sd(spdiv, na.rm=TRUE)),
         RdDist = (rddistcd - mean(rddistcd, na.rm=TRUE)) / (sd(rddistcd, na.rm=TRUE)),
         Elev = (elev - mean(elev, na.rm=TRUE)) / (sd(elev, na.rm=TRUE)),
         Slope = (slope - mean(slope, na.rm=TRUE)) / (sd(slope, na.rm=TRUE)),
         SiteClass = (siteclcd - mean(siteclcd, na.rm=TRUE)) / (sd(siteclcd, na.rm=TRUE)),
         OverstoryRD = (rd.ac.over - mean(rd.ac.over, na.rm=TRUE)) / (sd(rd.ac.over, na.rm=TRUE)),
         UnderstoryRD = (rd.ac.regen - mean(rd.ac.regen, na.rm=TRUE)) / (sd(rd.ac.regen, na.rm=TRUE))) %>%
  distinct()

################################################################################
######################### Run the Bayesian Model ###############################
diffmod <- brm(meangrow ~ BAremv + BAAC + ChangeQMD + SpDiversity + SiteClass + RdDist + SiteClass + 
                 OverstoryRD + UnderstoryRD + 
                 (1 | method), data=qmd,
               chains = 2, control = list(adapt_delta = 0.99))

### Strongest drivers are thinning strategy and overstory relative density 
sjPlot::plot_model(diffmod, type = "est", ci.lvl = 0.89, colors=c("firebrick4", "blue4"),
                   title="Model estimate of additional MtCO2e/ac/yr", ci.style="bar",
                   sort.est = TRUE) + theme_classic() +
  geom_hline(yintercept=0, linetype="dotted", col="grey60")

sjPlot::plot_model(diffmod, type = "re", ci.lvl = 0.89, colors=c("firebrick4", "blue4"),
                   title="Model estimate of additional MtCO2e/ac/yr", ci.style="bar",
                   sort.est = TRUE, bpe="median") + theme_classic() +
  geom_hline(yintercept=0, linetype="dotted", col="grey60")

png("figures/modeloutput_covariates.png", 
    width=6,
    height=7, units="in", res = 350 )
sjPlot::plot_model(diffmod, type = "est", ci.lvl = 0.89, colors=c("firebrick4", "blue4"),
                   title="Model estimate of additional MtCO2e/ac/yr", ci.style="bar",
                   sort.est = TRUE) + theme_classic() +
  geom_hline(yintercept=0, linetype="dotted", col="grey60")
dev.off()


qmd$modgrow <- as.data.frame(predict(diffmod))$Estimate


quantiles_89 <- function(x) {
  r <- quantile(x, probs=c(0.11, 0.25, 0.5, 0.75, 0.89))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

diffp <- ggplot(qmd %>% mutate(method = ifelse(method=="staticmean", "single model - static", 
                                  ifelse(method=="staticmean.comp", "blended model - static",
                                         ifelse(method=="static.blend", "blended model - dynamic", 
                                                "measured composite - dynamic"))),
                      order = factor(method, levels=c('single model - static', 'blended model - static', 'blended model - dynamic', 'measured composite - dynamic'))), 
       aes(x=order, col=order, y=modgrow, group=order, fill=order)) + 
  stat_summary(fun.data = quantiles_89, geom="boxplot",
               width=0.1) + theme_bw() + scale_color_colorblind(name="Method") + 
  scale_fill_colorblind(name="Method") + theme(legend.position = "none") + facet_wrap(~forestname) +
  scale_x_discrete(guide = guide_axis(angle=45)) + xlab("") + ylab("Model estimate of\nadditional MtCO2e/ac/yr")

png("figures/modeloutput_method.png", 
    width=5,
    height=4, units="in", res = 350 )
diffp
dev.off()

### Some relationship between removal rates and difference
over.p <- ggplot(qmd, aes(x = rd.ac.over, y = modgrow)) +
  geom_smooth(method="lm", formula=y~poly(x,2), col="red4", fill="red4") + theme_bw() +
  ylab("Model estimate of\nadditional MtCO2e/ac/yr") + xlab("Overstory Relative Density") 

siteclass.p <- ggplot(qmd, aes(x = siteclcd, y = modgrow)) +
  geom_smooth(method="lm", formula=y~poly(x,2), col="red4", fill="red4") + theme_bw() +
  ylab("Model estimate of\nadditional MtCO2e/ac/yr") + xlab("Site Productivity Code") 

remv.p <- ggplot(qmd, aes(x = fiabau, y = modgrow)) +
  geom_smooth(method="lm", formula=y~poly(x,2), col="red4", fill="red4") + theme_bw() +
  ylab("Model estimate of\nadditional MtCO2e/ac/yr") + xlab("% BA removed in common practice") 



png("figures/modeloutput.png", 
    width=12,
    height=6, units="in", res = 350 )
over.p + remv.p + siteclass.p + 
  plot_layout(ncol = 3, guides = "collect")
dev.off()

################################################################################
################ Run the Bayesian Model for approach characteristics ###########
qmd <- qmd %>%
  mutate(modeled = ifelse(method=="dynamic", 0, 1),
         static = ifelse(method %in% c("staticmean", "staticmean.comp"), 1, 0),
         composite = ifelse(method == "staticmean", 0 , 1),
         group = paste(forestname, region))


methodtypemod <- brm(meangrow ~ modeled + static + composite +
                       (modeled + static + composite | group), data=qmd,
               chains = 2, control = list(adapt_delta = 0.99))


### Strongest drivers are thinning strategy and overstory relative density 
sjPlot::plot_model(methodtypemod, type = "est", ci.lvl = 0.89, colors=c("firebrick4", "blue4"),
                   title="Model estimate of additional MtCO2e/ac/yr", ci.style="bar",
                   sort.est = TRUE) + theme_classic() +
  geom_hline(yintercept=0, linetype="dotted", col="grey60")

png("figures/typemodeloutput_covariates.png", 
    width=6,
    height=7, units="in", res = 350 )
sjPlot::plot_model(methodtypemod, type = "est", ci.lvl = 0.89, colors=c("firebrick4", "blue4"),
                   title="Model estimate of additional MtCO2e/ac/yr", ci.style="bar",
                   sort.est = TRUE) + theme_classic() +
  geom_hline(yintercept=0, linetype="dotted", col="grey60")
dev.off()

qmd$methodtype <- as.data.frame(predict(methodtypemod))$Estimate


quantiles_89 <- function(x) {
  r <- quantile(x, probs=c(0.11, 0.25, 0.5, 0.75, 0.89))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

typep <- ggplot(qmd %>% mutate(method = ifelse(method=="staticmean", "single model - static", 
                                               ifelse(method=="staticmean.comp", "blended model - static",
                                                      ifelse(method=="static.blend", "blended model - dynamic", 
                                                             "measured composite - dynamic"))),
                               order = factor(method, levels=c('single model - static', 'blended model - static', 'blended model - dynamic', 'measured composite - dynamic'))), 
                aes(x=order, col=order, y=methodtype, group=order, fill=order)) + 
  stat_summary(fun.data = quantiles_89, geom="boxplot",
               width=0.1) + theme_bw() + scale_color_colorblind(name="Method") + 
  scale_fill_colorblind(name="Method") + theme(legend.position = "none") + 
  scale_x_discrete(guide = guide_axis(angle=45)) + xlab("") + ylab("Model estimate of\nadditional MtCO2e/ac/yr")

png("figures/modeloutput_type.png", 
    width=5,
    height=4, units="in", res = 350 )
typep
dev.off()

### Some relationship between removal rates and difference
modp <- ggplot(qmd, aes(x = modeled, y = methodtype)) +
  geom_smooth(method="lm", col="red4", fill="red4") + theme_bw() +
  ylab("Model estimate of\nadditional MtCO2e/ac/yr") + xlab("") + coord_cartesian(ylim=c(0.5, 2.25)) +
  scale_x_continuous(breaks=c(0, 1), labels=c("Measured", "Modeled"),
                     guide=guide_axis(angle=45)) +
  ggtitle("a) Measured vs Modeled")

staticp <- ggplot(qmd, aes(x = static, y = methodtype)) +
  geom_smooth(method="lm", col="red4", fill="red4") + theme_bw() +
  ylab("Model estimate of\nadditional MtCO2e/ac/yr") + xlab("") + coord_cartesian(ylim=c(0.5, 2.25)) +
  scale_x_continuous(breaks=c(0, 1), labels=c("Dynamic", "Static"),
                     guide=guide_axis(angle=45)) +
  ggtitle("b) Dynamic vs Static")

compositep <- ggplot(qmd, aes(x = composite*1, y = methodtype)) +
  geom_smooth(method="lm", col="red4", fill="red4") + theme_bw() +
  ylab("Model estimate of\nadditional MtCO2e/ac/yr") + xlab("") + coord_cartesian(ylim=c(0.5, 2.25)) +
  scale_x_continuous(breaks=c(0, 1), labels=c("Composite BAU", "Single BAU"),
                     guide=guide_axis(angle=45)) +
  ggtitle("c) Composite/Blended vs 100% harvest likelihood")



png("figures/modeloutput_type.png", 
    width=12,
    height=6, units="in", res = 350 )
modp + staticp + compositep + 
  plot_layout(ncol = 3, guides = "collect")
dev.off()


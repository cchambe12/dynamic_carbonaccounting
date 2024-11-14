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
library(brms)
library(bayesplot)

## Set working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/")

### Load in the data frames
capps_oaks <- read.csv("centralapps/output/centralapps_oaks_scenarios.csv")
capps_mbbs <- read.csv("centralapps/output/centralapps_mbbs_scenarios.csv")

ne_oaks <- read.csv("northeast/output/northeast_oaks_scenarios.csv")
ne_mbbs <- read.csv("northeast/output/northeast_mbbs_scenarios.csv")

nw_oaks <- read.csv("northwoods/output/northwoods_oaks_scenarios.csv")
nw_mbbs <- read.csv("northwoods/output/northwoods_mbbs_scenarios.csv")

sapps_oaks <- read.csv("southernapps/output/southernapps_oaks_scenarios.csv")
sapps_mbbs <- read.csv("southernapps/output/southernapps_mbbs_scenarios.csv")

### Build plots
################## Central Apps
cappoak.p <- ggplot(capps_oaks %>%
         mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
       aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("b) Central Apps: Oak / hickory group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")

cappmbb.p <- ggplot(capps_mbbs %>%
                      mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
                    aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("a) Central Apps: Maple / beech / birch group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")

################ Northeast
neoak.p <- ggplot(ne_oaks %>%
                      mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
                    aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("d) Northeast: Oak / hickory group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")

nembb.p <- ggplot(ne_mbbs %>%
                      mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
                    aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("c) Northeast: Maple / beech / birch group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")

################## Northwoods
nwoak.p <- ggplot(nw_oaks %>%
                    mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
                  aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("f) Northwoods: Oak / hickory group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")

nwmbb.p <- ggplot(nw_mbbs %>%
                    mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
                  aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("e) Northwoods: Maple / beech / birch group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")

################## Southern Apps
sappsoak.p <- ggplot(sapps_oaks %>%
                    mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
                  aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("h) Southern Apps: Oak / hickory group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")

sappsmbb.p <- ggplot(sapps_mbbs %>%
                    mutate(meangrow = ifelse(method=="static" & time >0, mean(meangrow, na.rm=TRUE), meangrow)),
                  aes(x=time, y=meangrow, col=method, fill=method)) +
  geom_line(size=1.2) +
  ggtitle("g) Southern Apps: Maple / beech / birch group") +
  theme_bw() + scale_color_viridis(name="Method", discrete=TRUE, begin=0.1, end=0.75, option="F") + 
  xlab("Years since project start") + ylab("Additional MtCO2e/ac/yr")


png("figures/dynamicvsstatic.png", 
    width=10,
    height=12, units="in", res = 350 )
cappmbb.p + cappoak.p +
  nembb.p + neoak.p +
  nwmbb.p + nwoak.p +
  sappsmbb.p + sappsoak.p +
  plot_layout(ncol = 2, nrow = 4, guides = "collect")
dev.off()


#################################################################################
############ Build average Mt CO2e/ac/yr figure combined ########################
#################################################################################
all <- full_join(capps_mbbs, capps_oaks) %>%
  full_join(ne_mbbs) %>%
  full_join(ne_oaks) %>%
  full_join(nw_mbbs) %>%
  full_join(nw_oaks) %>%
  full_join(sapps_mbbs) %>%
  full_join(sapps_oaks)


mbbsub <- all %>%
  filter(forestname == "Maple / beech / birch group") %>%
  group_by(region, method) %>%
  summarize(meangrow = mean(meangrow, na.rm=TRUE),
            segrow = mean(segrow, na.rm=TRUE))

oaksub <- all %>%
  filter(forestname == "Oak / hickory group") %>%
  group_by(region, method) %>%
  summarize(meangrow = mean(meangrow, na.rm=TRUE),
            segrow = mean(segrow, na.rm=TRUE))




mbb.p <- ggplot(mbbsub, aes(x=method, y=meangrow, group=method, fill=method)) +
  ggtitle("a) Maple / beech / birch group") +
  geom_col(position = position_dodge()) + theme_bw() + theme(legend.position="none") +
  scale_fill_d3(palette="category20c") + facet_wrap(~region) + xlab("") +
  ylab("Avg additional Mt CO2e/ac/yr") + coord_cartesian(ylim=c(0,5)) +
  geom_text(aes(label = round(meangrow, digits=2)), vjust=-0.25)

oak.p <- ggplot(oaksub, aes(x=method, y=meangrow, group=method, fill=method)) +
  ggtitle("b) Oak / hickory group") +
  geom_col(position = position_dodge()) + theme_bw() + theme(legend.position="none") +
  scale_fill_d3(palette="category20c") + facet_wrap(~region) + xlab("") +
  ylab("Avg additional Mt CO2e/ac/yr") + coord_cartesian(ylim=c(0,5)) +
  geom_text(aes(label = round(meangrow, digits=2)), vjust=-0.25)


png("figures/method_compare.png", 
    width=6,
    height=4, units="in", res = 350 )
mbb.p + oak.p +
  plot_layout(ncol = 2, nrow = 1, guides = "collect")
dev.off()


#################################################################################
############ Determine predictors of largest differences ########################
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
  full_join(sappsmbbfvs) %>%
  mutate(method = "static")

fvs <- allfvs %>%
  ungroup() %>%
  select(plt_cn, time, delta.oak.grow, delta.mbb.grow, method, region, forestname, baac.remv) %>% 
  filter(time < 20) %>%
  group_by(plt_cn, time, forestname, region, method, baac.remv) %>%
  summarize(static = ifelse(forestname == "Oak / hickory group", mean(delta.oak.grow, na.rm=TRUE),
                              mean(delta.mbb.grow, na.rm=TRUE))) %>%
  select(plt_cn, time, forestname, region, baac.remv, static) %>%
  rename(fvsbau = baac.remv)
  


#### Bring static and dynamic together
## First try merging by plt_cn if possible
dynamstat <- left_join(projectvbau, fvs) %>%
  ungroup() %>%
  group_by(plt_cn, forestname, region) %>%
  mutate(staticmean = ifelse(time > 0, mean(static, na.rm=TRUE), static),
         diff = staticmean - dynamic) %>%
  ungroup() %>%
  group_by(forestname, region) %>%
  mutate(staticmean.grouped = mean(staticmean, na.rm=TRUE),
         dynamic.grouped = mean(dynamic, na.rm=TRUE),
         diff.grouped = staticmean.grouped - dynamic.grouped)


################################################################################
############ Prepare covariates for Bayesian Hierarchical Model ################

caqmd <- read.csv("centralapps/output/clean_centralapps_fiadata.csv") %>%
  mutate(region = "Central Apps", variant = "southern") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()
neqmd <- read.csv("northeast/output/clean_northeast_fiadata.csv") %>%
  mutate(region = "Northeast", variant = "northeast") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()
nwqmd <- read.csv("northwoods/output/clean_northwoods_fiadata.csv") %>%
  mutate(region = "Northwoods", variant = "lakestates") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()
saqmd <- read.csv("southernapps/output/clean_southernapps_fiadata.csv") %>%
  mutate(region = "Southern Apps", variant = "southern") %>% 
  select(plt_cn, harvest, qmdchange, baac.remv, forestname, region, baac.prev, shannon_index, variant) %>%
  rename(baac = baac.prev, spdiv = shannon_index) %>%
  ungroup() %>% distinct()


qmd <- full_join(caqmd, neqmd) %>%
  full_join(nwqmd) %>%
  full_join(saqmd) %>%
  right_join(dynamstat %>% select(plt_cn, forestname, region, diff, fiabau)) %>%
  ungroup() %>%
  mutate(baacz = (baac - mean(baac, na.rm=TRUE)) / (sd(baac, na.rm=TRUE)),
         remvz = (fiabau - mean(fiabau, na.rm=TRUE)) / (sd(fiabau, na.rm=TRUE)),
         qmdz = (qmdchange - mean(qmdchange, na.rm=TRUE)) / (sd(qmdchange, na.rm=TRUE)),
         divz = (spdiv - mean(spdiv, na.rm=TRUE)) / (sd(spdiv, na.rm=TRUE))) %>%
  distinct()

################################################################################
######################### Run the Bayesian Model ###############################
diffmod <- brm(diff ~ remvz + baacz + qmdz + divz, data=qmd,
                       control = list(adapt_delta=0.99))
  

color_scheme_set("purple")
diffp <- mcmc_intervals(diffmod, pars=c("b_remvz", 
                                       "b_baacz",
                                       "b_qmdz",
                                       "b_divz"), 
                        prob=0.5, prob_outer = 0.89) +
  coord_cartesian(ylim=c(1, 4.15), clip = 'off') +
  geom_vline(linetype = "dotted", xintercept = 0) +
  theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  annotate("segment", x = 0.01, xend = 1.8, y = 4.9, yend = 4.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = -.01, xend = -0.9, y = 4.9, yend = 4.9, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = 0.9, y = 5.05, colour = "black", size=3.5, label=bquote("Lower dynamic estimate")) + 
  annotate("text", x = -0.45, y = 5.05, colour = "black", size=3.5, label=bquote("Higher dynamic estimate")) +
  scale_y_discrete(limits=rev(c("b_remvz", 
                                  "b_baacz",
                                  "b_qmdz",
                                  "b_divz")), 
                   labels=rev(c("Avg % BA removed in harvest", "Initial BAAC", "Change in QMD from harvest",
                                "Species diversity"))) +
  xlab("Model estimate of difference in additional Mt CO2e/ac/yr")  #+
  #ggtitle("a) Model output: estimating difference in additional Mt CO2e/ac/yr")

diffp


### Some relationship between removal rates and difference
baac.p <- ggplot(qmd, aes(x = baac, y = diff)) +
  geom_smooth(method="lm", formula=y~poly(x,2), col="red4", fill="red4") + theme_bw() +
  ylab("Static - Dynamic (Mt CO2e/ac/yr)") + xlab("Initial BAAC (sqft/ac)")  #+
  #ggtitle("b) Effect of initial BAAC on difference")

### Interesting relationship but not linear... is it due to thinning strategy?
qmd.p <- ggplot(qmd, aes(x = qmdchange*-1, y = diff)) +
  geom_smooth(method="lm", formula=y~poly(x,2), col="red4", fill="red4") + theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  ylab("Static - Dynamic (Mt CO2e/ac/yr)") + xlab("QMD change (in)") +
  coord_cartesian(xlim=c(-1.15, 0.37), ylim=c(-4.75, 6.7), clip = 'off') +
  annotate("segment", x = -0.01, xend = -1, y = 7.5, yend = 7.5, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = .01, xend = 0.35, y = 7.5, yend = 7.5, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = -0.6, y = 7.9, colour = "black", size=3.5, label=bquote("Thin from below")) + 
  annotate("text", x = 0.2, y = 7.9, colour = "black", size=3.5, label=bquote("Thin from above")) +
  geom_vline(xintercept = 0, linetype = "dotted") #+
  #ggtitle("c) Effect of thinning strategy on difference")

covs <- baac.p + qmd.p +
  plot_layout(ncol = 1,guides = "collect")



png("figures/modeloutput.png", 
    width=10,
    height=6, units="in", res = 350 )
diffp + covs + plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 2, guides = "collect")
dev.off()


### Interesting relationship but not linear... is it due to thinning strategy?
remv.p <- ggplot(dynamstat, aes(x = baac.remv*100, y = diff.grouped)) +
  geom_smooth(method="lm", formula=y~poly(x,2), col="red4", fill="red4") + theme_bw() +
  ylab("Static - Dynamic (Mt CO2e/ac/yr)") + xlab("% BA removed during harvest")

### Interesting relationship but not linear... is it due to thinning strategy?
qmd.p <- ggplot(dynamstat, aes(x = meanqmd, y = diff.grouped)) +
  geom_smooth(method="lm", formula=y~poly(x,2), col="red4", fill="red4") + theme_bw() +
  theme(panel.border = element_rect(colour = "black", fill=NA),
        plot.margin = unit(c(3,3,1,1), "lines")) +
  ylab("Static - Dynamic (Mt CO2e/ac/yr)") + xlab("QMD change (in)") +
  coord_cartesian(ylim=c(1, 2.15),  clip = 'off') +
  annotate("segment", x = -0.01, xend = -1, y = 2.24, yend = 2.24, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("segment", x = .01, xend = 0.35, y = 2.24, yend = 2.24, colour = "black", linewidth=0.2, arrow=arrow(length=unit(0.20,"cm"))) + 
  annotate("text", x = -0.6, y = 2.28, colour = "black", size=3.5, label=bquote("Thin from below")) + 
  annotate("text", x = 0.2, y = 2.28, colour = "black", size=3.5, label=bquote("Thin from above"))+
  geom_vline(xintercept = 0, linetype = "dotted") #+

png("figures/delta_staticvdynamic.png", 
    width=7,
    height=5, units="in", res = 350 )
remv.p + qmd.p + plot_annotation(tag_levels = 'A') +
  plot_layout(ncol = 2, guides = "collect")
dev.off()

### Very clear difference across regions
ggplot(dynamstat, aes(x = region, y = diff.grouped)) +
  geom_boxplot() + theme_bw() 

### A lot more variation across forest type 
ggplot(dynamstat, aes(x = forestname, y = diff.grouped)) +
  geom_boxplot() + theme_bw() 



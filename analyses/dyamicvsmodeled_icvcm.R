#### Build figures for Verra to share with ICVCM
### Demonstrating extreme scenarios - modeled baseline vs dynamic
## Started 24 March 2025 by Cat

### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)
options(timeout = 5000)

# Load Libraries
library(ggplot2)
library(ggsci)
library(dplyr)
library(tidyr)

### Set the working directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/")

### Take outputs from FVS runs for the Modeled Baseline and Project scenarios
### Then take matched FIA plots using same plots run in FVS


################################################################################
################################################################################
## 1) Central Apps - Oak / Hickory Group

## Clean up output from "centralapps/dynamicbaseline.R"
## Get selected Project plots from FIA and changes in Mt CO2e over last three measurements
projectplots <- read.csv("centralapps/output/oak_gof_plots.csv") %>%
  mutate(method = "Dynamic",
         type = "Project",
         time = year - 2000) %>%
  select(plt_cn, time, method, type, deltac)

## Get 10 Matched plots per Project plot from FIA and changes in Mt CO2e over last three measurements
matchedplots <- read.csv("centralapps/output/oak_donorpool_plots.csv")%>%
  mutate(method = "Dynamic",
         type = "Baseline",
         time = year - 2000) %>%
  select(plt_cn, time, method, type,  deltac) 

### Join Project and Baseline under VM0045 type scenario
dynamic = full_join(projectplots, matchedplots)

### Clean up FVS runs for both the Project and Baseline scenarios - from "centralapps/fvs/02_carbonaccounting/02_carboncalcs.R"
modeled <- read.csv("centralapps/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  select(plt_cn, time, calc.oak.bau, calc.oak.grow) %>%
  pivot_longer(cols = c(calc.oak.bau, calc.oak.grow), names_to = "method", values_to = "deltac") %>%
  mutate(type = ifelse(method == "calc.oak.bau", "Baseline", "Project"),
         method = "Modeled")

### Join all runs together but subset to only select FIA plots in all scenarios
allplots = dynamic %>%
  ## Subsets FVS runs to selected FIA Project Plots and Matched Plots
  full_join(modeled %>% filter(plt_cn %in% unique(dynamic$plt_cn)))


### Estimate Average Mt CO2e/ac/yr plus SD for entire contract
# Dynamic
addit.dynam <- read.csv("centralapps/output/centralapps_oaks_projectvbau_fia.csv") %>%
  summarize(additional = round(mean(deltac - deltac.bau, na.rm = TRUE), digits=2),
            additional.se = round(sd(deltac - deltac.bau, na.rm = TRUE)/sqrt(length(deltac)), digits=2)) %>%
  mutate(method = "Dynamic", 
         type = "Project",
         y = -3, x = 12)

# Modeled
addit.mod <- read.csv("centralapps/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  summarize(additional = round(mean(delta.oak.grow, na.rm = TRUE), digits=2),
            additional.se = round(sd(delta.oak.grow, na.rm = TRUE)/sqrt(length(delta.oak.grow)), digits=2)) %>%
  mutate(method = "Modeled",
         type = "Project",
         y = -3, x = 12)

## Group together to plot outputs
addit = full_join(addit.dynam, addit.mod)

### Plot the output
png("figures/dynamicvsmodeled_capps_oak_icvcm.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(allplots, aes(x=time, y=deltac, col=type, fill=type)) +
  geom_smooth(method="lm", formula = y~poly(x,3)) +
  ggtitle("Central Apps: Oak / Hickory group") + facet_wrap(~method) +
  theme_bw() + 
  scale_color_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  xlab("Years since project start") + ylab("Mt CO2e per acre per year") +
  geom_text(data = addit, aes(x, y, label = paste0("Avg is ", additional, " +/- ", additional.se)), col = "red4")
dev.off()




################################################################################
################################################################################
## 2) Central Apps - Maple / Beech / Birch Group

## Clean up output from "centralapps/dynamicbaseline.R"
## Get selected Project plots from FIA and changes in Mt CO2e over last three measurements
projectplots <- read.csv("centralapps/output/mbb_gof_plots.csv") %>%
  mutate(method = "Dynamic",
         type = "Project",
         time = year - 2000) %>%
  select(plt_cn, time, method, type, deltac)

## Get 10 Matched plots per Project plot from FIA and changes in Mt CO2e over last three measurements
matchedplots <- read.csv("centralapps/output/mbb_donorpool_plots.csv")%>%
  mutate(method = "Dynamic",
         type = "Baseline",
         time = year - 2000) %>%
  select(plt_cn, time, method, type,  deltac) 

### Join Project and Baseline under VM0045 type scenario
dynamic = full_join(projectplots, matchedplots)

### Clean up FVS runs for both the Project and Baseline scenarios - from "centralapps/fvs/02_carbonaccounting/02_carboncalcs.R"
modeled <- read.csv("centralapps/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  select(plt_cn, time, calc.mbb.bau, calc.mbb.grow) %>%
  pivot_longer(cols = c(calc.mbb.bau, calc.mbb.grow), names_to = "method", values_to = "deltac") %>%
  mutate(type = ifelse(method == "calc.mbb.bau", "Baseline", "Project"),
         method = "Modeled")

### Join all runs together but subset to only select FIA plots in all scenarios
allplots = dynamic %>%
  ## Subsets FVS runs to selected FIA Project Plots and Matched Plots
  full_join(modeled %>% filter(plt_cn %in% unique(dynamic$plt_cn)))


### Estimate Average Mt CO2e/ac/yr plus SD for entire contract
# Dynamic
addit.dynam <- read.csv("centralapps/output/centralapps_mbbs_projectvbau_fia.csv") %>%
  summarize(additional = round(mean(deltac - deltac.bau, na.rm = TRUE), digits=2),
            additional.se = round(sd(deltac - deltac.bau, na.rm = TRUE)/sqrt(length(deltac)), digits=2)) %>%
  mutate(method = "Dynamic", 
         type = "Project",
         y = -4, x = 12)

# Modeled
addit.mod <- read.csv("centralapps/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  summarize(additional = round(mean(delta.mbb.grow, na.rm = TRUE), digits=2),
            additional.se = round(sd(delta.mbb.grow, na.rm = TRUE)/sqrt(length(delta.mbb.grow)), digits=2)) %>%
  mutate(method = "Modeled",
         type = "Project",
         y = -4, x = 12)

## Group together to plot outputs
addit = full_join(addit.dynam, addit.mod)

### Plot the output
png("figures/dynamicvsmodeled_capps_mbb_icvcm.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(allplots, aes(x=time, y=deltac, col=type, fill=type)) +
  geom_smooth(method="lm", formula = y~poly(x,3)) +
  ggtitle("Central Apps: Maple / Beech / Birch group") + facet_wrap(~method) +
  theme_bw() + 
  scale_color_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  xlab("Years since project start") + ylab("Mt CO2e per acre per year") +
  geom_text(data = addit, aes(x, y, label = paste0("Avg is ", additional, " +/- ", additional.se)), col = "red4")
dev.off()



################################################################################
################################################################################
## 3) Northwoods - Oak / Hickory Group

## Clean up output from "northwoods/dynamicbaseline.R"
## Get selected Project plots from FIA and changes in Mt CO2e over last three measurements
projectplots <- read.csv("northwoods/output/oak_gof_plots.csv") %>%
  mutate(method = "Dynamic",
         type = "Project",
         time = year - 2000) %>%
  select(plt_cn, time, method, type, deltac)

## Get 10 Matched plots per Project plot from FIA and changes in Mt CO2e over last three measurements
matchedplots <- read.csv("northwoods/output/oak_donorpool_plots.csv")%>%
  mutate(method = "Dynamic",
         type = "Baseline",
         time = year - 2000) %>%
  select(plt_cn, time, method, type,  deltac) 

### Join Project and Baseline under VM0045 type scenario
dynamic = full_join(projectplots, matchedplots)

### Clean up FVS runs for both the Project and Baseline scenarios - from "northwoods/fvs/02_carbonaccounting/02_carboncalcs.R"
modeled <- read.csv("northwoods/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  select(plt_cn, time, calc.oak.bau, calc.oak.grow) %>%
  pivot_longer(cols = c(calc.oak.bau, calc.oak.grow), names_to = "method", values_to = "deltac") %>%
  mutate(type = ifelse(method == "calc.oak.bau", "Baseline", "Project"),
         method = "Modeled")

### Join all runs together but subset to only select FIA plots in all scenarios
allplots = dynamic %>%
  ## Subsets FVS runs to selected FIA Project Plots and Matched Plots
  full_join(modeled %>% filter(plt_cn %in% unique(dynamic$plt_cn)))


### Estimate Average Mt CO2e/ac/yr plus SD for entire contract
# Dynamic
addit.dynam <- read.csv("northwoods/output/northwoods_oaks_projectvbau_fia.csv") %>%
  summarize(additional = round(mean(deltac - deltac.bau, na.rm = TRUE), digits=2),
            additional.se = round(sd(deltac - deltac.bau, na.rm = TRUE)/sqrt(length(deltac)), digits=2)) %>%
  mutate(method = "Dynamic", 
         type = "Project",
         y = -3, x = 12)

# Modeled
addit.mod <- read.csv("northwoods/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  summarize(additional = round(mean(delta.oak.grow, na.rm = TRUE), digits=2),
            additional.se = round(sd(delta.oak.grow, na.rm = TRUE)/sqrt(length(delta.oak.grow)), digits=2)) %>%
  mutate(method = "Modeled",
         type = "Project",
         y = -3, x = 12)

## Group together to plot outputs
addit = full_join(addit.dynam, addit.mod)

### Plot the output
png("figures/dynamicvsmodeled_nw_oak_icvcm.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(allplots, aes(x=time, y=deltac, col=type, fill=type)) +
  geom_smooth(method="lm", formula = y~poly(x,3)) +
  ggtitle("Northwoods: Oak / Hickory group") + facet_wrap(~method) +
  theme_bw() + 
  scale_color_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  xlab("Years since project start") + ylab("Mt CO2e per acre per year") +
  geom_text(data = addit, aes(x, y, label = paste0("Avg is ", additional, " +/- ", additional.se)), col = "red4")
dev.off()





################################################################################
################################################################################
## 4) Northwoods - Maple / Beech / Birch Group

## Clean up output from "northwoods/dynamicbaseline.R"
## Get selected Project plots from FIA and changes in Mt CO2e over last three measurements
projectplots <- read.csv("northwoods/output/mbb_gof_plots.csv") %>%
  mutate(method = "Dynamic",
         type = "Project",
         time = year - 2000) %>%
  select(plt_cn, time, method, type, deltac)

## Get 10 Matched plots per Project plot from FIA and changes in Mt CO2e over last three measurements
matchedplots <- read.csv("northwoods/output/mbb_donorpool_plots.csv")%>%
  mutate(method = "Dynamic",
         type = "Baseline",
         time = year - 2000) %>%
  select(plt_cn, time, method, type,  deltac) 

### Join Project and Baseline under VM0045 type scenario
dynamic = full_join(projectplots, matchedplots)

### Clean up FVS runs for both the Project and Baseline scenarios - from "northwoods/fvs/02_carbonaccounting/02_carboncalcs.R"
modeled <- read.csv("northwoods/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  select(plt_cn, time, calc.mbb.bau, calc.mbb.grow) %>%
  pivot_longer(cols = c(calc.mbb.bau, calc.mbb.grow), names_to = "method", values_to = "deltac") %>%
  mutate(type = ifelse(method == "calc.mbb.bau", "Baseline", "Project"),
         method = "Modeled")

### Join all runs together but subset to only select FIA plots in all scenarios
allplots = dynamic %>%
  ## Subsets FVS runs to selected FIA Project Plots and Matched Plots
  full_join(modeled %>% filter(plt_cn %in% unique(dynamic$plt_cn)))


### Estimate Average Mt CO2e/ac/yr plus SD for entire contract
# Dynamic
addit.dynam <- read.csv("northwoods/output/northwoods_mbbs_projectvbau_fia.csv") %>%
  summarize(additional = round(mean(deltac - deltac.bau, na.rm = TRUE), digits=2),
            additional.se = round(sd(deltac - deltac.bau, na.rm = TRUE)/sqrt(length(deltac)), digits=2)) %>%
  mutate(method = "Dynamic", 
         type = "Project",
         y = -5, x = 12)

# Modeled
addit.mod <- read.csv("northwoods/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  summarize(additional = round(mean(delta.mbb.grow, na.rm = TRUE), digits=2),
            additional.se = round(sd(delta.mbb.grow, na.rm = TRUE)/sqrt(length(delta.mbb.grow)), digits=2)) %>%
  mutate(method = "Modeled",
         type = "Project",
         y = -5, x = 12)

## Group together to plot outputs
addit = full_join(addit.dynam, addit.mod)

### Plot the output
png("figures/dynamicvsmodeled_nw_mbb_icvcm.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(allplots, aes(x=time, y=deltac, col=type, fill=type)) +
  geom_smooth(method="lm", formula = y~poly(x,3)) +
  ggtitle("Northwoods: Maple / Beech / Birch group") + facet_wrap(~method) +
  theme_bw() + 
  scale_color_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  xlab("Years since project start") + ylab("Mt CO2e per acre per year") +
  geom_text(data = addit, aes(x, y, label = paste0("Avg is ", additional, " +/- ", additional.se)), col = "red4")
dev.off()

################################################################################
################################################################################
## 5) Northeast - Oak / Hickory Group

## Clean up output from "northeast/dynamicbaseline.R"
## Get selected Project plots from FIA and changes in Mt CO2e over last three measurements
projectplots <- read.csv("northeast/output/oak_gof_plots.csv") %>%
  mutate(method = "Dynamic",
         type = "Project",
         time = year - 2000) %>%
  select(plt_cn, time, method, type, deltac)

## Get 10 Matched plots per Project plot from FIA and changes in Mt CO2e over last three measurements
matchedplots <- read.csv("northeast/output/oak_donorpool_plots.csv")%>%
  mutate(method = "Dynamic",
         type = "Baseline",
         time = year - 2000) %>%
  select(plt_cn, time, method, type,  deltac) 

### Join Project and Baseline under VM0045 type scenario
dynamic = full_join(projectplots, matchedplots)

### Clean up FVS runs for both the Project and Baseline scenarios - from "northeast/fvs/02_carbonaccounting/02_carboncalcs.R"
modeled <- read.csv("northeast/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  select(plt_cn, time, calc.oak.bau, calc.oak.grow) %>%
  pivot_longer(cols = c(calc.oak.bau, calc.oak.grow), names_to = "method", values_to = "deltac") %>%
  mutate(type = ifelse(method == "calc.oak.bau", "Baseline", "Project"),
         method = "Modeled")

### Join all runs together but subset to only select FIA plots in all scenarios
allplots = dynamic %>%
  ## Subsets FVS runs to selected FIA Project Plots and Matched Plots
  full_join(modeled %>% filter(plt_cn %in% unique(dynamic$plt_cn)))


### Estimate Average Mt CO2e/ac/yr plus SD for entire contract
# Dynamic
addit.dynam <- read.csv("northeast/output/northeast_oaks_projectvbau_fia.csv") %>%
  summarize(additional = round(mean(deltac - deltac.bau, na.rm = TRUE), digits=2),
            additional.se = round(sd(deltac - deltac.bau, na.rm = TRUE)/sqrt(length(deltac)), digits=2)) %>%
  mutate(method = "Dynamic", 
         type = "Project",
         y = -3, x = 12)

# Modeled
addit.mod <- read.csv("northeast/fvs/02_carbonaccounting/output/clean_fvsoutput_oak.csv") %>%
  summarize(additional = round(mean(delta.oak.grow, na.rm = TRUE), digits=2),
            additional.se = round(sd(delta.oak.grow, na.rm = TRUE)/sqrt(length(delta.oak.grow)), digits=2)) %>%
  mutate(method = "Modeled",
         type = "Project",
         y = -3, x = 12)

## Group together to plot outputs
addit = full_join(addit.dynam, addit.mod)

### Plot the output
png("figures/dynamicvsmodeled_ne_oak_icvcm.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(allplots, aes(x=time, y=deltac, col=type, fill=type)) +
  geom_smooth(method="lm", formula = y~poly(x,3)) +
  ggtitle("Northeast: Oak / Hickory group") + facet_wrap(~method) +
  theme_bw() + 
  scale_color_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  xlab("Years since project start") + ylab("Mt CO2e per acre per year") +
  geom_text(data = addit, aes(x, y, label = paste0("Avg is ", additional, " +/- ", additional.se)), col = "red4")
dev.off()




################################################################################
################################################################################
## 6) Northeast - Maple / Beech / Birch Group

## Clean up output from "northeast/dynamicbaseline.R"
## Get selected Project plots from FIA and changes in Mt CO2e over last three measurements
projectplots <- read.csv("northeast/output/mbb_gof_plots.csv") %>%
  mutate(method = "Dynamic",
         type = "Project",
         time = year - 2000) %>%
  select(plt_cn, time, method, type, deltac)

## Get 10 Matched plots per Project plot from FIA and changes in Mt CO2e over last three measurements
matchedplots <- read.csv("northeast/output/mbb_donorpool_plots.csv")%>%
  mutate(method = "Dynamic",
         type = "Baseline",
         time = year - 2000) %>%
  select(plt_cn, time, method, type,  deltac) 

### Join Project and Baseline under VM0045 type scenario
dynamic = full_join(projectplots, matchedplots)

### Clean up FVS runs for both the Project and Baseline scenarios - from "northeast/fvs/02_carbonaccounting/02_carboncalcs.R"
modeled <- read.csv("northeast/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  select(plt_cn, time, calc.mbb.bau, calc.mbb.grow) %>%
  pivot_longer(cols = c(calc.mbb.bau, calc.mbb.grow), names_to = "method", values_to = "deltac") %>%
  mutate(type = ifelse(method == "calc.mbb.bau", "Baseline", "Project"),
         method = "Modeled")

### Join all runs together but subset to only select FIA plots in all scenarios
allplots = dynamic %>%
  ## Subsets FVS runs to selected FIA Project Plots and Matched Plots
  full_join(modeled %>% filter(plt_cn %in% unique(dynamic$plt_cn)))


### Estimate Average Mt CO2e/ac/yr plus SD for entire contract
# Dynamic
addit.dynam <- read.csv("northeast/output/northeast_mbbs_projectvbau_fia.csv") %>%
  summarize(additional = round(mean(deltac - deltac.bau, na.rm = TRUE), digits=2),
            additional.se = round(sd(deltac - deltac.bau, na.rm = TRUE)/sqrt(length(deltac)), digits=2)) %>%
  mutate(method = "Dynamic", 
         type = "Project",
         y = -4, x = 12)

# Modeled
addit.mod <- read.csv("northeast/fvs/02_carbonaccounting/output/clean_fvsoutput_mbb.csv") %>%
  summarize(additional = round(mean(delta.mbb.grow, na.rm = TRUE), digits=2),
            additional.se = round(sd(delta.mbb.grow, na.rm = TRUE)/sqrt(length(delta.mbb.grow)), digits=2)) %>%
  mutate(method = "Modeled",
         type = "Project",
         y = -4, x = 12)

## Group together to plot outputs
addit = full_join(addit.dynam, addit.mod)

### Plot the output
png("figures/dynamicvsmodeled_ne_mbb_icvcm.png", 
    width=7,
    height=6, units="in", res = 350 )
ggplot(allplots, aes(x=time, y=deltac, col=type, fill=type)) +
  geom_smooth(method="lm", formula = y~poly(x,3)) +
  ggtitle("Northeast: Maple / Beech / Birch group") + facet_wrap(~method) +
  theme_bw() + 
  scale_color_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  scale_fill_viridis(discrete = TRUE, begin = 0.1, end=0.8, name="") + 
  xlab("Years since project start") + ylab("Mt CO2e per acre per year") +
  geom_text(data = addit, aes(x, y, label = paste0("Avg is ", additional, " +/- ", additional.se)), col = "red4")
dev.off()




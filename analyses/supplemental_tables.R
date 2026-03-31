### Build additional tables
### 1) to consolidate the region decriptions
### and 2) to show the differences in harvest likelihood and harvest intensity across the regions/forest types

### Doing in R to maintain consistency in appearence

### housekeeping
rm(list=ls()) 
options(stringsAsFactors = FALSE)

## Load Libraries
library(tidyr)
library(dplyr)
library(gridExtra)

# Set Working Directory
setwd("~/Documents/git/dynamic_carbonaccounting/analyses/")


#### Start with Regional Descriptions
ne <- data.frame(Region = "Northeast",
                 States = "Massachusetts, Maine, \nNew Hampshire, New York \nand Vermont",
                 Ecoprovinces = "Northeastern Mixed Forest, \nEastern Broadleaf Forest, 
                 \nMidwest Broadleaf Forest, \nand Central Appalachian Broadleaf Forest")

centralapps <- data.frame(Region = "Central Appalachians",
                          States = "Maryland, Ohio, \nPennsylvania, \nand West Virginia",
                          Ecoprovinces = "Northeastern Mixed Forest, \nEastern Broadleaf Forest, 
                          \nMidwest Broadleaf Forest, \nand Central Appalachian Broadleaf Forest")
southernapps <- data.frame(Region = "Southern Appalachians",
                           States = "Alabama, Georgia, \nKentucky, North Carolina, 
                           \nSouth Carolina, Tennessee, \nand Virigina",
                           Ecoprovinces = "Eastern Broadleaf Forest, \nSouthern Mixed Forest, 
                           \nand Central Appalachian Broadleaf Forest")

northwoods <- data.frame(Region = "Northwoods",
                         States = "Michigan, Minnesota, \nand Wisconsin",
                         Ecoprovinces = "Midwest Broadleaf Forest")

regions <- full_join(ne, centralapps) %>%
  rbind(southernapps) %>%
  rbind(northwoods)


png("figures/regional_descriptions.png", 
    width=8,
    height=4.5, units="in", res = 350 )
grid.arrange(regions %>%
               tableGrob(theme = ttheme_default(
                 core = list(bg_params=list(fill=c("grey90", "white"))),
                 colhead = list(fg_params=list(col="white"),
                                bg_params=list(fill="#084594"))), rows = NULL))
dev.off()






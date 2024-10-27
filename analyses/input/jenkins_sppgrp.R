### Create a Jenkins Equation Function
# Started 22 December 2022 by Cat
# Since TreeMap isn't accurate to the species level, we found grouping species
# by Jenkins Eqn group is more accurate


refspp.df <- read.csv("source/Z0311_FIA_REF_SPECIES.csv")

  
  refspp.df$species <- refspp.df$COMMON_NAME
  
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==1, "Cedar/larch", NA)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==2, "Douglas-fir", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==3, "True fir/hemlock", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==4, "Pine", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==5, "Spruce", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==6, "Aspen/alder/cottonwood/willow", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==7, "Soft maple/birch", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==8, "Mixed hardwood", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==9, "Hard maple/oak/hickory/beech", refspp.df$sppgrp)
  refspp.df$sppgrp <- ifelse(refspp.df$JENKINS_SPGRPCD==10, "Juniper/oak/mesquite", refspp.df$sppgrp)
  
  refspp.df <- subset(refspp.df, select=c(species, sppgrp))
  refspp.df <- refspp.df[!duplicated(refspp.df),]
  refspp.df <- refspp.df[complete.cases(refspp.df),]
  
  write.csv(refspp.df, "source/refspp_sppgrp.csv", row.names=FALSE)

  
# Top commands ----
# Create empty R application (no figures, data frames, packages, etc.)
# https://stackoverflow.com/questions/7505547/detach-all-packages-while-working-in-r
detachAllPackages <- function() {
        basic.packages <- c("package:stats","package:graphics","package:grDevices","package:utils","package:datasets","package:methods","package:base")
        package.list <- search()[ifelse(unlist(gregexpr("package:",search()))==1,TRUE,FALSE)]
        package.list <- setdiff(package.list,basic.packages)
        if (length(package.list)>0)  for (package in package.list) detach(package, character.only=TRUE)
        
}
detachAllPackages()

rm(list=ls(all=TRUE))

# load library
library(tidyverse)
library(readxl)

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_1/")
data_files = "data_files/labor_force_participation/"

# Load data ----

df_lmp <- read_xlsx(paste0(data_files, "male/m_master_file.xlsx"))

# Clean data ----

df_lmp <- df_lmp %>%
        mutate(unadjusted_lfp = unadjusted_lfp*100,
               adjusted_lfp = adjusted_lfp*100)

# Graph data ----

ggplot() +
        geom_line(data = subset(df_lmp,geo_code==1 & comparison == 1), aes(y=unadjusted_lfp, x = year, color = "Dauphin")) +
        geom_line(data = subset(df_lmp,geo_code==1 & comparison == 1), aes(y=adjusted_lfp, x = year, color = "Dauphin age adjusted")) +
        geom_line(data = subset(df_lmp,geo_code==2 & comparison == 1), aes(y=unadjusted_lfp, x = year, color = "Manitoba")) +
        theme_bw() + 
        scale_x_continuous(breaks = c(1971,1976,1981)) +
        scale_y_continuous(breaks = seq(65,80,5), limits = c(65,80)) +
        ylab("Census labor force participation rate") +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )


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

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_latner_2017/")
data_files = "data_files/"
graphs = "graphs/"

# Load data ----

df_mincome <- readRDS(file = paste0(data_files, "mincome.rds"))

# Clean data ----

df_mincome <- df_mincome %>%
        filter(sitetreat_2==1) %>%#
        filter(month>23) %>%
        select(month, benefit, participation_rate, participation_cum, walkin_date, dropout_date)

df_graph <- df_mincome %>%
        group_by(month) %>%
        summarise(across(everything(), ~ sum(.x, na.rm = TRUE))) %>%
        ungroup()

# Graph data ----

ggplot(data = df_graph, aes(x = month)) +
        geom_line(aes(y=walkin_date, color = "Walk-ins")) +
        geom_line(aes(y=dropout_date, color = "Dropouts")) +
        geom_line(aes(y=participation_rate/10, color = "Participation")) +
        theme_bw() + 
        scale_y_continuous(
                limits = c(0, 65),
                breaks = seq(0, 60, 20),
                labels = c(0, 200, 400, 600),
                name = "Monthly participation",
                sec.axis = sec_axis(~.,
                                    name="Monthly walkins/dropouts",
                                    # breaks = seq(0, 60, 15),
                )
        ) +
        theme(panel.grid.minor = element_blank(), 
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"figure_1_participation.pdf"), plot = last_plot(), height = 8, width = 12, units = "in")


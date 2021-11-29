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
library(zoo) # as.yearmon
library(lubridate) # make_date

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_latner_2017/")
data_files = "data_files/"
graphs = "graphs/"

# Load data ----

df_mincome <- readRDS(paste0(data_files, "mincome_balanced.rds"))

# Clean data ----

# sitetreat_3 1 "MB control"
# sitetreat_3 2 "Dauphin"
# sitetreat_3 3 "Rural treatment"

df_mincome <- df_mincome %>%
        filter(!is.na(sitetreat_3)) %>%
        mutate(sitetreat_3=as.factor(sitetreat_3))

table(df_mincome$sitetreat_3, useNA = "ifany")

# Summarize data ----

df_summary <- df_mincome %>%
        group_by(sitetreat_3, year, halfyear) %>%
        summarise(across(c("lmp_0", "lmp_1"), ~ sum(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(total = rowSums(select(., .dots = c("lmp_0", "lmp_1")), na.rm = TRUE),
               lmp_0_per = lmp_0/total,
               lmp_1_per = lmp_1/total
        ) %>%
        arrange(sitetreat_3, year, halfyear) %>%
        mutate(month = 1)

df_summary <- df_summary %>%
        mutate(lmp_1_per = ifelse(halfyear == 4, yes = lag(lmp_1_per), # ignore first month of the program (december, 1974)
                                  ifelse(halfyear == 5, yes = NA, no = lmp_1_per))) # ignore first 6 months of 1975 (lots of walkins/dropouts)

# Expand data ----

df_baseline <- df_summary %>%
        group_by(sitetreat_3, year, halfyear) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(month = 2)
df_months <- data.frame()
months <- seq(3,6,1)
for (m in months) {
        df_test <- df_baseline %>%
                mutate(month=m)
        df_months <- rbind(df_months,df_test)
}                

# Append earnings data
df_summary <- rbind(df_summary,df_baseline,df_months)
rm(df_baseline,df_months,df_test,m,months)

df_summary <- df_summary %>%
        select(sitetreat_3, year, halfyear, month, everything()) %>%
        mutate(month = ifelse(halfyear == 2 | halfyear == 4 | halfyear == 6 | halfyear == 8 | halfyear == 10, yes = month + 6, no = month),
               ym = as.Date(as.yearmon((make_datetime(year,month)))),
               ) %>%
        arrange(sitetreat_3,halfyear,month)

# Graph data ----

df_graph <- df_summary

df_graph$sitetreat_3 <- factor(df_graph$sitetreat_3, labels=c("MB Control", 
                                                          "Dauphin", 
                                                          "MB Treatment"))

ggplot(data = df_graph, aes(x = ym, y = lmp_1_per, group = sitetreat_3, linetype = sitetreat_3, color = sitetreat_3)) +
        geom_line(size=1) +
        theme_bw() + 
        scale_x_date(date_breaks = "1 year", 
                     date_labels = "%Y") +
        scale_y_continuous(breaks = c(seq(.5, 1, by = .1)), limits = c(.5, 1)) +
        ylab("Labor market participation (%)") + 
        scale_linetype_manual(values = c("solid","solid","dashed")) +
        scale_color_manual(values = c("gray50","black","gray50")) +
        theme(panel.grid.minor = element_blank(), 
              axis.title.x = element_blank(),
              axis.line.y = element_line(color="black", size=.5),
              axis.line.x = element_line(color="black", size=.5),
              legend.title=element_blank(),
              legend.key.width = unit(2,"cm"),
              legend.position = "bottom"
        )

ggsave(filename = paste0(graphs,"figure_4_diff_in_diff.pdf"), plot = last_plot(), height = 8, width = 12, units = "in")


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
library(estimatr) # lm_robust
library(gridExtra)
library(zoo) # as.yearmon
library(lubridate) # make_date
library(car) # recode

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_latner_2017/")
data_files = "data_files/"
graphs = "graphs/"
tables = "tables/"

options(scipen=999)

# Load data ----

df_mincome <- readRDS(paste0(data_files, "mincome_balanced.rds"))

# Clean data ----

# sitetreat_3 1 "MB control"
# sitetreat_3 2 "Dauphin"
# sitetreat_3 3 "Rural treatment"

# Please note coding error with regard to high school and no highschool
# Original code mean that some families were included as both completed and not completed high school

df_mincome <- df_mincome %>%
        filter(!is.na(period)) %>%
        filter(!is.na(sitetreat_3)) %>%
        mutate(sitetreat_3=as.factor(sitetreat_3),
        ) %>%
        select(famno, sitetreat_3, lmp, period, month, 
               period,
               matches("lmp"),
               famsize, onehead_dummy, twohead_dummy, agem, agef, numchild, # family vars at baseline
               mh_high, fh_high, # education (high school dummy)
               uic73, uic74, # unemployment insurance
               sa73, sa74, # social assistance (i.e. welfare)
               ) %>%
        mutate(adults = ifelse(twohead_dummy==0, yes = 1, 
                               ifelse(twohead_dummy==1, yes = 2, no = 0)),
               kids = famsize - adults, # the assumption is if you are not a head, then you are a child.  even if numchild = 0 & "kids" > 0, heads are responsible for nonheads
               test = kids-numchild,
               age_head = ifelse(is.na(agem), yes = agef,
                                 ifelse(is.na(agef), yes = agem,
                                 ifelse(!is.na(agef) & !is.na(agem), yes = (agef+agem)/2, no = NA))),
               # edu_hs = ifelse(mh_high==1, yes = 1, no = NA),
               # edu_hs = ifelse(is.na(edu_hs)&fh_high==1, yes = 1, no = edu_hs),
               # edu_nhs = ifelse(mh_high==2, yes = 1, no = NA),
               # edu_nhs = ifelse(is.na(edu_nhs)&fh_high==2, yes = 1, no = edu_nhs),
               edu_hs = ifelse(is.na(mh_high)==1|is.na(fh_high)==1, yes = 1, no = 0),
               edu_nhs = ifelse(edu_hs==0, yes = 1, no = 0),
               welfare = ifelse(uic73>0 | uic74>0 | sa73>0 | sa74>0, yes = 1, no = 0),
               younger_age = ifelse(age_head<30, yes = 1, no = 0),
               middle_age = ifelse(age_head>=30 & age_head <= 50, yes = 1, no = 0),
               older_age = ifelse(age_head > 50, yes = 1, no = 0),
        )

# select(df_mincome,famno,month,sitetreat_3,edu_hs,edu_nhs,mh_high,fh_high) %>% filter(famno==1523)

with(df_mincome,table(edu_hs,edu_nhs,useNA = "ifany"))

with(df_mincome,table(edu_hs,useNA = "ifany"))
with(df_mincome,table(mh_high,useNA = "ifany"))
with(df_mincome,table(fh_high,useNA = "ifany"))

# Difference in difference ----

# Two head 
df_subgroup <- df_mincome %>%
        filter(twohead_dummy==1)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(twohead_dummy==1) %>%
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Dual headed")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_two <- df_subgroup

#  Single
df_subgroup <- df_mincome %>%
        filter(twohead_dummy==0)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(twohead_dummy==0) %>%
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Single")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_one <- df_subgroup
df_subgroup_one

# Parents

df_subgroup <- df_mincome %>%
        filter(kids>0)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(kids>0) %>% 
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Parent")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_parent <- df_subgroup

# Younger age

df_subgroup <- df_mincome %>%
        filter(age_head<30)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(age_head<30) %>% 
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Younger")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_younger <- df_subgroup


# Middle age

df_subgroup <- df_mincome %>%
        filter(age_head>=30 & age_head <= 50)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(age_head>=30 & age_head <= 50) %>% 
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Middle")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]
df_subgroup

df_subgroup_middle <- df_subgroup

# Older age

df_subgroup <- df_mincome %>%
        filter(age_head>50)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(age_head>50) %>% 
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Older")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_older <- df_subgroup


# Highschool education

df_subgroup <- df_mincome %>%
        filter(edu_hs==1)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(edu_hs==1) %>% 
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Highschool")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_hs <- df_subgroup

# No highschool education

df_subgroup <- df_mincome %>%
        filter(edu_nhs==1)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(edu_nhs==1) %>% 
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "No highschool")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_nhs <- df_subgroup

# Receive unemployment or welfare

df_subgroup <- df_mincome %>%
        filter(welfare==1)

model <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_subgroup, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

df_subgroup <- df_mincome %>%
        filter(welfare==1) %>% 
        select(sitetreat_3, famno, period, month, matches("lmp_")) %>%
        mutate(term = "Unmp or Welfare")
df_subgroup$beta <- summary(model)$coefficients[4,1]*100
df_subgroup$p_value <- summary(model)$coefficients[4,4]

df_subgroup_welfare <- df_subgroup

# Combine tables to create table 4 and figure 5 ----

df_table <- rbind(df_subgroup_two,df_subgroup_one,df_subgroup_parent,df_subgroup_younger,df_subgroup_middle,df_subgroup_older,df_subgroup_hs,df_subgroup_nhs,df_subgroup_welfare)
df_figure <- rbind(df_subgroup_two,df_subgroup_one,df_subgroup_parent,df_subgroup_younger,df_subgroup_middle,df_subgroup_older,df_subgroup_hs,df_subgroup_nhs,df_subgroup_welfare)
rm(df_subgroup,df_subgroup_two,df_subgroup_one,df_subgroup_parent,df_subgroup_younger,df_subgroup_middle,df_subgroup_older,df_subgroup_hs,df_subgroup_nhs,df_subgroup_welfare,model)
# rm(list=ls(pattern="df_subgroup"))

# Summarize data for figure ----

df_figure <- df_figure %>%
        mutate(stars = ifelse(p_value < .001, yes = "***",
                              ifelse(p_value >= .001 | p_value < .01, yes = "**",
                                     ifelse(p_value >= .01 | p_value < .05, yes = "*", no = NA))),
               did = paste0(round(beta,1),stars))

df_did <- df_figure %>%
        select(term,did)
df_did <- unique(df_did)
df_did

df_summary <- df_figure %>%
        group_by(term, sitetreat_3, period) %>%
        summarise(across(c("lmp_0", "lmp_1"), ~ sum(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(total = rowSums(select(., .dots = c("lmp_0", "lmp_1")), na.rm = TRUE),
               lmp_0_per = lmp_0/total,
               lmp_1_per = lmp_1/total
        ) %>%
        arrange(term, sitetreat_3, period) %>%
        mutate(month = ifelse(period == 0, yes = 1, no = 25))

# Expand data for figure - this is the "R" equivalent for Stata expand command

# expand by 24 months in baseline period
df_baseline_period <- df_summary %>%
        filter(period == 0) %>%
        group_by(term,sitetreat_3,period) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(month = 2)
df_months <- data.frame()
months <- seq(3,24,1)
for (m in months) {
        df_test <- df_baseline_period %>%
                mutate(month=m)
        df_months <- rbind(df_months,df_test)
}                

# Append earnings data
df_summary <- rbind(df_summary,df_baseline_period,df_months)
rm(df_baseline_period,df_months,df_test,m,months)
df_summary <- df_summary %>%
        select(term, sitetreat_3, period, month, everything()) %>%
        arrange(term,sitetreat_3, period, month)

# expand by 36 months in study period
df_study_period <- df_summary %>%
        filter(period == 1) %>%
        group_by(term,sitetreat_3, period) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(month = 26)

df_months <- data.frame()
months <- seq(27,60,1)
for (m in months) {
        df_test <- df_study_period %>%
                mutate(month=m)
        df_months <- rbind(df_months,df_test)
}                


# Append earnings data
df_summary <- rbind(df_summary,df_study_period,df_months)
rm(df_study_period,df_months,df_test,m,months)

df_summary$year <- recode(df_summary$month, "1:12=1973;13:24=1974;25:36=1975;37:48=1976;49:60=1977")
df_summary$halfyear <- recode(df_summary$month, "1:6=1;7:12=2;13:18=3;19:24=4;25:30=5;31:36=6;37:42=7;43:48=8;49:54=9;55:60=10")

df_summary <- df_summary %>%
        select(term, sitetreat_3, period, year, month, everything()) %>%
        mutate(lmp_1_per = ifelse(month>=24 & month<30, yes = NA, no = lmp_1_per),
        ) %>%
        group_by(term, sitetreat_3, period, year) %>%
        mutate(month = row_number()) %>%
        ungroup() %>%
        mutate(ym = as.Date(as.yearmon((make_datetime(year,month)))),
        ) %>%
        arrange(term,sitetreat_3,period,year,month)

# Graph figure 5 ----

df_graph <- df_summary

df_graph$sitetreat_3 <- factor(df_graph$sitetreat_3, labels=c("MB Control", 
                                                              "Dauphin", 
                                                              "MB Treatment"))

df_graph$term <- factor(df_graph$term, labels=c("Dual headed", "Single", "Parent",
                                                "Younger", "Middle", "Older",
                                                "Highschool", "No highschool", "Unmp or Welfare"))

# merge in difference in difference estimates from model
df_graph <- merge(df_graph,df_did)
df_graph <- data.frame(df_graph)

ggplot(data = df_graph, aes(x = ym, y = lmp_1_per, group = sitetreat_3, linetype = sitetreat_3, color = sitetreat_3)) +
        facet_wrap(~term) + 
        geom_line(size=1) +
        theme_bw() + 
        geom_text(aes(label=paste("DiD = ", did)), 
                  size = 3,
                  check_overlap = TRUE,
                  inherit.aes = FALSE, 
                  show.legend = FALSE,
                  x=-Inf, y=.45, hjust=-0.2, vjust=1.2)+
        scale_x_date(date_breaks = "1 year", 
                     date_labels = "%Y") +
        scale_y_continuous(breaks = c(seq(.4, 1, by = .2)), limits = c(.4, 1)) +
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

ggsave(filename = paste0(graphs,"figure_5_diff_in_diff_subgroup.pdf"), plot = last_plot(), height = 8, width = 12, units = "in")

# Summarize data for table 4 ----

df_sitetreat_total <- df_mincome %>%
        group_by(sitetreat_3,famno) %>%
        slice(1) %>%
        group_by(sitetreat_3) %>%
        summarise(count=n()) %>%
        ungroup()

df_sitetreat_total$sitetreat_3 <- factor(df_sitetreat_total$sitetreat_3, 
                                         labels=c("MB_Control", "Dauphin", "Rural_treatment"))
df_sitetreat_total <- pivot_wider(df_sitetreat_total, names_from = sitetreat_3, values_from = count)
df_sitetreat_total$term <- "Total unique observations"

df_sitetreat_subgroup <- df_table %>%
        group_by(sitetreat_3,term,famno) %>%
        slice(1) %>%
        group_by(sitetreat_3,term) %>%
        summarise(count=n()) %>%
        ungroup()

df_sitetreat_subgroup$sitetreat_3 <- factor(df_sitetreat_subgroup$sitetreat_3, 
                                            labels=c("MB_Control", "Dauphin", "Rural_treatment"))

df_sitetreat_subgroup$term <- factor(df_sitetreat_subgroup$term, levels=c("Dual headed", "Single", "Parent",
                                                                          "Younger", "Middle", "Older",
                                                                          "Highschool", "No highschool", "Unmp or Welfare"))
df_sitetreat_subgroup <- pivot_wider(df_sitetreat_subgroup, names_from = sitetreat_3, values_from = count) %>%
        arrange(term)

df_table_observations_subgroup <- rbind(df_sitetreat_total,df_sitetreat_subgroup) %>%
        select(term,everything())
rm(df_sitetreat_total,df_sitetreat_subgroup)

df_table_observations_subgroup

pdf(paste0(tables,"table_4_baseline_characteristics.pdf"))
grid.table(df_table_observations_subgroup, rows = NULL)
dev.off()

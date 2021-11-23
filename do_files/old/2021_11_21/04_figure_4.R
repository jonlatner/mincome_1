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
library(zoo) # na.locf
library(car)

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_1/")
data_files = "data_files/"

# Load data ----

df_mincome <- readRDS(paste0(data_files, "mincome_balanced.rds"))

# Clean data ----

# sitetreat 1 "MB control"
# sitetreat 2 "Dauphin"
# sitetreat 3 "Rural treatment"

df_mincome <- df_mincome %>%
        filter(!is.na(sitetreat_2) | sitetreat == 4)  %>%
        mutate(sitetreat_3 = ifelse(sitetreat_2==0, yes = 1,
                                    ifelse(sitetreat_2==1, yes = 2, no = 0))) %>%
        mutate(sitetreat_3 = ifelse(sitetreat==4, yes = 3, no = sitetreat_3)) %>%
        select(-sitetreat, sitetreat_2) %>%
        rename(sitetreat = sitetreat_3)

# Baseline earnings data ----

df_mincome <- df_mincome %>%
        mutate(earnings_74 = rowSums(select(., .dots = c("totnhinc74", "mhtotern74", "fhtotern74")), na.rm = TRUE),
               earnings_73 = rowSums(select(., .dots = c("totnhinc73", "mhtotern73", "fhtotern73")), na.rm = TRUE),
        ) %>%
        mutate(earnings_74 = ifelse(earnings_74==0, yes = NA, no = earnings_74),
               earnings_74 = ifelse(is.na(earnings_74) & (mhtotern74 == 0 | fhtotern74 == 0 | fhtotern74 == 0), yes = 0, no = earnings_74),
               earnings_74 = earnings_74/12,
               earnings_73 = ifelse(earnings_73==0, yes = NA, no = earnings_73),
               earnings_73 = ifelse(is.na(earnings_73) & (mhtotern73 == 0 | fhtotern73 == 0 | totnhinc73 == 0), yes = 0, no = earnings_73),
               earnings_73 = earnings_73/12,
        ) %>%
        mutate(month = month + 23)

summary(df_mincome$earnings_74)
summary(df_mincome$earnings_73)

df_earnings_baseline <- df_mincome %>%
        group_by(famno) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(month = 1)

df_months <- data.frame()
months <- seq(2,23,1)
for (m in months) {
        df_test <- df_earnings_baseline %>%
                mutate(month=m)
        df_months <- rbind(df_months,df_test)
}                

df_earnings_baseline <- rbind(df_earnings_baseline,df_months) %>%
        arrange(famno,month) %>%
        mutate(wages = ifelse(month<13, yes = earnings_73, earnings_74)) %>%
        select(-earnings_73, -earnings_74)

rm(df_months,df_test,m,months)

# Study period earnings data ----

df_earnings_study <- df_mincome %>%
        rename(wages = wages_tvc)

# Append earnings data ----

df_earnings <- bind_rows(df_earnings_baseline, df_earnings_study)
rm(df_earnings_baseline, df_earnings_study)

df_earnings <- df_earnings %>%
        mutate(lmp = ifelse(wages>0, yes = 1, no = 0),
               lmp_0 = ifelse(lmp==0, yes = 1, no = 0),
               lmp_1 = ifelse(lmp==1, yes = 1, no = 0))

table(df_earnings$unique)

# Sample cleaning ----

# Modify the walkin variable to indicate "ever walked in" as opposed to identifying the period at which a person walked in
df_earnings <- df_earnings %>%
        mutate(walkin = ifelse(walkin == 0, yes = NA, no = walkin)) %>%
        group_by(famno) %>%
        mutate(walkin = na.locf(walkin, na.rm = FALSE), # replace missing value with previous non-missing value
               walkin = last(walkin), # replace all cases with the last value
        ) %>%
        ungroup()

# You are not in the sample if you are a walkin participant and have missing baseline information
df_earnings <- df_earnings %>%
        mutate(missing = ifelse(unique == 1 & walkin == 1 & is.na(earnings_74) & is.na(earnings_73), yes = 1, no = 0),
        ) %>%
        group_by(famno) %>%
        mutate(missing = last(missing), # replace all cases with the last value
        ) %>%
        ungroup() %>%
        filter(missing==0|is.na(missing)) %>%
        select(-missing)

# You are not in the sample if you are a panel participant and have missing baseline information
df_earnings <- df_earnings %>%
        mutate(missing = ifelse(first_irf == 1 & last_irf == 37 & is.na(earnings_74) & is.na(earnings_73), yes = 1, no = 0),
        ) %>%
        group_by(famno) %>%
        mutate(missing = last(missing), # replace all cases with the last value
        ) %>%
        ungroup() %>%
        filter(missing==0|is.na(missing)) %>%
        select(-missing)

table(df_earnings$sitetreat)

# Recode date ----

df_earnings$year <- recode(df_earnings$month, "1:12=1973;13:24=1974;25:36=1975;37:48=1976;49:60=1977")
df_earnings$halfyear <- recode(df_earnings$month, "1:6=1;7:12=2;13:18=3;19:24=4;25:30=5;31:36=6;37:42=7;43:48=8;49:54=9;55:60=10")
with(df_earnings,table(month,year, useNA = "ifany"))
with(df_earnings,table(month,halfyear, useNA = "ifany"))

# Summarize data ----

df_summary <- df_earnings %>%
        group_by(halfyear, sitetreat) %>%
        summarise(across(c("lmp_0", "lmp_1"), ~ sum(.x, na.rm = TRUE))) %>%
        ungroup() %>%
        mutate(total = rowSums(select(., .dots = c("lmp_0", "lmp_1")), na.rm = TRUE),
               lmp_0_per = lmp_0/total,
               lmp_1_per = lmp_1/total
        )

df_summary
        

df_summary <- df_summary %>%
        rename(lmp_0_num = lmp_0,
               lmp_1_num = lmp_1)
rename lmp_1 lmp_0_num
rename lmp_2 lmp_1_num
gen total = lmp_0_num + lmp_1_num
gen lmp_0_per = lmp_0/total
gen lmp_1_per = lmp_1/total

# Graph data ----


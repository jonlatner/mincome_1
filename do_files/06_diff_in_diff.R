# Top commands ----
# Create emp# Top commands ----
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
library(broom) # tidy
library(gridExtra)
library(Cairo)

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_latner_2017/")
data_files = "data_files/"
tables = "tables/"

options(scipen=999)

# Load data ----

df_mincome <- readRDS(paste0(data_files, "mincome_balanced.rds"))

# Clean data ----

# sitetreat_3 1 "MB control"
# sitetreat_3 2 "Dauphin"
# sitetreat_3 3 "Rural treatment"

df_mincome <- df_mincome %>%
        filter(!is.na(period)) %>%
        filter(!is.na(sitetreat_3)) %>%
        mutate(sitetreat_3=as.factor(sitetreat_3),
        ) %>%
        select(famno, sitetreat_3, lmp, period, month)

table(df_mincome$sitetreat_3)

# Difference in difference ----

# Baseline
model_mb_0 <- lm_robust(lmp~relevel(as.factor(period), ref = "0"),data=subset(df_mincome, sitetreat_3 == 1), se_type = "stata")
model_da_0 <- lm_robust(lmp~relevel(as.factor(period), ref = "0"),data=subset(df_mincome, sitetreat_3 == 2), se_type = "stata")
model_ru_0 <- lm_robust(lmp~relevel(as.factor(period), ref = "0"),data=subset(df_mincome, sitetreat_3 == 3), se_type = "stata")

# Study period
model_mb_1 <- lm_robust(lmp~relevel(as.factor(period), ref = "1"),data=subset(df_mincome, sitetreat_3 == 1), se_type = "stata")
model_da_1 <- lm_robust(lmp~relevel(as.factor(period), ref = "1"),data=subset(df_mincome, sitetreat_3 == 2), se_type = "stata")
model_ru_1 <- lm_robust(lmp~relevel(as.factor(period), ref = "1"),data=subset(df_mincome, sitetreat_3 == 3), se_type = "stata")

# diff 1: experimental effect (baseline)
model_diff_1_exp_0 <- lm_robust(lmp~sitetreat_3,data=subset(df_mincome, period == 0 & (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

# diff 1: experimental effect (study period)
model_diff_1_exp_1 <- lm_robust(lmp~sitetreat_3,data=subset(df_mincome, period == 1 & (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

# diff 1: experimental effect (change)
model_diff_1_exp_change <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_mincome, (sitetreat_3 == 1 | sitetreat_3 == 2)), se_type = "stata")

# diff 1: community effect (baseline)
model_diff_2_com_0 <- lm_robust(lmp~sitetreat_3,data=subset(df_mincome, period == 0 & (sitetreat_3 == 2 | sitetreat_3 == 3)), se_type = "stata")

# diff 2: community effect (study period)
model_diff_2_com_1 <- lm_robust(lmp~sitetreat_3,data=subset(df_mincome, period == 1 & (sitetreat_3 == 2 | sitetreat_3 == 3)), se_type = "stata")

# diff 1: community effect (change)
model_diff_2_com_change <- lm_robust(lmp~sitetreat_3 + period + sitetreat_3:period,data=subset(df_mincome, (sitetreat_3 == 2 | sitetreat_3 == 3)), se_type = "stata")

# Extract coefficients (b, se, p) ----

# Baseline
table_mb_0 <- tidy(model_mb_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==1)

table_da_0 <- tidy(model_da_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==1)

table_ru_0 <- tidy(model_ru_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==1)

table_diff_1_exp_0 <- tidy(model_diff_1_exp_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==2)

table_diff_2_com_0 <- tidy(model_diff_2_com_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==2)


# Study period
table_mb_1 <- tidy(model_mb_1) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==1)

table_da_1 <- tidy(model_da_1) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==1)

table_ru_1 <- tidy(model_ru_1) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==1)

table_diff_1_exp_1 <- tidy(model_diff_1_exp_1) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==2)

table_diff_2_com_1 <- tidy(model_diff_2_com_1) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==2)

# Change in LMP
table_mb_change <- tidy(model_mb_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==2)

table_da_change <- tidy(model_da_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==2)

table_ru_change <- tidy(model_ru_0) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==2)

table_diff_1_exp_change <- tidy(model_diff_1_exp_change) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==4)

table_diff_2_com_change <- tidy(model_diff_2_com_change) %>%
        select(term, estimate, std.error, p.value) %>%
        filter(row_number()==4)

rm(list=ls(pattern="model_"))

# Create column for MB Control ----

table_mb_0$term[1] <- "LMP baseline"
table_mb_0$order <- "b"
table_mb_1$term[1] <- "LMP study period"
table_mb_1$order <- "b"
table_mb_change$term[1] <- "Change in LMP"
table_mb_change$order <- "b"

table_mb_0_se <- table_mb_0 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_mb_1_se <- table_mb_1 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_mb_change_se <- table_mb_change %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

df_mb_control <- bind_rows(table_mb_0, table_mb_0_se, table_mb_1, table_mb_1_se, table_mb_change, table_mb_change_se)
df_mb_control$term <- factor(df_mb_control$term, levels = c("LMP baseline", "LMP study period", "Change in LMP"))

df_mb_control <- df_mb_control %>%
        arrange(term, order) %>%
        select(term, order, estimate, p.value) %>%
        mutate(across(where(is.numeric), round, 5)) %>%
        rename(estimate_mb = estimate,
               pvalue_mb = p.value)

df_mb_control

rm(list=ls(pattern="table_mb"))

# Create column for Dauphin ----

table_da_0$term[1] <- "LMP baseline"
table_da_0$order <- "b"
table_da_1$term[1] <- "LMP study period"
table_da_1$order <- "b"
table_da_change$term[1] <- "Change in LMP"
table_da_change$order <- "b"

table_da_0_se <- table_da_0 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_da_1_se <- table_da_1 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_da_change_se <- table_da_change %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

df_da_treatment <- bind_rows(table_da_0, table_da_0_se, table_da_1, table_da_1_se, table_da_change, table_da_change_se)
df_da_treatment$term <- factor(df_da_treatment$term, levels = c("LMP baseline", "LMP study period", "Change in LMP"))

df_da_treatment <- df_da_treatment %>%
        arrange(term, order) %>%
        select(term, order, estimate, p.value) %>%
        mutate(across(where(is.numeric), round, 5)) %>%
        rename(estimate_da = estimate,
               pvalue_da = p.value)

df_da_treatment

rm(list=ls(pattern="table_da"))

# Create column for MB Treatment (rural) ----

table_ru_0$term[1] <- "LMP baseline"
table_ru_0$order <- "b"
table_ru_1$term[1] <- "LMP study period"
table_ru_1$order <- "b"
table_ru_change$term[1] <- "Change in LMP"
table_ru_change$order <- "b"

table_ru_0_se <- table_ru_0 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_ru_1_se <- table_ru_1 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_ru_change_se <- table_ru_change %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

df_ru_treatment <- bind_rows(table_ru_0, table_ru_0_se, table_ru_1, table_ru_1_se, table_ru_change, table_ru_change_se)
df_ru_treatment$term <- factor(df_ru_treatment$term, levels = c("LMP baseline", "LMP study period", "Change in LMP"))

df_ru_treatment <- df_ru_treatment %>%
        arrange(term, order) %>%
        select(term, order, estimate, p.value) %>%
        mutate(across(where(is.numeric), round, 5)) %>%
        rename(estimate_ru = estimate,
               pvalue_ru = p.value)

rm(list=ls(pattern="table_ru"))

# Create column for Diff 1: experimental ----

table_diff_1_exp_0$term[1] <- "LMP baseline"
table_diff_1_exp_0$order <- "b"
table_diff_1_exp_1$term[1] <- "LMP study period"
table_diff_1_exp_1$order <- "b"
table_diff_1_exp_change$term[1] <- "Change in LMP"
table_diff_1_exp_change$order <- "b"

table_diff_1_exp_0_se <- table_diff_1_exp_0 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_diff_1_exp_1_se <- table_diff_1_exp_1 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_diff_1_exp_change_se <- table_diff_1_exp_change %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

df_diff_1_exp_control <- bind_rows(table_diff_1_exp_0, table_diff_1_exp_0_se, table_diff_1_exp_1, table_diff_1_exp_1_se, table_diff_1_exp_change, table_diff_1_exp_change_se)
df_diff_1_exp_control$term <- factor(df_diff_1_exp_control$term, levels = c("LMP baseline", "LMP study period", "Change in LMP"))

df_diff_1_exp_control <- df_diff_1_exp_control %>%
        arrange(term, order) %>%
        select(term, order, estimate, p.value) %>%
        mutate(across(where(is.numeric), round, 5)) %>%
        rename(estimate_diff_1_exp = estimate,
               pvalue_diff_1_exp = p.value)

df_diff_1_exp_control

rm(list=ls(pattern="table_diff_1_exp"))

# Create column for Diff 2: community ----

table_diff_2_com_0$term[1] <- "LMP baseline"
table_diff_2_com_0$order <- "b"
table_diff_2_com_1$term[1] <- "LMP study period"
table_diff_2_com_1$order <- "b"
table_diff_2_com_change$term[1] <- "Change in LMP"
table_diff_2_com_change$order <- "b"

table_diff_2_com_0_se <- table_diff_2_com_0 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_diff_2_com_1_se <- table_diff_2_com_1 %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

table_diff_2_com_change_se <- table_diff_2_com_change %>%
        select(term,std.error) %>%
        mutate(order = "se") %>%
        rename(estimate = std.error)

df_diff_2_com_control <- bind_rows(table_diff_2_com_0, table_diff_2_com_0_se, table_diff_2_com_1, table_diff_2_com_1_se, table_diff_2_com_change, table_diff_2_com_change_se)
df_diff_2_com_control$term <- factor(df_diff_2_com_control$term, levels = c("LMP baseline", "LMP study period", "Change in LMP"))

df_diff_2_com_control <- df_diff_2_com_control %>%
        arrange(term, order) %>%
        select(term, order, estimate, p.value) %>%
        mutate(across(where(is.numeric), round, 5)) %>%
        rename(estimate_diff_2_com = estimate,
               pvalue_diff_2_com = p.value)

rm(list=ls(pattern="table_diff_2_com"))

# Create results table ----

# please note: results differ slightly due to coding error in original paper
# see "error note" in 02_clean.R file

df_results <- merge(df_mb_control,df_da_treatment)
df_results <- merge(df_results,df_ru_treatment)
df_results <- merge(df_results,df_diff_1_exp_control)
df_results <- merge(df_results,df_diff_2_com_control)
df_results <- df_results %>%
        arrange(term,order)
df_results

df_results_2 <- df_results %>%
        arrange(term,order) %>%
        select(-order) %>%
        mutate(stars_mb = ifelse(pvalue_mb < .001, yes = "***",
                              ifelse(pvalue_mb >= .001 & pvalue_mb < .01, yes = "**",
                                     ifelse(pvalue_mb >= .01 & pvalue_mb < .05, yes = "*",
                                            ifelse(pvalue_mb >= .05 & pvalue_mb < .15, yes = "†",
                                                   no = "")))),
               ) %>%
        group_by(term) %>%
        mutate(MB_Control = ifelse(row_number()==1,
                                   yes = paste0(round(estimate_mb,3),stars_mb),
                                   no = paste0("(",round(estimate_mb,3),")")),
               ) %>%
        ungroup() %>%
        mutate(stars_da = ifelse(pvalue_da < .001, yes = "***",
                                 ifelse(pvalue_da >= .001 & pvalue_da < .01, yes = "**",
                                        ifelse(pvalue_da >= .01 & pvalue_da < .05, yes = "*",
                                               ifelse(pvalue_da >= .05 & pvalue_da < .15, yes = "†",
                                                      no = "")))),
        ) %>%
        group_by(term) %>%
        mutate(Dauphin = ifelse(row_number()==1,
                                   yes = paste0(round(estimate_da,3),stars_da),
                                   no = paste0("(",round(estimate_da,3),")")),
        ) %>%
        ungroup() %>%
        mutate(stars_ru = ifelse(pvalue_ru < .001, yes = "***",
                                 ifelse(pvalue_ru >= .001 & pvalue_ru < .01, yes = "**",
                                        ifelse(pvalue_ru >= .01 & pvalue_ru < .05, yes = "*",
                                               ifelse(pvalue_ru >= .05 & pvalue_ru < .15, yes = "†",
                                                      no = "")))),
        ) %>%
        group_by(term) %>%
        mutate(MB_Treatment = ifelse(row_number()==1,
                                   yes = paste0(round(estimate_ru,3),stars_ru),
                                   no = paste0("(",round(estimate_ru,3),")")),
        ) %>%
        ungroup() %>%
        mutate(stars_diff_1_exp = ifelse(pvalue_diff_1_exp < .001, yes = "***",
                                         ifelse(pvalue_diff_1_exp >= .001 & pvalue_diff_1_exp < .01, yes = "**",
                                                ifelse(pvalue_diff_1_exp >= .01 & pvalue_diff_1_exp < .05, yes = "*",
                                                       ifelse(pvalue_diff_1_exp >= .05 & pvalue_diff_1_exp < .15, yes = "†",
                                                              no = "")))),
        ) %>%
        group_by(term) %>%
        mutate(Diff_1_Exp_effect = ifelse(row_number()==1,
                                   yes = paste0(round(estimate_diff_1_exp,3),stars_diff_1_exp),
                                   no = paste0("(",round(estimate_diff_1_exp,3),")")),
        ) %>%
        ungroup() %>%
        mutate(stars_diff_2_com = ifelse(pvalue_diff_2_com < .001, yes = "***",
                                         ifelse(pvalue_diff_2_com >= .001 & pvalue_diff_2_com < .01, yes = "**",
                                                ifelse(pvalue_diff_2_com >= .01 & pvalue_diff_2_com < .05, yes = "*", 
                                                       ifelse(pvalue_diff_2_com >= .05 & pvalue_diff_2_com < .15, yes = "†", 
                                                              no = "")))),
        ) %>%
        group_by(term) %>%
        mutate(Diff_2_Com_effect = ifelse(row_number()==1, 
                                   yes = paste0(round(estimate_diff_2_com,3),stars_diff_2_com),
                                   no = paste0("(",round(estimate_diff_2_com,3),")")),
        ) %>%
        ungroup() %>%
        select(term, MB_Control, Dauphin, MB_Treatment, Diff_1_Exp_effect, Diff_2_Com_effect) 

df_results_2$term <- c("LMP baseline", "", "LMP study period", "", "Change in LMP", "")
colnames(df_results_2) <- c("", "MB Control", "Dauphin", "MB Treatment", "DiD 1", "DiD 2")

cairo_pdf(paste0(tables,"table_3_diff_in_diff.pdf"), family="DejaVu Sans",height=11, width=8.5)
grid.table(df_results_2, rows = NULL)
dev.off()

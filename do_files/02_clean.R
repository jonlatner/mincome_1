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
library(readxl) # read_excel
library(janitor) # clean_names
library(data.table) # melt
library(zoo) # na.locf
library(car) # recode

# FOLDERS - ADAPT THIS PATHWAY
setwd("/Users/jonathanlatner/Documents/GitHub/mincome_calnitsky_latner_2017/")
data_files = "data_files/"

# Load data ----

df_minc1 <- read_excel(paste0(data_files,"minc1.xlsx"), skip = 1)
df_minc2 <- read_excel(paste0(data_files,"minc2.xlsx"), skip = 1)

# Merge data ----

df_minc1 <- clean_names(df_minc1) # janitor package
df_minc2 <- clean_names(df_minc2) # janitor package

# Merge data ----

df_minc1 <- clean_names(df_minc1) # janitor package
df_minc2 <- clean_names(df_minc2) # janitor package

names(df_minc2) <- c("famnum","h2","h3","h4","h5","h6","h7","h8","h9","h10","h11","h12","h13","h14",
                     "famsize_tvc1","num_adults_tvc1","num_kids_tvc1","fsi1","gai1","ac1","wages_tvc1","wealth_tax_amount1","nrr_inc1","hundred_inc1","lm_carry1","cur_carry1","payments1",
                     "famsize_tvc2","num_adults_tvc2","num_kids_tvc2","fsi2","gai2","ac2","wages_tvc2","wealth_tax_amount2","nrr_inc2","hundred_inc2","lm_carry2","cur_carry2","payments2",
                     "famsize_tvc3","num_adults_tvc3","num_kids_tvc3","fsi3","gai3","ac3","wages_tvc3","wealth_tax_amount3","nrr_inc3","hundred_inc3","lm_carry3","cur_carry3","payments3",
                     "famsize_tvc4","num_adults_tvc4","num_kids_tvc4","fsi4","gai4","ac4","wages_tvc4","wealth_tax_amount4","nrr_inc4","hundred_inc4","lm_carry4","cur_carry4","payments4",
                     "famsize_tvc5","num_adults_tvc5","num_kids_tvc5","fsi5","gai5","ac5","wages_tvc5","wealth_tax_amount5","nrr_inc5","hundred_inc5","lm_carry5","cur_carry5","payments5",
                     "famsize_tvc6","num_adults_tvc6","num_kids_tvc6","fsi6","gai6","ac6","wages_tvc6","wealth_tax_amount6","nrr_inc6","hundred_inc6","lm_carry6","cur_carry6","payments6",
                     "famsize_tvc7","num_adults_tvc7","num_kids_tvc7","fsi7","gai7","ac7","wages_tvc7","wealth_tax_amount7","nrr_inc7","hundred_inc7","lm_carry7","cur_carry7","payments7",
                     "famsize_tvc8","num_adults_tvc8","num_kids_tvc8","fsi8","gai8","ac8","wages_tvc8","wealth_tax_amount8","nrr_inc8","hundred_inc8","lm_carry8","cur_carry8","payments8",
                     "famsize_tvc9","num_adults_tvc9","num_kids_tvc9","fsi9","gai9","ac9","wages_tvc9","wealth_tax_amount9","nrr_inc9","hundred_inc9","lm_carry9","cur_carry9","payments9",
                     "famsize_tvc10","num_adults_tvc10","num_kids_tvc10","fsi10","gai10","ac10","wages_tvc10","wealth_tax_amount10","nrr_inc10","hundred_inc10","lm_carry10","cur_carry10","payments10",
                     "famsize_tvc11","num_adults_tvc11","num_kids_tvc11","fsi11","gai11","ac11","wages_tvc11","wealth_tax_amount11","nrr_inc11","hundred_inc11","lm_carry11","cur_carry11","payments11",
                     "famsize_tvc12","num_adults_tvc12","num_kids_tvc12","fsi12","gai12","ac12","wages_tvc12","wealth_tax_amount12","nrr_inc12","hundred_inc12","lm_carry12","cur_carry12","payments12",
                     "famsize_tvc13","num_adults_tvc13","num_kids_tvc13","fsi13","gai13","ac13","wages_tvc13","wealth_tax_amount13","nrr_inc13","hundred_inc13","lm_carry13","cur_carry13","payments13",
                     "famsize_tvc14","num_adults_tvc14","num_kids_tvc14","fsi14","gai14","ac14","wages_tvc14","wealth_tax_amount14","nrr_inc14","hundred_inc14","lm_carry14","cur_carry14","payments14",
                     "famsize_tvc15","num_adults_tvc15","num_kids_tvc15","fsi15","gai15","ac15","wages_tvc15","wealth_tax_amount15","nrr_inc15","hundred_inc15","lm_carry15","cur_carry15","payments15",
                     "famsize_tvc16","num_adults_tvc16","num_kids_tvc16","fsi16","gai16","ac16","wages_tvc16","wealth_tax_amount16","nrr_inc16","hundred_inc16","lm_carry16","cur_carry16","payments16",
                     "famsize_tvc17","num_adults_tvc17","num_kids_tvc17","fsi17","gai17","ac17","wages_tvc17","wealth_tax_amount17","nrr_inc17","hundred_inc17","lm_carry17","cur_carry17","payments17",
                     "famsize_tvc18","num_adults_tvc18","num_kids_tvc18","fsi18","gai18","ac18","wages_tvc18","wealth_tax_amount18","nrr_inc18","hundred_inc18","lm_carry18","cur_carry18","payments18",
                     "famsize_tvc19","num_adults_tvc19","num_kids_tvc19","fsi19","gai19","ac19","wages_tvc19","wealth_tax_amount19","nrr_inc19","hundred_inc19","lm_carry19","cur_carry19","payments19",
                     "famsize_tvc20","num_adults_tvc20","num_kids_tvc20","fsi20","gai20","ac20","wages_tvc20","wealth_tax_amount20","nrr_inc20","hundred_inc20","lm_carry20","cur_carry20","payments20",
                     "famsize_tvc21","num_adults_tvc21","num_kids_tvc21","fsi21","gai21","ac21","wages_tvc21","wealth_tax_amount21","nrr_inc21","hundred_inc21","lm_carry21","cur_carry21","payments21",
                     "famsize_tvc22","num_adults_tvc22","num_kids_tvc22","fsi22","gai22","ac22","wages_tvc22","wealth_tax_amount22","nrr_inc22","hundred_inc22","lm_carry22","cur_carry22","payments22",
                     "famsize_tvc23","num_adults_tvc23","num_kids_tvc23","fsi23","gai23","ac23","wages_tvc23","wealth_tax_amount23","nrr_inc23","hundred_inc23","lm_carry23","cur_carry23","payments23",
                     "famsize_tvc24","num_adults_tvc24","num_kids_tvc24","fsi24","gai24","ac24","wages_tvc24","wealth_tax_amount24","nrr_inc24","hundred_inc24","lm_carry24","cur_carry24","payments24",
                     "famsize_tvc25","num_adults_tvc25","num_kids_tvc25","fsi25","gai25","ac25","wages_tvc25","wealth_tax_amount25","nrr_inc25","hundred_inc25","lm_carry25","cur_carry25","payments25",
                     "famsize_tvc26","num_adults_tvc26","num_kids_tvc26","fsi26","gai26","ac26","wages_tvc26","wealth_tax_amount26","nrr_inc26","hundred_inc26","lm_carry26","cur_carry26","payments26",
                     "famsize_tvc27","num_adults_tvc27","num_kids_tvc27","fsi27","gai27","ac27","wages_tvc27","wealth_tax_amount27","nrr_inc27","hundred_inc27","lm_carry27","cur_carry27","payments27",
                     "famsize_tvc28","num_adults_tvc28","num_kids_tvc28","fsi28","gai28","ac28","wages_tvc28","wealth_tax_amount28","nrr_inc28","hundred_inc28","lm_carry28","cur_carry28","payments28",
                     "famsize_tvc29","num_adults_tvc29","num_kids_tvc29","fsi29","gai29","ac29","wages_tvc29","wealth_tax_amount29","nrr_inc29","hundred_inc29","lm_carry29","cur_carry29","payments29",
                     "famsize_tvc30","num_adults_tvc30","num_kids_tvc30","fsi30","gai30","ac30","wages_tvc30","wealth_tax_amount30","nrr_inc30","hundred_inc30","lm_carry30","cur_carry30","payments30",
                     "famsize_tvc31","num_adults_tvc31","num_kids_tvc31","fsi31","gai31","ac31","wages_tvc31","wealth_tax_amount31","nrr_inc31","hundred_inc31","lm_carry31","cur_carry31","payments31",
                     "famsize_tvc32","num_adults_tvc32","num_kids_tvc32","fsi32","gai32","ac32","wages_tvc32","wealth_tax_amount32","nrr_inc32","hundred_inc32","lm_carry32","cur_carry32","payments32",
                     "famsize_tvc33","num_adults_tvc33","num_kids_tvc33","fsi33","gai33","ac33","wages_tvc33","wealth_tax_amount33","nrr_inc33","hundred_inc33","lm_carry33","cur_carry33","payments33",
                     "famsize_tvc34","num_adults_tvc34","num_kids_tvc34","fsi34","gai34","ac34","wages_tvc34","wealth_tax_amount34","nrr_inc34","hundred_inc34","lm_carry34","cur_carry34","payments34",
                     "famsize_tvc35","num_adults_tvc35","num_kids_tvc35","fsi35","gai35","ac35","wages_tvc35","wealth_tax_amount35","nrr_inc35","hundred_inc35","lm_carry35","cur_carry35","payments35",
                     "famsize_tvc36","num_adults_tvc36","num_kids_tvc36","fsi36","gai36","ac36","wages_tvc36","wealth_tax_amount36","nrr_inc36","hundred_inc36","lm_carry36","cur_carry36","payments36",
                     "famsize_tvc37","num_adults_tvc37","num_kids_tvc37","fsi37","gai37","ac37","wages_tvc37","wealth_tax_amount37","nrr_inc37","hundred_inc37","lm_carry37","cur_carry37","payments37")

# site_code 
# 0 = Rural; 1 Winnipeg; 2 Dauphin
df_minc1 <- df_minc1 %>%
        filter(site_code < 3) # delete tabulations at the bottom of excel file

# merge
df_merge <- merge(df_minc1,df_minc2, by = c("famnum"), all = TRUE)
rm(df_minc1,df_minc2)

# reshape
times <- gsub("ac", "", grep("ac", names(df_merge), value = TRUE))
df_long <- melt(as.data.table(df_merge), 
                measure.vars = patterns("famsize_tvc","num_adults_tvc","num_kids_tvc","fsi","gai","ac","wages_tvc","wealth_tax_amount","nrr_inc","hundred_inc","lm_carry","cur_carry","payments"),
                value.name = c("famsize_tvc","num_adults_tvc","num_kids_tvc","fsi","gai","ac","wages_tvc","wealth_tax_amount","nrr_inc","hundred_inc","lm_carry","cur_carry","payments"))[
                        , variable := factor(variable, labels = times)][]

df_long$variable <- as.numeric(as.character(df_long$variable))
df_long <- df_long %>%
        rename(month=variable) %>%
        arrange(famnum,month) %>%
        select(famnum,month,everything())

# rename
df_mincome <- df_long %>%
        rename(famno = famnum,
               wpg_dummy = h2, # Winnipeg dummy
               dau_dummy = h3, # Dauphin dummy
               nrr = h8, # normal reduction rate (NRR)
               enrollment_date = h10,
               first_irf = h11, # First IRF period
               last_irf = h12 # Last IRF period
        ) %>%
        select(famno, month, 
               famsize, hhead, shead, agem, agef, numchild, # family vars at baseline
               fh_high, mh_high, # education (high school dummy)
               first_irf, last_irf,
               uic73, uic74, # unemployment insurance
               sa73, sa74, # social assistance (i.e. welfare)
               mhweeks73, mhweek74, fhweeks73, fhweek74, # weeks worked
               fhnotlook, mhnotlook, # why not looking for work
               wpg_dummy, dau_dummy, site_code, treat, # site treatment
               famsize_tvc, enrollment_date, # participation
               wages_tvc, matches("tot"), # earnings
               ac, payments # time varying vars
               ) %>%
        arrange(famno, month) %>%
        rename(twohead_dummy = hhead,
               onehead_dummy = shead) %>%
        select(famno, month, everything())

# Clean data ----

# destring variables
df_mincome <- data.frame(lapply(df_mincome, function(x) as.numeric(as.character(x))))

# change all values below 0 (-9, -7, -1) to missing
df_mincome[df_mincome < 0] <- NA

# Unique count
df_mincome <- df_mincome %>%
        group_by(famno) %>%
        mutate(count = row_number(),
               unique = ifelse(row_number()==n(), yes = 1, no = NA), # identify unique families at end of study period
        ) %>% 
        ungroup()

# Combine site and treatment ----

# The assignment cell (ac) is assigned to the unit at enrollment, 
# The first digit is the treatment plan assigned to the household.
# The last 2 digits are the normal income cell of the household which 
# determined the probability of being assigned to each of the 9 plans.
# (set User Manual for more information)

df_mincome <- df_mincome %>%
        group_by(famno) %>%
        mutate(ac = na.locf(ac, na.rm = FALSE), # replace missing value with previous non-missing value
               ac = last(ac), # replace all cases with the last value
        ) %>% 
        ungroup()

df_mincome$treat_recode <- recode(df_mincome$treat,"1:8=1; 9=0")
df_mincome$ac_recode <- recode(df_mincome$ac,"lo:899=1; 900:hi=0")

# Replace treatment and ac with 1 if case is in dauphin because if they are in dauphin, they are treated
# There are about 25% of cases with missing site and treat info
# basically it appears as if these people entered the data set after the baseline period
# Replace treatment with ac if treatment is missing, but ac is not

df_mincome <- df_mincome %>%
        mutate(treat_recode = ifelse(dau_dummy == 1 & is.na(treat_recode), yes = 1, no = treat_recode),
               ac_recode = ifelse(dau_dummy == 1 & is.na(treat_recode), yes = 1, no = ac_recode),
               site_code = ifelse(wpg_dummy == 1 & is.na(site_code), yes = 1, no = site_code),
               site_code = ifelse(dau_dummy == 1 & is.na(site_code), yes = 2, no = site_code),
               site_code = ifelse(wpg_dummy == 0 & dau_dummy == 0 & is.na(site_code), yes = 0, no = site_code),
               treat_recode = ifelse(is.na(treat_recode) & !is.na(ac_recode), yes = ac_recode, no = treat_recode),
        )

# if you ever received a payment, you are treated.
df_mincome <- df_mincome %>%
        mutate(test = ifelse(payments > 0 & !is.na(payments), yes = 1, no = NA)) %>%
        group_by(famno) %>%
        mutate(test = na.locf(test, na.rm = FALSE), # replace missing value with previous non-missing value
               test = last(test), # replace all cases with the last value
               test = ifelse(is.na(test), yes = 0, no = test),
               ) %>%
        ungroup() %>%
        mutate(treat_recode = ifelse(test == 1, yes = 1, no = treat_recode)) 

# Site treat
# sitetreat_1 1 "Winnipeg treat"
# sitetreat_1 2 "Winnipeg contr"
# sitetreat_1 3 "Dauphin"
# sitetreat_1 4 "Rural treat"
# sitetreat_1 5 "Rural contr"
df_mincome <- df_mincome %>%
        mutate(sitetreat_1 = ifelse(site_code == 1 & treat_recode == 1, yes = 1,
                                  ifelse(site_code == 1 & treat_recode == 0, yes = 2,
                                         ifelse(site_code == 2, yes = 3,
                                                ifelse(site_code == 0 & treat_recode == 1, yes = 4,
                                                       ifelse(site_code == 0 & treat_recode == 0, yes = 5, no = 0))))))

# sitetreat_2 0 "MB Control"
# sitetreat_2 1 "Dauphin", add
df_mincome <- df_mincome %>%
        mutate(sitetreat_2 = ifelse(sitetreat_1 == 3, yes = 1,
                                  ifelse(sitetreat_1 == 2 | sitetreat_1 == 5, yes = 0, no = NA)))

# sitetreat_3 1 "MB control"
# sitetreat_3 2 "Dauphin"
# sitetreat_3 3 "Rural treatment"

df_mincome <- df_mincome %>%
        mutate(sitetreat_3 = ifelse(sitetreat_2==0, yes = 1,
                                    ifelse(sitetreat_2==1, yes = 2, no = 0))) %>%
        mutate(sitetreat_3 = ifelse(sitetreat_1==4, yes = 3, no = sitetreat_3))

# Clean variables ----

# Family (baseline)
df_mincome <- df_mincome %>%
        mutate(kids = ifelse(numchild>0, yes = 1, no = 0))

# Unemployment
df_mincome$unemployed_1973 <- recode(df_mincome$uic73,"1:hi=1")
df_mincome$unemployed_1974 <- recode(df_mincome$uic74,"1:hi=1")

# Received unemployment in 1973 & 1974
df_mincome <- df_mincome %>% 
        mutate(unemployed_both = ifelse(unemployed_1973==1 & unemployed_1974==1, yes = 1, no = 0),
               unemployed_or = ifelse(unemployed_1973==1 | unemployed_1974==1, yes = 1, no = 0),
        )

# Welfare
df_mincome$welfare_1973 <- recode(df_mincome$sa73,"1:hi=1")
df_mincome$welfare_1974 <- recode(df_mincome$sa74,"1:hi=1")

# Received welfare in 1973 & 1974
df_mincome <- df_mincome %>% 
        mutate(welfare_both = ifelse(welfare_1973==1 & welfare_1974==1, yes = 1, no = 0),
               welfare_or = ifelse(welfare_1973==1 | welfare_1974==1, yes = 1, no = 0),
        )

# Baseline labor force participation
df_mincome$inlaborf73_m <- recode(df_mincome$mhweeks73,"1:hi=1")
df_mincome$inlaborf74_m <- recode(df_mincome$mhweek74,"1:hi=1")

df_mincome$inlaborf73_f <- recode(df_mincome$fhweeks73,"1:hi=1")
df_mincome$inlaborf74_f <- recode(df_mincome$fhweek74,"1:hi=1")

# Participation

df_mincome <- df_mincome %>%
        filter(!is.na(enrollment_date)) %>%
        mutate(participation_rate = ifelse(!is.na(famsize_tvc), yes = 1, no = NA),
               participation_cum = ifelse(!is.na(famsize_tvc), yes = 1, no = NA),
               benefit = ifelse(payments > 0, yes = 1, no = NA),
               walkin = ifelse(first_irf>1, yes = 1, no = 0),
               dropout = ifelse(last_irf<37, yes = 1, no = 0),
               walkin_date = ifelse(first_irf!=count, yes = NA, no = walkin),
               dropout_date = ifelse(last_irf!=count, yes = NA, no = dropout),
        ) %>%
        group_by(famno) %>%
        mutate(participation_cum = na.locf(participation_cum, na.rm = FALSE), # replace missing value with previous non-missing value
               benefit = na.locf(benefit, na.rm = FALSE), # replace missing value with previous non-missing value
               dropout_date = lag(dropout_date, 1),
        ) %>%
        ungroup()


# Earnings data ----
# Error note: Small coding error
# means that 1 family is now included, but was not in the original paper
# famno == 35723

# Baseline
df_mincome <- df_mincome %>%
        mutate(earnings_74 = rowSums(select(., .dots = c("totnhinc74", "mhtotern74", "fhtotern74")), na.rm = TRUE),
               earnings_73 = rowSums(select(., .dots = c("totnhinc73", "mhtotern73", "fhtotern73")), na.rm = TRUE),
        ) %>%
        mutate(earnings_74 = ifelse(earnings_74==0, yes = NA, no = earnings_74),
               earnings_74 = ifelse(is.na(earnings_74) & (mhtotern74 == 0 | fhtotern74 == 0 | fhtotern74 == 0), yes = 0, no = earnings_74),
               earnings_74 = earnings_74/12,
               earnings_73 = ifelse(earnings_73==0, yes = NA, no = earnings_73),
               # earnings_73 = ifelse(is.na(earnings_73) & (mhtotern74 == 0 | fhtotern74 == 0 | totnhinc74 == 0), yes = 0, no = earnings_73), # this original code is an error
               earnings_73 = ifelse(is.na(earnings_73) & (mhtotern73 == 0 | fhtotern73 == 0 | totnhinc73 == 0), yes = 0, no = earnings_73),
               earnings_73 = earnings_73/12,
        ) %>%
        mutate(month = month + 23)

select(df_mincome,famno,month,earnings_74,earnings_73,mhtotern74,fhtotern74,totnhinc74) %>% filter(famno == 35723)

# Expand
df_baseline <- df_mincome %>%
        group_by(famno) %>%
        filter(row_number()==1) %>%
        ungroup() %>%
        mutate(month = 1)
df_months <- data.frame()
months <- seq(2,23,1)
for (m in months) {
        df_test <- df_baseline %>%
                mutate(month=m)
        df_months <- rbind(df_months,df_test)
}                

# Append earnings data
df_mincome <- rbind(df_mincome,df_baseline,df_months)
rm(df_baseline,df_months,df_test,m,months)

# Date

df_mincome$year <- recode(df_mincome$month, "1:12=1973;13:24=1974;25:36=1975;37:48=1976;49:60=1977")
df_mincome$halfyear <- recode(df_mincome$month, "1:6=1;7:12=2;13:18=3;19:24=4;25:30=5;31:36=6;37:42=7;43:48=8;49:54=9;55:60=10")

# Labor market participation
df_mincome <- df_mincome %>%
        arrange(famno,month) %>%
        mutate(wages = ifelse(month<13, yes = earnings_73, no = 0),
               wages = ifelse(month>12&month<24, yes = earnings_74, no = wages),
               wages = ifelse(month>23, yes = wages_tvc, no = wages)) %>% # First month is December, 1974
        mutate(lmp = ifelse(wages>0, yes = 1, no = 0),
               lmp_0 = ifelse(lmp==0, yes = 1, no = 0),
               lmp_1 = ifelse(lmp==1, yes = 1, no = 0))

# View(select(df_mincome, famno, year, month, wages, wages_tvc, earnings_73, earnings_74))

# Study period
df_mincome <- df_mincome %>%
        mutate(period = ifelse(month<24, yes = 0, 
                               ifelse(month>29, yes = 1, no = NA)))

# Turning into balanced panel ----
df_mincome_balanced <- df_mincome %>%
        filter(first_irf <= 6 & last_irf==37)

# Modify the walkin variable to indicate "ever walked in" as opposed to identifying the period at which a person walked in
df_mincome_balanced <- df_mincome_balanced %>%
        mutate(walkin_ever = ifelse(walkin == 1, yes = 1, no = NA)) %>%
        group_by(famno) %>%
        mutate(walkin_ever = na.locf(walkin_ever, na.rm = FALSE), # replace missing value with previous non-missing value
               walkin_ever = last(walkin_ever), # replace all cases with the last value
        ) %>%
        ungroup()

# You are not in the sample if you are a walkin participant and have missing baseline information
df_mincome_balanced <- df_mincome_balanced %>%
        mutate(missing = ifelse(unique == 1 & walkin_ever == 1 & is.na(earnings_74) & is.na(earnings_73), yes = 1, no = 0),
        ) %>%
        group_by(famno) %>%
        mutate(missing = last(missing), # replace all cases with the last value
        ) %>%
        ungroup() %>%
        filter(missing==0|is.na(missing)) %>%
        select(-missing)

# You are not in the sample if you are a panel participant and have missing baseline information
# There is an error in the original code
df_mincome_balanced <- df_mincome_balanced %>%
        mutate(missing = ifelse(first_irf == 1 & last_irf == 37 & (is.na(earnings_74) | is.na(earnings_73)), yes = 1, no = 0),
        ) %>%
        group_by(famno) %>%
        mutate(missing = last(missing), # replace all cases with the last value
        ) %>%
        ungroup() %>%
        filter(missing==0|is.na(missing)) %>%
        select(-missing)



# Save data ----

saveRDS(df_mincome, file = paste0(data_files, "mincome.rds"))

saveRDS(df_mincome_balanced, file = paste0(data_files, "mincome_balanced.rds"))

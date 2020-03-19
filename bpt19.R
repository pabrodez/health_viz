####################################################################
# Script to read BPT, format and wrangle BPT spreadsheet from 2019 
# Used to create bpt_19_dash.Rmd and bpt19_compliance.Rmd
# it is previously saved in ./bpt19.xls
####################################################################

#### libraries ####
library(tidyverse)
library(readxl)
library(lubridate)
library(glue)

#### read data ####
bpt_df <- read_xls("./flexdashboard/bpt19.xls", sheet = 1, skip = 13, col_types = "text") 

#### tidy up ####
bpt_df <- keep(bpt_df, function(x) sum(is.na(x)) != nrow(bpt_df))
bpt_df <- bpt_df[rowSums((is.na(bpt_df))) != ncol(bpt_df) ,]

names(bpt_df) <- c("site", "sub_id", "nhs_n", "date_ob", "arrival_date", "outcome_date", 
   "iss", "bpt_actual", "dispatch_days", "rehab", "referral", "gcs_intub",
    "consultant", "ct", "txa", "frailty", "ccg", "approval_date")

bpt_df <- modify_if(bpt_df, is.character, ~ tolower(.x))

bpt_df <- modify_at(bpt_df, grep("date", names(bpt_df)), ~ as.Date(as.numeric(.x), origin = "1899-12-30"))

bpt_df$nhs_n <- with(bpt_df, match(nhs_n, unique(nhs_n)))

bpt_df$dispatch_days <- as.numeric(bpt_df$dispatch_days)

#### transform ####
bpt_df$iss <- as.numeric(bpt_df$iss)

bpt_df <- mutate(bpt_df,
  bpt_pot = case_when(
    iss >= 9 & iss <= 15 ~ "level 1",
    iss > 15 ~ "level 2",
    TRUE ~ as.character(iss)
  ),
  tariff_actual = case_when(
    bpt_actual == "level 1" ~ 1599.69,
    bpt_actual == "level 2" ~ 3075.1,
    TRUE ~ 0
  ),
  tariff_pot = case_when(
    bpt_pot == "level 1" ~ 1599.69,
    bpt_pot == "level 2" ~ 3075.1,
    TRUE ~ 0
  ),
  tariff_diff = tariff_pot - tariff_actual,
  tariff_loss = if_else(bpt_actual != bpt_pot, "yes", "no"),
  month_dis = lubridate::floor_date(outcome_date, "month"),
  month_arr = lubridate::floor_date(arrival_date, "month")
)

age <- function(from, to) {
  from_lt <- as.POSIXlt(from)
  to_lt <- as.POSIXlt(to)
  
  age <- to_lt$year - from_lt$year
  
  ifelse(to_lt$mon < from_lt$mon |
           (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age
  )
}

bpt_df$age <- with(bpt_df, age(date_ob, arrival_date))

bpt_df$los <- with(bpt_df, as.numeric(outcome_date - arrival_date))

bpt_df$iss <- with(bpt_df, factor(iss, levels = as.character(sort(unique(iss)))))

# bpt_df$frailty <- sub(",\\s.*", "", bpt_df$frailty)

## add dummy data ##
# dummy_outcome <- seq.Date(min(bpt_df$outcome_date), as.Date("2020-03-31"), "day")[1:200]
# dummy_arrival <- seq.Date(min(bpt_df$arrival_date), as.Date("2020-03-31"), "day")[1:200]
# bpt_df <- map_dfc(bpt_df, ~ sample(.x, 200, replace = TRUE)) %>%
#   mutate(outcome_date = dummy_arrival, 
#          arrival_date = dummy_outcome, 
#          month_dis = lubridate::floor_date(outcome_date, "month")) %>%
#   rbind(bpt_df)

## Create subset of patients with criteria affecting the tariff, 
## then replace values that indicate non compliance with new ones that make it more explicit.

## The suffix `level1` means it belongs to level 1 criteria; `level2` refers to level 2 criteria
bpt_loss <- filter(bpt_df, tariff_loss == "yes") %>%
  select(tariff_diff, outcome_date, month_dis, month_arr, bpt_actual, bpt_pot, iss, dispatch_days:frailty) %>%
  transmute(
    iss,
    tariff_diff,
    outcome_date,
    month_dis,
    month_arr,
    bpt_actual,
    dispatch_days = replace(dispatch_days, dispatch_days > 25, "level1_dispatch- late"),
    rehab = case_when(rehab == "no to detail questions" ~ "level1_rehab pres- no to detail questions",
                      rehab == "not rec to need" ~ "level1_rehab pres- not rec to need",
                      TRUE ~ rehab),
    transfer = replace(referral, referral == "trans after 48h", "level1_transfer- trans after 48h"),
    gcs_intub = case_when(
      gcs_intub == "missing intub time & gcs not in 30" ~ "level1_gcs- missing intub time & gcs not in 30",
      gcs_intub == "missing gcs" ~ "level1_gcs- missing gcs",
      gcs_intub == "not intub, not considered" ~ "level1_gcs- not intub, not considered",
      gcs_intub == "intub > 30 and not considered in 30" ~ "level1_gcs- intub > 30 and not considered in 30",
    TRUE ~ gcs_intub
    ),
    consultant = case_when(
      bpt_pot == "level 2" & consultant == "direct, no cons in 5" ~ "level2_consultant- after 5mins",
      bpt_pot == "level 2" & consultant == "no consultant recorded" ~ "level2_consultant- not recorded",
      bpt_pot == "level 2" & consultant == "recorded with incomplete times" ~ "level2_consultant- recorded with incomplete times",
      bpt_pot == "level 2" & consultant == "trans in 12h, no cons in 5" ~ "level2_consultant- trans in 12h, no cons in 5",
      TRUE ~ consultant
    ),
    ct = case_when(
       bpt_pot == "level 2" & ct == "ct > 60" ~ "level2_ct- >60",
       bpt_pot == "level 2" & ct == "no ct recorded" ~ "level2_ct- not recorded",
      TRUE ~ ct
    ),
    txa = case_when(
       bpt_pot == "level 2" & txa == "no txa recorded" ~ "level2_txa- not recorded",
       bpt_pot == "level 2" & txa == "incomplete times" ~ "level2_txa- incomplete times",
       bpt_pot == "level 2" & txa == "txa > 60" ~ "level2_txa- >60",
      TRUE ~ txa
    ),
    frailty = case_when(
       bpt_pot == "level 2" & frailty == "not rec" ~ "level2_frailty- not rec",
       bpt_pot == "level 2" & frailty == "late, geriatric medicine: consultant" ~ "level2_frailty- late",
       bpt_pot == "level 2" & frailty == "late, geriatric medicine: fy/st1-2/trust grade" ~ "level2_frailty- late fy/st1-2/trust grade",
       bpt_pot == "level 2" & frailty == "in time, geriatric medicine: advanced practitioner" ~ "level2_frailty- in time, geriatric medicine: advanced practitioner",
       bpt_pot == "level 2" & frailty == "in time, emergency medicine: consultant" ~ "level2_frailty- in time, emergency medicine: consultant",
       bpt_pot == "level 2" & frailty == "in time, emergency medicine: staff grade/speciality" ~ "level2_frailty- in time, emergency medicine: staff grade/speciality",
      TRUE ~ frailty
    )
  )

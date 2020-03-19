#################################################################
# Script to read and transform Performance Review Indicators data
# from 2014 to 2018 financial years
################################################################

library(tidyverse)
library(readxl)

perf1418_df <- read_xls("./perf_ind/perf_ind_2014_2018.xls", skip = 6, col_types = "text", 
                    .name_repair = function(x) {
                      a <- tolower(gsub(" ","_", x));
                      gsub("_\\(.*\\)", "", a)
                    }
)

#### tidy and transform ####
perf1418_df <- perf1418_df[setdiff(names(perf1418_df), c("site", "prf_number"))]
# perf1418_df$submissionid <- seq_along(perf1418_df$submissionid)
perf1418_df$nhs_number <- match(perf1418_df$nhs_number, unique(perf1418_df$nhs_number))

perf1418_df <- modify_at(perf1418_df, grep("date", names(perf1418_df)), ~ as.Date(as.numeric(.x), origin = "1899-12-30"))

perf1418_df$age <- as.integer(floor(as.double(perf1418_df$age)))

perf1418_df$los <- as.integer(perf1418_df$los)

perf1418_df$icu_los <- as.integer(perf1418_df$icu_los)

perf1418_df$number_of_operations <- as.integer(perf1418_df$number_of_operations)

perf1418_df$discharge_month <- strftime(perf1418_df$discharge_date, "%B")
perf1418_df$discharge_month <- factor(perf1418_df$discharge_month, levels = strftime(seq.Date(as.Date("2019-04-01"), as.Date("2020-03-30"), "month"), "%B"))

perf1418_df$iss_band <- gsub(" ", "", perf1418_df$iss_band)
perf1418_df$iss_band <- factor(perf1418_df$iss_band, levels = c(">15", "9-15", "1-8"))

perf1418_df[["fin_year"]] <- with(
  perf1418_df,
  case_when(
    between(discharge_date, as.Date("2018-04-01"), as.Date("2019-03-31")) ~ "2018",
    between(discharge_date, as.Date("2017-04-01"), as.Date("2018-03-31")) ~ "2017",
    between(discharge_date, as.Date("2016-04-01"), as.Date("2017-03-31")) ~ "2016",
    between(discharge_date, as.Date("2015-04-01"), as.Date("2016-03-31")) ~ "2015",
    between(discharge_date, as.Date("2014-04-01"), as.Date("2015-03-31")) ~ "2014"
  )
)

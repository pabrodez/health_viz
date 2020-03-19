##############################################################################
# This script reads and transforms data from the 
# Performance Review Indicators .xls document (for the 2019 financial year)
# .xls file is saved as ./perf_ind/perf19.xls
##############################################################################

#### libraries ####
library(tidyverse)
library(readxl)

#### read #####
perf_df <- read_xls("./perf_ind/perf19.xls", skip = 6, col_types = "text", 
                    .name_repair = function(x) {
                      a <- tolower(gsub(" ","_", x));
                      gsub("_\\(.*\\)", "", a)
                      }
                    )

#### tidy and transform ####
perf_df <- perf_df[setdiff(names(perf_df), c("site", "prf_number"))]
# perf_df$submissionid <- seq_along(perf_df$submissionid)
perf_df$nhs_number <- match(perf_df$nhs_number, unique(perf_df$nhs_number))

perf_df <- modify_at(perf_df, grep("date", names(perf_df)), ~ as.Date(as.numeric(.x), origin = "1899-12-30"))

perf_df$age <- as.integer(floor(as.double(perf_df$age)))

perf_df$los <- as.integer(perf_df$los)

perf_df$icu_los <- as.integer(perf_df$icu_los)

perf_df$number_of_operations <- as.integer(perf_df$number_of_operations)

perf_df$discharge_month <- strftime(perf_df$discharge_date, "%B")
perf_df$discharge_month <- factor(perf_df$discharge_month, levels = strftime(seq.Date(as.Date("2019-04-01"), as.Date("2020-03-30"), "month"), "%B"))

perf_df$iss_band <- gsub(" ", "", perf_df$iss_band)
perf_df$iss_band <- factor(perf_df$iss_band, levels = c(">15", "9-15", "1-8"))

# get financial year
get_fin_year <-
  Vectorize(function(date) {
    year <- strftime(date, "%Y")
    m <- strftime(date, "%b")
    output <- vector("character", 1)
    
    if (m %in% month.abb[4:12]) {
      output <- year
      
    } else if (m %in% month.abb[1:3]) {
      output <- as.character(as.numeric(year) - 1)
    }
    return(output)
  })

perf_df[["fin_year"]] <- with(
  perf_df,
  get_fin_year(discharge_date)
)

#### this data.frame will be used in ./perf_ind/perf_ind_flow.R ####
flow_df <- perf_df[, c("iss_band", "previous_hospital", "transfer", "went_ed", "ward_1", "ward_2")]
flow_df <- flow_df %>%
  mutate(
    previous_hospital = case_when(
      is.na(previous_hospital) ~ "No transfer",
      previous_hospital == "Manchester Royal Infirmary" ~ "MRI",
      previous_hospital == "Wythenshawe Hospital" ~ "Wythen",
      previous_hospital == "Royal London Hospital" ~ "Royal London",
      previous_hospital == "Macclesfield District General Hospital" ~ "Maccles",
      previous_hospital == "Tameside General Hospital" ~ "Tameside",
      previous_hospital == "Royal Albert Edward Infirmary" ~ "RAEI",
      previous_hospital == "Royal Bolton Hospital" ~ "Bolton",
      previous_hospital == "Fairfield General Hospital" ~ "Fairfield",
      previous_hospital == "North Manchester General Hospital" ~ "NMGH",
      previous_hospital == "Trafford General Hospital" ~ "Trafford",
      previous_hospital == "Stepping Hill Hospital" ~ "SHH",
      previous_hospital == "Royal Victoria Hospital Belfast" ~ "Belfast",
      previous_hospital == "Leighton Hospital" ~ "Leighton",
      previous_hospital == "Other hospital (non-UK)" ~ "non-UK",
      previous_hospital == "Royal Oldham Hospital" ~ "Oldham",
      TRUE ~ previous_hospital
    ),
    transfer = case_when(
      transfer == "No Transfer" ~ "No",
      transfer == "Transfer In" ~ "In",
      transfer == "Transfer Out" ~ "Out",
      transfer == "Transfer In & Out" ~ "In & Out",
      TRUE ~ transfer
    ),
    ward_1 = case_when(
      ward_1 == "Neurosurgical ward" ~ "Neuro",
      ward_1 == "Surgical ward (inc. paediatric)" ~ "Surgical",
      ward_1 == "Major Trauma Ward" ~ "Trauma",
      ward_1 == "Spinal injuries unit" ~ "Neuro",
      ward_1 == "Orthopaedic (inc. paediatric)" ~ "Ortho",
      ward_1 == "Emergency Admissions Unit (EAU)" ~ "EAU",
      ward_1 == "Medical ward (inc. Pallative care)" ~ "Medical",
      ward_1 == "Major trauma ward" ~ "Trauma",
      TRUE ~ ward_1 
    ),
    ward_2 = case_when(
      ward_2 == "Major Trauma Ward" ~ "Trauma",
      ward_2 == "Neurosurgical ward" ~ "Neuro",
      ward_2 == "General acute (inc. paediatric)" ~ "Gen.\nacute",
      ward_2 == "Surgical ward (inc. paediatric)" ~ "Surgical",
      ward_2 == "Spinal injuries unit" ~ "Neuro",
      ward_2 == "Orthopaedic (inc. paediatric)" ~ "Ortho",
      ward_2 == "Medical ward (inc. Pallative care)" ~ "Medical",
      ward_2 == "Emergency Admissions Unit (EAU)" ~ "EAU",
      ward_2 == "Major trauma ward" ~ "Trauma",
      is.na(ward_2) ~ "D/C",
      TRUE ~ ward_2
    )
    
  )

saveRDS(perf_df, file = "./perf_ind/perf_df.RDS")

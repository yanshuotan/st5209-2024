require(tidyverse)
require(fpp3)

RAW_DIR <- "_data/raw/"
CLEANED_DIR <- "_data/cleaned/"

process_sgcpi <- function(save = FALSE) {
  # Return cleaned dataset for SG CPI data.
  sgcpi <- read_csv(str_c(RAW_DIR, "sg-cpi.csv"), skip = 10, n_max = 152)
  col_names <- sgcpi$`Data Series`
  time_index <- colnames(sgcpi)[-1]
  sgcpi <- sgcpi |> 
    select(-`Data Series`) |>
    t() |>
    as_tibble(cols())
  colnames(sgcpi) <- col_names
  selected_cols <- c("All Items", "Food", "Clothing & Footwear", "Health Care", 
  "Transport", "Education")
  sgcpi <- sgcpi |> 
    select(all_of(selected_cols)) |>
    mutate_all(as.double) |>
    mutate(Month = yearmonth(time_index))
  sgcpi <- sgcpi |> 
    as_tsibble() |>
    pivot_longer(cols = all_of(selected_cols), names_to = c("Type"))
  if (save) {
    saveRDS(sgcpi, file = str_c(CLEANED_DIR, "sgcpi.rds"))
  }
  sgcpi
}

process_diabetes <- function(save = FALSE) {
  # Return diabetes drug sales in Australia
  diabetes <- PBS |>
    filter(ATC2 == "A10") |>
    select(Month, Concession, Type, Cost) |>
    summarise(TotalC = sum(Cost)) |>
    mutate(Cost = TotalC / 1e6)
  if (save) {
    saveRDS(diabetes, file = str_c(CLEANED_DIR, "diabetes.rds"))
  }
  diabetes
}

process_bejing <- function(save = FALSE) {
  beijing_pm25 <- read_csv(str_c(RAW_DIR, "beijing-pm25.csv"))
  beijing_pm25$Datetime <- as.POSIXct(
    paste(beijing_pm25$year, beijing_pm25$month, beijing_pm25$day, 
          beijing_pm25$hour, sep = " "), format = "%Y %m %d %H")
  if (save) {
    saveRDS(beijing_pm25, file = str_c(CLEANED_DIR, "beijing_pm25.rds"))
  }
  beijing_pm25
}
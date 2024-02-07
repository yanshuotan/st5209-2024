require(tidyverse)
require(fpp3)

RAW_DIR <- "../_data/raw/"
CLEANED_DIR <- "../_data/cleaned/"

process_sgcpi <- function(save = FALSE) {
  # Return cleaned dataset for SG CPI data.
  sgcpi <- read_csv(str_c(RAW_DIR, "sg-cpi.csv"), skip = 10, n_max = 152)
  col_names <- sgcpi$`Data Series`
  time_index <- colnames(sgcpi)[-1]
  sgcpi <- sgcpi |> 
    select(-`Data Series`) |>
    t() |>
    as_tibble()
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

process_rice <- function(save = FALSE) {
  rice_price <- read_csv(
    str_c(RAW_DIR, "retail_prices.csv"), 
    skip=9, n_max = 162, na="na") |> 
    select(1, 2) |>
    rename("Month" = `Data Series`, 
           "thai_rice" = `Premium Thai Rice (Per 5 Kilogram) (Dollar)`) |> 
    mutate(Month = yearmonth(Month)) |>
    as_tsibble(index = Month)
  if (save) {
    saveRDS(rice_price, file = str_c(CLEANED_DIR, "rice_price.rds"))
  }
  rice_price
}

process_control_charts <- function(save = FALSE) {
  ccharts <- read_table(str_c(RAW_DIR, "synthetic_control.data"), 
                        col_names = FALSE)
  chart_type <- as.factor(rep(c("Normal", "Cyclic", "Increasing", "Decreasing", 
                                "Upward", "Downward"), each = 100))
  ccharts <- ccharts |> 
    mutate(Type = chart_type,
           id = 1:600) |> 
    pivot_longer(cols = contains("X"),
                 values_to = "value") |>
    mutate(Time = rep(1:60, 600)) |>
    select(Time, value, id, Type) |>
    as_tsibble(index = Time,
               key = c(id, Type))
  ccharts_train <- ccharts |>
    filter((id %% 100) <= 80)
  ccharts_test <- ccharts |>
    filter((id %% 100) > 80)
  ccharts = list("train" = ccharts_train, "test" = ccharts_test)
  if (save) {
    saveRDS(ccharts, file = str_c(CLEANED_DIR, "ccharts.rds"))
  }
  ccharts
}

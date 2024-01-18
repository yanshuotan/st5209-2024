require(rlang)

gg_custom_season <- function(data, y, period, start = 1) {
  # Make a seasonal plot with period specified in integer
  # start argument specifies the row number that will be the first season
  # in the period
  y <- enquo(y)
  data |>
    mutate(Season = (row_number() - start) %% period + start,
           Iteration = as.factor((row_number() - start) %/% period + 1)) |>
    ggplot(aes(x = Season, y = !!y, color = Iteration)) +
    geom_line()
}

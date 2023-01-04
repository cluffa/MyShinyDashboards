library(tidyverse)
library(lubridate)

df <- readr::read_csv(
  "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
  col_names = c("date", "weight", "unit", "fat", "lean"),
  col_types = "cncnn",
  skip = 1
  ) |>
  arrange(date) |> 
  mutate(
    date = as.POSIXct(date),
    date = force_tz(date, tzone = "EST")
  ) |> 
  filter(
    date > as.POSIXct("2022-04-03 0:00:00 EST")
  )



stopdate = Sys.time()
w = 10

avgs = tibble()
n = nrow(df)
for (i in n:1) {
  avgs = df |> 
    filter(
      date > df$date[i] - days(w),
      date < df$date[i] + days(w)
    ) |>
    summarise(
      weight = mean(weight, na.rm = TRUE),
      n = n()
    ) |>
    bind_rows(
      avgs
    )
}

df = df |> 
  mutate(
    avg = avgs$weight,
    n = avgs$n,
    dif = weight - avg,
    time = hour(date) + minute(date)/60
  )

ggplot(df) +
  geom_line(aes(date, avg)) +
  geom_point(aes(date, weight), alpha = 0.25)

ggplot(df) +
  geom_boxplot(aes(weekdays(date), dif))

ggplot(df, aes(time, dif)) +
  geom_point() + 
  geom_smooth()

ggplot(df, aes(date, abs(dif))) +
  geom_point() + 
  geom_smooth()

ggplot(df, aes(time)) +
  geom_histogram(bins = 24)

spline = smooth.spline(df$date, df$weight)



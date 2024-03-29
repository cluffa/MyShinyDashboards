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


spl = smooth.spline(df$date, df$weight)
rng_start <- floor_date(df$date[1], "days")
rng_stop <- ceiling_date(tail(df$date, 1), "days")
rng <- seq(rng_start, rng_stop, by = "24 hours")

pred <- predict(spl, as.numeric(rng)) |>
    data.frame() |> 
    transmute(
        date = x,
        weight = y,
        cals = c(diff(weight), NA) * 3500
    )

uniroot.all(approxfun(pred$date, pred$cals), interval = range(pred$date))

suppressWarnings({
    library("tidyverse")
    library("readr")
    library("forecast")
    library("tsibble")
    library("tibbletime")
    library("mice")
})

sheet <- read_csv(
    "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
    col_names = c("date", "weight", "unit", "fat", "lean"),
    col_types = "cncnn",
    skip = 1
)


df <- sheet |>
    mutate(
        date = as.POSIXct(date) |> force_tz(tzone = "EST") |> date()
    ) |>
    group_by(
        date
    ) |>
    summarise(
        weight = round(mean(weight), 1)
    ) |>
    as_tsibble(index = date) |>
    fill_gaps() |>
    mutate(
        weight = na.interp(weight)
    ) |>
    tail(n = 180)

df |> head()

plot(df)

mean_roll <- rollify(mean, window = 10)
df |> mutate(weight = mean_roll(weight)) |> plot()

ts <- ts(df$weight)

aa <- auto.arima(ts); aa

predict(aa, n.ahead = 30)

forecast(ts, h = 20) |> autoplot()



bmr <- function(weight, mult = 1.35, bfp = 0.25) {
    kg <- weight * 0.453592
    body_fat_mass <- bfp * kg
    lean_body_mass <- kg - body_fat_mass
    base_metabolic_rate <- 370 + 21.6 * lean_body_mass
    return(base_metabolic_rate * mult)
}

loseit <- readr::read_csv(
        "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1838432377&format=csv",
        skip = 1,
        col_names = c("date", "budget", "food", "exercise", "net", "difference", "weight", "weighed"),
        col_types = "c-n---n-",
    ) |> mutate(
        date = as_date(date, format = "%m/%d/%y"),
        food = na_if(food, 0),
        bmr = bmr(weight)
    )
    








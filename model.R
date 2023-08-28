library(tidyverse)
library(lubridate)

raw <- read_csv(
    "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
    col_names = c("date", "weight"),
    col_types = "cn---",
    skip = 1,
    lazy = TRUE
)

df <- raw |> filter(
    # filter to newer observations with accurate times
    date > as_datetime("2022-04-02 00:00:00") |> force_tz(tzone = "EST"),
) |> mutate(
    date = as_datetime(date) |> force_tz(tzone = "EST"),
    time_decimal = hour(date) + minute(date) / 60,
    is_morning = time_decimal <= 12 & time_decimal >= 4,
    is_late_night = time_decimal < 4,
    time_decimal = time_decimal + if_else(is_late_night, 24, 0),
    date_adjusted = date(date) - if_else(is_late_night, 1, 0)
)

train <- df |> select(
    date_adjusted,
    weight,
    time_decimal
) |> left_join(
    df |> filter(
        is_morning
    ) |> group_by(
        date_adjusted
    ) |> summarise(
        std_weight = mean(weight),
        std_time_decimal = mean(time_decimal)
    )
) |> mutate(
    time_dif = time_decimal - std_time_decimal,
    weight_dif = weight - std_weight
) |> remove_missing()

plot(train$time_dif, train$weight_dif)

model <- lm(
    std_weight ~ weight + time_dif,
    data = train
)

predict(
    model,
    newdata = data.frame(
        weight = 200,
        time_dif = -3:15
    )
)

summary(model)


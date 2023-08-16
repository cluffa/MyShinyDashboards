library(shiny)

function(input, output, session) {
    library(ggplot2)
    library(readr)
    library(dplyr)
    library(lubridate)
    library(zoo)
    
    df <- {read_csv(
        "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
        col_names = c("date", "weight"),
        col_types = "cn---",
        skip = 1,
        lazy = TRUE
    ) |> mutate(
        date = as_datetime(date) |> force_tz(tzone = "EST")
    ) |> arrange(
        date
    ) |> select(
        date,
        weight
    )}
    
    
    
    
    # result_df <- data.frame(date = index(rolling_exp_avg), weight = coredata(rolling_exp_avg))

    output$distPlot <- renderPlot({
        zoo_data <- zoo(df$weight, order.by = df$date)
        rolling_exp_avg <- rollapply(zoo_data, input$span, function(x) mean(x, na.rm = TRUE), align = "right", fill = NA)
        df$avgweight <- coredata(rolling_exp_avg)
        
        df %>%
            filter(date >= Sys.Date() - months(input$months)) %>% 
            ggplot(aes(date)) +
            geom_point(aes(y = weight)) +
            geom_line(aes(y = avgweight)) +
            theme_minimal()
    })

}




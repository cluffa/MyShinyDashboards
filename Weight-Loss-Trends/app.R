# Weight Loss Trend
library(shiny)
library(ggplot2)
library(shinydashboard)
library(reactable)
library(lubridate)

ui <- dashboardPage(
    dashboardHeader(disable = TRUE),
    dashboardSidebar(
        disable = TRUE
    ),
    dashboardBody(
        # Boxes need to be put in a row (or column)
        fluidRow(
            box(
                radioButtons(
                    "dateRange",
                    label = "Date Range",
                    choices = c(
                        `30 Days` = Sys.Date() - 30,
                        `90 Days` = Sys.Date() - 90,
                        `1 Year` = Sys.Date() - 365,
                        `2 Years` = Sys.Date() - 365*2,
                        `3 Years` = Sys.Date() - 365*3,
                        `All Time` = Sys.Date() - 99999
                        ),
                    selected = c(`90 Days` = Sys.Date() - 90),
                    inline = TRUE
                ),
                # dateRangeInput(
                #   "dateRange",
                #   label = NULL,
                #   start = Sys.Date() - 90,
                #   end = Sys.Date()
                # ),
                plotOutput("plot1"),
            ),
            tabBox(
                tabPanel(
                "Stats",
                verbatimTextOutput("stats"),
                ),
                tabPanel(
                    "Goal Projection",
                    sliderInput(
                        "goalwt",
                        label = "Goal Weight",
                        min = 185,
                        max = 275,
                        value = 225
                    ),
                    verbatimTextOutput("summary"),
                ),
                tabPanel(
                    "Table",
                    reactableOutput("table")
                ),
                tabPanel(
                    "Info",
                    p("This dashboard was created so that I could easily track my weight over time.
                        I aim for a specific average pounds lost per week over the last 30 or 90 days.
                        With this dashboard I can easily view those trends and adjust accordingly."),
                    p("Because the observations are not spread evenly, I created a spline to
                        approximate the weight with values spread exactly 24 hours apart.
                        The linear regression line is fit using those points. The slope is then used
                        to predict when I will meet my goal weight.
                        ")
                )
            )
        )
    )
)

server <- function(input, output) {
  
    df <- readr::read_csv(
        "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
        col_names = c("date", "weight", "unit", "fat", "lean"),
        col_types = "cncnn",
        skip = 1
    ) |> dplyr::arrange(
        date
    )
    
    df$date <- as.POSIXct(df$date) |> force_tz(tzone = "EST")
    spl <- smooth.spline(df$date, df$weight, spar = 0.4)
    
    rng_start <- floor_date(df$date[1], "days")
    rng_stop <- ceiling_date(tail(df$date, 1), "days")

    rng <- seq(rng_start, rng_stop, by = "24 hours")
    
    pred <- predict(spl, as.numeric(rng))
    
    
    get_date_range <- reactive({
        c(
          as.POSIXct(input$dateRange),
          as.POSIXct(Sys.Date())
        )
    })
    
    get_data <- reactive({
        df <- dplyr::arrange(df, desc(date))
        df
    })
    
    get_df <- reactive({
        range <- get_date_range()
        df <- get_data()
        df <- df[df$date >= range[1] & df$date <= range[2] + 86400,]
        df
    })
    
    get_model <- reactive({
        pred <- get_spline_pred_in_range()
        df <- get_df()
        lm(weight ~ date, pred)
    })
    
    get_spline_pred_in_range <- reactive({
        range <- get_date_range()
    
        in_rng <- pred$x >= range[1] & pred$x <= range[2] + 86400
        
        data.frame(
            date = as.POSIXct(pred$x[in_rng], origin = "1970-1-1"),
            weight = pred$y[in_rng]
        )
    })
    
    get_gw <- reactive({
        input$goalwt
    })
    
    output$plot1 <- renderPlot({
        model <- get_model()
        coefs <- model$coefficients
        range <- get_date_range()
        spl <- get_spline_pred_in_range()
        df <- get_df()
        
        ggplot() +
            geom_point(aes(y = weight, x = date, color = "Observed Weight"), data = df, alpha = 0.7) +
            geom_line(aes(date, weight, color = "Spline Fit"), data = spl, size = 1) +
            geom_abline(color = "red", intercept = coefs[1], slope = coefs[2], linetype = 2, size = 1) +
            scale_color_manual(
                name = NULL,
                breaks = c(
                    "Observed Weight",
                    "Spline Fit",
                    "Linear Regression"
                ),
                values = c(
                    "Observed Weight" = "black",
                    "Spline Fit" = "green",
                    "Linear Regression" = "red"
                )
            ) +
            theme_bw() +
            theme(
                legend.position = c(0.2, 0.1),
                legend.background = element_rect(fill = "transparent")
            )
    })
    
    output$summary <- renderPrint({
        model <- get_model()
        coef <- unname(model$coefficients[2])*86400
        df <- get_df()
        gw <- get_gw()
        
        gwdate <- (gw - model$coefficients[1])/model$coefficients[2]
        
        cat(
            "Daily Trend:", coef, "lbs/day",
            "\nWeekly Trend:", coef*7, "lbs/week",
            "\nGoal Projection:", gw, "on", as.character(as.Date(as.POSIXct.numeric(gwdate, origin = "1970-1-1")))
            )
        
    })
    
    output$stats <- renderPrint({
        df <- get_df() |> 
            dplyr::mutate(
                date = format(date, "%Y-%m-%d")
            )
        full_df <- get_data() |> 
            dplyr::mutate(
                date = format(date, "%Y-%m-%d")
            )
        
        range_52 <- c(Sys.Date() - (52*7), Sys.Date() + 1)
        df_52 <- full_df[full_df$date >= range_52[1] & full_df$date <= range_52[2],]
        
        df$date <- as.character(df$date)
        full_df$date <- as.character(full_df$date)
        
        range <- get_date_range()
        range <- as.Date(range)
        range <- range[2] - range[1]
        suppressWarnings(
            cat(
                "    Range Low:", min(df$weight), "on", df$date[which.min(df$weight)],
                "\n   Range High:", max(df$weight), "on", df$date[which.max(df$weight)],
                "\n   Range Mean:", round(mean(df$weight, na.rm = TRUE),1),
                "\n All Time Low:", min(full_df$weight), "on", full_df$date[which.min(full_df$weight)],
                "\nAll Time High:", max(full_df$weight), "on", full_df$date[which.max(full_df$weight)],
                "\nAll Time Mean:", round(mean(full_df$weight, na.rm = TRUE),1)
            )
        )
    })
    
    output$model <- renderPrint({
        model <- get_model()
        coef <- unname(model$coefficients[2])*86400
        
        cat("Model (date in seconds):", sep = "\n")
        
        summary(model)
    })
    
    output$table <- renderReactable({
        df1 <- dplyr::transmute(
            get_data(),
            date = date(date),
            weight = weight
        ) |> dplyr::group_by(
            date
        ) |> dplyr::summarise(
            weight = round(mean(weight), 1)
        ) |> dplyr::arrange(
            desc(date)
        )
            
                                
        reactable(
            df1,
            striped = TRUE,
            compact = TRUE,
            wrap = FALSE,
            showSortable = TRUE,
            defaultPageSize = 25,
            showPageSizeOptions = FALSE,
            pageSizeOptions = c(25,50,100),
            outlined = TRUE,
            resizable = FALSE,)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

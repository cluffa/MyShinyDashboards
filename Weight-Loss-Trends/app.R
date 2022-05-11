# WEIGHT LOSS TRENDS
library(shiny)
library(ggplot2)

googlesheets4::gs4_auth("alexcluff16@gmail.com", cache = ".secrets")
df <- googlesheets4::read_sheet("151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0")

ui <- fluidPage(mainPanel(
    titlePanel("Weight Loss Trend"),
    fluidRow(
        column(4,dateRangeInput('dateRange',
            label = 'Date Range:',
            start = as.POSIXct("2022-03-28"), end = Sys.Date()
        )),
        column(4,
            numericInput('goalwt',
                label = "Goal Weight:",
                value = 225,
                step = 5
            ),
        ),
    ),
    fluidRow(
        verbatimTextOutput("summary"),
        plotOutput("plot1"),
        verbatimTextOutput("stats"),
        #verbatimTextOutput("model")
    )
))

server <- function(input, output) {
    get_date_range <- reactive({
        c(
            as.POSIXct(input$dateRange[1]),
            as.POSIXct(input$dateRange[2])
        )
    })
    
    get_df <- reactive({
        range <- get_date_range()
        df <- df[df$date >= range[1] & df$date <= range[2] + 86400,]
        df$date <- lubridate::floor_date(df$date, unit = "1 days")
        df <- aggregate(df, list(df$date), function(x) min(x, na.rm = TRUE))
        df$Group.1 <- NULL
        df$unit <- NULL
        df
    })
    
    get_model <- reactive({
        lm(weight ~ date, get_df())
    })
    
    get_gw <- reactive({
        input$goalwt
    })
    
    output$plot1 <- renderPlot({
        model <- get_model()
        coefs <- model$coefficients
        range <- get_date_range()
        df <- get_df()
        
        ggplot2::ggplot() +
            ggplot2::geom_point(aes(y = weight, x = date), data = df) +
            ggplot2::geom_abline(intercept = coefs[1], slope = coefs[2], color = "red", linetype = 2) +
            ggplot2::theme_bw()
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
        df <- get_df()
        df$date <- as.character(df$date)
        range <- get_date_range()
        range <- as.Date(range)
        range <- range[2] - range[1]
        cat("STATS",
            "\n\nDate range is", round(as.numeric(range)/7,1), "weeks",
            "\nMin:", min(df$weight), "on", df$date[which.min(df$weight)],
            "\nMax:", max(df$weight), "on", df$date[which.max(df$weight)],
            "\nAverage:", round(mean(df$weight, na.rm = TRUE),1)
            )
        
        
        
        cat("\n\nLast 5 weights:\n")
        df <- df %>% dplyr::arrange(desc(date))
        head(df,5)
    })
    
    output$model <- renderPrint({
        model <- get_model()
        coef <- unname(model$coefficients[2])*86400
        
        cat("Model (date in seconds):", sep = "\n")
        
        summary(model)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

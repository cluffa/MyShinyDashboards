# Weight Loss Trend
library(shiny)
library(ggplot2)
library(shinydashboard)
library(reactable)

ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        dateRangeInput(
          "dateRange",
          label = "Date Range:",
          start = as.POSIXct("2022-07-01"), end = Sys.Date()
        ),
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
        )
      )
    )
  )
)

server <- function(input, output) {
  
    df <- readr::read_csv(
        "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
        col_names = c("date", "weight", "unit", "fat", "lean"),
        col_types = "Tncnn"
    )
    
    get_date_range <- reactive({
        c(
            as.POSIXct(input$dateRange[1]),
            as.POSIXct(input$dateRange[2])
        )
    })
    
    get_data <- reactive({
        df$date <- lubridate::floor_date(df$date, unit = "1 days")
        df <- aggregate(df, list(df$date), function(x) min(x, na.rm = TRUE))
        df$Group.1 <- NULL
        df$unit <- NULL
        df <- dplyr::arrange(df,desc(date))
        df
    })
    
    get_df <- reactive({
        range <- get_date_range()
        df <- get_data()
        df <- df[df$date >= range[1] & df$date <= range[2] + 86400,]
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
        full_df <- get_data()
        
        range_52 <- c(Sys.Date() - (52*7), Sys.Date() + 1)
        df_52 <- full_df[full_df$date >= range_52[1] & full_df$date <= range_52[2],]
        
        df$date <- as.character(df$date)
        full_df$date <- as.character(full_df$date)
        df_52$date <- as.character(df_52$date)
        
        range <- get_date_range()
        range <- as.Date(range)
        range <- range[2] - range[1]
        suppressWarnings(
            cat("   Date Range:", round(as.numeric(range)/7,1), "weeks",
                "\n    Range Low:", min(df$weight), "on", df$date[which.min(df$weight)],
                "\n   Range High:", max(df$weight), "on", df$date[which.max(df$weight)],
                "\n   Range Mean:", round(mean(df$weight, na.rm = TRUE),1),
                "\n  52 Week Low:", min(df_52$weight), "on", df_52$date[which.min(df_52$weight)],
                "\n 52 Week High:", max(df_52$weight), "on", df_52$date[which.max(df_52$weight)],
                "\n 52 Week Mean:", round(mean(df_52$weight, na.rm = TRUE),1),
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
      reactable(
        get_df(),
        striped = TRUE,
        compact = TRUE,
        wrap = FALSE,
        showSortable = FALSE,
        defaultPageSize = 25,
        showPageSizeOptions = FALSE,
        pageSizeOptions = c(25,50,100),
        outlined = TRUE,
        resizable = FALSE,)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

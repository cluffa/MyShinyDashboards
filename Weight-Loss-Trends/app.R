# WEIGHT LOSS TRENDS
library(shiny)
library(ggplot2)
library(googlesheets4)
library(plotly)
library(randomForest)

gs4_auth("alexcluff16@gmail.com", cache = ".secrets")
df <- read_sheet("151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0")

ui <- fluidPage(

    titlePanel("Weight Loss Trend"),
    dateRangeInput('dateRange',
        label = 'Date range input: yyyy-mm-dd',
        start = as.POSIXct("2022-03-28"), end = Sys.Date() + 1
    ),
    mainPanel(
        plotOutput("plot1"),
        verbatimTextOutput("summary"),
    )
)

server <- function(input, output) {
    
    get_date_range <- reactive({
        c(
            as.POSIXct(input$dateRange[1]),
            as.POSIXct(input$dateRange[2])
        )
    })
    
    get_df <- reactive({
        range <- get_date_range()
        df <- df[df$date >= range[1] & df$date < range[2],]
        df$date = lubridate::floor_date(df$date, unit = "1 days")
        df = aggregate(df, list(df$date), min)
    })
    
    get_model <- reactive({
        df = get_df()
        lm(weight ~ date, df)
    })
    
    output$plot1 <- renderPlot({
        model <- get_model()
        coefs <- model$coefficients
        range <- get_date_range()
        df <- get_df()
        
        graph <- ggplot() +
            geom_point(aes(y = weight, x = date), data = df) +
            geom_abline(intercept = coefs[1], slope = coefs[2], color = "red", linetype = 2)
            
        #graph <- graph + geom_point(aes(y = model$y, x = model$x), color = "blue")
        
        graph + theme_bw()#%>% ggplotly()
    })
    
    output$summary <- renderPrint({
        model <- get_model()
        coef <- unname(model$coefficients[2])*86400
        
        cat("Daily Trend:",coef,"","Weekly Trend:",coef*7,"","Model (date in seconds):", sep = "\n")
        
        summary(model)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

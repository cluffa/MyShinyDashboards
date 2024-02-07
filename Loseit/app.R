#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(
    titlePanel(NULL),
    sidebarLayout(
        sidebarPanel(
            div(
                style="display:inline-block",
                numericInput(
                    inputId="months", 
                    label="Number of months shown", 
                    value = 6,
                    min = 1
                )
            ),
        ),
        
        mainPanel(
            plotOutput("distPlot"),
            plotOutput("calPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    library(ggplot2)
    library(readr)
    library(dplyr)
    library(lubridate)
    
    df <- read_csv(
        "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1838432377&format=csv",
        skip = 1,
        col_names = c("date", "budget", "food", "exercise", "net", "difference", "weight", "weighed", "garmin", "protein", "sugar", "goal_deficit"),
        col_types = "c-nn--n-nnnn",
    ) |> mutate(
        date = as_datetime(date, format = "%m/%d/%y"),
        food = if_else(food < 1100, NA_real_, food)
    ) |> select(
        date, food, weight, garmin
    ) |> remove_missing()
    
    df_trunc <- reactive({
        df %>% 
            filter(date >= Sys.Date() %m-% months(input$months))
    }) |> bindCache(input$months)
    
    model <- reactive({
        lm(weight ~ date, data = df_trunc())
    }) |> bindCache(input$months)
    
    intercept <- reactive({
        model()$coefficients[1]
    })
    
    slope <- reactive({
        model()$coefficients[2]
    })
    
    output$distPlot <- renderPlot({
        df_trunc() %>%
            ggplot(aes(date)) +
            geom_point(
                aes(y = weight),
                show.legend = FALSE,
            ) +
            geom_abline(
                slope = slope(),
                intercept = intercept(),
                color = "red",
                linetype = "dashed"
            ) +
            theme_minimal() +
            geom_label(
                x = mean(df_trunc()$date),
                y = max(df_trunc()$weight),
                aes(label = paste("losing", format(slope() * -604800, digits = 3), "lbs per week")),
                color = "red"
            ) +
            scale_color_manual(values=c("gray", "black"))
    }) |> bindCache(input$months)
    
    output$calPlot <- renderPlot({
        df_trunc() %>%
            ggplot(aes(date)) +
            geom_point(aes(y = garmin), color = "red") +
            geom_point(aes(y = food), color = "blue") +
            geom_smooth(aes(y = garmin), color = "red") +
            geom_smooth(aes(y = food), color = "blue") +
            
            ylab("calories")
    }) |> bindCache(input$months)
    
}

# Run the application 
shinyApp(ui = ui, server = server)

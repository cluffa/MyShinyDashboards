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
                    value = 6
                )
            ),
            div(
                style="display:inline-block",
                numericInput(
                    inputId="span",
                    label="Span of linear model in days", 
                    value = 28
                )
            )
        ),
        
        mainPanel(
            plotOutput("distPlot")
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
        "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
        col_names = c("date", "weight"),
        col_types = "cn---",
        skip = 1,
        lazy = TRUE
    ) |> mutate(
        date = as_datetime(date) |> force_tz(tzone = "EST")
    ) |> select(
        date,
        weight
    )
    
    df_trunc <- reactive({
        df %>% 
            filter(date >= Sys.Date() - months(input$months)) %>%
            mutate(modeled = date >= Sys.Date() - days(input$span))
    })
    
    model <- reactive({
        lm(weight ~ date, data = df_trunc(), subset = df_trunc()$modeled)
    })
    
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
                aes(y = weight, color = modeled),
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
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

library(shiny)

fluidPage(
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

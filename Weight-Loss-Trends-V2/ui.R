library(shiny)

fluidPage(

    titlePanel("Weight Loss Trends"),

    sidebarLayout(
        sidebarPanel(
            sliderInput("months",
                        "Number of months:",
                        min = 1,
                        max = 50,
                        value = 6),
            sliderInput("span",
                        "span of exponential avg",
                        min = 1,
                        max = 50,
                        value = 6),
        ),

        mainPanel(
            plotOutput("distPlot")
        )
    )
)

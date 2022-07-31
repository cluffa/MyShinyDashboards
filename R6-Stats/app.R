library(shiny)
library(shinydashboard)
library(tidyverse)
library(Cairo); options(shiny.usecairo=TRUE)

gids <- c(
  "768069849",
  "1042944539",
  "1577317040",
  "1573491615"
)

getdf <- function(gid) {
  read_csv(url(paste0("https://docs.google.com/spreadsheets/d/1lC4L6msviQZwKEYut7dC5FKz7ktKybnhiAZBZCrhXGU/export?gid=", gid, "&format=csv")))
}

#overview = getdf(gids[1])

perf <- getdf(gids[2])
names(perf) <- names(perf) |> tolower() |> str_remove_all(" ")

#rounds = getdf(gids[3])
#rounds_breakdown = getdf(gids[4])

ui <- dashboardPage(
  dashboardHeader(title = "R6 Stats"),
  dashboardSidebar(
    selectInput(
      "att",
      "Select Attribute",
      names(perf),
      selected = "kost"
    )
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("plot1")),
    )
  )
)

server <- function(input, output) {
  output$plot1 <- renderPlot({
    ggplot(perf) +
      geom_boxplot(aes_string(x = "player", y = input$att, fill = "player"))
  })
}

shinyApp(ui, server)

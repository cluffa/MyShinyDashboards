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

overview <- getdf(gids[1])
names(overview) <- names(overview) |> tolower() |> str_remove_all(" ")

perf <- getdf(gids[2])
names(perf) <- names(perf) |> tolower() |> str_remove_all(" ")

rounds = getdf(gids[3])
names(rounds) <- names(rounds) |> tolower() |> str_remove_all(" ")

rounds_breakdown = getdf(gids[4])
names(rounds_breakdown) <- names(rounds_breakdown) |> tolower() |> str_remove_all(" ")


ui <- dashboardPage(
  dashboardHeader(title = "R6 Stats", disable = TRUE),
  dashboardSidebar(
    disable = TRUE
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(
        plotOutput("plot1"),
        selectInput(
          "att1",
          "Select Attribute",
          names(perf),
          selected = "kost"
        )
      ),
      box(
        plotOutput("plot2"),
        selectInput(
          "att2x",
          "Select Attribute",
          names(rounds),
          #selected = "kost"
        ),
        selectInput(
          "att2y",
          "Select Attribute",
          names(rounds),
          #selected = "kost"
        ),
        selectInput(
          "att2col",
          "Select Attribute",
          names(rounds),
          #selected = "kost"
        )
      )
    )
  )
)

server <- function(input, output) {
  
  # getdf1 <- reactive({
  #   dfs[input$df1]
  # })
  # 
  # getdf2 <- reactive({
  #   dfs[input$df2]
  # })
  
  for (df in list(overview, perf, rounds, rounds_breakdown)) { 
    names(df) <- names(df) |> tolower() |> str_remove_all(" ")
  }
  
  output$plot1 <- renderPlot({
    ggplot(perf) +
      geom_boxplot(aes_string(x = "player", y = input$att1, fill = "player"))
  })
  
  output$plot2 <- renderPlot({
    ggplot(rounds) +
      geom_point(aes_string(x = input$att2x, y = input$att2y, color = input$att2col))
  })
  
}

shinyApp(ui, server)

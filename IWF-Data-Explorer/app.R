# IWF DATA EXPLORER
library(tidyverse)
library(googlesheets4)
library(reactable)
library(plotly)

load(url(
  "https://github.com/cluffa/IWF_data/raw/main/all_data.Rdata"
))

names <- athletes %>%
  transmute(name = paste0(name, " (", date_of_birth, ")")) %>% 
  distinct() %>% 
  arrange(name)

countries <- results %>% 
  select(nation) %>% 
  distinct() %>% 
  arrange(nation)

cats <- results %>% 
  select(category) %>% 
  distinct() %>% 
  arrange(category)

age_range <- c(min(results$age), max(results$age))

event_names <- events %>% 
  transmute(name = paste0(event, " (", date, ")")) %>% 
  distinct() %>% 
  arrange(name)

age_groups <- events %>% 
  select(age_group) %>% 
  distinct() %>% 
  arrange(age_group)

cities <- events %>% 
  select(city) %>% 
  distinct() %>% 
  arrange(city)

countries_event <- events %>% 
  select(iso_code) %>% 
  distinct() %>% 
  arrange(iso_code)

date_range <- c(min(events$date), max(events$date))

ui <- fluidPage(
  titlePanel("Explore IWF Event Results Data"),
  tabsetPanel(
    type = "pills",
    tabPanel(
      title = "By Athlete/Country",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput(
            "nations",
            label = "Filter By Country (ISO code)",
            choices = countries,
            multiple = TRUE,
            options = list(placeholder = "Search Country Code e.g. 'USA'")
          ),
          selectizeInput(
            "athletes",
            label = "Filter By Athlete",
            choices = names,
            multiple = TRUE,
            selected = c("ROGERS Martha (1995-08-23)", "NYE Katherine (1999-01-05)"),
            options = list(placeholder = "Search Athlete (last first)")
          ),
          h3("-- Notes --"),
          h4("For the results tab:"),
          h5("- Negative weights mean the lifts are missed"),
          h4("For the graph:"),
          h5("- No filtering turns the graph into a 2d density plot."),
          h5("- Select more than one athlete and no countries to compare athletes"),
          h5("- Select more than one countries and no athletes to compare countries")
        ),
        
        mainPanel(
          tabsetPanel(
            type = "pills",
            tabPanel(
              "Info",
              mainPanel(
                downloadButton("downloadathletes", "Download as .CSV"),
                reactableOutput("table")
              )
            ),
            tabPanel(
              "Results",
              mainPanel(
                downloadButton("downloadresults", "Download as .CSV"),
                reactableOutput("tableResults")
              )
            ),
            tabPanel(
              "Graph",
              mainPanel(
                selectizeInput(
                  inputId = "xAxis",
                  label = "Graph X axis",
                  choices = c("Date", "Age"),
                  selected = "Date"
                ),
                plotOutput("graph")
                
              )
            )
          )
        )
      ),
    ),
    tabPanel(
      title = "By Event",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          selectizeInput(
            "country",
            label = "Filter By Location",
            choices = countries_event,
            multiple = TRUE,
            options = list(placeholder = "Search Country Code e.g. 'USA'")
          ),
          selectizeInput(
            "city",
            label = "Filter By City",
            choices = cities,
            multiple = TRUE,
            options = list(placeholder = "Search City")
          ),
          selectizeInput(
            "age_group",
            label = "Filter By Age Group",
            choices = age_groups,
            multiple = TRUE,
            options = list(placeholder = "Search Country Code e.g. 'USA'")
          ),
          selectizeInput(
            "events",
            label = "Filter By Event Name",
            choices = event_names,
            multiple = TRUE,
            options = list(placeholder = "Search Event Names")
          ),
          sliderInput(
            "date_range",
            label = "Filter By Date",
            min = as.Date("1997-01-01","%Y-%m-%d"),
            max = as.Date("2023-01-01","%Y-%m-%d"),
            value = c(as.Date("1997-01-01","%Y-%m-%d"), as.Date("2023-01-01","%Y-%m-%d")),
            timeFormat = "%Y-%m-%d"
          ),
          selectizeInput(
            "special",
            label = "Filter By Special Events",
            choices = c("Olympics", "Universities"),
            multiple = TRUE,
            options = list(placeholder = "None")
          ),
        ),
        mainPanel(
          downloadButton("downloadevents", "Download as .CSV"),
          reactableOutput("tableResultsEvent")
        )
      )
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  datasetInput <- reactive({
    athlete_ids <- athletes$athlete_id[if (length(input$athletes) > 0) athletes$name %in% str_split(input$athletes, " \\(", simplify = TRUE)[,1] else TRUE]
    output = list(
      athletes = athletes %>%
        filter(
          if (length(input$athletes) > 0) athlete_id %in% athlete_ids else TRUE,
          if (length(input$nations) > 0) grepl(paste(input$nations, collapse = "|"), nations) else TRUE
          ),
      results = results %>% 
        filter(
          if (length(input$athletes) > 0) athlete_id %in% athlete_ids else TRUE,
          if (length(input$nations) > 0) nation %in% input$nations else TRUE
        )
    )
    
    output
  })

  datasetInputEvent <- reactive({
    events %>% 
      filter(
        if (length(input$country) > 0) iso_code %in% input$country else TRUE,
        if (length(input$city) > 0) city %in% input$city else TRUE,
        if (length(input$age_group) > 0) age_group %in% input$age_group else TRUE,
        if (length(input$events) > 0) name %in% input$events else TRUE,
        date >= input$date_range[1] & date <= input$date_range[2],
        if ("Olympics" %in% input$special) is_olympics == 1 else TRUE,
        if ("Universities" %in% input$special) is_university == 1 else TRUE,
      )
  })
  
  observe({
    #updateSelectizeInput(inputId = 'nations', choices = c("test", "asdf"), server = TRUE)
    #updateSelectizeInput(inputId = 'athletes', choices = c("test", "asdf"), server = TRUE)
  })
  
  output$summary <- renderPrint({
    summary(datasetInput()$athletes)
  })
  
  output$table <- renderReactable({
    reactable(
      datasetInput()$athletes,
      striped = TRUE,
      compact = TRUE,
      wrap = FALSE,
      showSortable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25,50,100),
      outlined = TRUE,
      resizable = TRUE,)
  })
  
  output$tableResults <- renderReactable({
    reactable(
      datasetInput()$results,
      striped = TRUE,
      compact = TRUE,
      wrap = FALSE,
      showSortable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25,50,100),
      outlined = TRUE,
      resizable = TRUE,
      )
  })

  output$tableResultsEvent <- renderReactable({
    reactable(
      datasetInputEvent(),
      striped = TRUE,
      compact = TRUE,
      wrap = FALSE,
      showSortable = TRUE,
      defaultPageSize = 25,
      showPageSizeOptions = TRUE,
      pageSizeOptions = c(25,50,100),
      outlined = TRUE,
      resizable = TRUE,
      )
  })
  
  xaxis <- reactive({input$xAxis})
  
  output$graph <- renderPlot({
    df <- datasetInput()$results %>%
      remove_missing() %>% 
      distinct()
    
    base <- ggplot(
      df,
      aes(x = if (xaxis() == "Date") date else age)
      ) + theme_bw()
    
    base = base +
      labs(shape = "Nation", color = "Athlete") +
      xlab(if (xaxis() == "Date") "Date" else "Age") +
      ylab("Total (kg)")
      
   
    
    if (length(input$nations) > 0 & length(input$athletes) == 0) {
      graph = base +
        geom_point(aes(y = total, color = nation, shape = NULL), alpha = 0.5) +
        geom_smooth(aes(y = total, color = nation))
    } else if (length(input$nations) == 0 & length(input$athletes) == 0){
      graph = base +
        geom_bin2d(aes(y = total)) +
        scale_fill_continuous(type = "viridis")

    } else {
      graph = base +
        geom_point(
          aes(y = total,
              color = if (length(input$athletes) > 0 & length(input$athletes) <= 10) name else NULL,
              shape = if (length(input$nations) > 0 & length(input$nations) <= 10) nation else NULL)) +
        geom_line(
          aes(y = total,
              color = if (length(input$athletes) > 0 & length(input$athletes) <= 10) name else NULL,
              shape = if (length(input$nations) > 0 & length(input$nations) <= 10) nation else NULL))
    }
    
    graph
  })
  
  output$downloadresults <- downloadHandler(
    filename = "results.csv",
    function(file) {
      write_csv(datasetInput()$results, file)
    }
  )
  
  output$downloadathletes <- downloadHandler(
    filename = "athletes.csv",
    function(file) {
      write_csv(datasetInput()$athletes, file)
    }
  )
  
  output$downloadevents <- downloadHandler(
    filename = "results.csv",
    function(file) {
      write_csv(datasetInput()$athletes, file)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)

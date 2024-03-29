# IWF DATA EXPLORER

#library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)

library(shiny)
library(reactable)
library(shinydashboard)

load(url(
    "https://github.com/cluffa/IWF_data/raw/main/all_data.Rdata"
))

names <- athletes |>
    transmute(name = paste0(name, " (", date_of_birth, ")")) |> 
    distinct() |> 
    arrange(name)
names <- names$name

countries <- results |> 
    select(nation) |> 
    distinct() |> 
    arrange(nation)
countries <- countries$nation

cats <- results |> 
    select(category) |> 
    distinct() |> 
    arrange(category)

age_range <- c(min(results$age), max(results$age))

event_names <- events |> 
    transmute(name = paste0(event, " (", date, ")"), event_id = event_id) |> 
    distinct() |> 
    arrange(name)

age_groups <- events |> 
    select(age_group) |> 
    distinct() |> 
    arrange(age_group)

cities <- events |> 
    select(city) |> 
    distinct() |> 
    arrange(city)

countries_event <- events |> 
    select(iso_code) |> 
    distinct() |> 
    arrange(iso_code)

date_range <- c(min(events$date), max(events$date))

gg_color_hue <- function(n) {
    hues = seq(15, 375, length = n + 1)
    hcl(h = hues, l = 65, c = 100)[n]
}

ui <- fluidPage(
    titlePanel("IWF Event Results"),
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
                        choices = NULL,
                        multiple = TRUE
                    ),
                    selectizeInput(
                        "athletes",
                        label = "Filter By Athlete",
                        choices = NULL,
                        multiple = TRUE
                    ),
                    h3("-- Notes --"),
                    h4("For the results tab:"),
                    h5("- Negative weights mean the lifts are missed"),
                    h4("For the plot:"),
                    h5("- No filtering turns the figure into a 2d density plot."),
                    h5("- Select more than one athlete and no countries to compare athletes"),
                    h5("- Select more than one countries and no athletes to compare countries")
                ),
                
                mainPanel(
                    tabsetPanel(
                        type = "pills",
                        tabPanel(
                            "Info",
                            fluidRow(
                                downloadButton("downloadathletes", "Download as .CSV"),
                                reactableOutput("table")
                            ),
                        ),
                        tabPanel(
                            "Results",
                            fluidRow(
                                downloadButton("downloadresults", "Download as .CSV"),
                                reactableOutput("tableResults")
                            )
                        ),
                        tabPanel(
                            "Plot",
                            fluidRow(
                                selectizeInput(
                                    inputId = "xAxis",
                                    label = "Plot X axis",
                                    choices = c("Date", "Age", "Bodyweight"),
                                    selected = "Date"
                                ),
                                selectizeInput(
                                    inputId = "category",
                                    label = "Weight Category",
                                    choices = c("All", cats),
                                    selected = "All"
                                ),
                                plotOutput("plot")
                                
                            )
                        ),
                        tabPanel(
                            "Distribution",
                            fluidRow(
                                selectizeInput(
                                    inputId = "histx",
                                    label = "Plot X axis",
                                    choices = c("total_rank", "snatch_rank", "cleanjerk_rank", "age", "bw", "dq", "date", "snatch_best", "cleanjerk_best", "total"),
                                    selected = "total"
                                ),
                                checkboxInput(
                                    inputId = "histbygender",
                                    label = "Separate By Gender",
                                    value = TRUE
                                ),
                                plotOutput("hist")
                                
                            )
                        )
                    )
                )
            ),
        ),
        tabPanel(
            title = "Results By Event",
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
                        multiple = TRUE
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
                mainPanel(tabsetPanel(
                    type = "pills",
                    tabPanel(
                        "Events",
                        fluidRow(
                            #downloadButton("downloadevents", "Download as .CSV"),
                            reactableOutput("tableEventSearch")
                        ),
                    ),
                    tabPanel(
                        "Results",
                        fluidRow(
                            selectizeInput(
                                "singleEvent",
                                label = "Select Event (selection filtered by left panel)",
                                choices = NULL,
                                multiple = FALSE
                            ),
                            downloadButton("downloadeventresults", "Download as .CSV"),
                            reactableOutput("tableEventResults")
                        )
                    )
                ))
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    library(ggplot2)
    
    updateSelectizeInput(
        session, 
        "athletes", 
        choices = names, server = TRUE,
        #selected = c("ROGERS Martha (1995-08-23)", "NYE Katherine (1999-01-05)"),
        options = list(placeholder = "Search Athlete (last first)")
    )
    
    updateSelectizeInput(
        session, 
        "nations", 
        choices = countries, 
        server = TRUE,
        selected = c("USA", "CHN", "RUS"),
        options = list(placeholder = "Search Country Code e.g. 'USA'")
    )
    
    
    datasetInput <- reactive({
        athlete_ids <- athletes$athlete_id[if (length(input$athletes) > 0) athletes$name %in% str_split(input$athletes, " \\(", simplify = TRUE)[,1] else TRUE]
        output = list(
            athletes = athletes |>
                filter(
                    if (length(input$athletes) > 0) athlete_id %in% athlete_ids else TRUE,
                    if (length(input$nations) > 0) grepl(paste(input$nations, collapse = "|"), nations) else TRUE
                ),
            results = results |> 
                filter(
                    if (length(input$athletes) > 0) athlete_id %in% athlete_ids else TRUE,
                    if (length(input$nations) > 0) nation %in% input$nations else TRUE
                )
        )
        
        output
    }) |> bindCache(input$athletes, input$nations)
    
    datasetInputEvent <- reactive({
        events |> 
            filter(
                if (length(input$country) > 0) iso_code %in% input$country else TRUE,
                if (length(input$city) > 0) city %in% input$city else TRUE,
                if (length(input$age_group) > 0) age_group %in% input$age_group else TRUE,
                date >= input$date_range[1] & date <= input$date_range[2],
                if ("Olympics" %in% input$special) is_olympics == 1 else TRUE,
                if ("Universities" %in% input$special) is_university == 1 else TRUE,
            )
    }) |> bindCache(input$country, input$city, input$age_group, input$date_range, input$special)
    
    observeEvent(datasetInputEvent(), {
        df = datasetInputEvent() |> 
            arrange(desc(date))
        
        updateSelectizeInput(
            session,
            "singleEvent",
            choices = df$event,
            server = TRUE,
        )
    })
    
    datasetInputEventResults <- reactive({
        results |> 
            filter(
                input$singleEvent == event
            ) |> 
            select(-event, -event_id)
    }) |> bindCache(input$singleEvent)
    
    output$summary <- renderPrint({
        summary(datasetInput()$athletes)
    }) |> bindCache(input$athletes, input$nations)
    
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
    }) |> bindCache(input$athletes, input$nations)
    
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
    }) |> bindCache(input$athletes, input$nations)
    
    output$tableEventSearch <- renderReactable({
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
    }) |> bindCache(input$country, input$city, input$age_group, input$date_range, input$special)
    
    output$tableEventResults <- renderReactable({
        reactable(
            datasetInputEventResults(),
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
    
    output$plot <- renderPlot({
        df <- datasetInput()$results |>
            remove_missing() |> 
            distinct()
        
        base <- df |> 
            filter(if (input$category != "All") category == input$category else TRUE) |> 
            ggplot(aes(x = if (xaxis() == "Date") date else if (xaxis() == "Age") age else bw)) +
            theme_bw()
        
        base = base +
            xlab(xaxis()) +
            ylab("Total (kg)")
        
        
        
        if (length(input$nations) > 0 & length(input$athletes) == 0) {
            plot <- base +
                geom_point(aes(y = total, color = nation, shape = NULL), alpha = 0.5) +
                geom_smooth(aes(y = total, color = nation)) +
                labs(color = "Country")
        } else if (length(input$nations) == 0 & length(input$athletes) == 0) {
            if (input$category == "All") {
                plot <- base +
                    geom_bin2d(aes(y = total)) +
                    scale_fill_continuous(type = "viridis")
            } else {
                plot <- base +
                    geom_point(aes(y = total))
            }
        } else {
            plot = base +
                geom_point(
                    aes(y = total,
                        color = if (length(input$athletes) > 0 & length(input$athletes) <= 10) name else NULL,
                        shape = if (length(input$nations) > 0 & length(input$nations) <= 10) nation else NULL)) +
                geom_line(
                    aes(y = total,
                        color = if (length(input$athletes) > 0 & length(input$athletes) <= 10) name else NULL,
                        shape = if (length(input$nations) > 0 & length(input$nations) <= 10) nation else NULL)) +
                labs(color = "Athlete", shape = "Country")
        }
        
        plot
    })
    
    output$hist <- renderPlot({
        df = datasetInput()$results
        plot = ggplot(df,aes_string(input$histx), color = "red") + theme_bw()
        
        if (input$histbygender) {
            plot = plot + geom_histogram( # separate by gender
                aes(fill = gender),
                position="dodge"
            )
        } else {
            plot = plot + geom_histogram( # not separate
                fill = gg_color_hue(1)
            )
        }
        plot
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
    
    output$downloadeventresults <- downloadHandler(
        filename = "event-results.csv",
        function(file) {
            write_csv(datasetInputEventResults(), file)
        }
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)

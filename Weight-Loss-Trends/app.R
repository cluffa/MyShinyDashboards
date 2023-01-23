# Weight Loss Trend
library(shiny)

#library(tidyverse)
library(dplyr)
library(ggplot2)
library(lubridate)
library(readr)

library(shinydashboard)
library(reactable)
library(shinyjs)
library(shinyWidgets)

ui <- dashboardPage(
    title = "Weight Loss Tracking Dashboard",
    dashboardHeader(
        title = "Weight Loss Tracking Dashboard"
    ),
    dashboardSidebar(
        useShinyjs(),
        a(
            p(
                "Github Link",
                style = "
                    font-size: 1em;
                    margin: 10px
                "
            ),
            href = "https://github.com/cluffa/MyShinyDashboards",
            
        ),
        collapsed = TRUE
    ),
    dashboardBody(
        fluidRow(
            tabBox(
                tabPanel(
                    title = "Body Weight",
                    plotOutput(
                        "plot1",
                        height = "500px",
                    ),
                ),
                tabPanel(
                    title = "Caloric Deficit/Excess",
                    plotOutput("calPlot", height = "500px"),
                ),
                height = "580px"
            ),
            tabBox(
                id = "tabs",
                height = "580px",
                tabPanel(
                    title = "Options",
                    radioButtons(
                        "drType",
                        label = "Date Range Type:",
                        choices = c(
                            "Preset",
                            "Range Input",
                            "Date Range Selector",
                            "Date Range Slider"
                        ),
                        selected = "Preset",
                        inline = TRUE
                    ),
                    numericInput(
                        "drNum",
                        label = "Date Range:",
                        value = 3,
                        width = "100px"
                    ),
                    radioButtons(
                        "drUnit",
                        label = NULL,
                        choices = c(
                            "Days",
                            "Weeks",
                            "Months",
                            "Years"
                        ),
                        selected = "Months",
                        inline = TRUE
                    ),
                    radioButtons(
                        "drSimple",
                        label = "Date Range:",
                        choices = c(
                            `30 Days` = Sys.Date() - 30,
                            `90 Days` = Sys.Date() - 90,
                            `1 Year` = Sys.Date() - 365,
                            `2 Years` = Sys.Date() - 365*2,
                            `3 Years` = Sys.Date() - 365*3,
                            `All Time` = Sys.Date() - 99999
                        ),
                        selected = c(`90 Days` = Sys.Date() - 90),
                        inline = TRUE
                    ),
                    dateRangeInput(
                        "drSelector",
                        label = "Date Range:",
                        start = Sys.Date() - 90,
                        end = Sys.Date()
                    ),
                    sliderInput(
                        "drSlider",
                        "Dates:",
                        min = as.Date("2021-01-01","%Y-%m-%d"),
                        max = Sys.Date(),
                        value = c(Sys.Date() - 90 ,Sys.Date()),
                        timeFormat="%Y-%m-%d"
                    ),
                    sliderInput(
                        "smoothing",
                        label = "Spline Smoothing:",
                        min = 0,
                        max = 1,
                        step = 0.05,
                        value = 0.5,
                    ),
                    awesomeCheckbox(
                        "showGoal",
                        label = "Show Projected Goal on Body Weight Plot",
                        status = "primary"
                    ),
                    awesomeCheckbox(
                        "showMM",
                        label = "Show Min/Max Weights",
                        status = "primary"
                    ),
                ),
                tabPanel(
                    "Stats",
                    verbatimTextOutput("stats"),
                ),
                tabPanel(
                    "Model Stats",
                    verbatimTextOutput("models"),
                    tags$head(tags$style("#models{font-size: 12px;}")),
                    #style="height: 500px ;overflow-y: scroll;"
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
                ),
                tabPanel(
                    "Info",
                    p("This dashboard was created so that I could easily track my weight over time.
                        I aim for a specific average pounds lost per week over the last 30 or 90 days.
                        With this dashboard I can easily view those trends and adjust accordingly."),
                    p("Because the observations are not spread evenly, I created a spline to
                        approximate the weight with values spread exactly 24 hours apart.
                        The linear regression line is fit using those points. The slope is then used
                        to predict when I will meet my goal weight.
                        ")
                )
            ),

        )
    )
)

server <- function(input, output) {
    df <- read_csv(
        "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
        col_names = c("date", "weight", "unit", "fat", "lean"),
        col_types = "cncnn",
        skip = 1
    ) |> arrange(
        date
    ) |> mutate(
        bfp = round(fat/lean * 100, 1)
    )
    
    df$date <- as.POSIXct(df$date) |> force_tz(tzone = "EST")
    
    spl <- reactive({
        smooth.spline(df$date, df$weight, spar = input$smoothing)
    })
    
    rng_start <- floor_date(df$date[1], "days")
    rng_stop <- ceiling_date(tail(df$date, 1), "days")

    rng <- seq(rng_start, rng_stop, by = "24 hours")
    
    pred <- reactive({
        predict(spl(), as.numeric(rng))
    })
    
    hide("drSelector")
    hide("drSlider")
    hide("drNum")
    hide("drUnit")
    get_date_range <- reactive({
        type = input$drType
        
        if(type == "Date Range Selector") {
            hide("drSimple")
            show("drSelector")
            hide("drSlider")
            hide("drNum")
            hide("drUnit")
  
            return(as.POSIXct(input$drSelector))

        } else if(type == "Date Range Slider") {
            hide("drSimple")
            hide("drSelector")
            show("drSlider")
            hide("drNum")
            hide("drUnit")
  
            return(as.POSIXct(input$drSlider))
          
        } else if(type == "Range Input") {
            hide("drSimple")
            hide("drSelector")
            hide("drSlider")
            show("drNum")
            show("drUnit")
            
            return(
                as.POSIXct(c(
                    Sys.Date() - period(input$drNum, tolower(input$drUnit)),
                    Sys.Date()
                ))
            )
          
        } else {
            show("drSimple")
            hide("drSelector")
            hide("drSlider")
            hide("drNum")
            hide("drUnit")
            
            return(
                c(
                    as.POSIXct(input$drSimple),
                    as.POSIXct(Sys.Date())
                )
            )
          
        }
        
    })
    
    get_data <- reactive({
        df <- arrange(df, desc(date))
        df
    })
    
    get_df <- reactive({
        range <- get_date_range()
        df <- get_data()
        df <- df[df$date >= range[1] & df$date <= range[2] + 86400,]
        df
    })
    
    get_model <- reactive({
        pred <- get_spline_pred_in_range()
        df <- get_df()
        lm(weight ~ date, pred)
    })
    
    get_cals <- reactive({
        pred <- pred()
        c(diff(pred$y), NA) * 3500
    })
    
    get_spline_pred_in_range <- reactive({
        pred <- pred()
        range <- get_date_range()
        cals <- get_cals()
    
        in_rng <- pred$x >= range[1] & pred$x <= range[2] + 86400
        
        data.frame(
            date = as.POSIXct(pred()$x[in_rng], origin = "1970-1-1"),
            weight = pred$y[in_rng],
            cals = cals[in_rng]
        )
    })
    
    get_where_cals_is_zero <- reactive({
        pred <- get_spline_pred_in_range()
        as.POSIXct(
            uniroot.all(
                approxfun(pred$date, pred$cals),
                interval = range(as.numeric(pred$date))
            ),
            origin = "1970-1-1"
        )
    })
    
    get_gw <- reactive({
        input$goalwt
    })
    
    get_daily_avg <- reactive({
        transmute(
                get_data(),
                date = date(date),
                weight = weight
            ) |> group_by(
                date
            ) |> summarise(
                weight = round(mean(weight), 1)
            ) |> arrange(
                desc(date)
            )
    })
    
    output$calPlot <- renderPlot({
        df <- get_spline_pred_in_range()
       
        p <- ggplot(df) +
            geom_hline(yintercept = 0, color = "red") +
            geom_line(aes(date, cals), color = "blue") +
            theme_bw() +
            ylab("calories") +
            labs(
                subtitle = "change spline smoothing to 0.65 to best reflect actual diet changes",
            )
        
        return(p)
    })
    
    output$plot1 <- renderPlot({
        range <- get_date_range()
        df <- get_df()
        gw_df <- get_goal_date()
        spl <- get_spline_pred_in_range()
        
        mm <- df[c(which.max(df$weight), which.min(df$weight)),]
        mm$label <- c("max", "min")
        
        p <- ggplot() +
            geom_point(
                aes(y = weight, x = date, color = "Observed Weight"),
                data = df,
            ) +
            geom_line(
                aes(date, weight, color = "Spline Fit"),
                data = spl,
                linewidth = 1
            ) +
            scale_color_manual(
                name = NULL,
                breaks = c(
                    "Observed Weight",
                    "Spline Fit",
                    "Linear Model",
                    "Projected Goal Date"
                ),
                values = c(
                    "Observed Weight" = "darkgray",
                    "Spline Fit" = "blue",
                    "Linear Model" = "red",
                    "Projected Goal Date" = "green"
                )
            ) +
            theme_bw() +
            theme(
                legend.position = c(0.18, 0.1),
                legend.background = element_rect(fill = "transparent")
            )
        
        if (input$showGoal) {
            p <- p + geom_point(
                aes(y = weight, x = date, color = "Projected Goal Date"),
                data = gw_df,
                size = 3
            ) + 
            geom_smooth(
                aes(x = date, y = weight, color = "Linear Model"),
                data = bind_rows(spl, gw_df),
                method = "lm",
                linetype = 2,
                linewidth = 1,
                formula = y ~ x,
                se = FALSE
            ) +
            geom_text(
                aes(
                    x = date,
                    y = weight,
                    label = format(date, "%Y-%m-%d"),
                    fontface = "bold"
                ),
                data = gw_df,
                vjust = "outward",
                hjust = "inward",
                nudge_y = -1
            )
        } else {
            p <- p +
                geom_smooth(
                    aes(x = date, y = weight, color = "Linear Model"),
                    data = spl,
                    method = "lm",
                    linetype = 2,
                    linewidth = 1,
                    formula = y ~ x,
                    se = FALSE
                )
        }
        
        if (input$showMM) {
            p <- p +
                geom_text(
                    aes(
                        x = date,
                        y = weight,
                        label = paste(" ", weight, label, " "), 
                        fontface = "bold"
                    ),
                    data = mm,
                    hjust = "inward",
                    vjust = "outward",
                ) +
                geom_point(
                        aes(
                            x = date,
                            y = weight,
                        ),
                        data = mm,
                        size = 2
                ) 
        }
        
        return(p)
    })
    
    get_goal_date <- reactive({
        gw <- get_gw()
        model <- get_model()
        gwdate <- (gw - model$coefficients[1])/model$coefficients[2]
        
        data.frame(
            weight = gw,
            date = as.POSIXct(gwdate, origin = "1970-1-1")
        )
    })
    
    output$summary <- renderPrint({
        model <- get_model()
        coef <- unname(model$coefficients[2])*86400
        spl <- get_spline_pred_in_range()
        gw <- get_gw()
        
        dif = tail(spl$weight, n = 1) - spl$weight[1] 
        
        cals = (dif * 3500) / nrow(spl)
        
        gwdate <- (gw - model$coefficients[1])/model$coefficients[2]
        gwdate <- as.Date(as.POSIXct.numeric(gwdate, origin = "1970-1-1"))
        weeks <- round((gwdate - Sys.Date()) / 7, 1)
        
        cat(
            "Daily Trend:", coef, "lbs/day",
            "\nWeekly Trend:", coef*7, "lbs/week",
            "\nGoal Projection:", as.character(gwdate), 
            paste0("(", as.character(weeks), " Weeks)"),
            "\nAvg Daily Diff From Net Calories:", round(cals, 0)
            )
        
    })
    
    output$stats <- renderPrint({
        df <- get_df() |> 
            mutate(
                date = format(date, "%Y-%m-%d")
            )
        full_df <- get_data() |> 
            mutate(
                date = format(date, "%Y-%m-%d")
            )
        
        range_52 <- c(Sys.Date() - (52*7), Sys.Date() + 1)
        df_52 <- full_df[full_df$date >= range_52[1] & full_df$date <= range_52[2],]
        
        df$date <- as.character(df$date)
        full_df$date <- as.character(full_df$date)
        
        range <- get_date_range()
        range <- as.Date(range)
        range <- range[2] - range[1]
        suppressWarnings(
            cat(
                "    Range Low:", min(df$weight), "on", df$date[which.min(df$weight)],
                "\n   Range High:", max(df$weight), "on", df$date[which.max(df$weight)],
                "\n   Range Mean:", round(mean(df$weight, na.rm = TRUE),1),
                "\n All Time Low:", min(full_df$weight), "on", full_df$date[which.min(full_df$weight)],
                "\nAll Time High:", max(full_df$weight), "on", full_df$date[which.max(full_df$weight)],
                "\nAll Time Mean:", round(mean(full_df$weight, na.rm = TRUE),1)
            )
        )
    })
    
    output$models <- renderPrint({
        model <- get_model()
        spl <- spl()
        print(spl)
        summary(model)
    })
    
    output$table <- renderReactable({
        df1 <- get_daily_avg()
        reactable(
            df1,
            striped = TRUE,
            compact = TRUE,
            wrap = FALSE,
            showSortable = TRUE,
            defaultPageSize = 15,
            showPageSizeOptions = FALSE,
            pageSizeOptions = c(25,50,100),
            outlined = TRUE,
            resizable = FALSE,
        )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Weight Loss Trend
library(shiny)
library(shinydashboard)
library(reactable)
library(shinyjs)
library(shinyWidgets)

ui <- dashboardPage(
    title = "Weight Loss Tracking Dashboard",
    dashboardHeader(
        title = "Weight Loss Tracking Dashboard"
    ),
    {dashboardSidebar(
        useShinyjs(),
        tags$head(
            tags$style(HTML(".sidebar {
                      height: 90vh; overflow-y: auto;
                    }"
            ) # close HTML       
            )            # close tags$style
        ),             # close tags#Head
        fluidRow(
            style = "margin: 3px",
            actionBttn(
                "test",
                label = "View on Github",
                onclick ="window.open('https://github.com/cluffa/MyShinyDashboards', '_blank')",
                style = "simple",
                color = "warning"
            ),
            radioButtons(
                "drType",
                label = "Date Range Type:",
                choices = c(
                    "Preset",
                    "Range Input",
                    "Date Range Selector"
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
                    `2 Years` = Sys.Date() - 365 * 2
                ),
                selected = c(`90 Days` = Sys.Date() - 90),
                inline = TRUE
            ),
            dateRangeInput(
                "drSelector",
                label = "Date Range:",
                start = as.Date("2023-01-01", "%Y-%m-%d"),
                end = Sys.Date()
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
            awesomeCheckbox(
                "model30",
                value = TRUE,
                label = "Fit Linear Model on Last 30 Days Only",
                status = "primary"
            ),
            sliderInput(
                "fitdays",
                label = "Adjust the Number of Days (if box checked above):",
                min = 7,
                max = 50,
                step = 1,
                value = 30,
            ),
            sliderInput(
                "mult",
                label = "Adjust BMR activity multiplier:",
                min = 1.0,
                max = 1.75,
                step = 0.01,
                value = 1.40,
            )
        ),
        collapsed = FALSE
    )}, # sidebar
    {dashboardBody(
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
                    "Stats",
                    h4("Weight Stats"),
                    verbatimTextOutput("stats"),
                    h4("Trends and Goal Projection"),
                    sliderInput(
                        "goalwt",
                        label = "Goal Weight:",
                        min = 185,
                        max = 235,
                        value = 225,
                        step = 5
                    ),
                    verbatimTextOutput("summary")
                ),
                tabPanel(
                    "Model Stats",
                    verbatimTextOutput("models"),
                    tags$head(tags$style("#models{font-size: 12px;}"))
                ),
                tabPanel(
                    "Table",
                    reactableOutput("table")
                ),
                tabPanel(
                    "Info",
                    strong("BMR Activity Multiplier"),
                    p("1.2 - Inactive Desk Job", style = "margin: 0px;"),
                    p("1.375 - Low 1-3 days a week 1hr", style = "margin: 0px;"),
                    p("1.55	- Medium 3-5 days a week 1hr", style = "margin: 0px;"),
                    p("1.65 - Medium-high 6-7days a week 1hr", style = "margin: 0px;"),
                    p("1.725 - High Twice a day heavy 1hr sessions", style = "margin: 0px;"),
                    p("1.9 - Intense Athlete 1.5-2.5hr sessions or activities", style = "top-margin: 0px;"),
                    strong("BMR/TDEE Formula used"),
                    p("Mifflin-St Jeor equation", a(href = "https://pubmed.ncbi.nlm.nih.gov/2305711/", "https://pubmed.ncbi.nlm.nih.gov/2305711/")),
                    p("Men: (10 × weight in kg) + (6.25 × height in cm) - (5 × age in years) + 5"),
                    p("Women: (10 × weight in kg) + (6.25 × height in cm) - (5 × age in years) - 161"),
                    strong("Background"),
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
    )} # body
)

server <- function(input, output) {
    hide("drSelector")
    hide("drNum")
    hide("drUnit")
    
    #library(tidyverse)
    library(dplyr)
    library(ggplot2)
    library(lubridate)
    library(readr)
    
    KGCONST <- 0.4536 # lbs to kg
    HEIGHT <- 188 # cm
    AGE <- decimal_date(Sys.Date()) - decimal_date(as_date("1997-07-22"))
    
    mifflin <- function(weight, isMale = TRUE) {
       (10 * weight * KGCONST) + (6.25 * HEIGHT) - (5 * AGE) + ifelse(isMale, 5, -161)
    }
    
    df <- {read_csv(
            "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1991942286&format=csv",
            col_names = c("date", "weight"),
            col_types = "cn---",
            skip = 1,
            lazy = TRUE
        ) |> mutate(
            date = as_datetime(date) |> force_tz(tzone = "EST")
        ) |> arrange(
            date
        ) |> select(
            date,
            weight
        )}
    
    loseit <- {read_csv(
            "https://docs.google.com/spreadsheets/d/151vhoZ-kZCnVfIQ7h9-Csq1rTMoIgsOsyj_vDRtDMn0/export?gid=1838432377&format=csv",
            skip = 1,
            col_names = c("date", "budget", "food", "exercise", "net", "difference", "weight", "weighed"),
            col_types = "c-n---n-",
        ) |> mutate(
            date = as_datetime(date, format = "%m/%d/%y"),
            food = if_else(food < 1100, NA_real_, food)
        )}
    
    get_loseit <- reactive({
        out <- loseit |> mutate(
                bmr = mifflin(weight),
                tdee = bmr * input$mult,
                diff = food - tdee
            )
        
        return(out)
    }) # |> bindCache(input$bfp, input$mult)
    
    df_desc <- arrange(df, desc(date))

    spl <- reactive({
        smooth.spline(df$date, df$weight, spar = input$smoothing)
    }) # |> bindCache(input$smoothing)
    
    pred <- reactive({
        rng_start <- floor_date(df$date[1], "days")
        rng_stop <- ceiling_date(tail(df$date, 1), "days")
        rng <- seq(rng_start, rng_stop, by = "24 hours")
        predict(spl(), as.numeric(rng))
    }) # |> bindCache(spl())
    
    observeEvent(input$drType, {
        type = input$drType
        if(type == "Date Range Selector") {
            hide("drSimple")
            show("drSelector")
            hide("drNum")
            hide("drUnit")
        } else if(type == "Range Input") {
            hide("drSimple")
            hide("drSelector")
            show("drNum")
            show("drUnit")
        } else {
            show("drSimple")
            hide("drSelector")
            hide("drNum")
            hide("drUnit")
        }
    })

    get_date_range <- reactive({
        type = input$drType
        
        if(type == "Date Range Selector") {
            return(as.POSIXct(input$drSelector))

        } else if(type == "Range Input") {
            return(
                as.POSIXct(c(
                    Sys.Date() - period(input$drNum, tolower(input$drUnit)),
                    Sys.Date()
                ))
            )

        } else {
            return(
                c(
                    as.POSIXct(input$drSimple),
                    as.POSIXct(Sys.Date())
                )
            )
          
        }
    }) # |> bindCache(input$drType, input$drSelector, input$drNum, input$drUnit, input$drSimple)
    
    get_df <- reactive({
        range <- get_date_range()
        df <- df_desc
        df <- df[df$date >= range[1] & df$date <= range[2] + 86400,]
        df
    }) # |> bindCache(get_date_range())
    
    get_loseit_in_range <- reactive({
        loseit <- get_loseit()
        range <- get_date_range()
        loseit[loseit$date >= range[1] & loseit$date <= range[2] + 86400,]
    }) # |> bindCache(get_date_range(), get_loseit())
    
    get_model <- reactive({
        shorten <- shorten()
        pred <- get_spline_pred_in_range() |> shorten()
        lm(weight ~ date, pred)
    }) # |> bindCache(shorten(), get_spline_pred_in_range())
    
    get_cals <- reactive({
        pred <- pred()
        c(diff(pred$y), NA) * 3500
    }) # |> bindCache(pred())
    
    get_spline_pred_in_range <- reactive({
        pred <- pred()
        range <- get_date_range()
        cals <- get_cals()
    
        in_rng <- pred$x >= range[1] & pred$x <= range[2] + 86400
        
        data.frame(
            date = as.POSIXct(pred$x[in_rng], origin = "1970-1-1"),
            weight = pred$y[in_rng],
            cals = cals[in_rng]
            )
    }) # |> bindCache(pred(), get_date_range(), get_cals())
    
    get_gw <- reactive({
        input$goalwt
    })
    
    output$calPlot <- renderPlot({
        df <- get_spline_pred_in_range()
        loseit <- get_loseit_in_range()
        
        p <- ggplot(df) +

            geom_point(
                aes(date, diff),
                data = loseit,
                color = "darkgray"
                ) +

            geom_hline(
                yintercept = 0,
                color = "red"
                ) +
            geom_line(
                aes(date, cals),
                color = "blue"
                ) +
            theme_bw() +
            ylab("calories per day") +
            scale_y_continuous(
                sec.axis = sec_axis(~ . / 500, name = "pounds per week", breaks = seq(-4, 4, by = 1)),
                breaks = seq(-5000, 5000, by = 500)
            )
        
        return(p)
    }) # |> bindCache(get_loseit_in_range(), get_spline_pred_in_range(), sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.1))
    
    get_mm <- reactive({
        df <- get_df()
        mm <- df[c(which.max(df$weight), which.min(df$weight)),]
        mm$label <- c("max", "min")
        
        return(mm)
    }) # |> bindCache(get_df())
    
    output$plot1 <- renderPlot({
        shorten <- shorten()
        range <- get_date_range()
        df <- get_df()
        gw_df <- get_goal_date()
        spl <- get_spline_pred_in_range()
        
        mm <- get_mm()
        
        leg <- c(
            "Observed Weight" = "darkgray",
            "Spline Fit" = "blue",
            "Linear Model" = "red",
            "Projected Goal Date" = "green"
        )
        
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
                breaks = names(leg),
                values = leg
                ) +
            scale_y_continuous(
                breaks = seq(0, 500, by = 5)
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
                data = bind_rows(spl, gw_df) |> shorten(extra = 1),
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
                    data = spl |> shorten(),
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
    }) # |> bindCache(shorten(), get_date_range(), get_df(), get_goal_date(), get_spline_pred_in_range(), get_mm(), shorten(), input$showGoal, input$showMM, sizePolicy = sizeGrowthRatio(width = 400, height = 400, growthRate = 1.1))
    
    shorten <- reactive({
        function(df, extra = 0) {
            if (input$model30) {
                return(tail(df, n = input$fitdays + extra))
            } else {
                return(df)
            }
        }
    }) # |> bindCache(input$model30, input$fitdays)
    
    get_goal_date <- reactive({
        gw <- get_gw()
        model <- get_model()
        gwdate <- (gw - model$coefficients[1])/model$coefficients[2]
        
        data.frame(
            weight = gw,
            date = as.POSIXct(gwdate, origin = "1970-1-1")
        )
    }) # |> bindCache(get_gw(), get_model())
    
    get_cw <- reactive({
        round(tail(get_spline_pred_in_range()$weight, n = 1), 1)
    }) # |> bindCache(get_spline_pred_in_range())
    
    output$summary <- renderPrint({
        model <- get_model()
        coef <- unname(model$coefficients[2])*86400
        gw <- get_gw()
        
        if (input$model30) {
            loseit <- get_loseit_in_range()
            loseit <- loseit[loseit$date > (Sys.Date() - input$fitdays),]
        } else {
            loseit <- get_loseit_in_range()
        }
            
        cals <- coef * 3500
        
        gwdate <- (gw - model$coefficients[1])/model$coefficients[2]
        gwdate <- as.Date(as.POSIXct.numeric(gwdate, origin = "1970-1-1"))
        weeks <- round((gwdate - Sys.Date()) / 7, 1)
        
        food_mean <- mean(loseit$food)
        tdee_mean <- mean(loseit$tdee)
        est_act <- (food_mean - cals)/(tdee_mean/input$mult)
        
        cat(
            ifelse(
                input$model30,
                paste("Linear Model Fit on Last", input$fitdays, "days"),
                "Linear Model Fit on Selected Date Range"
            ),
            "\nDaily Trend:", coef |> round(2), "lbs/day",
            "\nWeekly Trend:", (coef*7) |> round(2), "lbs/week",
            "\nGoal Projection:", as.character(gwdate),
            paste0("(", as.character(weeks), " Weeks)"),
            "\nAvg Daily Diff Based on Trend:", round(cals, 0),
            "\nDays Not Tracked (NA Intake):", loseit$food |> is.na() |> sum(),
            "\nAvg Daily Intake:", food_mean |> round(),
            "\nAvg Est. TDEE (BMR*Activity):", tdee_mean |> round(),
            "\nAvg Daily Diff Based on Intake:", mean(loseit$diff) |> round(),
            "\nEst. BMR Activity Mult.", paste0("(",input$mult," set):"), est_act |> round(2)
            )
    }) # |> bindCache(get_loseit(), get_model(), get_gw(), input$model30, input$fitdays, input$mult)
    
    output$stats <- renderPrint({
        df <- get_df() |> 
            mutate(
                date = format(date, "%Y-%m-%d")
            )
        
        df$date <- as.character(df$date)
    
        suppressWarnings(
            cat(
                "Current Weight:", get_cw(), "lbs",
                "\nRange Low:", min(df$weight), "on", df$date[which.min(df$weight)],
                "\nRange High:", max(df$weight), "on", df$date[which.max(df$weight)],
                "\nRange Mean:", round(mean(df$weight, na.rm = TRUE),1)
            )
        )
    }) # |> bindCache(get_df(), get_date_range(), get_cw())

    output$models <- renderPrint({
        model <- get_model()
        spl <- spl()
        print(spl)
        summary(model)
    }) # |> bindCache(get_model(), spl())
    
    output$table <- renderReactable({
        data_frame <- get_spline_pred_in_range()
        
        data_frame[nrow(data_frame):1,] |>
            mutate(
                date = as_date(date),
                weight = round(weight, 1),
                cals = round(cals)
            ) |>
            reactable(
                striped = TRUE
            )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)


# Load packages ----
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library(quantmod)

# Load data ----
#ncov_tbl <- readRDS("ncov_tbl.rds")
#chn_prov <- readRDS("chn_prov.rds")



# Define tab-based UI
ui <- navbarPage(
    theme = shinytheme("united"),
    "2019-20 Coronavirus Outbreak",
    
    #
    tabPanel(
        titlePanel("Plot one case type on a specific date"),
        dateInput("date1", "Select the date:", "2020-02-14"),
        checkboxGroupInput("case1", "Select the case type:", c("confirmed", 
                      "recovered", "death"), selected = "confirmed")
    ),
    
    mainPanel(
        plotOutput("plotaday")
    ),
    
    #
    tabPanel(
        titlePanel("Plot the line graph of one case type over time"),
        dateRangeInput("dates2",
                       "Date range",
                       start = "2020-01-21",
                       end = as.character(Sys.Date())
        ),
        checkboxGroupInput("case2", "Select the case type:", c("confirmed", 
                      "recovered", "death"), selected = "confirmed")
    ),
    
    mainPanel(
        plotOutput("plotvstime")
    ),
    
    #
    tabPanel(
        titlePanel("Animation"),
        dateRangeInput("dates3",
                       "Date range",
                       start = "2020-01-21",
                       end = as.character(Sys.Date())
        ),
        checkboxGroupInput("case3", "Select the case type:", c("confirmed", 
                    "recovered", "death"), selected = "confirmed")
    ),
    
    mainPanel(
        plotOutput("animation")
    ),
    
    #
    tabPanel(
        titlePanel("Select a stock to examine.
                 Information will be collected from yahoo finance"),
        textInput("symb", "Symbol", "GOOG"),
        dateRangeInput("dates4",
                       "Date range",
                       start = "2020-01-21",
                       end = as.character(Sys.Date())
        ),
        br(),
        br(),
        checkboxInput("log", "Plot y axis on log scale",
                      value = FALSE),
        checkboxInput("adjust",
                      "Adjust prices for inflation", value = FALSE)
    ),
    mainPanel(plotOutput("plotstock"))
)

# 
server <- function(input, output) {
    #
    output$plotaday <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau", 
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date == input$date1, Case == input$case1) %>%
            group_by(`Province/State`) %>%
            top_n(1, Date) %>%
            right_join(chn_prov, by = c("Province/State" = "NAME_ENG")) %>%
            ggplot() +
            geom_sf(mapping = aes(fill = Count, geometry = geometry)) +
            scale_fill_gradientn(colors = wes_palette("Zissoul", 100, 
                                type = "continuous"), trans = "log10") + 
            theme_bw() + 
            labs(title = str_c(input$case1, "cases"), subtitle = input$date1)
    })
    
    #
    output$plotvstime <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau", 
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date >= input$dates2[1], Date <= input$dates[2], 
                   Case == input$case2) %>%
            group_by(Date, Case) %>% 
            summarise(total_count = sum(Count)) %>% 
            ggplot() + 
            geom_line(mapping = aes(x = Date, y = total_count, color = Case),
                      size = 2) + 
            scale_color_manual(values = c("blue", "black", "greem")) + 
            scale_y_log10() + 
            labs(y = "Count") +
            theme_bw()
    })
    
    #
    output$animation <- renderPlot({
        ncov_tbl %>% 
            filter(`Country/Region` %in% c("Mainland China", "Macau", 
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date >= input$dates3[1], Date <= input$dates3[2], 
                   Case == input$case3) %>% 
            right_join(chn_prov, by = c("Province/State" = "NAME_ENG")) %>% 
            ggplot() +
            geom_sf(mapping = aes(fill = Count, geometry = geometry)) +
            scale_fill_gradientn(colors = wes_palette("Zissoul", 100, 
                                 type = "continuous"), trans = "log10") + 
            theme_bw() + 
            labs(title = str_c(case, "cases"))
    })
    
    #
    dataInput <- reactive({
        getSymbols(
            input$symb, src = "yahoo", 
            from = input$dates4[1], 
            to = input$dates4[2],
            auto.assign = FALSE
        )
    })
    output$plotstock <- renderPlot({
        chartSeries(dataInput(), theme = chartTheme("white"), 
                    type = "line", log.scale = input$log, TA = NULL)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

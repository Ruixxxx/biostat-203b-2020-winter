# Load packages ----
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library(gapminder)
library(gganimate)
library(transformr)
library(quantmod)

# Load data ----
ncov_tbl <- readRDS("ncov_tbl.rds")
chn_prov <- readRDS("chn_prov.rds")



# Define tab-based UI
ui <- navbarPage(
    theme = shinytheme("united"),
    "2019-20 Coronavirus Outbreak",
    
    #
    tabPanel(
        "A day",
        titlePanel("Plot one case type on a specific date"),
        sidebarLayout(
            sidebarPanel(
                dateInput("date1", "Select the date:", "2020-02-14"),
                radioButtons("case1", "Select the case type:", c("confirmed", 
                             "recovered", "death"), selected = "confirmed")    
            ),
            mainPanel(
                plotOutput(outputId = "plotaday"),
                plotOutput(outputId = "plotaday2")
            )
        )
    ),
    
    #
    tabPanel(
        "Over time",
        titlePanel("Plot the line graph of one case type over time"),
        sidebarLayout(
            sidebarPanel(
                dateRangeInput("dates2",
                               "Date range",
                               start = "2020-01-21",
                               end = as.character(Sys.Date())
                ),
                radioButtons("case2", "Select the case type:", c("confirmed", 
                        "recovered", "death", "all"), selected = "confirmed")    
            ),
            mainPanel(
                plotOutput(outputId = "plotvstime")
            )
        )
    ),
    
    #
    tabPanel(
        "Animation",
        titlePanel("Animation"),
        sidebarLayout(
            sidebarPanel(
                dateRangeInput("dates3",
                               "Date range",
                               start = "2020-01-21",
                               end = as.character(Sys.Date())
                ),
                radioButtons("case3", "Select the case type:", c("confirmed", 
                             "recovered", "death"), selected = "confirmed")    
            ),
            mainPanel(
                imageOutput(outputId = "animation")
            )
        )
        
    ),
    
    #
    tabPanel(
        "Stock",
        titlePanel("Select a stock to examine.
                 Information will be collected from yahoo finance"),
        sidebarLayout(
            sidebarPanel(
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
            mainPanel(plotOutput(outputId = "plotstock"))
        )
    )
    
    # # previous outbreaks' information
    # tabPanel(
    #     titlePanel("Select a outbreak"), 
    #     checkboxGroupInput("outbreak", "Select a outbreak:", c("2003 SARS", 
    #                        "2009 H1N1", "2012 MERS", "2014 Ebola"), 
    #                        selected = "2003 SARS")
    # ),
    # mainPanel(plotOutput("outbreak"))
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
            scale_fill_gradientn(colors = wes_palette("Zissou1", 100,
                                type = "continuous"), trans = "log10") +
            theme_bw() +
            labs(title = str_c(input$case1, " cases"), subtitle = input$date1)
    })
    output$plotaday2 <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau",
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date == input$date1) %>%
            ggplot() +
            geom_col(mapping = aes(x = `Province/State`, y = `Count`,
                                   fill = `Case`)) +
            scale_y_log10() +
            labs(title = input$date1) +
            theme(axis.text.x = element_text(angle = 90))
    })

    #
    output$plotvstime <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau",
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date >= input$dates2[1], Date <= input$dates2[2]) %>%
            filter(if (input$case2 != "all") Case == input$case2
                   else Case == "confirmed" | Case == "recovered" | 
                        Case == "death") %>%
            group_by(Date, Case) %>%
            summarise(total_count = sum(Count)) %>%
            ggplot() +
            geom_line(mapping = aes(x = Date, y = total_count, color = Case),
                      size = 2) +
            scale_color_manual(values = c("blue", "black", "green")) +
            scale_y_log10() +
            labs(y = "Count") +
            theme_bw()
    })

    #
    output$animation <- renderImage({
        p <- ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau",
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date >= input$dates3[1], Date <= input$dates3[2],
                   Case == input$case3) %>%
            right_join(chn_prov, by = c("Province/State" = "NAME_ENG")) %>%
            ggplot() +
            geom_sf(mapping = aes(fill = Count, geometry = geometry)) +
            scale_fill_gradientn(colors = wes_palette("Zissou1", 100,
                                 type = "continuous"), trans = "log10") +
            theme_bw() +
            labs(title = str_c(input$case3, " cases"))
        anim <- p + 
            transition_time(Date) + 
            labs(title = str_c(input$case3, " cases"), 
                 subtitle = "Date: {frame_time}")
        anim_save("anim.gif", animate(anim, renderer = gifski_renderer()))
        
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

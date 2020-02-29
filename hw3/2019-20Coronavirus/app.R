# Load packages ----
library(shiny)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(wesanderson)
library(gapminder)
library(gganimate)
library(quantmod)

# Load data ----
ncov_tbl <- readRDS("ncov_tbl.rds")
chn_prov <- readRDS("chn_prov.rds")
countries <- readRDS("countries.rds")


# Define tab-based UI
ui <- navbarPage(
    theme = shinytheme("united"),
    "2019-20 Coronavirus Outbreak",
    
    # Plot a day in China
    tabPanel(
        "A day (CN)",
        titlePanel("Plot one day's Coronavirus information in China"),
        sidebarLayout(
            sidebarPanel(
                helpText("Plot one case type on a specific date vs province"),
                dateInput("date1", "Select the date:", "2020-02-14"),
                radioButtons("case1", "Select the case type:", c("confirmed", 
                             "recovered", "death"), selected = "confirmed")    
            ),
            mainPanel(
                plotOutput(outputId = "plotaday")
            )
        ),
        sidebarLayout(
            sidebarPanel(
                helpText("Plot three case types on a specific date 
                         vs province"),
                dateInput("date2", "Select the date:", "2020-02-14")   
            ),
            mainPanel(
                plotOutput(outputId = "plotaday2")
            )
        )
    ),
    
    # Plot over time in China
    tabPanel(
        "Over time (CN)",
        titlePanel("Plot the line graph of one/all case type(s) over time 
                   in China"),
        sidebarLayout(
            sidebarPanel(
                dateRangeInput("dates3",
                               "Date range",
                               start = "2020-01-21",
                               end = as.character(Sys.Date())
                ),
                radioButtons("case3", "Select the case type:", c("confirmed", 
                        "recovered", "death", "all"), selected = "confirmed")    
            ),
            mainPanel(
                plotOutput(outputId = "plotvstime")
            )
        )
    ),
    
    # Animation over time
    tabPanel(
        "Animation (CN)",
        titlePanel("Animation over time in China"),
        sidebarLayout(
            sidebarPanel(
                radioButtons("animation", "Select the animation:", 
                             c("confirmed", "recovered", "death", "data"), 
                             selected = "confirmed")    
            ),
            mainPanel(
                imageOutput(outputId = "gif")
            )
        )
    ),
    
    # Plot a day in the world
    tabPanel(
        "A day (national)",
        titlePanel("Plot one case type on a specific date vs country"),
        sidebarLayout(
            sidebarPanel(
                dateInput("date4", "Select the date:", "2020-02-14"),
                radioButtons("case4", "Select the case type:", c("confirmed", 
                             "recovered", "death"), selected = "confirmed")    
            ),
            mainPanel(
                plotOutput(outputId = "plotadayn")
            )
        )
    ),
    
    # Plot over time in the world
    tabPanel(
        "Over time (national)",
        titlePanel("Plot the line graph of one/all case type(s) over time 
                   in the world"),
        sidebarLayout(
            sidebarPanel(
                dateRangeInput("dates5",
                               "Date range",
                               start = "2020-01-21",
                               end = as.character(Sys.Date())
                ),
                radioButtons("case5", "Select the case type:", c("confirmed", 
                        "recovered", "death", "all"), selected = "confirmed")    
            ),
            mainPanel(
                plotOutput(outputId = "plotvstimen")
            )
        )
    ),    
    
    # Plot impact on economy
    tabPanel(
        "Stock",
        titlePanel("Select a stock to examine. 
                 Information will be collected from yahoo finance"),
        sidebarLayout(
            sidebarPanel(
                textInput("symb", "Symbol", "GOOG"),
                dateRangeInput("dates6",
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
    
    #
    output$plotaday2 <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau",
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date == input$date2) %>%
            ggplot() +
            geom_col(mapping = aes(x = `Province/State`, y = `Count`,
                                   fill = `Case`)) +
            scale_y_log10() +
            labs(title = input$date2) +
            theme(axis.text.x = element_text(angle = 90))
    })

    #
    output$plotvstime <- renderPlot({
        ncov_tbl %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau",
                                           "Hong Kong", "Taiwan")) %>%
            filter(Date >= input$dates3[1], Date <= input$dates3[2]) %>%
            filter(if (input$case3 != "all") Case == input$case3
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
    output$gif <- renderImage({
        # p <- str_c("../hw3sol/", input$animation, "_anim.gif") %>% 
        #     as.character() %>% as.vector()
        list(src = paste0("../hw3sol/", input$animation, "_anim.gif"))
        # gif_file(p) %>%
        #     print()
    }, deleteFile = FALSE)
    
    #
    output$plotadayn <- renderPlot({
        info <- ncov_tbl %>%
             filter(Date == input$date4, Case == input$case4) %>%
             group_by(`Country/Region`) %>%  
             top_n(1, Date)
        China <- info %>%
            filter(`Country/Region` %in% c("Mainland China", "Macau", 
                                           "Hong Kong", "Taiwan")) %>%
            group_by(Case) %>%
            mutate(tmp = sum(Count)) %>% 
            top_n(1, `Province/State`) %>%
            select(-`Count`)
        colnames(China)[7] <- "Count"
        Other <- info %>% 
            filter(`Country/Region` != "Mainland China", 
                   `Country/Region` != "Macau", 
                   `Country/Region` != "Hong Kong", 
                   `Country/Region` != "Taiwan",
                   `Country/Region` != "Others") %>%
            mutate(tmp = sum(Count)) %>%
            top_n(1, `Province/State`) %>%
            select(- `Count`)
        colnames(Other)[7] <- "Count"
        newInfo <- bind_rows(China, Other)
        map <- newInfo %>% 
            left_join(countries, by = c("Country/Region" = "new_name"))
        ggplot(map, aes(long, lat, group = group, color = Count)) + 
            geom_polygon(aes(fill = Count)) + 
            # scale_fill_gradientn(colours = wes_palette("Zissou1", 100, 
            #                     type =   "continuous"), trans = "log10") + 
            scale_fill_gradientn(
                colours = c("#461863", "#404E88", "#2A8A8C", 
                            "#7FD157", "#F9E53F"), 
                values = scales::rescale(c(5, 10, 20, 100, 100000)), 
                labels = scales::comma, 
                breaks = c(5, 10, 20, 100, 100000)
            ) +
            guides(fill = guide_legend(reverse = T)) +
            theme_bw() +
            labs(title = str_c(input$case4, " cases")) +
            coord_quickmap()
    })
    
    #
    output$plotvstimen <- renderPlot({
        ncov_tbl %>%
            filter(Date >= input$dates5[1], Date <= input$dates5[2]) %>%
            filter(if (input$case5 != "all") Case == input$case5
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
    dataInput <- reactive({
        getSymbols(
            input$symb, src = "yahoo",
            from = input$dates6[1],
            to = input$dates6[2],
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

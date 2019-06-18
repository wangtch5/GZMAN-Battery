#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

packages <- c("ggplot2", "tidyverse", "timeDate", "lubridate", "ggpubr", "plotly", "shiny", "shinythemes") 
if(length(setdiff(packages, rownames(installed.packages()))) > 0){
    install.packages(setdiff(packages, rownames(installed.packages())))
}

library(shiny)
library(tidyverse)
library(ggplot2)
library(timeDate)
library(lubridate)
library(ggpubr)
library(plotly)
library(shinythemes)

source("doc/utils.R")

# # load data
# setwd("data/")
# REG.UP <- read_csv("reg.up.price.csv")
# REG.DOWN <- read_csv("reg.down.price.csv")
# RTLMP <- read_csv("RTLMP.price.csv")

# data preprocessing...
processing <- function(df){
    df$DATE <-  standard.date(row.names(df))$DATE
    df$TIME <- standard.date(row.names(df))$TIME
    # df$WD <- as.numeric(unname(isWeekday(df$DATE)))
    df$MONTH <- as.factor(month(df$DATE))
    df$TIME.N <- as.numeric(regmatches(df$TIME, regexpr("[0-9]+", df$TIME)))
    names(df)[1] <- "PRICE"
    narow <- which(is.na(df$PRICE))
    
    return(df[-narow, ])
}

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("simplex"),
    
    h3("ERCOT EXPLORATION"),
                
                # date control
                column(2,
                       selectInput("year", "Select a year",
                                   choices = list("2010"=2010,
                                                  "2011"=2011,
                                                  "2012"=2012,
                                                  "2013"=2013,
                                                  "2014"=2014,
                                                  "2015"=2015,
                                                  "2016"=2016,
                                                  "2017"=2017,
                                                  "2018"=2018,
                                                  "2019"=2019
                                   ),
                                   selected = 2018
                       )
                       
                ),
                
                # Sidebar with a slider input for number of bins 
                tabsetPanel(
                    tabPanel(h4("Lines of Date"),
                             sidebarLayout(
                                 sidebarPanel(
                                     sliderInput("day",
                                                 "Select Day:",
                                                 min = 1,
                                                 max = 31,
                                                 value = c(1,5)),
                                     
                                     sliderInput("month",
                                                 "Select till which month:",
                                                 min = 1,
                                                 max = 12,
                                                 value = c(1, 12)
                                     ),
                                     
                                     checkboxInput("single_month",
                                                   "Select Only One Month",
                                                   value = TRUE),
                                     
                                     # sidebarPanel size    
                                     width = 3),
                                 
                                 # Show Price plot
                                 mainPanel(
                                     tabsetPanel(
                                         tabPanel("RTMLP", plotOutput("RTLMPPlot",
                                                                      height = "600px", width = "100%")),
                                         tabPanel("REG.UP", plotOutput("REG.UPPlot",
                                                                       height = "600px", width = "100%")),
                                         tabPanel("REG.DOWN", plotOutput("REG.DOWNPlot",
                                                                         height = "600px", width = "100%"))
                                     ),
                                     
                                     # h5("ERCOT Data 2018", style="color:red")
                                 )
                             )
                    ),
                    
                    tabPanel(h4("Data Exploration"),
                             column(1,
                                    # h3('Control Pad'),
                                    
                                    checkboxGroupInput('month_check',
                                                       label = h4('Check Month'),
                                                       choices = list('Jan'=1, 'Feb'=2, 'Mar'=3, 'Apr'=4, 'May'=5,
                                                                      'Jun'=6, 'Jul'=7, 'Aug'=8, 'Sep'=9, 'Oct'=10,
                                                                      'Nov'=11, 'Dec'=12),
                                                       selected = 1:12
                                    )
                             ),
                             
                             column(11,
                                    h4("Month Avg Price Plot", align = "center"),
                                    plotOutput('MONTH_AVGPlot',
                                               height = "500px", width = "100%"),
                                    
                                    h5("  "),
                                    plotlyOutput('RTLMP_BOXPlot'),
                                    
                                    h5("  "),
                                    plotlyOutput('REG.UP_BOXPlot'),
                                    
                                    h5("  "),
                                    plotlyOutput('REG.DOWN_BOXPlot')
                             )
                    )
                )
                
                
                # page ends here
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    RTLMP <- reactive({
        # start <- format(as.Date(input$date[1]), format = "%m/%d/%Y")
        # end <- format(as.Date(input$date[2]), format = "%m/%d/%Y")
        start <- paste0("01/01/", input$year)  
        end <- paste0("12/31/", input$year)
        RTLMP <- yes.timeseries(list(items.list = paste("RTLMP", "HB_HOUSTON", "ERCOT", sep=':')), list(start.date = start, end.date = end, agg.level = 'hour', peak.type = 'ALL'))
        processing(RTLMP)
    })
    
    REG.UP <- reactive({
        start <- paste0("01/01/", input$year)  
        end <- paste0("12/31/", input$year)
        REG.UP <- yes.timeseries(list(items.list = paste("ASM_DA_REGUP", "ERCOT", "ERCOT", sep=':')), list(start.date = start, end.date = end, agg.level = 'hour', peak.type = 'ALL'))
        processing(REG.UP)
    })
    
    REG.DOWN <- reactive({
        start <- paste0("01/01/", input$year)  
        end <- paste0("12/31/", input$year)
        REG.DOWN <- yes.timeseries(list(items.list = paste("ASM_DA_REGDOWN", "ERCOT", "ERCOT", sep=':')), list(start.date = start, end.date = end, agg.level = 'hour', peak.type = 'ALL'))
        processing(REG.DOWN)
    })
    
    dataInput <- reactive({
        flag <- NULL
        if(input$single_month){
            days <- NULL
            for (each in input$day[1]:input$day[2]) {
                if(each < 10){
                    days <- c(days, paste0("-",0,each))
                }
                else{
                    days <- c(days, paste0("-",each))
                }
            }
        } 
        else{
            if(input$day[1] < 10){
                days <- paste0("-",0,input$day[1])
            }
            else{
                days <- paste0("-",input$day[1])
            }
        }
        
        
        if(input$single_month){
            if(input$month[1] < 10) i <- paste0(0,input$month[1])
            else i <- input$month[1]
            for (day in days) {
                flag <- c(flag, which(REG.DOWN()$DATE == paste0(input$year, '-', i, day))[1])
            }
        }
        else{
            for(i in input$month[1]:input$month[2]){
                if(i < 10) i <- paste0(0,i)
                flag <- c(flag, which(REG.DOWN()$DATE == paste0(input$year, '-', i, days))[1])
            }
        }
        
        sams <- flag
        for(i in 1:23){
            sams <- c(sams, flag+i)
        }
        sams <- sort(sams)
        sams
    })
    
    
    output$REG.DOWNPlot <- renderPlot({
        # REG.DOWN plot
        sams <- dataInput()
        ggplot(REG.DOWN()[sams,],aes(color = DATE))+
            geom_point(aes(x=TIME, y=PRICE))+
            geom_line(aes(x=rep(1:24,length(sams)/24), y=PRICE))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))
        
    })
    
    output$REG.UPPlot <- renderPlot({
        # REG.UP plot
        sams <- dataInput()
        ggplot(REG.UP()[sams,],aes(color = DATE))+
            geom_point(aes(x=TIME, y=PRICE))+
            geom_line(aes(x=rep(1:24,length(sams)/24), y=PRICE))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$RTLMPPlot <- renderPlot({
        sams <- dataInput()
        
        ggplot(RTLMP()[sams,],aes(color = DATE))+
            geom_point(aes(x=TIME, y=PRICE))+
            geom_line(aes(x=rep(1:24,length(sams)/24), y=PRICE))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$MONTH_AVGPlot <- renderPlot({
        
        rtlmp.plot <- ggplot(
            filter(RTLMP(), MONTH %in% input$month_check) %>%
                group_by(TIME, TIME.N, MONTH) %>%
                summarise(pmean = mean(PRICE))
        )+
            geom_point(aes(x=TIME, y=pmean,  color=MONTH))+
            geom_line(aes(x=TIME.N, y=pmean, color=MONTH))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))+
            scale_color_discrete(name = "Month")+
            labs(title = "RTLMP Avg")
        
        regup.plot <- ggplot(filter(REG.UP(), MONTH %in% input$month_check) %>%
                                 group_by(TIME, TIME.N, MONTH) %>%
                                 summarise(pmean = mean(PRICE))
        )+
            geom_point(aes(x=TIME, y=pmean, color=MONTH))+
            geom_line(aes(x=TIME.N, y=pmean, color=MONTH))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))+
            scale_color_discrete(name = "Month")+
            labs(title = "REG.UP Avg")
        
        regdown.plot<- ggplot(filter(REG.DOWN(), MONTH %in% input$month_check) %>%
                                  group_by(TIME, TIME.N, MONTH) %>%
                                  summarise(pmean = mean(PRICE))
        )+
            geom_point(aes(x=TIME, y=pmean, color=MONTH))+
            geom_line(aes(x=TIME.N, y=pmean, color=MONTH))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))+
            scale_color_discrete(name = "Month")+
            labs(title = "REG.DOWN Avg")
        
        ggarrange(rtlmp.plot, regup.plot, regdown.plot,
                  ncol = 3, common.legend = TRUE, label.x = NULL)
    })
    
    output$RTLMP_BOXPlot <- renderPlotly({
        
        plot_ly(RTLMP(), y = ~PRICE, color = ~MONTH, type = "box") %>%
            layout(title = "RTLMP", xaxis = list(title="Month"))
    })
    
    output$REG.UP_BOXPlot <- renderPlotly({
        
        plot_ly(REG.UP(), y = ~PRICE, color = ~MONTH, type = "box") %>%
            layout(title = "REG.UP", xaxis = list(title="Month"))
    })
    
    output$REG.DOWN_BOXPlot <- renderPlotly({
        
        plot_ly(REG.DOWN(), y = ~PRICE, color = ~MONTH, type = "box") %>%
            layout(title = "REG.DOWN", xaxis = list(title="Month"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
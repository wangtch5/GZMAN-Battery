#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(ggplot2)
library(timeDate)
library(lubridate)
library(ggpubr)
library(plotly)

source("utils.R")

# load data
setwd("../data")
REG.UP <- read_csv("reg.up.price.csv")
REG.DOWN <- read_csv("reg.down.price.csv")
RTLMP <- read_csv("RTLMP.price.csv")

# data preprocessing...
REG.UP$DATE <- standard.date(REG.UP$X1)$DATE
REG.UP$TIME <- standard.date(REG.UP$X1)$TIME

REG.DOWN$DATE <- standard.date(REG.DOWN$X1)$DATE
REG.DOWN$TIME <- standard.date(REG.DOWN$X1)$TIME

RTLMP$DATE <- standard.date(RTLMP$X1)$DATE
RTLMP$TIME <- standard.date(RTLMP$X1)$TIME
    # add WeekDay and Month var
RTLMP$WD = as.numeric(unname(isWeekday(RTLMP$DATE)))
REG.DOWN$WD = as.numeric(unname(isWeekday(REG.DOWN$DATE)))
REG.UP$WD = as.numeric(unname(isWeekday(REG.UP$DATE)))
REG.DOWN$MONTH <- as.factor(month(REG.DOWN$DATE))
RTLMP$MONTH <- as.factor(month(RTLMP$DATE))
REG.UP$MONTH <- as.factor(month(REG.UP$DATE))
    # add ancillary variable for plotting
RTLMP$TIME.N <- as.numeric(regmatches(RTLMP$TIME, regexpr("[0-9]+", RTLMP$TIME)))
REG.DOWN$TIME.N <- as.numeric(regmatches(REG.DOWN$TIME, regexpr("[0-9]+", REG.DOWN$TIME)))
REG.UP$TIME.N <- as.numeric(regmatches(REG.UP$TIME, regexpr("[0-9]+", REG.UP$TIME)))

# Rename DataFrames var, easy for plotting..
names(RTLMP)[2] = "PRICE"
names(REG.DOWN)[2] = "PRICE"
names(REG.UP)[2] = "PRICE"

# Get rid of NA col
narow <- which(is.na(REG.DOWN$PRICE))
REG.UP <- REG.UP[-narow,]
REG.DOWN <- REG.DOWN[-narow,]
RTLMP <- RTLMP[-narow, ]


# Define UI for application that draws a histogram
ui <- navbarPage(
    "ERCOT EXPLORATION",

    # Sidebar with a slider input for number of bins 
     tabPanel("Lines of Date",
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
              
        h5("ERCOT Data 2018", style="color:red")
          )
        )
     ),
    
    tabPanel("Data Exploration",
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
            h3("Month Avg Price Plot", align = "center"),
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
    
    # page ends here
)

# Define server logic required to draw a histogram
server <- function(input, output) {
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
                flag <- c(flag, which(REG.DOWN$DATE == paste0("2018-", i, day))[1])
            }
        }
        else{
            for(i in input$month[1]:input$month[2]){
                if(i < 10) i <- paste0(0,i)
                flag <- c(flag, which(REG.DOWN$DATE == paste0("2018-", i, days))[1])
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
        ggplot(REG.DOWN[sams,],aes(color = DATE))+
            geom_point(aes(x=TIME, y=PRICE))+
            geom_line(aes(x=rep(1:24,length(sams)/24), y=PRICE))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))
            
    })
    
    output$REG.UPPlot <- renderPlot({
        # REG.UP plot
        sams <- dataInput()
        ggplot(REG.UP[sams,],aes(color = DATE))+
            geom_point(aes(x=TIME, y=PRICE))+
            geom_line(aes(x=rep(1:24,length(sams)/24), y=PRICE))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))
    })

    output$RTLMPPlot <- renderPlot({
        sams <- dataInput()

        ggplot(RTLMP[sams,],aes(color = DATE))+
            geom_point(aes(x=TIME, y=PRICE))+
            geom_line(aes(x=rep(1:24,length(sams)/24), y=PRICE))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))
    })
    
    output$MONTH_AVGPlot <- renderPlot({

        rtlmp.plot <- ggplot(
            filter(RTLMP, MONTH %in% input$month_check) %>%
                group_by(TIME, TIME.N, MONTH) %>%
                summarise(pmean = mean(PRICE))
        )+
            geom_point(aes(x=TIME, y=pmean,  color=MONTH))+
            geom_line(aes(x=TIME.N, y=pmean, color=MONTH))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))+
            scale_color_discrete(name = "Month")+
            labs(title = "RTLMP Avg")
        
        regup.plot <- ggplot(filter(REG.UP, MONTH %in% input$month_check) %>%
                                 group_by(TIME, TIME.N, MONTH) %>%
                                 summarise(pmean = mean(PRICE))
                             )+
            geom_point(aes(x=TIME, y=pmean, color=MONTH))+
            geom_line(aes(x=TIME.N, y=pmean, color=MONTH))+
            theme_bw()+
            theme(axis.text.x = element_text(angle = 90))+
            scale_color_discrete(name = "Month")+
            labs(title = "REG.UP Avg")

        regdown.plot<- ggplot(filter(REG.DOWN, MONTH %in% input$month_check) %>%
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
        
        plot_ly(RTLMP, y = ~PRICE, color = ~MONTH, type = "box") %>%
            layout(title = "RTLMP", xaxis = list(title="Month"))
    })
    
    output$REG.UP_BOXPlot <- renderPlotly({
        
        plot_ly(REG.UP, y = ~PRICE, color = ~MONTH, type = "box") %>%
            layout(title = "REG.UP", xaxis = list(title="Month"))
    })
    
    output$REG.DOWN_BOXPlot <- renderPlotly({
        
        plot_ly(REG.DOWN, y = ~PRICE, color = ~MONTH, type = "box") %>%
            layout(title = "REG.DOWN", xaxis = list(title="Month"))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

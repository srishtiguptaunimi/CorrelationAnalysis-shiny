#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#Load the packages
library(shiny)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(readr)

#Load the data of Car Sales US and explantory variables 
data_US <- read.csv("final.csv")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Correlation Analysis of Car Demand"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
                radioButtons("variable", label = h3("Select a variable to explore the growth"), 
                        choices = c("Car Sales", "Car Prices" , "Disposable Income", "Unemployment Rate" ,
                                       "Gasoline Price", "Population"),
             
                ), 
                radioButtons("select", label = h3("Select below to visualize the scatter plot or correlation matrix of all the variables"), 
                             choices = c("Correlation Scatter Plot of all variables",
                                         "Visualize correlation matrix of all variables"),
                ),
              
            ),
        
    mainPanel (
        plotOutput("dataplot"),
        plotOutput("value")
    )
    )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
    output$dataplot <- renderPlot({
     
       #creating a function for the argument  
      plot.fun <- function(){
        if(input$variable == "Car Sales") {print(p1)} 
        if(input$variable == "Unemployment Rate") {print(p2)}
        if(input$variable == "Car Prices") {print(p6)}
        if(input$variable == "Disposable Income") {print(p4)}
        if(input$variable == "Population") {print(p3)}
        if(input$variable == "Gasoline Price") {print(p5)}
      }
      #plot data of car sales 
        p1 <- ggplot(data_US, aes(date, sales)) + geom_line(stat = "identity") + labs(title = input$select, x = "Year", y = "Car Sales",subtitle = "2005 - 2020",caption = "Source - BEA")
        p2 <- ggplot(data_US, aes(date, ur)) + geom_line(stat = "identity") + labs(title = input$select, x = "Year", y = "Unemployment Rate", subtitle = "2005 - 2020",caption = "Source - BEA")
        p3 <- ggplot(data_US, aes(date, pop)) + geom_line(stat = "identity") + labs(title = input$select, x = "Year", y = "Population growth", subtitle = "2005 - 2020",caption = "Source - BEA")
        p4 <- ggplot(data_US, aes(date, di)) + geom_line(stat = "identity") + labs(title = input$select, x = "Year", y = "Disposable Income", subtitle = "2005 - 2020",caption = "Source - BEA")
        p5 <- ggplot(data_US, aes(date, gp)) + geom_line(stat = "identity") + labs(title = input$select, x = "Year", y = "Gasoline Price", subtitle = "2005 - 2020",caption = "Source - BEA")
        p6 <- ggplot(data_US, aes(date, price)) + geom_line(stat = "identity") + labs(title = input$select, x = "Year", y = "Car Prices", subtitle = "2005 - 2020",caption = "Source - BEA")
        
       #Calling the plot function
         plot.fun()   
        
})
    output$value <- renderPlot ({ 
      #scatter plot matrix of the variables
      s1 <- pairs(data_US[2:7], col = "red", pch = 18, 
                  labels = c ("population", "Car Prices", "Disposable Income", "Car Sales", "Unemployment Rate", "Gasoline Price"),
                  lower.panel = NULL,
                  upper.panel = panel.smooth)
      
      # correlation matrix
      corr <- round(cor(data_US[2:7]),1)
      p.mat <- cor_pmat(data_US[2:7])
      s2 <- ggcorrplot(corr,
                       hc.order = TRUE,
                       type = "lower",
                       lab = TRUE)
      #Argument for ploting the Matrix
      if(input$select == "Correlation Scatter Plot of all variables"){print(s1)}
      if(input$select == "Visualize correlation matrix of all variables"){print(s2)}
      })  
}

# Run the application 
shinyApp(ui = ui, server = server)

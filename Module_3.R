---
output: html_document
runtime: shiny
---
library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(stats)
library(graphics)
setwd("~/Insidesherpa kpmg")
data <- read_excel("KPMG_VI_New_raw_data_update_final.xlsx")
data <- read_excel("KPMG_VI_New_raw_data_update_final.xlsx", sheet = "NewCustomerList")

ui <- fluidPage(
  titlePanel("Sprocket Central Pty Ltd: Analysis of New Customers"),
  
  sidebarLayout(
    sidebarPanel(
      sliderInput("age", "Age", min(0),max(90), c(20,45)),
      selectInput("state", "State", choices = c("All", data$state), selected = "NSW"),
      selectInput("wealth", "Wealth", choices = c("All", data$wealth_segment), selected = "Mass Customer"),
      downloadButton(outputId = "download_data", label ="Download")),

    mainPanel(
      tabsetPanel(
        tabPanel( title = "Plot",
      plotOutput("plot")),
      
      tabPanel(title = "Pie",
      plotlyOutput("diskpie"),
      plotlyOutput("diskpie2")),
     
      tabPanel( title= "Table",
        DT::dataTableOutput("table"))
    )
  )
))


server <- function(input, output) {
  
   output$plot <- renderPlot({
     data1 <- subset(data,
                     Age >= input$age[1] & Age <= input$age[2])
     
     if (input$state != "All") {
       data1 <- subset(
         data1,
         state == input$state
       )
     }
     
     if (input$wealth != "All") {
       data1 <- subset(
         data1,
         wealth_segment == input$wealth
       )
     }
   
     ggplot2::ggplot(data1, aes(x=gender, fill= wealth_segment)) +
       geom_bar(position = "dodge") + facet_wrap(~state) + ylab("Number of customers") + ggtitle("Number of New Customers based on Gender, State and Wealth Segment")})
    
   
   output$download_data <- downloadHandler(
     filename = "CustomerList.csv",
     content = function(file) {
       data1 <- subset(data,
                       Age >= input$age[1] & Age <= input$age[2])
       
       if (input$state != "All") {
         data1 <- subset(
           data1,
           state == input$state
         )
       }
       
       if (input$wealth != "All") {
         data1 <- subset(
           data1,
           wealth_segment == input$wealth
         )
       }
       
       # Write the filtered data into a CSV file
       write.csv(data1, file, row.names = FALSE)
     }
   )
   
   output$diskpie <- renderPlotly({
     data1 <- subset(data,
                     Age >= input$age[1] & Age <= input$age[2])
     
     if (input$state != "All") {
       data1 <- subset(
         data1,
         state == input$state
       )
     }
     
     if (input$wealth != "All") {
       data1 <- subset(
         data1,
         wealth_segment == input$wealth
       )
     }
     data1 <- data1[,c('job_industry_category')]
     plot_ly(data1,
         labels = ~job_industry_category, title = "Job Industry of New Customers", type="pie")
      })
   
   output$diskpie2 <- renderPlotly({
     data1 <- subset(data,
                     Age >= input$age[1] & Age <= input$age[2])
     
     if (input$state != "All") {
       data1 <- subset(
         data1,
         state == input$state
       )
     }
     
     if (input$wealth != "All") {
       data1 <- subset(
         data1,
         wealth_segment == input$wealth
       )
     }
     
     data1 <- data1[,c('owns_car')]
     plot_ly(data1,
             labels = ~owns_car, title = "Car owned of New Customers", type="pie")
   })
   
   
   
   output$table <- DT::renderDataTable({
     data1 <- subset(data,
                     Age >= input$age[1] & Age <= input$age[2])
     
     if (input$state != "All") {
       data1 <- subset(
         data1,
         state == input$state
       )
     }
     
     if (input$wealth != "All") {
       data1 <- subset(
         data1,
         wealth_segment == input$wealth
       )
     }
     
     data1
   })
}

# Run the application
shinyApp(ui = ui, server = server)
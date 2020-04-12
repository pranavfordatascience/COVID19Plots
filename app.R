#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggpubr)
source('Charts.R', local = TRUE)

list_of_graphs=c( '%Poverty vs Cases/1000 Population'='plot1' 
  ,'% Unemplyment vs Cases/1000 Population'='plot2'
  ,'Per Capita Income vs Cases/1000 Population'                       = 'plot3'                 
  ,'% People without high school diploma vs Cases/1000 Population'='plot4'
  ,'% Population above age of 65 years vs Cases/1000 Population'='plot5'
  ,'% Population above age upto 17 years vs Cases/1000 Population'='plot6'
  ,'% Population with Disabilities vs Cases/1000 Population'='plot7'
  ,'% Housholds with Single Parent vs Cases/1000 Population'='plot8'
  ,'% of Minority in Population vs Cases/1000 Population'='plot9'
  ,'% Population not fluent in English vs Cases/1000 Population'='plot10'
  ,'% of Housing with units more than 10 vs Cases/1000 Population'='plot11'
  ,'% of Mobile Housing Units vs Cases/1000 Population'='plot12'
  ,'% of Housing with more people than rooms vs Cases/1000 Population' ='plot13'    
  ,'% of Households with no vehicle vs Cases/1000 Population'='plot14'
  ,'% of institutionalized group quarters  vs Cases/1000 Population'='plot15'
)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID 19 Cases for US Counties per 1000 Population vs various Factors"),
    
    
    
    # Sidebar layout with input and output definitions ----
    
    fluidRow(
      
        
        column(5,
               sidebarPanel(
               fluidRow(
         sliderInput("DatesMerge",
                    "Dates:",
                    min = as.Date("2020-02-17","%Y-%m-%d"),
                    max = as.Date(max(df_us_county_svi_covid$date),"%Y-%m-%d"),
                    value=as.Date("2020-01-21"),
                    timeFormat="%Y-%m-%d",
                   animate=animationOptions(interval =900)
                    )
                   
        
      ),
      fluidRow(
                      h4("SVI Index vs County COVID19 Counts"),
                      
                      
                      selectInput('plotx', 'Please select a plot from the list', choices= list_of_graphs , selected = 'plot1', multiple = FALSE,
                                  selectize = F, width = '1000px',  size = 15)),
        width=50
      )
      ),
      
      column(7,align = "center",
             h4(textOutput("result")),
             
             plotOutput("distPlot")
             
      )
      ),

    
   

   hr(),


fluidRow(
  align = "center",
  
  
  h6(a('*SVI Data Source',href='https://svi.cdc.gov/data-and-tools-download.html')),
  
  h6(a('*COVID County Level Data',href='https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv')),
  
  h6(a(href="mailto:pranavtengshe@gmail.com","pranavtengshe@gmail.com"))
)
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({

        output$result <- renderText({
            paste("Displaying Plot for ", names(list_of_graphs)[list_of_graphs==input$plotx])
        })
        
        output$seldate<- renderText(
          {
            
          }
        )
       
        figure=ggarrange(
            # if(input$plotx=='plot1') {plot1}
          
          get(paste(input$plotx,input$DatesMerge,sep = '_'))
            
            # else if(input$plotx=='plot2') {plot2}
            # else if(input$plotx=='plot3') {plot3}
            # else if(input$plotx=='plot4') {plot4}
            # else if(input$plotx=='plot5') {plot5}
            # else if(input$plotx=='plot6') {plot6}
            # else if(input$plotx=='plot7') {plot7}
            # else if(input$plotx=='plot8') {plot8}
            # else if(input$plotx=='plot9') {plot9}
            # else if(input$plotx=='plot10') {plot10}
            # else if(input$plotx=='plot11') {plot11}
            # else if(input$plotx=='plot12') {plot12}
            # else if(input$plotx=='plot13') {plot13}
            # else if(input$plotx=='plot14') {plot14}
            # else if(input$plotx=='plot15') {plot15}
            )
        annotate_figure(figure)
    
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

##ScotPHO - website updates
##Code to create interactive chart for Epilepsy section.
##https://www.scotpho.org.uk/health-wellbeing-and-disease/epilepsy/data/secondary-care/

############################.
## Global ----
############################.
############################.

#Packages 
library(dplyr) #data manipulation
library(plotly) #charts
library(shiny) #shiny app
library(readr)
library(tidyr) #preparing data - not needed unless new data coming through


#Read in data
data <- readRDS("data/epilepsy_incidence.rds")
         
         
############################.
## UI ----
############################.
#Height and widths as percentages to allow responsiveness
#Using divs as issues with classing css 
ui <- fluidPage(style="width: 650px; height: 500px; ", 
                div(style= "width:100%", #Filters on top of page
                    h4("Chart 1. New cases (incidence) per 100,000 population with a 
                       main diagnosis of epilepsy, by age and sex, Scotland"),
                    div(style = "width: 50%; float: left;",
                        selectizeInput("sex", label = "Select sex",
                                       choices = c("Female", "Male"), 
                                       multiple = TRUE, 
                                       selected = c("Female", "Male"),
                                       options = list(maxItems = 2L)))
                    ),
                div(style = "width: 50%; float: left;",
                    selectizeInput("agegrp", label = "Select age group/s",
                                   choices = c("All ages", "<15", "15-54", "55+"), 
                                   multiple = TRUE, 
                                   selected = "All ages",
                                   options = list(maxItems =8L))
                    ),
                div(style= "width:100%; float: left;", #Main panel
                    plotlyOutput("chart", width = "100%", height = "350px"),
                    p(div(style = "width: 25%; float: left;", #Footer
                          HTML("Source: <a href='https://www.ndc.scot.nhs.uk/National-Datasets/data.asp?SubID=5'>PHS, SMR01</a>")),
                      div(style = "width: 25%; float: right;",
                          downloadLink('download_data', 'Download data')),
                      div(style = "width: 100%; float: left;",
                          h6("Notes:", tags$br(),"
                             1. These statistics are derived from data collected on 
                             discharges from hospitals for non-obstetric and non-psychiatric 
                             hospitals (SMR01) in Scotland.", tags$br(), "
                             2. Directly age-sex standardised to the European Standard population 2013.", tags$br(), "
                             3. ICD-10 codes for epilepsy: G40 and G41.")
                          )
                      )
                    )
                )#fluid page bracket    


############################.
## Server ----
############################.
server <- function(input, output) {
  
  #Allowing user to download data
  output$download_data <- downloadHandler( 
    filename =  'epilepsy_incidence.csv', content = function(file) { 
      write.csv(data, file, row.names=FALSE) })

  #Visualization
  output$chart <- renderPlotly({
    
    #Filter data by inputs
    data_agesex <- data %>% filter(sex %in% input$sex & agegrp %in% input$agegrp)
    
    #Information to be displayed in tooltip
    tooltip <- c(paste0(data_agesex$year, "<br>",
                        data_agesex$sex_agegrp, "<br>",
                        "Age-sex standardised rate: ", data_agesex$measure))
    
    
    # Buttons to remove
    bttn_remove <- list('select2d',
                        'lasso2d', 'zoomIn2d',
                        'zoomOut2d','autoScale2d', 'toggleSpikelines',
                        'hoverCompareCartesian',
                        'hoverClosestCartesian', 'zoom2d', 'pan2d', 'resetScale2d')
    
    #Create plot
    plot <- plot_ly(data=data_agesex, x=~year, y = ~measure, color = ~sex_agegrp,
                    colors = c('#2166ac','#4393c3', '#92c5de', '#053061', 
                               '#8c510a', '#bf812d', '#dfc27d', '#543005'),
                    type = "scatter", mode = 'lines+markers',
                    width = 650, height = 350,
                    text=tooltip,
                    hoverinfo="text") %>%
      #Layout
      layout(annotations = list(), #It needs this because of a buggy behaviour
             yaxis = list(title = "Age-sex standardised rate<br> per 100,000 population", rangemode="tozero", fixedrange=TRUE), 
             xaxis = list(title = "Financial year",  fixedrange=TRUE, tickangle = 270),  
             font = list(family = 'Arial, sans-serif'), #font
             margin = list(pad = 4, t = 50, r = 30)) %>% #margin-paddings
      config(displayModeBar= T, displaylogo = F, modeBarButtonsToRemove = bttn_remove) #taking out plotly logo and collaborate button
  }
  ) 
} # end of server part


############################.
## Calling app ----
############################.

shinyApp(ui = ui, server = server)


############################.
## End ----
############################.

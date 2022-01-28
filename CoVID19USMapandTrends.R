"===============================================
        COVID-19 US TRENDS TRACKING APP
==============================================="

library(shiny)
library(shinydashboard)
library(fullPage)
library(covid19.analytics)
library(tidyverse)
library(data.table)
library(rhandsontable)
library(leaflet)
library(highcharter)
library(DT)


"================================
      FUNCTIONS AND VARIABLES 
================================="

us_county_names_data_file = paste("CountyNames.csv")
us_covid_data_file = paste("us_covid_data", Sys.Date(), ".csv", sep = "_" )

# GET THE COVID DATA
get_covid_data = function( us_covid_data_file, us_county_names )
{
  
  if( file.exists( us_covid_data_file ) ){  return( read.csv( us_covid_data_file ) ) }
  else {
        
        # CONVERT WIDE TO LONG  
        covid_data = covid19.US.data() %>%  
                     select( -c(Country_Region)) %>%
                     rename( State = Province_State, Long = Long_ ) %>%
                     filter( Lat != 0 & Long != 0) %>% 
                     gather( key = date, value = case_count, -c(State, Lat, Long) ) %>%
                     arrange( State, Lat, Long, date )
        
        # CATEGORIZE CASES
        cases = c(rep( c("confirmed", "deaths"), nrow(covid_data)/2 ))
        covid_data = add_column( covid_data, cases, .after=4 )
        
        # DISAGGREGATION
        covid_data = as.data.table(covid_data)
        covid_data = covid_data[, case_agg:=c(0, case_count[-.N]), by = list(State, Lat, Long, cases)]
        covid_data$count = covid_data$case_count - covid_data$case_agg
        
        # JOIN WITH COUNTY NAME
        covid_data = covid_data %>%
                     left_join(us_county_names, by = c("Lat" = "Lat", "Long" = "Long")) %>%
                     select( State, County, Lat, Long, date, cases, count)
        
        write.csv( covid_data, us_covid_data_file, row.names = FALSE )
        return(covid_data)
  }
            
}

# GET US STATES
get_states = function(covid_df)
{
    covid_df %>% 
    arrange(State) %>% 
    distinct(State) %>% 
    pull(State)
}

# GET STATE AGGREGATES
get_state_aggregates = function(covid_df)
{
    covid_df %>% 
    mutate(date = as.Date(date)) %>%
    group_by(State, date, cases) %>%
    summarize(daily_cases = sum(count))
}

# GET TOP COUNTIES
get_top_counties = function(covid_df, n)
{
    counties = covid_df %>%
               filter( date == max(date)) %>%
               arrange( desc(count) ) %>%
               group_by(State) %>%
               slice(1:n) %>%
               spread( cases, count, fill = 0 )
    
}


"================================
           UI SECTION
================================="

ui = pagePiling(
    sections.color = c("#dddddd", "#F6EADF", "#E2DDCA", "#C9D2C6"),
    menu = c("US Map" = "map", "Trends" = "trends", "Dataset" = "dataset", "About" = "about"),
    pageSection( center = TRUE, menu = "map", uiOutput("map")),
    pageSection( center = TRUE, menu = "trends", uiOutput("trends")),
    pageSection( center = TRUE, menu = "dataset", uiOutput("dataset")),
    pageSection( center = TRUE, menu = "about", uiOutput('about') )

)


"================================
        SERVER SECTION
================================="

# GET DATASETS
us_county_names = read.csv(us_county_names_data_file)
us_covid_data = get_covid_data(us_covid_data_file, us_county_names)
us_states = get_states(us_covid_data)
state_aggregates = get_state_aggregates(us_covid_data)
top_counties = get_top_counties(us_covid_data, 5)

server = function(input, output)
{
  
  # MAP SECTION
  output$us_map = renderLeaflet({
      top_counties %>%
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView( -95.712891, 37.09024, zoom = 5 ) %>%
      addCircleMarkers( lat = ~Lat,
                        lng = ~Long,
                        radius = ~confirmed/1200,
                        label = ~paste("County: ", County, 
                                       "Confirmed Cases:", confirmed,
                                       "Death Cases:", deaths),
                        popup = ~paste("County: ", County, "<br/>",
                                       "Confirmed Cases:", confirmed, "<br/>",
                                       "Death Cases:", deaths))
      
      
    
  })
  
  output$map = renderUI({ 
    div(
        tags$style( type="text/css", "html, body {width:100%;height:100%;}" ),
        tags$style( type="text/css", "#us_map {height: calc(100vh - 100px) !important;}"),
        leafletOutput("us_map")
    ) 
    })
  
  
  
  
  # TREND OUTPUT
  output$state_selector = renderUI({
      selectizeInput(inputId = "state", 
                     label = "Select/Type State:",
                     choices = us_states,
                     selected = sample(us_states, 1)
                    )
  })
  
  output$trend_chart = renderHighchart({
    state_aggregates %>% filter( State %in% input$state) %>%
      hchart(., 'line', hcaes( x = date, y = daily_cases, group = cases) )
    
  })
  
  output$trends = renderUI({ pageContainer( h3("US State Daily CoVID Trends"), br(),
                                            uiOutput('state_selector'),
                                            highchartOutput("trend_chart")) })
  
 
  # OUTPUT DATASET
  output$data = renderDataTable({ datatable(us_covid_data , options = list(paging =TRUE, pageLength =  10)) })
  output$dataset = renderUI({  pageContainer(  h3("CoVID-19 US County Level Data"), br(),
                                               dataTableOutput("data"))  })
  
  # OUTPUT ABOUT
  output$about = renderUI({
    div(
          h3("About CoVID-19 App:"), br(),
          p("App Source Code: ", tags$a("Github", href = "https://github.com/Sifael/CoVIDTrendTrackingApp")),
          p("CoVID-19 R Data API: ", tags$a("covid19.analytics", href = "https://github.com/mponce0/covid19.analytics")),
          p("Learn More about CoVID-19: ", tags$a("CDC Website", href = "https://www.cdc.gov/coronavirus/2019-ncov/index.html"))
    )
    
  })
}


"================================
         SHINY APP
================================="
shinyApp( ui = ui, server = server )
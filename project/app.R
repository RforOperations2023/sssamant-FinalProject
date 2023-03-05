#Author- Shantanu Samant


#THE AIM OF THIS FINAL PROJECT IS TO WORK ON ANSWERING AND INVESTIGATING THE FOLLOWING QUESTION VISUALLY
#Question How is the EV charging station infrastructure at any county in USA provided that counties party allegience over time,
#Reports suggest that 70% of republicans are not in favor of EV Vehicle subsidy,
#We wanted to explore if there is any co-relation whatsoever by taking a looking at a county EV charging station infrastructure

#This analysis is in no way causal. This is a simple data exploration excercise done on R shiny.


#Inputs: State, EVSE number slider, Opening Date range.
#Based on the inputs made,
#Outputs:
#you get three tabs: A) Statewise map analytics MAP  B) Plots C) Data Table
#I have also added a countrywise map analytics tab (first tab) that shows
# 
# Libraries
library(shiny)
library(shinythemes)
library(leaflet)
library(leaflet.extras)
library(sf)
library(shinyjs)
library(dplyr)
library(shinydashboard)
library(reshape2)
library(dplyr)
library(plotly)
library(ggplot2)
library(shiny)
library(hms)
library(DT)
library(stringr)
library(tools)
library(readr)
library(tidyverse)

# Load and clean  data -------------------------------------------------------------------------------------
ev_data <- read_csv("my_data_new.csv")
ev_data <- ev_data %>% rename(latitude = `Latitude`) #Renaming the column name to be case sensituve
ev_data <- ev_data %>% rename(longitude = `Longitude`)#Renaming the column name to be case sensituve
ev_data <- ev_data %>% rename(open_date = `Open Date`) #Renaming Open Date (I need an '_' in the column name for simplicity)
ev_data <- ev_data %>% rename(address = `Station Name`)
ev_data <- ev_data %>% rename(EV_count = `EV Level2 EVSE Num`)
ev_data$party<-str_to_title(ev_data$party) #I want all entires "Republican" or "Democratic" to be case senitive
ev_data <- mutate(ev_data, open_date = as.Date(open_date, format= "%d-%m-%Y")) #Converting open_date to date column
ev_data

#-----------------------------------------------------------------------
#create new dataframe "counties" from "ev_data"
#Used in output for map 1 (i.e "leaflet)

#County data and merging files
counties <- ev_data %>%
  select(county_fips, county_name, State, party) %>%
  distinct() #Selecting only distinct entries of fips_number, county_name, state, party for map 1 (and for left join)
counties <- counties[complete.cases(counties), ] #Dropping all null and NaN Values
counties <- na.omit(counties)
counties <- counties %>% mutate(Fips_str = as.character(county_fips)) # Convert Fips_number to string column
# Add leading zero to 4-digit numbers in Fips_str column
counties$Fips_str <- ifelse(nchar(counties$Fips_str) == 4, paste0("0", counties$Fips_str), counties$Fips_str) #This had to be done sice the fips column is numeric, and has to be converted into a string
counties <- counties %>% rename(GEOID = `Fips_str`)# Rename 'Fips_str' column () which has the geoid values as strings) to GEOID

#Storing all unique instances of states in ev_data into states vector
#Will be used as a hyperparameter on the Sidepanel as a select type input
states_vector <- as.vector(unique(counties$State))
#--------------------------------------------------------------------------------------------------
#Loading shape file as Lines.load
lines.load <- st_read("./cb_2018_us_county_500k/cb_2018_us_county_500k.shp")
#Joining .shp file with counties 
co <- lines.load %>%
  left_join(counties, by = c("GEOID" = "GEOID"))
#--------------------------------------------------------------------------------------------------
#Defining the icons
icons <- awesomeIconList(
  MS4 = makeAwesomeIcon(icon = "road", library = "fa", markerColor = "gray"),
  Combined = makeAwesomeIcon(icon = "cloud", library = "fa", markerColor = "blue"),
  `Non-combined` = makeAwesomeIcon(icon = "tint", library = "fa", markerColor = "green"),
  `On-site management` = makeAwesomeIcon(icon = "building-o", library = "fa", markerColor = "cadetblue")
)
#----------------------------------------------------------------------------------------------------


ui <- fluidPage(
    theme = shinythemes::shinytheme("yeti"),
    #Title Panel
    titlePanel(title=div(img(height = 105, width = 300, src="cs2.png") , " Politics Behind Green Energy"), windowTitle = "myBrowserTitle"),
  
    #Designing and structuring the sidebar layout
    sidebarLayout(
      
      sidebarPanel( #Things to be accomodated into the side bar panel
      ### INPUT 1---------------------------------------------------------------------------------------
      #Radio Button which the user selects party they want to investigate further in the dataset
      #Outputs coverd by input: a) Map2 i.e "leaflet2"  b) plot under plot tab
      radioButtons(inputId = "selected_type",
                   label = "Select Party",
                   choices = c("Republican", "Democratic" ),
                   selected = "Democratic"),
      hr(),
      ### INPUT 2---------------------------------------------------------------------------------------
      #A drop down list that will allow a states charging infrastructure thay want to analyse
      selectInput(inputId = "state_select", 
                  label = "Select a state:", 
                  choices = states_vector),
      hr(),
      ### INPUT 3---------------------------------------------------------------------------------------
      #Adding a date slider input for filtering opening date
      sliderInput(inputId = "opendate",
                  label = "Select Date Range for analysis:", 
                  min = as.Date("1997-02-10"), max = as.Date("2023-02-10"), 
                  value = c(as.Date("1997-02-10"), as.Date("2023-02-10"))),
      
      ### INPUT 4---------------------------------------------------------------------------------------
      # Add Download Button
      downloadButton("downloadData", "Download"),
      h6("Press the download button to save the dataset you are looking at."),
      
      ### MAP OUTPUT 'leaflet2'-------------------------------------------------------------------------
      # Reference map description
      h6("Reference Map: Counties By Their Associated Political Party (2018-2020 congressional district elections)"),
      h6("Red = Republican Counties | Blue = Democrat Counties"),
      # Map Output
      leafletOutput("leaflet2")
      ),
#--------------------------------------------------------------------
      mainPanel(
        tabsetPanel(
          
          ### TAB 1-----------------------------------------------------
          tabPanel("Country Wise Map Analytics", shinyjs::useShinyjs(),
                    # Style the background and change the page
                    tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                        body {background-color: white}"),
                    h4("Input: Select party and date range :"),
                    h4("Output: Distribution of ev charging stations in counties across the entire nation for a given party democratic/republican"),
                   
                    # Map Output
                    leafletOutput("leaflet3"),
                    # Number of projects
                    textOutput("text1")),
          
          ### TAB 2----------------------------------------------------
          tabPanel("State Wise Map Analytics", shinyjs::useShinyjs(),
                    # Style the background and change the page
                    tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                        body {background-color: white}"),
                    h4("Input: Select state, party and date range :"),
                    h4("Output: Gives you only electric charging station locations for republican/democratic counties of a state in a given date range"),
                    h6("Note: You may not see the map intearct right away, please look at the number of points plotted below the map, if the number 'electric charging stations:' is greater than 0, then re-enter inputs for the points to show"),
                   
                   
                    # Map Output
                    leafletOutput("leaflet"),
                    # Number of projects
                    textOutput("text")),
          
          ### TAB 3----------------------------------------------------
          tabPanel("Data Table output",
                   fluidPage(
                   wellPanel(DT::dataTableOutput("table")))),
          ### TAB 4----------------------------------------------------
          tabPanel("plots", 
                   h4("Plot 1.  "),
                   h6("Input: Select state, party and date range :"),
                   h6("output: Number of new ev charging stations added for democratic/republican counties in the State vs year"),
                   h6("Intuition: If the number of charging stations added per year are stagnant or decreasing, further investigation of State's counties EV sentiment needs to be assesed "),
                   plotlyOutput("ev_count_plot"), 
                   hr(), 
                   h4("Plot 2."), 
                   h6("Input: Select state, party and date range :"),
                   h6("output: A share of single point and multipoint charging stations in the state for its democratic/republican counties"),
                   h6("Intuition: we can interpret that states (with either democratic or republican counties with higher percentage of multipoint charging stations have a positive perception of EV subsidy"),
                   plotlyOutput("pie_chart"),
                   hr(), 
                   h4("Plot 3."),
                   h6("Input: Select state, party and date range :"),
                   h6("output: A scatter plot (for republican or Democratic counties, based on the input) and their distribution of the number of counts of charging stations they have"),
                   h6("Intuition: This is to be used for summary statistics, toggle between the party inputs to see the differences for democratic and republican counties"),
                   plotlyOutput("scatter_plot")
                   )
          
      )))
) 
#UI Ends here
#-----------------------------------------------------------------------------------------------------------------------------------------

#Server Begins here
#-----------------------------------------------------------------------------------------------------------------------------------------
server <- function(input, output) {

### Define server logic required to create a map-------------------------------------------------------------------------------------  
    #Basic Map1 (Output for State-wise ev_data centers)
        output$leaflet <- renderLeaflet({
                        leaflet() %>%
                        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
                        addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
                        setView(-74.0060, 40.7128, 3) %>%
                        addLayersControl(baseGroups = c("Google", "Wiki"))})
    # Basic Map 2 
        output$leaflet2 <- renderLeaflet({
                        leaflet() %>%
                        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
                        addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
                        setView(-95.7129, 37.0902, 3) %>%
                        addLayersControl(baseGroups = c("Google", "Wiki"))})
    # Basic Map 3 (Output for country-wise ev_data centers)
        output$leaflet3 <- renderLeaflet({
                        leaflet() %>%
                        addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
                        addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
                        setView(-95.7129, 37.0902, 3) %>%
                        addLayersControl(baseGroups = c("Google", "Wiki"))})
  
  #-----------------------------------------------------------------------------------------------------      
  # Electric vehicle charging station data (Filtered data) for statewise map
  EvDataInf <- reactive({
    EvInf <-  ev_data %>% 
    req(input$selected_type)
    req(input$state_select)
    req(input$opendate)
    filter(EvInf, party %in% input$selected_type & State %in% input$state_select & open_date <= input$opendate[2] & open_date >= input$opendate[1])
    
                        })
  # Electric vehicle charging station data (Filtered data) for countrywise map
  EvDataInf_country <- reactive({
    EvInf <-  ev_data %>% 
    # ensure availablity of value before proceeding
    req(input$selected_type)
    req(input$opendate)
    filter(EvInf, party %in% input$selected_type & open_date <= input$opendate[2] & open_date >= input$opendate[1])
    
                        })
  
#------------------------------------------------------------------------------------------------------
#Replace layer with filtered partisan data 
#For Leaflet and leaflet3
  observe({
    EvInf <- EvDataInf()
    
    leafletProxy("leaflet", data = EvInf) %>%
      clearGroup(group = "EvInf") %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(icon = ~icons[party], clusterOptions = markerClusterOptions(), popup = ~paste0("<b>", "</b>: ", address), group = "EvInf", layerId = ~...1)
  })
  
  observe({
    EvInf <- EvDataInf_country()
    leafletProxy("leaflet3", data = EvInf) %>%
      clearGroup(group = "EvInf") %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(icon = ~icons[party], clusterOptions = markerClusterOptions(), popup = ~paste0("<b>", "</b>: ", party), group = "EvInf", layerId = ~...1)
  })
#-----------------------------------------------------------------------------------------
#For Leaflet 2             
#Filter for county partisan data
  county_input <- reactive({
    counttt <- subset(co, party == input$selected_type)
    return(counttt)
  })
  #Dataset for Republican counties
  Republican_counties <- subset(co, party == "Republican")
  #Dataset for Democrat counties
  Democratic_counties <- subset(co, party == "Democratic")
  
  
  #Plot partisan county map
  observe({
    par_count <- county_input()
    # Map2 with Republican counties
    leafletProxy("leaflet2", data = Republican_counties) %>%
      clearGroup(group = "par_count") %>%
      addPolygons(popup = ~paste0("<b>", county_name, "</b>"), group = "county", layerId = ~GEOID, fill = FALSE, color = "red") #%>%
    
  })
  
  # Map2 with democrat counties
  observe({
    par_count <- county_input()
    # Data is par_count
    leafletProxy("leaflet2", data = Democratic_counties) %>%
      clearGroup(group = "par_count") %>%
      addPolygons(popup = ~paste0("<b>", county_name, "</b>"), group = "county", layerId = ~GEOID, fill = FALSE, color = "blue") #%>%
    
  })
  
#-------------------------------------------------------------------------------------------  
  #For Text Output seen on the screen below the maps
  onScreen <- reactive({
    req(input$leaflet_bounds)
    bounds <- input$leaflet_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    subset(EvDataInf(), latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  onScreen_country <- reactive({
    req(input$leaflet3_bounds)
    bounds <- input$leaflet3_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    subset(EvDataInf_country(), latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  output$text <- renderText({
    paste("You are viewing", nrow(onScreen()), "Number of Electric Charging stations on screen")
  })
  
  output$text1 <- renderText({
    paste("You are viewing", nrow(onScreen_country()), "Number of Electric Charging stations on screen")
  })
  
  #------------------------------------------------------------------------------
  output$table <- DT::renderDataTable({
    subset(EvDataInf(),select = c(address, State, county_name, party, open_date))
    })
  
  #Download button
  output$downloadData <- downloadHandler(filename = function() {
    paste("ev-state-party ", Sys.Date(), ".csv", sep="")},
    
  content = function(file) {
    write.csv(EvDataInf(), file)})
  #-----------------------------------------------------------------------------------------------
  #Plots
  output$ev_count_plot <- renderPlotly({
    plot1 <- EvDataInf() %>%
            group_by(year = lubridate::year(open_date)) %>%
            summarize(total_count = sum(`EV_count`))
            plot_ly(plot1, x = ~year, y = ~total_count, type = "bar") %>%
                  layout(
                          margin = list(l = 50, r = 50, b = 50, t = 50, pad = 4),
                          xaxis = list(title = "Year", tickmode = "linear", tick0 = min(plot1$year), dtick = 1),
                          yaxis = list(title = "Total Count of EV Charging points added per year"),
                          bargap = 0.1,bargroupgap = 0.2)
      
                                      })
  #-------------------------------------------------------------------------------------------------
  output$pie_chart <- renderPlotly({
    categorize_ev_count <- function(x) {
                  if (x == 1) {
                      return("locations with Single outlet charging stations: ")
                } else if (x <= 10) {
                      return("locations with atleast 10 charging stations: ")
                } else {
                      return("Locations with multiple charging points: ")
                        }
                                            }
    EVinfPie <- na.omit(EvDataInf())
    plot2 <- EVinfPie %>%
              mutate(category = sapply(EV_count, categorize_ev_count)) %>%
              group_by(category) %>%
              summarize(count = n())
    
    plot_ly(plot2, labels = ~category, values = ~count, type = "pie")
                                  })
  
  #------------------------------------------------------------------------
  output$scatter_plot <- renderPlotly({
    
    EVinfScat <- na.omit(EvDataInf())
    
    df_summarized <- EVinfScat %>%
      group_by(county_name) %>%
      summarise(total_ev_count = sum(EV_count))
    
    plot_ly(df_summarized, x = ~county_name, y = ~total_ev_count, type = 'scatter', mode = 'lines+markers')
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
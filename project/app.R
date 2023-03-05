#Author- Shantanu Samant


#THE AIM OF THIS FINAL PROJECT IS TO WORK ON ANSWERING AND INVESTIGATING THE FOLLOWING QUESTION VISUALLY
#Question How is the EV charging station infrastructure at any county in USA, 
#co-related/associated with the political inclination of that location and its population.
#Inputs: State, EVSE number slider, Time range

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



# Load and clean  data ----------------------------------------------
ev_data <- read_csv("my_data_new.csv")
ev_data <- ev_data %>% rename(latitude = `Latitude`) #Renaming the column name to be case sensituve
ev_data <- ev_data %>% rename(longitude = `Longitude`)#Renaming the column name to be case sensituve
ev_data <- ev_data %>% rename(open_date = `Open Date`) #Renaming Open Date (I need an '_' in the column name for simplicity)
ev_data <- ev_data %>% rename(address = `Station Name`)
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
  titlePanel(title=div(img(height = 105, width = 300, src="cs2.png") , "Politics Behind Green Energy"), windowTitle = "myBrowserTitle"),
  
  #Desiginging and structuring the sidebar layout
  sidebarLayout(
    sidebarPanel( #Things to be accomodated into the side bar panel
      
      #1
      #Radio Button which the user selects party they want to investigate further in the dataset
      #Outputs coverd by input: a) Map2 i.e "leaflet2"  b) plot under plot tab
      radioButtons(inputId = "selected_type",
                   label = "Select Party [Filter for Tabs: \n1)Maps 2)Plots    \n3)Data Table]",
                   choices = c("Republican", "Democratic" ),
                   selected = "Democratic"),
      
      
      hr(),
      #2
      #A dropdown list that will allow a states charging infrastructure thay want to analyse
      selectInput(inputId = "state_select", 
                  label = "Select a state: <For Plots Tab>", 
                  choices = states_vector),
      #4
      # Add Download Button
      downloadButton("downloadData", "Download"),
      h6("Press the download button to save the dataset you are looking at."),
      
      #5
      # Reference map description
      h6("Reference Map: Counties By Their Associated Political Party (2018-2020 congressional district elections)"),
      h6("Red = Republican Counties | Blue = Democrat Counties"),
      # Map Output
      leafletOutput("leaflet2")
      
    ),
    mainPanel(
      tabsetPanel(
        #--------------------------------------------------------------------
        tabPanel("Country Wise Map Analytics", shinyjs::useShinyjs(),
                 # Style the background and change the page
                 tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                        body {background-color: white}"),
                 # Map Output
                 leafletOutput("leaflet3"),
                 
                 # Number of projects
                 textOutput("text1")),
        #-------------------------------------------------------------
        tabPanel("State Wise Map Analytics", shinyjs::useShinyjs(),
                 # Style the background and change the page
                 tags$style(type = "text/css", ".leaflet {height: calc(100vh - 90px) !important;}
                                        body {background-color: white}"),
                 # Map Output
                 leafletOutput("leaflet"),
                 # Number of projects
                 textOutput("text")),
        #--------------------------------------------------------------
        tabPanel("Data Table output",
                 fluidPage(
                   wellPanel(DT::dataTableOutput("table"))
                 ))
        
        
      )))
) 
#UI Ends here


#Define server logic required to create a map

server <- function(input, output) {
  
  # Basic Map1 (Output for State-wise ev_data centers)
  output$leaflet <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-74.0060, 40.7128, 3) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  # Basic Map 2 
  output$leaflet2 <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-95.7129, 37.0902, 3) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  # Basic Map 3 (Output for country-wise ev_data centers)
  output$leaflet3 <- renderLeaflet({
    leaflet() %>%
      addTiles(urlTemplate = "http://mt0.google.com/vt/lyrs=m&hl=en&x={x}&y={y}&z={z}&s=Ga", attribution = "Google", group = "Google") %>%
      addProviderTiles(provider = providers$Wikimedia, group = "Wiki") %>%
      setView(-95.7129, 37.0902, 3) %>%
      addLayersControl(baseGroups = c("Google", "Wiki"))
  })
  
  #-----------------------------------------------------------------------------------------------------      
  # Electric vehicle charging station data (Filtered data) for statewise map
  EvDataInf <- reactive({
    EvInf <-  ev_data %>% 
      #req(input$state) # ensure availablity of value before proceeding
      req(input$selected_type)
    req(input$state_select)
    filter(EvInf, party %in% input$selected_type & State %in% input$state_select)# & Created_at <= input$startdate[2] & BotP >= input$bot_prob[1] & BotP <= input$bot_prob[2])
    
  })
  # Electric vehicle charging station data (Filtered data) for countrywise map
  EvDataInf_country <- reactive({
    EvInf <-  ev_data %>% 
      #req(input$state) # ensure availablity of value before proceeding
      req(input$selected_type)
    filter(EvInf, party %in% input$selected_type)# & State %in% input$state_select)# & Created_at <= input$startdate[2] & BotP >= input$bot_prob[1] & BotP <= input$bot_prob[2])
    
  })
  
  #------------------------------------------------------------------------------------------------------
  # Replace layer with filtered partisan data 
  #For Leaflet and leaflet3
  observe({
    EvInf <- EvDataInf()
    
    leafletProxy("leaflet", data = EvInf) %>%
      clearGroup(group = "EvInf") %>%
      clearMarkerClusters() %>%
      addAwesomeMarkers(icon = ~icons[party], clusterOptions = markerClusterOptions(), popup = ~paste0("<b>", "</b>: ", party), group = "EvInf", layerId = ~...1)
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
  # Filter for county partisan data
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
}

# Run the application 
shinyApp(ui = ui, server = server)
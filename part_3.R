  
# load packages ####


library(leaflet)
library(leafem)
library(mapview)
library(sf)
options("rgdal_show_exportToProj4_warnings"="none")
library(raster)
library(rgbif)
library(ggplot2)
library(dplyr)
library(rgbif)
library(BIRDS)
library(behaviouR)
library(tuneR)
library(seewave)
library(ggplot2)
library(dplyr)
library(warbleR)
library(shiny)

# elevation data ####

elevation <- raster("www/spatial/elevation.tif")

# enter lat and long ####

ll_crs <- CRS("+init=epsg:4326")  
elevation_ll <- projectRaster(elevation, crs=ll_crs)
elevation500m <- aggregate(elevation, fact=10)
elevation500m_ll <- projectRaster(elevation500m, crs=ll_crs)
elevation500m_ll





# lakes data ####

lakes <- st_read("www/spatial/cumbria_lakes.shp")
lakes_ll <- st_transform(lakes,crs=ll_crs)

lake_view <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>%
  addFeatures(lakes_ll) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"),
    overlayGroups = c("Lakes"),
    options = layersControlOptions(collapsed = TRUE)
  )

# settlement data ####

settlements <- st_read("www/spatial/cumbria_settlements.shp")
settlements_ll <- st_transform(settlements,crs=ll_crs)

settlement_view <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>%
  addFeatures(settlements_ll) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"),
    overlayGroups = c("settlements"),
    options = layersControlOptions(collapsed = TRUE)
  )


# rivers data ####

rivers <- st_read("www/spatial/cumbria_rivers.shp")
rivers_ll <- st_transform(rivers,crs=ll_crs)

rivers_view <- leaflet() %>%
  addTiles(group = "OSM (default)") %>%
  addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
  setView(lng = -3.0886, lat=54.4609, zoom=9) %>%
  addFeatures(rivers_ll) %>%
  addLayersControl(
    baseGroups = c("OSM (default)", "Satellite"),
    overlayGroups = c("rivers"),
    options = layersControlOptions(collapsed = TRUE)
  )


# read in the data ####

# first we will start with the red kite data ####

red_kite <- read.csv("data/red_kite/red_kite.csv")

# Plotting trends over time ####

ggplot(red_kite, aes(x=year.processed)) +
  geom_histogram()

yearly_redkite_records <- red_kite %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())

red_kite_plot <- ggplot(yearly_redkite_records, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Total Observed")

red_kite_plot

# repeat the same process for the other two species ####

goshawk <- read.csv("data/goshawk/goshawk.csv")

# Plotting trends over time ####

ggplot(goshawk, aes(x=year.processed)) +
  geom_histogram()

yearly_goshawk_records <- goshawk %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())

goshawk_plot <- ggplot(yearly_goshawk_records, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Total Observed")

goshawk_plot

# finally we repeat this step for the last species ####

kestrel <- read.csv("data/kestrel/kestrel.csv")

# Plotting trends over time ####

ggplot(kestrel, aes(x=year.processed)) +
  geom_histogram()

yearly_kestrel_records <- kestrel %>%
  group_by(year.processed) %>%
  summarise(count_per_year = n())

kestrel_plot <- ggplot(yearly_kestrel_records, aes(x = year.processed, y=count_per_year)) +
  geom_line() + xlab("Years") + ylab("Total Observed")

kestrel_plot

# reading in and saving images ####

goshawk_image <- base64enc::dataURI(file="www/goshawk.jpeg", mime="image/jpeg")
kestrel_image <- base64enc::dataURI(file="www/kestrel .jpeg", mime="image/jpeg")
red_kite_image <- base64enc::dataURI(file="www/red_kite.jpeg", mime="image/jpeg")


# read in sound ####

readWave("www/bird_audio.wav")


# Define UI ----
ui <- fluidPage(
  
  # Application title
  titlePanel("Cumbria Environments"),
  
  # Sidebar ----
  sidebarLayout(
    sidebarPanel(titlePanel("Bird species in rural environment of Cumbria"),
                 p("This will show images and information about three bird species which are found in Cumbria. All species listed here will correlate with ones found on the interactive map."),
                 p("Kestrels have a prominent black malar stripe down their back. They are the only UK bird species who can 'hover, suspending theselves still in the air whilst looking for prey."),
                 
                 img(src=kestrel_image,height="50%", width="50%"),
                 
                 p("The goshawk is often recognizable simply by its shape and large size, but adults can also be readily identified by the gray breast which sets it apart from the smaller accipiters."),
                 
                 img(src=goshawk_image,height="50%", width="50%"),
                 
                 p("Red kites have a rusty reddish-brown body with a deeply forked tail. Their head is pale grey and patterned with dark streaks, and they have a yellow beak with a dark hook and pale, striking eyes."),
                 
                 img(src=red_kite_image,height="60%", width="60%"),
                 
                 
    ),
    
    
    # Main panel ----
    mainPanel( p("This website has been comprised to encompass environmental data for public access. Rivers, elevation,  settlements, lakes and three bird species data are all found within this website;", strong("Kestrel, Goshawk and Red Kite."), p("The map is interactive, therefore, you can minipulate it for your own purpose.")),
               leafletOutput(outputId = "map"),
               p("Below are graphs indicating Kestrel, Goshawk and Red Kite populations over time. These records were taken from the National Biodiversity Network (NBN). This database uses citizen science, in which members of the public can submit oberservations and sightings of species they have seen."),
               plotOutput(outputId = "kestrel_plot"),
               plotOutput(outputId = "goshawk_plot"),
               plotOutput(outputId = "red_kite_plot"),
               p("From these graphs, it is clear that kestrels are on the decline in Cumbria, or recording efforts are lesser.")
    )))



# server logic ----
server <- function(input, output) {
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addRasterImage(elevation500m_ll,col=terrain.colors(30), group = "elevation") %>%
      addCircleMarkers(kestrel$decimalLongitude.processed,
                       kestrel$decimalLatitude.processed,
                       label = kestrel$common_name, group = "Kestrel",
                       labelOptions = labelOptions(interactive = "TRUE"),
                       radius = 4, fillOpacity = 0.5, opacity = 0.5, col="green") %>%
      addCircleMarkers(goshawk$decimalLongitude.processed,
                       goshawk$decimalLatitude.processed,
                       label = goshawk$common_name, group = "Goshawk",
                       labelOptions = labelOptions(interactive = "TRUE"),
                       radius = 4, fillOpacity = 0.5, opacity = 0.5, col="purple") %>%
      addCircleMarkers(red_kite$decimalLongitude.processed,
                       red_kite$decimalLatitude.processed,
                       label = red_kite$common_name, group = "Red Kite",
                       labelOptions = labelOptions(interactive = "TRUE"),
                       radius = 4, fillOpacity = 0.5, opacity = 0.5, col="red") %>%
      addFeatures(lakes_ll, group = "lakes") %>%
      addFeatures(rivers_ll, group = "rivers") %>%
      addFeatures(settlements_ll, group = "settlements", label = settlements_ll$NAME, labelOptions = labelOptions(interactive = "TRUE")) %>%
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("elevation", "lakes", "rivers", "settlements", "Kestrel", "Goshawk", "Red Kite"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
  })
  observeEvent(input$map, {
    click<-input$map
    text<-paste("Lattitude ", click$lat, "Longtitude ", click$lng)
    print(text)
    
  })
  
  output$kestrel_plot <- renderPlot( ggplot(yearly_kestrel_records, aes(x = year.processed, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Kestrels Observed"))
  
  output$goshawk_plot <- renderPlot( ggplot(yearly_goshawk_records, aes(x = year.processed, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Goshawks Observed"))
  
  output$red_kite_plot <- renderPlot( ggplot(yearly_redkite_records, aes(x = year.processed, y=count_per_year)) + geom_line() + xlab("Years") + ylab("Red Kites Observed"))
  
}



# Run the application
shinyApp(ui = ui, server = server)

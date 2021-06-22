library(shiny)
library(sf)
library(leaflet)
library(RColorBrewer)
library(htmltools)

geodata <- read_sf("https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.geojson")

#color-palette for polygons
pal <-colorNumeric(
  palette = "YlGn",
  domain = geodata$cases7_per_100k,
  reverse = T
)


#labels for polygons
labs <- as.list(paste0("<b>Infos</b> <br>", 
                       "<b>Name:</b> ", geodata$GEN, "<br>",
                       "<b>Einwohnerzahl:</b> ", geodata$EWZ, "<br>",
                       "<b>Bundesland:</b> ", geodata$BL, "<br>",
                       "<b>7-Tages-Inzidenzwert:</b> ", round(geodata$cases7_per_100k,2), "<br>",
                       "<b>Fälle (insg.):</b> ", geodata$cases, "<br>",
                       "<b>Todesfälle (insg.):</b> ", geodata$deaths))

ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 in Deutschland"),
  
  #PLACEHOLDER
  sidebarLayout(
    sidebarPanel(
      checkboxInput("placeholder", "PLACEHOLDER", FALSE)
    ),
    
    
    mainPanel(
      leafletOutput("c19map")
    )
  )
)


server <- function(input, output) {
  
  output$c19map <- renderLeaflet({
    #leaflet map
    leaflet(geodata) %>% 
      addTiles() %>% 
      addPolygons(stroke = F, smoothFactor = 0.2, fillOpacity = 0.3,
                  color = ~pal(cases7_per_100k)
      ) %>% 
      addLegend(position = "topright", pal = pal, values = ~cases7_bl_per_100k,
                title = "7-Tages-Inzidenzwerte"
      ) %>% 
      addPolygons(stroke = T, weight = 0.5, color = "black", 
                  label = lapply(labs, HTML))
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
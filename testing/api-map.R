library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
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
labs <- as.list(paste0("<b>Informations</b> <br>", 
                       "<b>Name:</b> ", geodata$GEN, "<br>",
                       "<b>Population:</b> ", geodata$EWZ, "<br>",
                       "<b>State:</b> ", geodata$BL, "<br>",
                       "<b>Cases per 100.000 (7 Days):</b> ", round(geodata$cases7_per_100k,2), "<br>",
                       "<b>Cases (total):</b> ", geodata$cases, "<br>",
                       "<b>Deaths (total):</b> ", geodata$deaths))

#Data for plots etc.
#overview
#str(geodata)

#Bundeslaender für selectInput()
bl <- sort(unique(geodata$BL))

#subset geodata for easier use
gdata <- geodata %>% select(c(BL, EWZ_BL, GEN, EWZ, cases, 
                              cases_per_100k, cases7_per_100k, 
                              death_rate,  deaths))

#Shiny App
ui <- fluidPage(
  
  # Application title
  titlePanel("COVID-19 in Germany"),
  
  #PLACEHOLDER
  fluidRow(
    column(3,
            selectInput("region", 
                  label = "Choose a region of Germany", 
                  choices = c("Germany (total)", bl),
                  selected = "Germany (total)"
                  )
    )
  ),
  fluidRow(
    column(4,
           conditionalPanel(
             condition = "input.region == 'Germany (total)'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Baden-Württemberg'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Bayern'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Berlin'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Brandenburg'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Bremen'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Hamburg'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Hessen'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Mecklenburg-Vorpommern'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Niedersachsen'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Nordrhein-Westfalen'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Rheinland-Pfalz'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Saarland'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Sachsen'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Sachsen-Anhalt'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Schleswig-Holstein'" 
           ),
           conditionalPanel(
             condition = "input.region == 'Thüringen'" 
           ),
           plotOutput("c7_hi"
           )
    ),
    column(4,
           plotOutput("c_hi"
           ),
           plotOutput("c_lo"
           )      
    ),
    column(4,
           plotOutput("d_hi"
           ),
           plotOutput("d_lo"
           )
    )
  ),
  
  fluidRow(
    column(12,
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
                  label = lapply(labs, HTML),
                  labelOptions = labelOptions(textsize = "12px"))
  })
  
  output$c7_hi <- renderPlot({
    #plot of LKs with highest c7/100k
    if (input$region == "Germany (total)"){
      gdata %>% 
        arrange(desc(cases7_per_100k)) %>% 
        slice(1:5) %>% 
        ggplot(., aes(x=reorder(GEN, -cases7_per_100k), y=cases7_per_100k)) + 
        geom_col()
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(desc(cases7_per_100k)) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, -cases7_per_100k), y=cases7_per_100k)) + 
          geom_col()
      }
    }
  })
  
  output$c_hi <- renderPlot({
    #plot of LKs with highest cases
    if (input$region == "Germany (total)"){
      gdata %>% 
        arrange(desc(cases)) %>% 
        slice(1:5) %>% 
        ggplot(., aes(x=reorder(GEN, -cases), y=cases)) + 
        geom_col()
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(desc(cases)) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, -cases), y=cases)) + 
          geom_col()
      }
    }
  })
  
  output$c_lo <- renderPlot({
    #plot of LKs with lowest cases
    if (input$region == "Germany (total)"){
      gdata %>% 
        arrange(cases) %>% 
        slice(1:5) %>% 
        ggplot(., aes(x=reorder(GEN, cases), y=cases)) + 
        geom_col()
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(cases) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, cases), y=cases)) + 
          geom_col()
      }
    }
  })
  
  output$d_hi <- renderPlot({
    #plot of LKs with highest deaths
    if (input$region == "Germany (total)"){
      gdata %>% 
        arrange(desc(deaths)) %>% 
        slice(1:5) %>% 
        ggplot(., aes(x=reorder(GEN, -deaths), y=deaths)) + 
        geom_col()
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(desc(deaths)) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, -deaths), y=deaths)) + 
          geom_col()
      }
    }
  })
  
  output$d_lo <- renderPlot({
    #plot of LKs with lowest deaths
    if (input$region == "Germany (total)"){
      gdata %>% 
        arrange(deaths) %>% 
        slice(1:5) %>% 
        ggplot(., aes(x=reorder(GEN, deaths), y=deaths)) + 
        geom_col()
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(deaths) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, deaths), y=deaths)) + 
          geom_col()
      }
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
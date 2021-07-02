library(shiny)
library(sf)
library(leaflet)
library(tidyverse)
library(RColorBrewer)
library(htmltools)
library(DT)


load_geodata <- function(url, save_flag=FALSE) {
  # Try to read geo data from the provided URL.
  # If reading fails, an older local copy is used.
  # Set save_flag = TRUE to replace the local copy
  # with newer data.
  tryCatch(
    expr = {
      geodata <<- read_sf(geodata_url)
    }, 
    error = function(e) {
      print(paste0("Error: Failed to read data from online source: ", geodata_url))
      print("Using local data set instead.")
      save_flag <<- FALSE
      load("geodata.RData")
      geodata <<- geodata
    },
    warning = function(w) {
      print(paste0("Warning: Failed to parse data from JSON source: ", geodata_url))
      print("Using local data set instead.")
      save_flag <<- FALSE
      load("geodata.RData")
      geodata <<- geodata
    }
  )
  
  if (save_flag) {
    # Saving data object in RData format.
    save(geodata, file = "geodata.RData")
  }
}


# Get geo data from online source.
geodata_url <- "https://opendata.arcgis.com/datasets/917fc37a709542548cc3be077a786c17_0.geojson"
load_geodata(geodata_url, save_flag = FALSE)


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
gdata <- geodata %>% as_tibble() %>% 
  select(c(BL, EWZ_BL, GEN, EWZ, cases, cases_per_100k, 
           cases7_per_100k, death_rate,  deaths))

#Shiny App
ui <- navbarPage("Covid-19 in Germany",
                 tabPanel("Map",
                          fillPage(
                            leafletOutput("c19map")
                            )
                          ),
                 tabPanel("Table",
                          fillPage(
                            DTOutput("tbl")
                            )
                          ),
                 tabPanel("Plots",
                          fluidRow(
                            column(3,
                                   selectInput("region", 
                                               label = "Choose a region of Germany", 
                                               choices = c("Germany (total)", bl),
                                               selected = "Germany (total)"))
                            ),
                          fluidRow(column(4,
                                          plotOutput("c7_hi")),
                                   column(4,
                                          plotOutput("c_hi"),
                                          plotOutput("c_lo")),
                                   column(4,
                                          plotOutput("d_hi"),
                                          plotOutput("d_lo"))
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
  
  output$tbl <- renderDT({
    gdata
  })
  
  output$c7_hi <- renderPlot({
    #plot of LKs with highest c7/100k
    if (input$region == "Germany (total)"){
      gdata %>% 
        arrange(desc(cases7_per_100k)) %>% 
        slice(1:5) %>% 
        ggplot(., aes(x=reorder(GEN, -cases7_per_100k), y=cases7_per_100k)) + 
        geom_col(aes(fill=cases7_per_100k)) + 
        scale_fill_distiller(palette = "Reds", direction = 1) +
        theme_classic() +
        theme(axis.text.x=element_text(angle=60,hjust=1)) +
        labs(title = "Cases per 100.000 (last 7 days)",
             subtitle = "Counties with the highest number of cases \n per 100.000",  
             x="County", 
             y="", fill="Cases per 100.000")
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(desc(cases7_per_100k)) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, -cases7_per_100k), y=cases7_per_100k)) + 
          geom_col(aes(fill=cases7_per_100k)) + 
          scale_fill_distiller(palette = "Reds", direction = 1) +
          theme_classic() +
          theme(axis.text.x=element_text(angle=60,hjust=1)) +
          labs(title = "Cases per 100.000 (last 7 days)",
               subtitle = "Counties with the highest number of cases \n per 100.000", 
               x="County", 
               y="", fill="Cases per 100.000")
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
        geom_col(aes(fill=cases)) +
        scale_fill_distiller(palette = "Reds", direction = 1) +
        theme_classic() +
        theme(axis.text.x=element_text(angle=60,hjust=1)) +
        labs(title = "Cases (total)",
             subtitle = "Counties with the highest number of cases", 
             x="County", 
             y="", fill="Total cases")
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(desc(cases)) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, -cases), y=cases)) + 
          geom_col(aes(fill=cases)) +
          scale_fill_distiller(palette = "Reds", direction = 1) +
          theme_classic() +
          theme(axis.text.x=element_text(angle=60,hjust=1)) +
          labs(title = "Cases (total)",
               subtitle = "Counties with the highest number of cases", 
               x="County", 
               y="", fill="Total cases")
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
        geom_col(aes(fill=cases)) +
        scale_fill_distiller(palette = "Greens", direction = -1) +
        theme_classic() +
        theme(axis.text.x=element_text(angle=60,hjust=1)) +
        labs(title = "Cases (total)",
             subtitle = "Counties with the lowest number of cases", 
             x="County", 
             y="", fill="Total cases")
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(cases) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, cases), y=cases)) + 
          geom_col(aes(fill=cases)) +
          scale_fill_distiller(palette = "Greens", direction = -1) +
          theme_classic() +
          theme(axis.text.x=element_text(angle=60,hjust=1)) +
          labs(title = "Cases (total)",
               subtitle = "Counties with the lowest number of cases", 
               x="County", 
               y="", fill="Total cases")
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
        geom_col(aes(fill=deaths)) +
        scale_fill_distiller(palette = "Reds", direction = 1) +
        theme_classic() +
        theme(axis.text.x=element_text(angle=60,hjust=1)) +
        labs(title = "Deaths (total)",
             subtitle = "Counties with the highest number of deaths", 
             x="County", 
             y="", fill="Total deaths")
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(desc(deaths)) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, -deaths), y=deaths)) + 
          geom_col(aes(fill=deaths)) +
          scale_fill_distiller(palette = "Reds", direction = 1) +
          theme_classic() +
          theme(axis.text.x=element_text(angle=60,hjust=1)) +
          labs(title = "Deaths (total)",
               subtitle = "Counties with the highest number of deaths", x="County", 
               y="", fill="Total deaths")
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
        geom_col(aes(fill=deaths)) +
        scale_fill_distiller(palette = "Greens", direction = -1) +
        theme_classic() +
        theme(axis.text.x=element_text(angle=60,hjust=1)) +
        labs(title = "Deaths (total)",
             subtitle = "Counties with the lowest number of deaths", 
             x="County", 
             y="", fill="Total deaths")
    } else {
      bundesland <- input$region
      if (input$region == bundesland){
        gdata %>% 
          filter(BL == bundesland) %>% 
          arrange(deaths) %>% 
          slice(1:5) %>% 
          ggplot(., aes(x=reorder(GEN, deaths), y=deaths)) + 
          geom_col(aes(fill=deaths)) +
          scale_fill_distiller(palette = "Greens", direction = -1) +
          theme_classic() +
          theme(axis.text.x=element_text(angle=60,hjust=1)) +
          labs(title = "Deaths (total)",
               subtitle = "Counties with the lowest number of deaths", 
               x="County", 
               y="", fill="Total deaths")
      }
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
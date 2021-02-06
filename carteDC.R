#-------------------------------------------------------------------------------------------------#
#                       Carte intéractive des DC par départements                                 #
#-------------------------------------------------------------------------------------------------#

# Packages nécessaires
# library(shiny)
# library(leaflet)
library(sf)
library(tidyverse)
library(lubridate)
library(zoo)
library(RColorBrewer)
library(htmltools)

# I. Préparation des données
#--------------------------------------------------------------------------------------

# departement_filtre <- c("85","49","37","36","18","03","63","15","46","82","32","65",
#                         "64","40","47","33","24","19","87","23","16","17","79","86")

# I.1. Cartes
reg <- st_read("data/maps/reg_francemetro_2019.shp",options = "ENCODING=WINDOWS-1252") %>% 
  st_transform(4326) %>% 
  mutate(label = paste0(libelle,"(",code,")")) %>% 
  select(label)
dep <- st_read("data/maps/dep_francemetro_2019.shp",options = "ENCODING=WINDOWS-1252") %>% 
  st_transform(4326) %>% 
  mutate(label = paste0(libelle,"(",code,")")) 
# com <- st_read("data/maps/commune_francemetro_2019.shp",options = "ENCODING=WINDOWS-1252") %>%
#   filter(dep %in% departement_filtre) %>% 
#   st_simplify(dTolerance = 100) %>% 
#   st_transform(4326) %>% 
#   mutate(label = paste0(libelle,"(",code,")")) %>% 
#   select(label)


# I.2. Borne de découpe des couleurs
couleur_surmortalite <- c(-Inf, 80,90,98,102,110,120,Inf)
palette_couleur_surmortalite <- colorBin("RdYlGn", domain = c(0,200), bins = couleur_surmortalite,reverse = TRUE)

# I.3. Décés 

departements <- read.csv2("data/metadata/departement.csv",sep = ",",stringsAsFactors = F,header = F) %>% 
  rename(codegeo = V2, libelle = V5)

dc <- bind_rows(
  read.csv2("data/D14/DC_2018_det.csv",stringsAsFactors = FALSE),
  read.csv2("data/D14/DC_2019_det.csv",stringsAsFactors = FALSE),
  read.csv2("data/D14/DC_2020_det.csv",stringsAsFactors = FALSE)
) %>% 
  mutate(date_dc = as.Date(parse_date_time(paste0(JDEC,"-",MDEC,"-",ADEC),orders = "dmy")))

# DC departements Nouvelle-Aquitaine
dc_dep <- dc %>% 
  # filter(DEPDEC %in% departements_NA) %>% 
  # slice(1:10000) %>%
  group_by(DEPDEC,ADEC,date_dc) %>%
  summarise(total_dc = n()) %>%
  left_join(departements %>% 
              select(codegeo,geo=libelle), by = c("DEPDEC" = "codegeo")) %>% 
  rename(codegeo=DEPDEC)

# Décés journalier par geographie
dc2 <- dc_dep %>% 
  mutate(semaine_iso = isoweek(date_dc)) %>% 
  group_by(codegeo,geo, ADEC, semaine_iso) %>% 
  summarise(total_dc = sum(total_dc)) %>% 
  ungroup() %>% 
  # mutate(Date = floor_date(ymd(paste0(ADEC,"-01-01")) + weeks(semaine_iso), "weeks",week_start = 1)) %>% 
  group_by(ADEC,codegeo,geo) %>% 
  mutate(roll_mean = rollmean(total_dc, 3, na.pad = T),
         # date_dc2 = format(date_dc, format="%d-%m")
  ) %>% 
  select(-total_dc) %>% 
  filter(!is.na(roll_mean)) %>% 
  spread(key = ADEC,value = roll_mean) %>% 
  mutate(surmortalite = 100*2* `2020` / (`2018` + `2019`),
         Date = floor_date(ymd("2020-01-01") + weeks(semaine_iso), "weeks",week_start = 1),
         date.f = factor(format(Date,"%d-%b"),levels = unique(format(Date,"%d-%b")))
         ) %>% 
  ungroup() %>% 
  mutate(dc20 = round(`2020`),
         dc1819_moyen = round((`2019` + `2018`)/2))

# II. Application Shiny
#-----------------------------------------------------------------------------------------------------------------
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mapAct", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("animation", "Semaine",
                #             min = as.POSIXct("2017-02-15 00:00:00",tz = "Europe/Budapest"),
                #             max = as.POSIXct("2017-02-15 23:59:59",tz = "Europe/Budapest"),
                #             value = as.POSIXct("2017-02-15 00:00:00",tz = "Europe/Budapest"),
                #             timezone = "+0200",
                #             animate =
                #               animationOptions(interval = 600, loop = TRUE))
                
                sliderInput("animation", "Semaines",
                            min = 3, max = 43,
                            value = 1, step = 1,
                            animate =
                              animationOptions(interval = 500, loop = FALSE))
                
  )
  
)

# Define server logic required
server <- function(input, output) {
  # Données département filtrées à partir du slider input
  filteredData <- reactive({
    
    print(input$animation)
    
    dep %>% 
      left_join(dc2 %>% filter(semaine_iso == input$animation) %>% 
                  ungroup() %>% 
                  select(codegeo,surmortalite,date.f,dc20,dc1819_moyen), by=c("code" = "codegeo") ) %>% 
      mutate(label_surmortalite = paste0("Département : ",label,"<br/>Date : ",date.f ,
                                         "<br/>Surmortalite : ", round(surmortalite,1),
                                         "<br/>Nb décès 2020 : ",dc20,
                                         "<br/>Moyenne décès 2018-2019 : ",dc1819_moyen))
  })
  
  
  output$mapAct<-renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     minZoom = 1, maxZoom = 14)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # Région
      addPolygons(data=reg,opacity = 3,group = "Région",
                  color = "#2F9599", stroke = TRUE, weight = 2,
                  fill = TRUE, fillOpacity = 0.01,
                  label = ~paste(label),
                  highlightOptions = highlightOptions(
                    color = "#F26B38", opacity = 1, weight = 3, fillOpacity = 0,
                    bringToFront = FALSE, sendToBack = TRUE)) %>%
      # Départements
      addPolygons(data=dep,opacity = 4,group = "Départements",
                  color = "#CC527A", stroke = TRUE, weight = 2,
                  fill = TRUE, fillOpacity = 0.01,
                  label = ~paste(label),
                  highlightOptions = highlightOptions(
                    color = "#A8A7A7", opacity = 1, weight = 3, fillOpacity = 0,
                    bringToFront = TRUE, sendToBack = TRUE)) %>% 
      addLegend(pal = palette_couleur_surmortalite, values = c(0,200),
                  # dc2$surmortalite[dc2$semaine_iso == 2], 
                opacity = 0.7, title = "Surmortalité (base 100)",
                position = "bottomright") %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("Région","Départements","Surmortalite"
                          
        ),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomleft"
      ) %>% 
      hideGroup(c("Région","Départements"))
      
      # fitBounds(lng1 = 5,lat1 = 52,lng2 = 5.2,lat2 = 52.2)# set to reactive minimums
  })
  
  observe({
    leafletProxy("mapAct") %>%
      # clearShapes() %>%
      addPolygons(data = filteredData(),layerId = filteredData()$code,
                  fillColor = ~palette_couleur_surmortalite(filteredData()$surmortalite),
                  weight = 2,
                  opacity = 1,
                  color = "white",
                  dashArray = "3",
                  fillOpacity = 0.7,
                  label = lapply(filteredData()$label_surmortalite, htmltools::HTML),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)
      ) 
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

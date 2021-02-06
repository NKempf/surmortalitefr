#-------------------------------------------------------------------------------------------------#
#                 Interactive map of excess mortality in France during Covid crisis               #
#-------------------------------------------------------------------------------------------------#

# TODO
# 1. Solutionner le problème de la 53eme semaine
# 2. Prolonger la tendance jusqu'au dernier jours voir une à deux semaine après.


# Packages
library(sf) # spatial object
library(tidyverse) # tidy data
library(lubridate) # date format
library(zoo) # Moving average
library(shiny) # App
library(leaflet) # Interactive map
library(RColorBrewer) # Color palette
library(htmltools)

# I. Import and tidy data
#------------------------------------------------------------------------------------------------

# Region map ("https://www.data.gouv.fr/fr/datasets/r/aacf9338-8944-4513-a7b9-4cd7c2db2fa9")
reg <- st_read("maps/region/regions-20180101.shp",options = "ENCODING=UTF-8") %>% 
  st_simplify(preserveTopology = T,0.008) %>%  # simplify geometry
  st_transform(4326) %>% # new coordinate system
  mutate(label = paste0(nom,"(",code_insee,")")) %>%
  select(code_insee,label)

# Departments map (https://www.data.gouv.fr/fr/datasets/contours-des-departements-francais-issus-d-openstreetmap/,
#  https://public.opendatasoft.com/explore/dataset/contours-geographiques-des-departements-2019/export/?flg=fr)
# dep <- st_read("maps/departments/departements-20180101.shp",options = "ENCODING=UTF-8") %>% 
dep <- st_read("maps/departments/contours-geographiques-des-departements-2019/contours-geographiques-des-departements-2019.shp",options = "ENCODING=UTF-8") %>% 
  st_simplify(preserveTopology = T,0.008) %>%  # simplify geometry
  st_transform(4326) %>% # new coordinate system
  mutate(label = paste0(nom_dep,"(",insee_dep,")")) %>% 
  select(code_insee=insee_dep,label)

# Aire d'attraction des villes (https://www.insee.fr/fr/information/4803954)

# death daily data (https://www.insee.fr/fr/statistiques/4487988)
death <- bind_rows(
  read.csv2("data/death/DC_2018_det.csv",stringsAsFactors = FALSE),
  read.csv2("data/death/DC_2019_det.csv",stringsAsFactors = FALSE),
  read.csv2("data/death/DC_20202021_det.csv",stringsAsFactors = FALSE)
) %>%
  mutate(date_dc = as.Date(parse_date_time(paste0(JDEC,"-",MDEC,"-",ADEC),orders = "dmy")))

# II. Statistical computation
#------------------------------------------------------------------------------------------------
death_dep <- death %>% 
  mutate(week_iso = isoweek(date_dc)) %>% 
  group_by(DEPDEC, ADEC, week_iso) %>% 
  summarise(total_dc = n()) %>% # weekly death by departments
  ungroup() %>% 
  group_by(DEPDEC) %>% 
  mutate(roll_mean = rollmean(total_dc, 3, na.pad = T),
         roll_mean = ifelse(is.na(roll_mean),total_dc,roll_mean)) %>% # Moving average => smoothing data
  select(-total_dc) %>% 
  filter(!is.na(roll_mean)) %>%
  spread(key = ADEC,value = roll_mean) %>% 
  mutate(death_ref = (`2018` + `2019`) / 2 ) %>% # Weekly average death (2018-2019) 
  select(-`2018`,-`2019`) %>% 
  gather(key = an, value = death, -DEPDEC,-week_iso,-death_ref) %>% 
  filter(!is.na(death_ref) & !is.na(death)) %>%   
  mutate(surmortalite = 100* death / death_ref, # Excess mortality
         week_first_day = floor_date(ymd(paste0(an,"-01-01")) + weeks(week_iso), "weeks",week_start = 1),
         week_first_day.f = factor(format(week_first_day,"%d %b %y"),levels = unique(format(week_first_day,"%d %b %y")))
  ) %>% 
  ungroup() 
 
# II. Application Shiny
#-----------------------------------------------------------------------------------------------------------------

# Color
couleur_surmortalite <- c(-Inf, 80,90,98,102,110,120,Inf)
palette_couleur_surmortalite <- colorBin("RdYlGn", domain = c(0,200), bins = couleur_surmortalite,reverse = TRUE)

#*********************************************************************************************
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mapAct", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("animation", "Date",
                            min = min(death_dep$week_first_day), 
                            max = max(death_dep$week_first_day),
                            value = death_dep$week_first_day[1], 
                            timeFormat="%d %b %y",
                            step = 7,
                            animate =
                              animationOptions(interval = 1000, loop = FALSE))
  )
)

#*********************************************************************************************
server <- function(input, output) {
  # Données département filtrées à partir du slider input
  filteredData <- reactive({

    dep %>%
      left_join(death_dep %>% 
                  filter(week_first_day == input$animation) %>%
                  ungroup() %>%
                  select(codegeo=DEPDEC,surmortalite,week_first_day.f,death,death_ref), by=c("code_insee" = "codegeo") ) %>%
      mutate(label_surmortalite = paste0("Area : ",label,"<br/>Date : ",week_first_day.f ,
                                         "<br/>Excess mortality (base 100) : ", round(surmortalite,1),
                                         "<br/>Smoot number weekly death : ",round(death,0),
                                         "<br/>Average number of death 2018-2019 : ",round(death_ref,0)))
  })
  
  output$mapAct<-renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = TRUE,
                                     minZoom = 1, maxZoom = 14)) %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      # Région
      addPolygons(data=reg,opacity = 3,group = "Region",
                  color = "#2F9599", stroke = TRUE, weight = 2,
                  fill = TRUE, fillOpacity = 0.01,
                  label = ~paste(label),
                  highlightOptions = highlightOptions(
                    color = "#F26B38", opacity = 1, weight = 3, fillOpacity = 0,
                    bringToFront = FALSE, sendToBack = TRUE)) %>%
      # Départements
      addPolygons(data=dep,opacity = 4,group = "Departments",
                  color = "#CC527A", stroke = TRUE, weight = 2,
                  fill = TRUE, fillOpacity = 0.01,
                  label = ~paste(label),
                  highlightOptions = highlightOptions(
                    color = "#A8A7A7", opacity = 1, weight = 3, fillOpacity = 0,
                    bringToFront = TRUE, sendToBack = TRUE)) %>% 
      addLegend(pal = palette_couleur_surmortalite, values = c(0,200),
                  # dc2$surmortalite[dc2$semaine_iso == 2], 
                opacity = 0.7, title = "Excess mortality (base 100)",
                position = "bottomright") %>% 
      addLayersControl(
        baseGroups = c("OSM (default)", "Satellite"),
        overlayGroups = c("Region","Departments","Excess mortality"
                          
        ),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomleft"
      ) %>% 
      hideGroup(c("Region","Departments"))
      
      # fitBounds(lng1 = 5,lat1 = 52,lng2 = 5.2,lat2 = 52.2)# set to reactive minimums
  })
  
  observe({
    leafletProxy("mapAct") %>%
      # clearShapes() %>%
      addPolygons(data = filteredData(),layerId = filteredData()$code_insee,
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

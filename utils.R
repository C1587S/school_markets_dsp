cat("\014") 

# clear environment
clear_env = readline("Clear the R environment? (y/n) > ")

if (clear_env=="y"){
  rm(list = ls()) 
}

Sys.sleep(1)

cat("\014")  

# preliminars
installations <- function(paquete) {
  if (!require(paquete,character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)) {
    install.packages(as.character(paquete), dependecies = TRUE, repos = "http://cran.us.r-project.org")
    library(paquete, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
  }
}

packages <- c('corrr', 'data.table', 'ddpcr', 'devtools', 'dplyr',  'plotly', 
             'foreign', 'GISTools', 'ggraph', 'glmnet', 'haven', 'htmlwidgets', 
             'igraph', 'linkcomm', 'maptools', 'mapview', 'network', 'RANN', 
             'raster', 'readr', 'RColorBrewer', 'rgdal', 'rgeos', 'rlist',
             'scales', 'sjlabelled', 'sf', 'sp', 'tidyverse', 'tidygraph')

lapply(packages, installations)

## utils for geospatial analysis
calc_buffers <- function(df, radious){
  "Calculates buffers for schools"
  db_sp <- db # create a df to be converted to a spatial object
  ## projections
  unproj <- CRS("+proj=longlat +datum=WGS84")
  proj <- CRS("+init=epsg:6370")  
  coordinates(db_sp) <- c(x="longitud", y="latitud") 
  proj4string(db_sp) <- unproj  
  db_sp <- spTransform(db_sp, proj) 
  ## calculate buffers
  buffers <- gBuffer(db_sp, width=1000) 
  db$buffer <- as.character(over(db_sp, disaggregate(buffers)))
  db <- db %>% arrange(as.numeric(buffer))
  # for graphing buffers
  buffers_proj <- spTransform(buffers, '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs')
  # return both df and spdf for graph maps
  buffers_outs <- list("df_buffers"=db, "buffers_proj"=buffers_proj)
  
  return(buffers_outs)
}


map_buffers <- function(proj_buffers, df_buffers){
  m <-leaflet() %>%
    addProviderTiles(providers$Stamen.TonerLite,
                     options = providerTileOptions(noWrap = TRUE)) %>%
    addPolygons(data=proj_buffers, weight = 3, fillColor = "yellow") %>% 
    addCircleMarkers(data=df_buffers, ~longitud, ~latitud, color = "red", 
                    stroke = FALSE, fillOpacity = 0.5,radius=2,
                     label = ~htmlEscape(paste("CCT:", cct, "Buffer", buffer)))
  # adds menu for visualization
  m %>% setView(0,0,3)
  esri <- grep("^Esri", providers, value = TRUE)
  for (provider in esri) {
    m <- m %>% addProviderTiles(provider, group = provider)
  }
  m %>%
    addLayersControl(baseGroups = names(esri),
                     options = layersControlOptions(collapsed = FALSE)) %>%
    addMiniMap(tiles = esri[[1]], toggleDisplay = TRUE,
               position = "bottomleft") %>%
    htmlwidgets::onRender("
    function(el, x) {
      var myMap = this;
      myMap.on('baselayerchange',
        function (e) {
          myMap.minimap.changeLayer(L.tileLayer.provider(e.name));
        })
    }") %>% 
  clearBounds()
  
}


map_buffers_v2 <- function(proj_buffers, df_buffers){
  leaflet() %>%
    addTiles() %>% 
    addMarkers(data=df_buffers, ~longitud, ~latitud,
               clusterOptions = markerClusterOptions(),
               label = ~htmlEscape(paste("CCT:", cct, "Buffer", buffer)),
               icon = list(
                 iconUrl = './data/school_icon.png',
                 iconSize = c(75, 75))
    ) %>% 
    clearBounds()
}

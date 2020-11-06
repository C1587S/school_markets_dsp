library(shiny)
library(leaflet)

setwd("~/Desktop/mercados_educativos/code/visualize_db")



r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()

ui <- fluidPage(
  leafletOutput("mymap"),
  p()#,
  # actionButton("recalc", "New points")
)

server <- function(input, output, session) {
  # read data
  db <- readRDS("../data/master_db_filtered.rds")
  db <- sample_n(db, 4000)
  db$latitud <- as.numeric(db$latitud)
  db$longitud <- as.numeric(db$longitud)
  
  output$mymap <- renderLeaflet({
    # create base map
    m <- leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addTiles() %>% 
      addCircles(data = db, lng=~longitud, lat=~latitud, radius=3, weight = 1, color="#E74B52") 
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
    }")
  })
}


shinyApp(ui, server)
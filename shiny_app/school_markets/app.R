## app.R ##
library(shinydashboard)
library(leaflet)
source('utils_v2.R')
df_buffers <- readRDS("../../data/df_buffers_cdmx.rds")
proj_buffers <- readRDS("../../data/proj_buffers_cdmx.rds")
# calculate convex hulls
ch_df <- calc_convexhulls(df_buffers)
# unions
unions <- st_union(ch_df$ch_polygons) %>%  st_sf()
# communities
path <- "../../data/agregados/"
df <- readRDS(paste0(path, "agregado_dist_sec_v2.rds"))
nodos <- df_buffers %>% rename(grupo=buffer, name=cct, lat=latitud, lon=longitud)

selected_list <- comp_communities(buffer=1, nodos, df) 
select_nodos <- selected_list$selected_nodos
select_relations <- selected_list$select_relations
# elements for spatial network
sp_network <- compute_spatial_network(select_nodos, select_relations)
network <- sp_network$network
vert <- sp_network$verts
edges <- sp_network$edges


ui <- dashboardPage(
  dashboardHeader(title = "School Markets"),
  
  
  ## Sidebar content
  dashboardSidebar(
    sidebarMenu(
      menuItem("Information", tabName = "tab_info", icon = icon("info-circle")),
      menuItem("Map explorer", tabName = "tab_map", icon = icon("globe-americas")),
      menuItem("Markets analysis", tabName = "tab_tbl", icon = icon("table"))
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "tab_info",
              fluidPage(
                leafletOutput("mymap"),
                p(),
                actionButton("recalc", "New points")
              )
      ),
      
      # Second tab content (map explorer)
      tabItem(tabName = "tab_map",
              h2("Map explorer for School Markets in Mexico")
      ),
      # Third tab content (data analysis)
      tabItem(tabName = "tab_tbl",
              h2("Data Analysis for School Markets")
      )
    )
  )
  
  
  
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$mymap <- renderLeaflet({
    map_layers(vert, df_buffers, ch_df, unions, proj_buffers, edges, edges_type="flujo")
  })
}

shinyApp(ui, server)
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

# number of available buffers
buffers_list <- df_buffers$buffer %>% unique() %>% as.vector()
# map layers
map_layers <- c(
  "Mapa",
  "Escuelas",
  "Buffers",
  "Envolventes convexas",
  "Mercados educativos"
)
# algorithms for computing communities
comm_algorithms <-c("Fast greedy", "Walktrap", "Leading Eigen", "Label Prop")
# fg, wt, le, lp

### App

ui <- dashboardPage(
  skin = "green",
  dashboardHeader(
    title = "Mercados Educativos en México",
    titleWidth = 450),
  
  
  ## Sidebar content
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      menuItem("Información", tabName = "tab_info", icon = icon("info-circle")),
      menuItem("Mapa de exploración", tabName = "tab_map", icon = icon("map-marked-alt")),
      menuItem("Estadísticas de comunidades", tabName = "tab_tbl", icon = icon("table"))
    )
  ),
  
  
  ## Body content
  dashboardBody(
    tags$head(tags$style(HTML('
      .main-header .logo {
        font-family: "Georgia", Times, "Verdana", serif;
        font-weight: bold;
        font-size: 24px;
      }
    '))), 
    tabItems(
      ##### First tab content
      tabItem(tabName = "tab_info",
              h2("Visualización y Análisis de Mercados Educativos en México")
      ),
      
      ##### Second tab content (map explorer)
      tabItem(tabName = "tab_map",
              h2("Mapa de Mercados Educativos en México"),
              fluidRow(
                column(width = 9,
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("map", height = 500)
                           ),
                       ),
                column(width = 3,
                       # Buffer size
                       box(width = NULL, status = "danger",
                           selectInput("buff_size", "Radio de los buffers", 
                                       c("5 kms", "10 kms", "15 kms")),
                           # Select commuting zone
                           selectInput("cz_id", "Zona de desplazamiento", buffers_list),
                           # Select algorithm for create communities
                           selectInput("net_alg", "Algoritmo de redes",
                                       choices = c("Fast greedy" = "fg",
                                                    "Walktrap" = "wt", 
                                                    "Leading Eigen" = "le", 
                                                    "Label Prop" = "lp"))
                                       ),
                       # Menu with map layers
                       box(width = NULL, status = "danger",
                           checkboxGroupInput("map_elements", "Elementos en el mapa",
                                              choices = map_layers,
                                              selected = map_layers)
                           )
                      )
                      )
      ),
      ##### Third tab content (data analysis)
      tabItem(tabName = "tab_tbl",
              h2("Data Analysis for School Markets")
      )
    )
  )
  
  
  
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$map <- renderLeaflet({
    # commuting zone number
    cz_id <- input$cz_id
    # compute graph network
    algo <- input$net_alg # selected algorithm
    ## compute communities
    selected_list <- comp_communities(buffer=cz_id, nodos, df, algorithm=algo) 
    select_nodos <- selected_list$selected_nodos
    select_relations <- selected_list$select_relations
    
    
    # elements for spatial network
    sp_network <- compute_spatial_network(select_nodos, select_relations)
    network <- sp_network$network
    vert <- sp_network$verts
    edges <- sp_network$edges
    
    map_layers(vert, df_buffers, ch_df, unions, proj_buffers, edges, edges_type="flujo")
  })
}

shinyApp(ui, server)
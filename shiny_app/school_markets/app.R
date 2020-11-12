## app.R ##
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
      menuItem("Estadísticas", tabName = "tab_tbl", icon = icon("table"))
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
      ##### First tab content (map explorer)
      tabItem(tabName = "tab_map",
              # h3("Mapa de Mercados Educativos en México"),
              fluidRow(
                column(width = 9,
                       # Map
                       box(width = NULL, solidHeader = TRUE,
                           leafletOutput("map", height = 400)
                           ),
                       # Box with algo stats
                       box(width = NULL, status = "success",
                           solidHeader=T, collapsible=T, collapsed = T,
                           title="Métricas del algoritmo",
                           tableOutput("algo_stats")
                       ),
                       # Box with group stats
                       box(width = NULL, 
                           solidHeader=T,collapsible=T, collapsed = T,
                           title="Estadísticas de mercados",
                           tableOutput("community_stats")
                       ),
                       # Box with centraility stats of the members
                       box(width = NULL, status = "danger",
                           solidHeader=T,collapsible=T, collapsed = T,
                           title="Estadísticas de centralidad de los miembros",
                           tableOutput("centrality_stats")
                       )
                    
                       ),
                column(width = 3,
                       # Buffer size
                       box(width = NULL,collapsible=T, collapsed=T, status = "info",solidHeader=T,
                           title="Opciones",
                           selectInput("buff_size", "Radio de los buffers", 
                                       c("5 kms", "10 kms", "15 kms")),
                           # Select commuting zone
                           selectInput("cz_id", "Zona de desplazamiento", buffers_list),
                           # Select algorithm for create communities
                           selectInput("net_alg", "Algoritmo de redes",
                                       choices = c("Fast greedy" = "fg",
                                                    "Walktrap" = "wt", 
                                                    "Leading Eigen" = "le", 
                                                    "Label Prop" = "lp",
                                                    "Multi-level" = 'cl')),
                            # Select edges type
                            selectInput("edges_type", "Tipo de aristas",
                                        choices = c("Flujo" = "flujo",
                                                     "Total" = "total", 
                                                     "Peso" = "peso")),
                           # Select color for edges
                           selectInput("edges_color", "Color de las aristas",
                                       choices = c("Gris" = "grey",
                                                   "Negro" = "black", 
                                                   "Rojo" = "red"))
                                       ),
                           # Box with group members
                          box(width = NULL, status = "danger",
                              solidHeader=T,collapsible=T,
                              title="Miembros de los mercados",
                              tableOutput("mkt_members_tbl")
                           )
                          
                      )
                      )
      ),
      ##### Second tab content (data analysis)
      tabItem(tabName = "tab_tbl",
              h2("")
      ),
      # fluidRow()
      tabItems(
      ##### First tab content
            tabItem(tabName = "tab_info",
            h2("")
                        
            )
      )
  )
  
  
  
)

server <- function(input, output) {
  set.seed(202011)
  # Reactive expressions
  values <- reactiveValues()
  observe({
    values$cz_id <- input$cz_id
    values$algo  <-  input$net_alg
    values$selected_list <- comp_communities(buffer=input$cz_id, nodos, df, algorithm=input$net_alg)
    values$sp_network <- compute_spatial_network(values$selected_list$selected_nodos,
                                          values$selected_list$select_relations)
    
    values$network <- values$sp_network$network
    values$vert <- values$sp_network$verts
    values$edges <- values$sp_network$edges
    values$edges_plot <- input$edges_type
    values$edges_color <- input$edges_color
  })
  #### Map
  output$map <- renderLeaflet({
    # elements for spatial network
    map_layers(values$vert, df_buffers, ch_df, unions, proj_buffers, values$edges,
               edges_type=values$edges_plot, edges_color=values$edges_color)
  })
  
  #### Markets schools
  output$mkt_members_tbl <-  function() {
    values$selected_list$mem_list %>%  
      kbl() %>%
      kable_minimal(full_width = F, position = "left") %>% 
      kable_styling(font_size = 8) %>% 
      scroll_box(width = "230px", height = "520px")
  }
  #### Community Stats
  output$community_stats <- function(){
    legends <- c("N: Nro. de escuelas;",
                 "DistMed: Distancia media (kms);", 
                 "MaxDist: Máxima distancia (kms);",
                 "MinDist: Mínima distancia (kms);",
                 "MedDist: Distancia media (kms);",
                 "Area: Area de Envolvente convexa (kms2);",
                 "Privs: Porcentaje de escuelas privadas (%).")
    
    values$selected_list$com_stats %>% 
      mutate(N = color_bar("lightgreen")(N)) %>% 
      kable("html", escape = F) %>%
      kable_styling("hover", full_width = F) %>%
      column_spec(1, width = "3cm") %>%
      footnote(number = c(legends))
  }
  #### Algoritmh Stats
  output$algo_stats <- function(){
    values$selected_list$algo_stats %>% 
      kable("html", escape = F) %>% 
      kable_styling(bootstrap_options = "striped", full_width = F, position = "left")%>% 
      column_spec(1, width = "4cm") %>% 
      kable_styling("hover", full_width = F, font_size = 10)
  }
  #### Centrality Stats
  output$centrality_stats <- function(){
    values$selected_list$central_stats %>% format_ctl()
  }  
}

shinyApp(ui, server)
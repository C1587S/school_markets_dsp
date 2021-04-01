## app.R ##
options(warn=-1)
source('utils.R')
library(shiny)
options(shiny.maxRequestSize = 100*1024^2)


# init buffer in list
init_buff <- 0

### Apphttp://127.0.0.1:7908/#shiny-tab-tab_map
ui <- dashboardPage(
  skin = "red",
  dashboardHeader(
    title = "Mercados Educativos en México",
    titleWidth = 850),
  
  
  ## Sidebar content
  dashboardSidebar(
    width = 180,
    sidebarMenu(
      # menuItem("Información", tabName = "tab_info", icon = icon("info-circle")),
      menuItem("Mapa de exploración", tabName = "tab_map", icon = icon("globe", lib = "glyphicon"))
      # menuItem("Redes", tabName = "tab_networks", icon = icon("option-vertical", lib = "glyphicon"))
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
    tabItems(
      tabItem(tabName = "tab_map",
            # h3("Mapa de Mercados Educativos en México"),
            fluidRow(
              column(width = 9,
                     # Map
                     box(width = NULL, solidHeader = TRUE,
                         leafletOutput("map", height = 700)
                     ),
                     # Box with algo stats
                     box(width = NULL, status = "success",
                         solidHeader=T, collapsible=T, collapsed = T,
                         title="Métricas del algoritmo",
                         tableOutput("algo_stats")
                     ),
                     # Box with group stats
                     box(width = NULL, status = "success",
                         solidHeader=T, collapsible=T, collapsed = T,
                         title="Estadísticas de mercados",
                         tableOutput("community_stats")
                     ),
                     # Box with centraility stats of the members
                     box(width = NULL, status = "success",
                         solidHeader=T,collapsible=T, collapsed = T,
                         title="Estadísticas de centralidad de los miembros",
                         tableOutput("centrality_stats")
                     )
                     
              ),
              column(width = 3,
                     # # Education level
                     # checkboxInput("educ_level", "Some value", FALSE),
                     # Buffer size
                     box(width = NULL,collapsible=T, collapsed=F, status = "info",solidHeader=T,
                         title="Opciones",
                         # Select algorithm for create communities
                         selectInput("educ_level", "Nivel educativo",
                                     choices = c("Secundaria" = "secundaria", 
                                                 "Primaria" = "primaria")),
                         # buffer size
                         selectInput("buff_size", "Radio de los buffers", 
                                     choices=(c("5 kms"="5k", "10 kms"="10k"))),
                         # Select commuting zone
                         numericInput("cz_id", "Zona de desplazamiento", value = 30),
                         # uiOutput("cz_id"),
                         # Select algorithm for create communities
                         selectInput("net_alg", "Algoritmo de redes",
                                     choices = c("Multi-level" = 'cl', 
                                                 "Fast greedy" = "fg",
                                                 "Walktrap" = "wt", 
                                                 # "Leading Eigen" = "le", 
                                                 "Label Prop" = "lp"
                                                 )),
                         # Select edges type
                         selectInput("edges_type", "Tipo de aristas",
                                     choices = c("Flujo" = "flujo",
                                                 "Total" = "total", 
                                                 "Peso" = "peso")),
                         # Select color for edges
                         selectInput("edges_color", "Color de las aristas",
                                     choices = c("Gris" = "grey",
                                                 "Negro" = "black", 
                                                 "Rojo" = "red")),
                         # Save results
                         checkboxInput("save", "Guardar tablas de resultados", F),
                         footer = "Se creará la carpeta \"results\" con todas las tablas generadas."
                     ),
                     # Box with group members
                     box(width = NULL, status = "info",
                         solidHeader=T, collapsible=T, collapsed=T,
                         title="Composición",
                         tableOutput("res_buffer")
                     ),
                     # Box with group members
                     box(width = NULL, status = "info",
                         solidHeader=T,collapsible=T, collapsed=T,
                         title="Miembros de los mercados",
                         tableOutput("mkt_members_tbl")
                     )
                     
              )
            )
    )#,
    # ##### Second tab content (Communities visualization)
    # tabItem(tabName = "tab_tbl",
    #         h2("Se adicionará en esta pestaña:")
    # ),
    # fluidRow()
    ##### First tab content
    #   tabItem(tabName = "tab_networks",
    #           p("En esta pestaña se adicionará visualización par análisis detallado de los mercados generados con:"),
    #           p(a("sigmajs", href="https://sigmajs.john-coene.com/"), "y/o", a("igraph", href="https://igraph.org/r/doc/"))
    # )
)
))

server <- function(input, output) {
  set.seed(202011)
  # Reactive expressions
  init_buff <- reactiveValues()
  values <- reactiveValues()
  df_reactive <- reactiveValues()
  buffers_reactive <- reactiveValues()
  
  observe({
    # saving options
    values$save <- input$save
    # select education level
    if(input$educ_level=="primaria"){
      df_reactive$df <- readRDS("../../data/agregados/agregado_dist_prim_v2.rds")
    } else if (input$educ_level=="secundaria"){
      df_reactive$df <- readRDS("../../data/agregados/agregado_dist_sec_v2.rds")
    }
    # selecting buffer size
      if(input$buff_size=="5k"){kms <- 5} else{kms <- 10}
      path_buff_df <-  str_c("../../data/buffers/buffers_organizados/", input$educ_level, "_df_buffers_", kms,"kms.rds")
      path_buff_proj <-  str_c("../../data/buffers/buffers_organizados/", input$educ_level, "_projections_buffers_", kms,"kms.rds")

      # Read buffers and projections      
      df_reactive$dfbuffers <- readRDS(path_buff_df)$df_buffers
      df_reactive$df_schools <- df_buffers_join(df_reactive$df, df_reactive$dfbuffers)
      df_reactive$projbuffers <- readRDS(path_buff_proj)
  
      # buffer list
      df_reactive$buffers_list <- df_reactive$dfbuffers %>% unique() %>% as.vector()
      # calculate convex hulls
      df_reactive$ch_df <- calc_convexhulls(df_reactive$dfbuffers)
      # calculate nodos
      df_reactive$nodos <- df_reactive$dfbuffers %>% rename(grupo=buffer, name=cct, lat=latitud, lon=longitud)
      # calculate unions
      df_reactive$unions <- st_union(df_reactive$ch_df$ch_polygons) %>%  st_sf()
      
      ## selecting commuting zone (buffer)
      values$cz_id <- input$cz_id
      values$algo  <-  input$net_alg
    
    ## selected list
    
    algo <- map_algo(values$algo)
    selected_list_path <- str_c('./results/full_results/', input$educ_level, '/', kms, 'kms/buffer_', values$cz_id, '/', algo, '/selected_list.rds')
    
    validate(need(try(values$selected_list <- readRDS(selected_list_path)), 'help'))
    # need(try(values$selected_list <- comp_communities(buffer=values$cz_id, df_reactive$nodos, df_reactive$df, 
    #                                          algorithm=values$algo, values$save)), "help")
    
    network_sp_path <- str_c('./results/full_results/', input$educ_level, '/', kms, 'kms/buffer_',  values$cz_id, '/', algo, '/sp_network.rds')
    print(network_sp_path)
    validate(need(try(network <- readRDS(network_sp_path)), 'help'))
    print(network_sp_path)
    # need(try(values$sp_network <- compute_spatial_network(values$selected_list$selected_nodos,
    #                                              values$selected_list$select_relations)), "help")
    
    values$network <- network$network
    values$vert <- network$verts
    values$edges <- network$edges
    values$edges_plot <- input$edges_type
    values$edges_color <- input$edges_color

  })
  #### Map
  output$map <- renderLeaflet({
    # elements for spatial network
    map_layers_v2(vert=values$vert,
                  df_buffers=df_reactive$df_schools, 
                  proj_buffers=df_reactive$projbuffers, 
                  ch_df=df_reactive$ch_df, 
                  unions=df_reactive$unions, 
                  edges=values$edges,
                  edges_type=values$edges_plot, 
                  edges_color=values$edges_color)
  })
  #### Markets schools
  output$mkt_members_tbl <-  function() {
    values$selected_list$mem_list %>%  format_mbrs()
  }
  #### Counts for buffers
  output$res_buffer <-  function() {
    df_reactive$df_schools %>% na.omit %>% format_buff_cts()
  } 
  #### Community Stats
  output$community_stats <- function(){
    values$selected_list$com_stats %>% format_mrcds()
  }
  #### Algoritmh Stats
  output$algo_stats <- function(){
    values$selected_list$algo_stats %>% format_algo()
  }
  #### Centrality Stats
  output$centrality_stats <- function(){
    values$selected_list$central_stats %>% format_ctl() %>%  kable_material(c("striped", "hover"))
  }  
  # ### Selected relations
  # output$cz_id < renderUI({
  #   availablenets <- df_reactive$df_schools %>% na.omit %>% pull(buffer)
  #   selectInput("cz_id","Zonas de desplazamiento", choices = c(availablenets), selected = 555)
  #   selectInput()
  # })
}

shinyApp(ui, server)
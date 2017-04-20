synapsesSA_sidebar <- function(){
  tagList( sidebar_menu(),
           br(),
           hr(),
           sidebar_controls()
           #,
           #br(),
           #hr()
           #,somaMS_build_sidebar_footer() 
  )
}

sidebar_menu <- function(){
  ##
  #  Sidebar Menu
  ##
  sidebarMenu(
    
    #Dashboard
    menuItem( "3D Visualization", icon = icon("eye"), tabName = "dashboard_view"),
    
    # Data loading
    customMenuItem( "Data loading", 
                    # Load files
                    menuSubItem("Load files", icon = icon("file-archive-o"), tabName = "dataloading_files" ),
                    # View data tables
                    menuSubItem("View data table", icon = icon("table"), tabName = "dataloading_view" ),
                    # Data loading tab config
                    icon = icon("database"), tabName = "dataloading_dash", hasTab = T),
    
    # Modelling
    customMenuItem( "Modelling",
                    # Create new model
                    menuSubItem("New model", icon = icon("bolt"), tabName = "modelling_new" ),
                    # Model viewer
                    menuSubItem("View model", icon = icon("eye"), tabName = "modelling_view" ),
                    # Sidebar config
                    icon = icon("stats", lib = "glyphicon"), tabName = "modelling_dash", hasTab = T),
    
    # Simulation
    customMenuItem( "Simulation",
                    # New simulation
                    menuSubItem("New simulation", icon = icon("play"), tabName = "simulation_new" ),
                    # Sidebar config
                    icon = icon("cogs"), tabName = "simulation_dash", hasTab = T),
    
    # Comparison
    customMenuItem( "Layer comparison", 
                    # Syn density
                    menuSubItem("Synaptic density", icon = icon("bullseye"), tabName = "lcomp_dens" ),
                    # Nearest synapse
                    menuSubItem("Nearest synapse distance", icon = icon("spinner"), tabName = "lcomp_dist" ),
                    # Sidebar config
                    icon = icon("list-ol"), hasTab = F ),

    br(),
    column(4, menuItem("",icon = icon("info-circle", class = "big-icon") , tabName = "info_tab", selected = T), offset = 4),
    br(),
    br(),
    # Menu id
    id  = "sb_menu"
  )
}

sidebar_controls <- function(){
  sidebarMenu(
    uiOutput("sb_controls")
  )
}

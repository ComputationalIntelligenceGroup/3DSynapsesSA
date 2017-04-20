#' 3D preview tab
#' 
#' 
synapsesSA_dash_view_tab <- function(){
  tabItem( tabName = "dashboard_view",
           fluidPage(
             
             # HEADER
             h1( "3D sample viewer"),
             p("The 3D visualization in this tab is implemented with ThreeJS, a javascript library that uses webGL. If you experience any problem during the visualization update your browser and/or check that your browser supports webGL."),
             p("To visualize a synapsis sample, select it using the selector on the left side. If no data file or simulation has been created yet, please proceed to data load or simulation tab."),
             p("For each synapsis type you can set its color. After all parameters have been set you can click on the render button."),
             
             hr(class = "hr-bluecolor"),
             # WORKSPACE (2 cols)
             bsAlert("dl_view_alert"),
             
             # FIRST COLUMN: Selector
             column(4,
                    # File selector
                    box( title = "Sample selector", status = "primary", solidHeader = T, width = NULL, collapsible = T,
                         hCBoxInput("dash_view_selector", NULL, animation = 0, expand.selected = F, multiple = F),
                         bsTooltip("dash_view_selector","Select one samples to display")
                    ),
                    # Plot options
                    box( title = "Plot options", status = "primary", solidHeader = T, width = NULL, collapsible = T,
                         # Sample name
                         checkboxInput("dl_dash_symcheck", h5("Draw symmetric synapses"), value = T),
                         conditionalPanel("input.dl_dash_symcheck",
                             colourInput("dl_dash_symcolor", h3("Symmetric synapses color"), value = "#AA3333"),
                             bsTooltip("dl_dash_symcolor","Pick a color")
                          ),
                          hr(class = "hr-bluecolor"),
                         checkboxInput("dl_dash_asymcheck", h5("Draw asymmetric synapses"), value = T),
                         conditionalPanel("input.dl_dash_asymcheck",
                                          colourInput("dl_dash_asymcolor", h3("Asymmetric synapses color"), value = "#33AA33"),
                                          bsTooltip("dl_dash_asymcolor","Pick a color")
                         )
                    ),
                    # Load button
                    column(10, 
                           bsButton("dash_view_draw", "Draw synapses", style = "primary", disabled = T, block = T),
                           offset = 1)
             ),
             # Right side: Draw
             column(8,
                    sceneOutput("dash_view_scene", width = "100%", height = "600px")
             )
           )
  )
}
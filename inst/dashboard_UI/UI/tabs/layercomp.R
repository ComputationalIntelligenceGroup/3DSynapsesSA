#' Synaptic density comparison
#' 
#' 
synapsesSA_lcomp_dens_tab <- function(){
  tabItem( tabName = "lcomp_dens",
           fluidPage(
             
             # HEADER
             h1( "Synaptic density comparison"),
             p( "This analysis compares, from a statistical point of view, the synaptic density between samples from two or more layers. The results are summarized in a stacked bar plot."),
             p( "To perform the test, at least 3 samples per layer are needed. As the number of samples increase, the accuracy of the statistical results increases."),
             
             hr(class = "hr-bluecolor"),
             # WORKSPACE (2 cols)
             bsAlert("lcomp_dens_alert"),
             
             # FIRST COLUMN: Selector
             column(4,
                    # File selector
                    box( title = "Layer selector", status = "primary", solidHeader = T, width = NULL, collapsible = F,
                         hCBoxInput("lcomp_dens_selector", NULL, animation = 0, expand.selected = F),
                         bsTooltip("lcomp_dens_selector","Select samples from several layers to perform the test")
                    ),
                    # Load button
                    column(8, 
                           bsButton("lcomp_dens_run", "Analyze", style = "primary", disabled = T, block = T),
                    offset = 2)
             ),
             # Right side: Log + plot
             column(8,
                    box( title = "Comparison output", status = "info", solidHeader = T, width = NULL, collapsible = F,
                         uiOutput("lcomp_dens_log", style = "height:200px"),
                         column(8, plotOutput("lcomp_dens_plot", height = "400px"), offset = 2)
                    )
                    
             )
           )
  )
}

synapsesSA_lcomp_dist_tab <- function(){
  tabItem( tabName = "lcomp_dist",
           fluidPage(
             
             # HEADER
             h1( "Nearest synapses distance"),
             p( "This analysis compares the distance to the nearest synapse distribution within samples of two or more layers. A statistical
                test is used to perform a pairwise comparison to determine whether the distances have the same distribution for each pair of layers
                or not."),
             
             hr(class = "hr-bluecolor"),
             # WORKSPACE (2 cols)
             bsAlert("lcomp_dist_alert"),
             
             # FIRST COLUMN: Selector
             column(4,
                    # File selector
                    box( title = "Layer selector", status = "primary", solidHeader = T, width = NULL, collapsible = F,
                         hCBoxInput("lcomp_dist_selector", NULL, animation = 0, expand.selected = F),
                         bsTooltip("lcomp_dist_selector","Select samples from several layers to perform the test")
                    ),
                    # Load button
                    column(8, 
                           bsButton("lcomp_dist_run", "Analyze", style = "primary", disabled = T, block = T),
                           offset = 2)
             ),
             # Right side: Log + plot
             column(8,
                    box( title = "Comparison output", status = "info", solidHeader = T, width = NULL, collapsible = F,
                         uiOutput("lcomp_dist_log", style = "height:200px"),
                         column(8, plotOutput("lcomp_dist_plot", height = "400px"), offset = 2)
                    )
                   
             )
           )
  )
}
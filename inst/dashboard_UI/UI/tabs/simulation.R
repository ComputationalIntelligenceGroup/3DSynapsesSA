synapsesSA_sim_new_tab <- function(){
  tabItem( tabName = "simulation_new",
           fluidPage(
             
             # HEADER
             h1( "Simulate synapses sample"),
             p( "In this tab, you can create RSA based samples based on existing models or by manually setting the simulation parameters." ),
             hr(class = "hr-bluecolor"),
             # WORKSPACE (2 cols)
             bsAlert("sim_new_alert"),
             
             
             # FIRST COLUMN: Controls
             column(4,
                    
                    # File selector
                    box( title = "Simulation options", status = "primary", solidHeader = T, width = NULL, collapsible = T,
                         numericInput("sim_new_nsims", label = "Number of samples", value = 10, min = 1, step = 1),
                         bsTooltip("sim_new_nsims","Number of samples to simulate"),
                         numericInput("sim_new_volume", label = "Sample volume (μm3)", value = 150.0, min = 1, max = 1E9 ),
                         bsTooltip("sim_new_volume","Set the volume of each simulation. Simulation space will be a cube"),
                         radioButtons("sim_new_manual", "Simulation parameters", c("Manual", "From model"),),
                         bsTooltip("sim_new_manual","Select whether input the parameters manually or obtain them from an existing model"),
                         conditionalPanel("input.sim_new_manual=='Manual'",
                                          tags$h4("Parameters"),
                                          numericInput("sim_new_lambda", label = "Intensity", value = 1, min = 0, step = 0.01),
                                          bsTooltip("sim_new_lambda","Number of synapses per volume unit"),
                                          numericInput("sim_new_ml", label = "Feret diameter log mean", value = 5.0, min = 0.1, max = 1E9, step = 0.01 ),
                                          bsTooltip("sim_new_ml","Feret's diameter log mean value"),
                                          numericInput("sim_new_sdl", label = "Feret diameter log standard deviation", value = 0.5, min = 0, max = 1E9, step = 0.01 ),
                                          bsTooltip("sim_new_sdl"," Feret's diameter log standard deviation value")
                         ),
                         conditionalPanel("input.sim_new_manual!='Manual'",
                                          selectInput("sim_new_model", "Model", NULL),
                                          bsTooltip("sim_new_model"," Get parameters from selected model")
                         )
                    ),
                    # Load button
                    column(8, 
                           bsButton("sim_new_run", "Run simulation", style = "primary", disabled = F, block = T),
                           offset = 2)
             ),
             # Right side: Log
             column(8,
                    box( title = "Process report", status = "info", solidHeader = T, width = NULL, collapsible = T, collapsed = T,
                         uiOutput("sim_new_log")
                    )
             )
           )
           )
}
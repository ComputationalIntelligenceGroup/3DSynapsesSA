#'
#' New model tab
#'
synapsesSA_mod_new_tab <- function(){
  tabItem( tabName = "modelling_new",
           fluidPage(
             bsAlert("mod_new_alert"),
             multiStepPage("mod_new_msp",
                           # Selector step
                           stepPage("Select model data",
                                    h1( "Sample selection"),
                                    p("Samples that will be used for building the model are selected using a hierarchical
                                   checkbox selector. You can select all samples that belong to one layer, or choose them individually"),
                                    hr(class = "hr-bluecolor"),
                                    # File selector
                                    box( title = "Sample selector", status = "primary", solidHeader = T, width = 10, collapsible = T,
                                         hCBoxInput("mod_new_selector", NULL, animation = 0, expand.selected = F),
                                         bsTooltip("mod_new_selector","Select one or more samples to create the model")
                                    )
                           ),
                           # Feret's diameter
                           stepPage("Feret's diameter",
                                    h1( "Fit Feret's diameter distribution"),
                                    p("Assuming that the Feret's diameter follows a log-normal distribution,
                                   the distribution parameters are computed based on the diameters in the selected samples"),
                                    hr(class = "hr-bluecolor"),
                                    fluidRow(
                                      column(4,
                                             box( title = "Controls", status = "primary", solidHeader = T, width = NULL, collapsible = F,
                                                  numericInput("mod_new_alpha", label = h4("Significance threshold (GOF)"),
                                                               value = 0.05, min = 0.01, max = 0.99, step = 0.01),
                                                  bsButton("mod_new_feret", label = "Fit distribution", disabled = F, block = T, style = "primary")
                                             )
                                      ),
                                      column(7,
                                             box( title = "Fit output", status = "info", solidHeader = T, width = NULL, collapsible = F,
                                                  uiOutput("mod_new_feretlog", style = "height:100px")
                                             ),
                                             plotOutput("mod_new_feretplot", height = "400px")
                                      )
                                    )
                           ),
                           # Spatial distribution
                           stepPage("Spatial distribution",
                                    h1( "Analize synapses spatial distribution"),
                                    p("Once the synapses diameters distribution has been modeled, this step
                                   tests if the spatial distribution of the selected samples follow a Random sequential adsorption process (RSA)
                                   where synapses are distributed in space almost randomly, with the only constraint that they cannot overlap, or if they are
                                   distributed completely at random (CSR)"),
                                    hr(class = "hr-bluecolor"),
                                    fluidRow(
                                      column(4,
                                             box( title = "RSA Advanced options", status = "primary", solidHeader = T, width = NULL, collapsible = F,
                                                  numericInput("mod_new_envsims", label = h4("Number of simulations to compute RSA envelope"),
                                                               value = 99, min = 10, max = 200, step = 1),
                                                  numericInput("mod_new_avgsims", label = h4("Number of simulations to compute RSA average"),
                                                               value = 99, min = 10, max = 200, step = 1),
                                                  uiOutput("mod_new_crossval_ui"),
                                                  bsButton("mod_new_rsa", label = "Compute RSA envelope", disabled = F, block = T, style = "primary")
                                             )
                                      ),
                                      column(7,
                                             box( title = "RSA envelope output", status = "info", solidHeader = T, width = NULL, collapsible = F,
                                                  uiOutput("mod_new_rsalog", style = "height:100px")
                                             ),
                                             plotOutput("mod_new_rsaplot", height = "400px")
                                      )
                                    )
                           ),
                           # Summary funcs (FGKL)
                           stepPage("Summary functions",
                                    h1( "Compute FGKL summary functions"),
                                    p("Finally, a set of well-known summary functions in spatital processes are computed for
                                   the model and the selected samples. There are four functions"),
                                    hr(class = "hr-bluecolor"),
                                    fluidRow(
                                      column(3,
                                        bsButton("mod_new_fgkl", label = "Compute FGKL functions", disabled = F, block = T, style = "primary")
                                      )
                                    ),
                                    fluidRow(
                                         plotOutput("mod_new_fgklplot", height = "600px")
                                    )
                           ),
                           # Intro
                           prePage = stepPage(
                             h1( "Create new model"),
                             hr(class = "hr-bluecolor"),
                             tags$p("The creation of a model for the spatial distribution of the synapses is a three-step process, 
                               with an additional step for computing a set of functions that describe that distribution. The four steps are:"),
                             tags$ol(
                               tags$li( tags$b("Sample selection"), ": Samples that will be used for building the model are selected using a hierarchical
                                   checkbox selector. You can select all samples that belong to one layer, or choose them individually"),
                               tags$li( tags$b("Feret's diameter distribution"), ": Assuming that the Feret's diameter follows a log-normal distribution,
                                   the distribution parameters are computed based on the diameters in the selected samples"),
                               tags$li( tags$b("Spatial distribution"), ": Once the synapses diameters distribution has been modeled, this step
                                   tests if the spatial distribution of the selected samples follow a Random sequential adsorption process (RSA)
                                   where synapses are distributed in space almost randomly, with the only constraint that they cannot overlap, or if they are
                                   distributed completely at random (CSR)"),
                               tags$li( tags$b("Summary functions"), ": Finally, a set of well-known summary functions in spatital processes are computed for
                                   the model and the selected samples. There are four functions",
                                   tags$ul( tags$li( tags$i("F Function"),": The empty space function"),
                                            tags$li( tags$i("G Function"),": The nearest-neighbor distance cumulative distribution function"),
                                            tags$li( tags$i("K Function"),": The Ripley's function, the expected number of points within a distance of a typical point of the process"),
                                            tags$li( tags$i("L Function"),": A common transformation of the K function that makes the plots much easier to assess visually")
                                   )
                               )
                             ),
                             tags$p( "To begin the process, click on the ",tags$b("Start process"), "button on the right.")
                           ),
                           # Save and exit
                           postPage = stepPage(
                             h1( "Process completed"),
                             hr(class = "hr-bluecolor"),
                             tags$h5("To save the model, give it an identificative name and click on Save model."),
                             tags$p("Saved models can be reviewed and downloaded in the view model page, accesible on the sidebar under Modelling."),
                             # Todo completado -> guardar y salir
                             fluidRow(
                               column(3,
                                      textInput("mod_new_name", label = tags$h4("Model name"),value = "")
                               )
                             )
                           ),
                           title = "Model building process",
                           bottomButtons = F,
                           topButtons = F
             ), # CLOSE MULTISTEP PAGE
             # Bottom right side: advance button
             fluidRow(
                    column(3, bsButton("mod_new_next", label = "Start process", disabled = F, block = T, style = "primary"), offset = 8)
             )
        ) # CLOSE FLUID PAGE
  ) # CLOSE TAB ITEM
}


#'
#' View Model tab
#'
synapsesSA_mod_view_tab <- function(){
  tabItem( tabName = "modelling_view",
           fluidPage(
             h1( "Model visualization"),
             p( "In this tab you can explore every model created in the application. After selecting a model, the plots and graphics correspondig to that model are displayed below."),
             hr(class = "hr-bluecolor"),
             bsAlert("mod_view_alert"),
             selectInput("mod_view_sel", "Model", NULL, width = "400px" ),
             hr(class = "hr-bluecolor"),
             conditionalPanel(condition="input.mod_view_sel",
                                downloadButton("mod_view_download", "Download model"),
                                # Summary
                                fluidRow(
                                  tags$h2("Model summary"),
                                  tags$ul(style="columns:2;-webkit-columns: 2;-moz-columns: 2;",
                                          uiOutput("mod_view_summary",inline = T)
                                  ),
                                  hr(class = "hr-bluecolor")
                                ),
                                # Plots
                                fluidRow(
                                   tags$h2("Plots"),
                                   fluidRow(column(6,tags$h3("Feret's diameter"),plotOutput("mod_view_feret",height = "400px" )),
                                            column(6,tags$h3("RSA envelope"),plotOutput("mod_view_rsaenv",height = "400px" ))
                                   ),
                                   fluidRow(column(12,tags$h3("FGKL functions"),plotOutput("mod_view_fgkl",height = "600px")))
                                )
                              )
             
           ) # CLOSE FLUID PAGE
  ) # CLOSE TAB ITEM
}
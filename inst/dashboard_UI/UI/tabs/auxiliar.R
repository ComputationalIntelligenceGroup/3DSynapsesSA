
synapsesSA_build_info_tab <- function(){
  
  tabItem( tabName = "info_tab",
           fluidPage(
             
             tags$h1("3D Synapses spatial analysis - Graphical User Interface"),
             
             fluidRow(tags$p("Welcome to 3DSynapsesSA, an R package for spatial analysis of synapses. The application is divided in five main sections:"),
                      tags$ul(
                        tags$li( tags$b("3D Visualization"), ": ", "This section provides a 3D sample visualizator of the spatial distribution of loaded and simulated synapses."),
                        tags$li( tags$b("Data loading"),": ", "In this tab the user can upload new files and export loaded and simulated data in CSV format." ),
                        tags$li( tags$b("Modelling"),": ", "This tab contains a four step process to build a new RSA spatial model based on selected samples as well as a model viewer." ),
                        tags$li( tags$b("Simulation"), ": ", "In the simulation tab the user can generate new distributions of synapses from the models built in the previous section."),
                        tags$li( tags$b("Layer comparison"), ": ", "Finally, in the layer comparison tab, the user can compare the synaptic density and the distance to the nearest synapse between layers.")
                      ),
                      "In the boxes below, you can find detailed information about these four sections. If you have any question about 3DsynapsesSA, this interface or you want to report a bug, the contact information is at the end of this page."
             ),
             br(),
             fluidRow(
               # DASHBOARD BOX
               box( 
                 # 3D View
                 tags$h3("3D Visualization tab"),
                 fluidRow(
                   tags$a(href="imgs/info/big/3dview.jpg", class="fancybox", rel="dash",
                          tags$img(src="imgs/info/small/3dview.png", alt="Dashboard tab", class="info-screenshot" )),
                   "The 3D visualization tab contains an interactive 3D plot of any sample in the application (real or simulated). In the controls on the left side you can find a soma selector and the render options, while the 3D plot is on the right side.",
                   br(),
                   "The first step is to select a sample in the selector. Once a sample has been selected you can select which synapses to plot (symmetric and/or asymmetric) and its color. After setting all the parameters, click on draw button to create or update the 3D plot. ",
                   br(),
                   "You can move the camera in the 3D plot using your keyboard, specific controls are detailed in the link on the top-left side of the plot."
                   ,style = "padding-right:20px"
                 ),
                 title = "3D visualization", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             
             fluidRow(  # DATA LOADING BOX
               box( 
                 tags$h3("Data load tab"),
                 fluidRow(
                   tags$a(href="imgs/info/big/load_files.jpg", class="fancybox", rel="load",
                          tags$img(src="imgs/info/small/load_files.png", alt="Data load tab", class="info-screenshot" )),
                   "In the data load tab the user can upload file in CSV or XSL format .",
                   "The controls are located  on the left side. The user can select one or more files to upload using the file explorer. Sample name, layer and number can be selected in the general options tab.",
                   "Each sample is automatically tested and validated. Validation results are shown in the process report tab."
                   ,style = "padding-right:20px"
                 ),
                 tags$h3("Data view"),
                 fluidRow(
                   tags$a(href="imgs/info/big/load_view.jpg", class="fancybox", rel="load",
                          tags$img(src="imgs/info/small/load_view.png", alt="Char table tab", class="info-screenshot" )),
                   "In the data view tab a table displays all variables in the data files (one row per sample).  On the left side you can select which data files to show in the table.",
                   "You can also download the table in CSV format. Just click the export button under the selector to download selected data in a ZIP file.",
                   style = "padding-right:20px"
                 ),
                 title = "Data loading", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             fluidRow( 
               # MODELLING BOX
               box( 
                 tags$h3("New model tab"),
                 fluidRow(
                   tags$a(href="imgs/info/big/mod_new.jpg", class="fancybox", rel="mod",
                          tags$img(src="imgs/info/small/mod_new.png", alt="New model tab", class="info-screenshot" )),
                   "In this tab the user can create a new model and:",
                   tags$ul(
                     tags$li("Select the data to build the model"),
                     tags$li("Fit the synapses Feret's diameter"),
                     tags$li("Compute the synaptic intensity and compare it to a RSA model"),
                     tags$li("Plot the characteristic FGKL functions for a spatial process"),
                     tags$li("Name and save the model")
                   )
                 ),
                 tags$h3("Model visualization tab"),
                 fluidRow(
                   tags$a(href="imgs/info/big/mod_view.jpg", class="fancybox", rel="mod",
                          tags$img(src="imgs/info/small/mod_view.png", alt="Model view tab", class="info-screenshot" )),
                   "Given a model, this tab shows detailed information about it. In the general info section some general number such as the number of instances used in the learning process, model parameters and the plots associated to the model.",
                   " You can also download the model in XML format along with its plot by clicking in the download model button."
                   ,style = "padding-right:20px"
                 ), 
                 title = "Modelling", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             fluidRow( 
               # SIMULATION BOX
               box(  tags$h3("New simulation tab"),
                     fluidRow(
                       tags$a(href="imgs/info/big/sim_new.jpg", class="fancybox", rel="sim",
                              tags$img(src="imgs/info/small/sim_new.png", alt="New simulation tab", class="info-screenshot" )),
                       "Simulation creates new synapses samples from a spatial RSA model. Model parameters can be setted either manually or taken from a previously created model.
                       To do so, users selects one of the previously computed models in Model selector. Additional simulation parameters, such as the volume or the number of simulations can also be specified in the boxes on the left."
                       ,style = "padding-right:20px"
                     ),
                     title = "Simulation", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             fluidRow( 
               # Layer comparison BOX
               box(  tags$h3("Synaptic density comparison"),
                     fluidRow(
                       tags$a(href="imgs/info/big/sd_comparison.jpg", class="fancybox", rel="sim",
                              tags$img(src="imgs/info/small/sd_comparison.png", alt="New simulation tab", class="info-screenshot" )),
                       "The analysis performed in the tab compares the synaptic density between samples from two or more layers. The results are summarized in a stacked bar plot."
                       ,style = "padding-right:20px"
                     ),
                     tags$h3("Nearest synapse distance comparison"),
                     fluidRow(
                       tags$a(href="imgs/info/big/sdist_comparison.jpg", class="fancybox", rel="sim",
                              tags$img(src="imgs/info/small/sdist_comparison.png", alt="New simulation tab", class="info-screenshot" )),
                       "The analysis performed in the tab compares the synaptic density between samples from two or more layers. The results are summarized in a stacked bar plot."
                       ,style = "padding-right:20px"
                     ),
                     title = "Layer comparison", status="info", solidHeader = T, width = 12, collapsible = T , collapsed = T )),
             
             fluidRow(hr(class="hr-greencolor")),
             # CIG row
             fluidRow(
               tags$a( href="http://cig.fi.upm.es/",
                       tags$img(src="imgs/logo_medq.png", alt="Computational Intelligence Group", style="height:125px; margin-right:40px; margin-left:50px; float:left" )),
               tags$a( href="http://www.upm.es/institucional",
                       tags$img(src="imgs/upm_logo.png", alt="Universidad Politécnica de Madrid", style="height:125px; margin-right:80px; margin-left:80px; float:left" )),
               tags$div(
                 "3DSynapsesSA library and this GUI have been developed by the Computational Intelligence Group (CIG) at the Universidad Politecnica de Madrid (Spain).",br(),
                 tags$b("Contact info:"),
                 tags$ul(
                   tags$li( tags$b("Webpage"), ": ", tags$a( href="http://cig.fi.upm.es/","link")),
                   tags$li( tags$b("Contact info"),": ", tags$a( href="mailto:luis.rodriguezl@upm.es","luis.rodriguezl@upm.es") ),
                   tags$li( tags$b("Phone"), ": +34 - 913363675")
                 ),
                 style= "float:left"
               )
             ),
             fluidRow(hr(class="hr-greencolor")),
             # HBP and EC row
             fluidRow(
               tags$a( href="https://www.humanbrainproject.eu",
                       tags$img(src="imgs/hbp-logo.png", alt="Human Brain Project", style="height:125px; margin-right:40px; float:left" )),
               tags$a( href="http://ec.europa.eu/",
                       tags$img(src="imgs/ec_logo.png", alt="European Commision", style="height:125px; margin-right:40px; float:left" )),
               tags$div(
                 " This work has been supported by European Union's Seventh Framework Programme (FP7/2007-2013) under grant agreement no. 604102 (Human Brain Project).",
                 style= "float:left"
               )
             )
           )
           )
}
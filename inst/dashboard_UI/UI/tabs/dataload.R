synapsesSA_dl_files_tab <- function(){
  tabItem( tabName = "dataloading_files",
           fluidPage(
             
             # HEADER
             h1( "Load CSV and/or XSL files"),
             p( "To load a synapsis data sample in CSV or XSL format, please use the file explorer on the sidebar to select the files. 
                If no sample id or layer is provided in the file, you can set it in the general options box. 
                For CSV files field delimiter and decimal dot options are available in the csv options box"),
             p("Any error detected during the upload or the parsing phase will be displayed in this same page. Files with errors will not be uploaded into the application."),
             hr(class = "hr-bluecolor"),
             # WORKSPACE (2 cols)
             bsAlert("dl_files_alert"),
             
             ## MODAL - CSV HELPER
             bsModal("dl_files_helpermod", "CSV file example", trigger = "examplelink",
                     fluidPage(
                       tags$p("Input CSV files must have a header and at least the following data:"),
                       fluidRow(align = "center",
                                tableOutput("dl_files_sampleTable")
                       ),
                       tags$p("Where:"),
                       tags$ul(
                         tags$li(tags$b("Layer: ")," Roman number that identifies the cerebral cortex layer of the sample (Required)"),
                         tags$li(tags$b("Sample.no: "),"Number that identifies the sample. Each sample should have a unique ID (Required)"),
                         tags$li(tags$b("Type: "),"Determines the type of synapse; Symmetric or Asymmetric (Optional)"),
                         tags$li(tags$b("Feret: "),"Synapse Feret's diameter in nanometers (Required)"),
                         tags$li(tags$b("X: "),"Synapse X coordinate in nanometers (Required)"),
                         tags$li(tags$b("Y: "),"Synapse Y coordinate in nanometers (Required)"),
                         tags$li(tags$b("Z: "),"Synapse Z coordinate in nanometers (Required)")
                       ),
                       tags$p("For instance, a semicolon separated file with dots as decimal points will look like:"),
                       fluidRow(align = "left",
                                verbatimTextOutput("dl_files_sampleTableV")
                       )
                     )
             ),
             
             # FIRST COLUMN: Controls
             column(4,
                    
                    # File selector
                    box( title = "File selector", status = "primary", solidHeader = T, width = NULL, collapsible = T,
                         fileInput('dl_files_files', h4('Choose CSV/XSL file'),
                                   accept = c('text/csv', 
                                            'text/comma-separated-values,text/plain',
                                            'application/vnd.ms-excel',
                                            '.xls',
                                            '.xlsx',
                                            '.csv'),
                                   multiple = T),
                         helpText("Note: CSV file must contain, at least, (x,y,z) coordinates and Feret's diameter of
                                     each synapse in nanometers, a sample ID number and its layer (in roman numbers)."),
                         tags$p(HTML("Click  <a id = 'examplelink' href = '#'>here</a> to see an example")),
                         bsTooltip("dl_files_files","Select one or more data files and press Load synapses button")
                    ),
                    
                    # Layer / sample number
                    box( title = "General options", status = "primary", solidHeader = T, width = NULL, collapsible = T, collapsed = T,
                         # Sample name
                         textInput("dl_files_samplename",h4("Sample name"), value = ""),
                         bsTooltip("dl_files_samplename","Set sample name. If name is empty it will take the name from the file or from the filename"),
                         checkboxInput("dl_files_nameoverwrite", "Overwrite sample name in the files"),
                         
                         # Layer
                         selectInput("dl_files_layer",h4("Layer"),c("In file","I","II","III","IV","V","VI")),
                         bsTooltip("dl_files_layer","Select layer. If any layer is selected it will overwrite any layer declared in the files"),
                         # Sample number
                         numericInput("dl_files_samplenum",h4("Sample number"),min = 0,max = 1E8,step = 1,value = 0),
                         bsTooltip("dl_files_samplenum","Select sample number. If sample number is not 0 it will overwrite any sample declared in the files")
                    ),
                    # CSV options
                    uiOutput("dl_files_csvopts_ui"),
                    # Load button
                    column(8, 
                           bsButton("dl_files_load", "Load synapses", style = "primary", disabled = T, block = T),
                           offset = 2)
             ),
             # Right side: Log
             column(8,
                    box( title = "Process report", status = "info", solidHeader = T, width = NULL, collapsible = T, collapsed = T,
                         uiOutput("dl_files_report")
                    )
             )
           )
  )
}

#' DATA TABLES VIEW TAB
#' 
#' 
synapsesSA_dl_view_tab <- function(){
  tabItem( tabName = "dataloading_view",
           fluidPage(
             
             # HEADER
             h1( "View data tables"),
             p("The table displays all variables in the data files (one row per sample). 
               On the left side you can select which data files to show in the table."),
             p("To export the table in CSV format, click the export button under the selector. A ZIP file containing selected data will be created."),
             
             hr(class = "hr-bluecolor"),
             # WORKSPACE (2 cols)
             bsAlert("dl_view_alert"),
             
             # FIRST COLUMN: Selector
             column(4,
                    # File selector
                    box( title = "Sample selector", status = "primary", solidHeader = T, width = NULL, collapsible = T,
                         hCBoxInput("dl_view_selector", NULL, animation = 0, expand.selected = F),
                         bsTooltip("dl_view_selector","Select one or more samples to populate the table")
                    ),
                    shiny::conditionalPanel("input.dl_view_selector", 
                                            downloadButton("dl_view_download", "Download selected data"))
             ),
             # Right side: Log
             column(8,
                    dataTableOutput("dl_view_table")
             )
           )
  )
}
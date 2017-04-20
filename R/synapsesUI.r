#' User interface launcher
#'
#' Shiniy created - HTML user interface launcher
#'
#' @author Laura Antón-Sánchez

#'run_SynapsesSA_UI
#'
#' Runs the shiny application and opens a browser with the UI
#'
#'@export
run_SynapsesSA_UI <- function (){
  shiny::runApp(
    system.file('dashboard_UI',                                                    
                package='synapsesSA'),
    launch.browser=T)
}
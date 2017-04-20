# Used packages
library(synapsesSA)
library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyjs)
library(shinyHierarchicalCbox)
library(multiStepPage)
library(threejswrapper)

# Options (NONE)

# Add static images/CSS path
addResourcePath('static', system.file("dashboard_UI/statics/", package = "synapsesSA"))

# Make logos (and static imgs) available
addResourcePath('imgs', system.file("imgs/", package = "synapsesSA"))
addResourcePath('fancybox', system.file("fancybox/", package = "synapsesSA"))

# Sub-source files
serverFolder <- "server/"
UIFolder <- "UI/"
# DO NOT CALL this folder AUX... Windows doesnt allow it
auxFolder <- "auxiliar/"

# Source any .R file in those folders

#if ( is.null(options()$SYNAPSES_SA_DEBUG) ) {
  sapply(list.files(path = serverFolder, pattern = "*.R", recursive = T , full.names = TRUE, ignore.case = T), source, .GlobalEnv)
  sapply(list.files(path = UIFolder, pattern = "*.R", recursive = T , full.names = TRUE, ignore.case = T), source, .GlobalEnv)
  sapply(list.files(path = auxFolder, pattern = "*.R", recursive = T , full.names = TRUE, ignore.case = T), source, .GlobalEnv)  
  


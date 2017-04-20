# Shiny UI 
shinyUI(
  
  dashboardPage(
    ##
    # HEADER
    ##
    dashboardHeader(
      
      ## Header title on the left
      title = tagList("SynapsesSA"),
      
      # build header
      .list = synapsesSA_header()
      
    ),
    
    ##
    # SIDEBAR
    ##
    dashboardSidebar(
      
      synapsesSA_sidebar()
      
    ),
    
    ##
    # BODY
    ##
    dashboardBody(
      tagList(),
      # BASE CSS 
      tags$link(rel = "stylesheet", type = "text/css", href = "static/styles/synapses.css"),
      # BODY BUILDER
      synapsesSA_body(),
      tags$script(src = "fancybox/jquery.fancybox.pack.js"),
      tags$link(rel = "stylesheet", type = "text/css", href = "fancybox/jquery.fancybox.css"),
      tags$script( "$(document).ready(function() {
                    $(\".fancybox\").fancybox();
                     });")
    ),
    
    title = "SynapsesSA",
    skin = "blue"
  )
)

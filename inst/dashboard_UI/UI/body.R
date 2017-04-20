#' Dashboard body ui builder functions
#'
synapsesSA_body <- function(){
  
  ## Tabbed pages
  tabItems(
    # Dashboard
    
    # VIEW
    synapsesSA_dash_view_tab(),
    
    # Loading
    
    # Load-files
    synapsesSA_dl_files_tab(),
    
    # Data tables
    synapsesSA_dl_view_tab(),
    
    
    #SIM
    synapsesSA_sim_new_tab(),
    
    # MODELLING
    
    # NEW
    synapsesSA_mod_new_tab(),
    synapsesSA_mod_view_tab(),
    
    # LAYER COMP
    synapsesSA_lcomp_dens_tab(),
    synapsesSA_lcomp_dist_tab(),
    
    # INFO
    synapsesSA_build_info_tab()
  )
}
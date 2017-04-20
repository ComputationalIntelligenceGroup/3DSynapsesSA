shinyServer(function(input, output, session) {
  
  # Base reactive values
  rv <- synapsesSA_setRV()
  
  # DASHBOARD
  
  # DASH VIEWER
  observe({update_dash_view_selector(rv,input,output,session)}) # Sample selector
  observe({dash_view_buttonControl(rv,input,output,session)}) # DRAW Button control
  
  observeEvent(input$dash_view_draw ,{dash_view_drawcomposition(rv,input,output,session)} )
  
  # DATA LOADING
  observe({dl_files_loadSampleTable(rv,input,output,session)}) # Sample tables
  observe({csvOpts_watcher(rv, input, output, session) }) # CSV UI BOX
  observe({dl_files_buttonControl(rv,input,output,session) }) # Load button enabler
  
  observeEvent( input$dl_files_load, {dl_files_loadListener(rv,input,output,session)} ) # LOAD EVENT LISTENER
  
  # DATA VIEWER
  observe({update_dl_view_selector(rv, input, output, session)})
  observe({update_dl_view_table(rv, input, output, session)})
  observe({manageCSVDownload(rv, input, output, session)})
  
  
  # MODEL
  
  # Avoid msp movements
  switchStepForward(session, "mod_new_msp", FALSE)
  switchStepBack(session, "mod_new_msp", FALSE)
  
  # General MSP
  observe({model_new_update_pageHeight(rv, input, output, session)})
  observeEvent(input$mod_new_next,{model_new_nextbutton(rv, input, output, session)})
  
  # FIRST STEP (SELECTOR)
  observe({update_mod_new_selector(rv, input, output, session)})
  observe({update_mod_new_select_nextbutton(rv, input, output, session)})
  
  # SECOND STEP (FERET)
  observeEvent(input$mod_new_feret, {update_mod_new_feret(rv, input, output, session)})
  
  # THIRD STEP (RSA)
  observe({update_mod_new_crossvalui(rv, input, output, session)})
  observeEvent(input$mod_new_rsa, {listen_mod_new_rsaenvelope(rv, input, output, session)})
  
  # FOURTH STEP (FGKL)
  observeEvent(input$mod_new_fgkl, {listen_mod_new_fgkl(rv, input, output, session)})
  
  # LAST: SAVE
  observe({watcher_mod_new_name(rv, input, output, session)})
  
  ## model view
  observe({update_model_view_selector(rv, input, output, session)})
  observe({update_model_view(rv, input, output, session)})
  
  # Model Download
  observe({manage_model_download(rv, input, output, session)})
  
  ## SIMULATION
  # Update selector
  observe({update_sim_new_modelsel(rv, input, output, session)})
  # Run!
  observeEvent(input$sim_new_run, {listener_sim_new_run(rv, input, output, session)})
  
  ## LAYER COMPARISON
  # Selector (BOTH!)
  observe({update_lcomp_selector(rv, input, output, session)})
  
  # DENSITY 
  
  # Button trigger
  observe({lcomp_dens_button_enable(rv, input, output, session)})
  # Run!
  observeEvent(input$lcomp_dens_run, {lcomp_dens_run_listener(rv, input, output, session)})
  
  # DISTANCE
  observe({lcomp_dist_button_enable(rv, input, output, session)})
  # Run!
  observeEvent(input$lcomp_dist_run, {lcomp_dist_run_listener(rv, input, output, session)})
  
})


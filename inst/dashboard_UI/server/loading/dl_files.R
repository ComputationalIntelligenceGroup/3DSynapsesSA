#'
#' Check the presence of CSV files in the selector to trigger csvOpts
#' and builds the parameter box
#' 
#' NOTE: Only uses input/output but header is standard to avoid problems
csvOpts_watcher <- function(rv, input, output, session ){
  
  # 
  # shinyLogger$entering() # Add function calling
  
  # Check file selector
  fs <- input$dl_files_files
  
  # Avoid ghost calls
  if ( is.null(fs) ) return();
  
  # Use files as list instead of df
  fs <- unlist(apply(fs, 1, list), recursive = FALSE)
  fs <- lapply(fs, as.list)
  
  # Check for any CSV file
  if ( any( vapply(fs, function(x){ tolower(tools::file_ext(x$name)) == "csv" }, FUN.VALUE = logical(1) ) ) ) {
    
    # shinyLogger$debug("Triggering CSV Options Box")
    
    # Create CSV UI BOX
    output$dl_files_csvopts_ui <- renderUI({
      box( title = "CSV options", status = "primary", solidHeader = T, width = NULL, collapsible = T, collapsed = F,
           radioButtons('dl_files_dec', h4('Decimal points'),
                        c(Dot = '.',
                          Comma = ','),
                        '.'),
           bsTooltip("decsep","Select numerical decimal point for CSV files"),
           radioButtons('dl_files_sep', h4('Field separator'),
                        c(Comma = ',',
                          Semicolon = ';',
                          Tab = '\t'),
                        ';'),
           bsTooltip("sep","Select field separator character for CSV files")
      )
    })
  }
  # shinyLogger$exiting() # Add function exit
}

#' Warns for unicity
sampleUnicity_watcher <- function(rv, input, output, session ){
  
  name <- input$dl_files_samplename
  number <- input$dl_files_samplenum
  
  if (sampleExists(rv,name,number))
    createAlert(session, "dl_files_alert", title = "Duplicated sample", 
                alertId = "dl_files_unicitywarn",
                content = "One or more samples with the same name and number have been already loaded",
                dismiss = T,
                append = F,
                style = "warning")
  else
    closeAlert(session, "dl_files_unicitywarn" )
  
}
  
#' Enables / disables the load synapsesbutton
#'
dl_files_buttonControl <- function(rv, input, output, session){
  
  # shinyLogger$entering() # Add function calling

  # Check file selector
  fs <- input$dl_files_files
  
  # Update dl_files button
  updateButton(session, "dl_files_load", disabled = is.null(fs) )
  
  #shinyLogger$exiting()
}

#' Load control (triggered when Load button is pressed)
#'
#'
dl_files_loadListener <- function(rv, input, output, session){
  
  # shinyLogger$entering()
  write("CALL load listener", file = stderr() )
  # Check file selector
  fs <- input$dl_files_files
  
  # Avoid ghost calls (observeEvent should avoid it, but its just a harmless extra check)
  if ( is.null(fs) ) return();
  
  # Get options
  sep <- isolate(input$dl_files_sep)
  dec <- isolate(input$dl_files_dec)
  samplenum <- isolate(input$dl_files_samplenum)
  samplename <- isolate(input$dl_files_samplename)
  layer <- isolate(input$dl_files_layer)
  overwrite <- isolate(input$dl_files_nameoverwrite)
  
  # Use files as list instead of df
  fs <- unlist(apply(fs, 1, list), recursive = FALSE)
  fs <- lapply(fs, as.list)
  
  # Same process for each file
  loaded <- lapply( fs, function(x, sep, dec, layer, samplenum, samplename, overwrite){
    
    # Get file extension
    ext <- tools::file_ext(x$name)
    if (is.null(samplename) || samplename == "" ) samplename <- basename(x$name)
    
    if ( tolower(ext) == "csv") {
      # Read CSV FILE
      l <- readSynapsesCSV(x$datapath,
                           fieldSeparator = sep,
                           decimalPoint = dec,
                           layer = layer,
                           samplenum = samplenum,
                           overwriteName = overwrite,
                           samplename = samplename)
    }
    else{
      ## Rename so we keep the extension (needed for XLS reading)
      file.rename(x$datapath,
                  paste0(x$datapath, ".", ext))
      x$datapath <- paste0(x$datapath, ".", ext)
      
      # REad file from EXCEL
      l <- readSynapsesXLS(x$datapath,
                             layer = layer,
                           samplenum = samplenum,
                           overwriteName = overwrite,
                           samplename = samplename)
    }
    # Add name b4 returning
    l$name <- x$name
    # Random id
    l$id <- paste(sample(LETTERS,20),collapse = "")
    
    # Add sample
    if (!l$error) {
      pairs <- unique(l$data[,c("sample","samplenum")])
      v <- mapply( function(sample,num, file){
                      r <- sampleExists(rv,sample,num)
                      synapsesSA_addSample(rv, sample, num, file, tools::file_ext(file) )
                      return(r)
                    },
                    sample = pairs$sample,
                    num = pairs$samplenum,
                    MoreArgs = list(file = l$name),
                    SIMPLIFY = T)
      
      
      if ( any(v) ) {
        l$errorDescription <- "One or more duplicated samples in the file."
        l$errorCode <- -1
      }
      else{
        overlap <- checkOverlappingSynapses(l$data)
        if (any(overlap)) {
          l$errorDescription <- sprintf("%d overlapping synapses in the file.",sum(overlap)/2)
          l$errorCode <- -2
        }  
      }
    }
    
    return(l)
    
  },sep, dec, layer, samplenum, samplename, overwrite)
  
  # Add data
  toLoad <- do.call(rbind, lapply(loaded, function(x){ if ( !x$error ) x$data else NULL }))
  
  if ( !is.null(toLoad) )
    synapsesSA_addSynapses(rv, toLoad)
  
  # Check for any error
  errFlag <- any(vapply(loaded,function(x) x$error , FUN.VALUE = logical(1) ))
  
  # Create report content
  report.content <- do.call(tagList, lapply(loaded, function(x){
    
    # Choose symbol
    if (x$error)
      sym <- icon("close", class = "big-red-close")
    else if ( x$errorCode != 0 )
      sym <- icon("warning", class = "big-orange-warn")
    else
      sym <- icon("check", class = "big-green-check")
    
    # Create a row for each file
    tags$tr( 
      tags$td( x$name ),
      tags$td( sym , id = x$id )
    )
  }))
  
  # Create tooltips
  tooltips <- do.call(tagList, lapply(loaded, function(x){
    return(bsTooltip(x$id, x$errorDescription))
  }))
  
  output$dl_files_report <- renderUI({
    tagList(
      tags$table(
        tags$tr(
          tags$th( "Filename"),
          tags$th( "Status")
        ),
        report.content,
        class = "table innerborder"
      ),
      tooltips
    )
  })
  
  # Disable load button
  if (!errFlag)
    updateButton(session, "dl_files_load", disabled = T )
  
  # Alerts
  if(errFlag)
    createAlert(session, "dl_files_alert", title = "Loading error", 
                content = "Errors encountered while loading files. Please check the process log for further details.",
                dismiss = T,
                append = F,
                style = "error")
  else
    createAlert(session, "dl_files_alert", title = "Success!", 
                content = "Loading process completed successfully.",
                dismiss = T,
                append = F,
                style = "success")
  #shinyLogger$exiting()
}

# Load sample table
dl_files_loadSampleTable <- function(rv, input, output, session){
  
  output$dl_files_sampleTable <- renderTable({
    read.csv(system.file("example.csv",package = "synapsesSA"), header = T, sep = ";", stringsAsFactors = F)
  },include.rownames = F,size = 10)
  
  output$dl_files_sampleTableV <- renderText({
    con <- file( system.file("example.csv",package = "synapsesSA") ,"r")
    txt <- readLines(con)
    close(con)
    paste0(txt,collapse = "\n")
  })
}
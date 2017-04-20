create_dlcomp_log <- function(diffs, stat ){
  
  ##
  ## Summary msg
  ## 
  #Check Homogeneity and normality and get pVal
  if (diffs$normAndHomo)
    pval <- min(diffs$gdif$groups[,4])
  else
    pval <- diffs$gdif$p.value
  
  if (is.null(diffs$pval)) {

    return(
      tags$p("There are", tags$b("no significative differences"), "between selected layers with respect to the ",
             stat,"(p-value: ",sprintf("%.4f",as.numeric(pval)),")")
    )
  } 
  else{
    first <- tags$p("There are", tags$b(" significative differences"), "between selected layers with respect to the ",
                   stat,"(p-value: ",sprintf("%.4f",as.numeric(pval)),")")

    # Comparisons and pvalues
    listElements <- list()
    for (i in 1:ncol(diffs$pval)) {
      if (as.numeric(diffs$pval[3,i]) > 0.05 ) {
        listElements[[ length(listElements) + 1 ]] <- tags$li( tags$b(diffs$pval[1,i], " vs. ", diffs$pval[2,i]),
                                                               " (p-value ",sprintf("%.4f",as.numeric(diffs$pval[3,i]))," )")
      }
    }

    return(tagList(
      first,
      tags$p("There is", tags$b("no significant difference"), "between layers:", 
             do.call(tags$ul, c(listElements, list(style = 'columns: 2;-webkit-columns: 2;-moz-columns: 2;')) ) )
    ))
  }
}
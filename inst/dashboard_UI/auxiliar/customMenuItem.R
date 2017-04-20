customMenuItem <- function (text, ..., icon = NULL, badgeLabel = NULL, badgeColor = "green", 
                            tabName = NULL, href = NULL, newtab = TRUE, selected = NULL, hasTab = F ) 
{
  subItems <- list(...)
  if (!is.null(icon)) 
    shinydashboard:::tagAssert(icon, type = "i")
  if (!is.null(href) + (!is.null(tabName) + (length(subItems) > 
                                             0) != 1)) {
    stop("Must have either href, tabName, or sub-items (contained in ...).")
  }
  if (!is.null(badgeLabel) && length(subItems) != 0) {
    stop("Can't have both badge and subItems")
  }
  shinydashboard:::validateColor(badgeColor)
  isTabItem <- FALSE
  target <- NULL
  if (!is.null(tabName)) {
    shinydashboard:::validateTabName(tabName)
    isTabItem <- TRUE
    href <- paste0("#shiny-tab-", tabName)
  }
  else if (is.null(href)) {
    href <- "#"
  }
  else {
    if (newtab) 
      target <- "_blank"
  }
  if (!is.null(badgeLabel)) {
    badgeTag <- tags$small(class = paste0("badge pull-right bg-", 
                                          badgeColor), badgeLabel)
  }
  else {
    badgeTag <- NULL
  }
  if (length(subItems) == 0) {
    return(tags$li(a(href = href, `data-toggle` = if (isTabItem) "tab", 
                     `data-value` = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL, 
                     target = target, icon, span(text), badgeTag)))
  }
  else if (!hasTab)
    return( tags$li(class = "treeview", a(href = href, icon, span(text), 
                                          shiny::icon("angle-left", class = "pull-right")), tags$ul(class = "treeview-menu", 
                                                                                                    subItems)) )
  else
    return( tags$li(class = "treeview", a(href = href, icon, span(text), 'data-toggle' = if (isTabItem) "tab", 
                                          'data-value' = if (!is.null(tabName)) tabName, `data-start-selected` = if (isTRUE(selected)) 1 else NULL,
                                          shiny::icon("angle-left", class = "pull-right")), tags$ul(class = "treeview-menu", 
                                                                                                    subItems)) )
}
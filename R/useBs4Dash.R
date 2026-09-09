#' Use 'bs4Dash' in 'shiny'
#' Allow to use functions from 'bs4Dash' into a classic 'shiny' app,
#' specifically `bs4ValueBox`, `bs4InfoBox` and `bs4Card`.
#'
#' @export
#'
#' @param ... Not used.
#'
useBs4Dash <- function(...) {
  if (!requireNamespace(package = "bs4Dash"))
    message("Package 'bs4Dash' is required to run this function")
  deps <- htmltools::findDependencies(bs4Dash::bs4DashPage(
    header = bs4Dash::bs4DashNavbar(),
    sidebar = bs4Dash::bs4DashSidebar(),
    body = bs4Dash::bs4DashBody()
  ))
  htmltools::attachDependencies(htmltools::tags$div(), value = deps)
}

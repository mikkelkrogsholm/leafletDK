#' Gives you the ids of a given area
#'
#' @param area is the name of the area you want ids from. must be either:
#' \itemize{
#'   \item "constituency"
#'   \item "district"
#'   \item "municipal"
#'   \item "parish"
#'   \item "regional"
#'   \item "rural"
#'   \item "zip"
#' }
#'
#' @return String of ids for the given area
#'
#' @examples
#' library(leafletDK)
#'
#' getIDs("constituency")
#'
#' @export

getIDs <- function(area){

  if(area == "constituency") ids <- sort(unique(constituency$name))

  if(area == "district") ids <- sort(unique(district$name))

  if(area == "municipal") ids <- sort(unique(municipal$name))

  if(area == "parish") ids <- sort(unique(parish$name))

  if(area == "regional") ids <- sort(unique(regional$name))

  if(area == "rural") ids <- sort(unique(rural$name))

  if(area == "zip") ids <- sort(unique(zip$id))

  return(ids)

}


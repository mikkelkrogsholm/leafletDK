#' Creates dynamic chloropleth maps of danish parishes
#'
#' @param value is the name of the column in the data where the values are stored
#' @param id is the name of the column in the data where the ids are stored
#' @param subplot is a string of ids you want to keep - excludes all others
#' @param data is the data frame that contains the data to map
#' @param map is a TRUE / FALSE of wether a real map should be plotted underneath
#'
#' @return An interactive Leaflet map
#'
#' @examples
#' library(leafletDK)
#'
#' km1 <- readr::read_csv2("http://api.statbank.dk/v1/data/KM1/CSV?SOGN=*&FKMED=*")
#' km1 <- tidyr::spread(km1, FKMED, INDHOLD)
#' km1$SOGN <- stringr::str_trim(stringr::str_replace_all(km1$SOGN, "(\\d+)|([-])", ""))
#' km1 <- dplyr::group_by(km1, SOGN)
#' km1 <- dplyr::summarise(km1,
#'                         medlem = sum(`Medlem af Folkekirken`, na.rm = T),
#'                         ikke_medlem = sum(`Ikke medlem af Folkekirken`, na.rm = T))
#' km1 <- dplyr::mutate(km1, pct = medlem/(ikke_medlem + medlem), pct = round(pct*100,2))
#'
#' parishDK("pct", "SOGN", data = km1)
#'

parishDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                     map = FALSE, legend = FALSE, pal = "YlOrRd", logcol = F){

  # Kortdata ----

  shapefile <- leafletDK::parish

  shapefile <- join_map_data(value = value, id = id, subplot = subplot, mapdata = data, shapefile)


  # Kortlægning

  leafletmap <- map_it(shapefile, map = map, legend = legend, pal = pal, logcol = logcol)


  return(leafletmap)
}

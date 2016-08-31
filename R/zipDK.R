#' Creates dynamic chloropleth maps of danish zip codes
#'
#' @param value is the name of the column in the data where the values are stored
#' @param id is the name of the column in the data where the ids are stored
#' @param subplot is a string of ids you want to keep - excludes all others
#' @param data is the data frame that contains the data to map
#' @param map is a TRUE / FALSE of wether a real map should be plotted underneath
#' @param legend is a TRUE / FALSE of wether the legend should be plotted
#' @param pal is the color palette for the chloropleth
#' @param logcol is a TRUE / FALSE of wether the chloropleth colors should be drawn against the log value
#' @param legendtitle provides an alternative title for the legend
#'
#' @return An interactive Leaflet map
#'
#' @examples
#' library(leafletDK)
#'
#' ejen4 <- readr::read_csv2("http://api.statbank.dk/v1/data/EJEN4/CSV?OMR%C3%85DE=*&EJENDOMSKATE=2103&BN%C3%98GLE=2&OVERDRAG=1&Tid=2001")
#'
#' ejen4 <- dplyr::select(ejen4, OMRÅDE, TID, INDHOLD)
#' ejen4 <- ejen4[stringr::str_detect(ejen4$OMRÅDE,"[A-ZÆØÅa-zæøå]"),]
#' ejen4$zip <- unlist(stringr::str_extract_all(ejen4$OMRÅDE, "\\d{4}"))
#' ejen4$zip <- ifelse(as.numeric(ejen4$zip) < 2400, round(as.numeric(ejen4$zip)/100)*100, as.numeric(ejen4$zip))
#' ejen4 <- dplyr::group_by(ejen4, zip)
#' ejen4 <- dplyr::summarise(ejen4, indhold = sum(INDHOLD))
#'
#' zipDK("indhold", "zip", data = ejen4)
#'
#' @export

zipDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                  map = FALSE, legend = FALSE, pal = "YlOrRd", logcol = F,
                  legendtitle = NULL){

  # Kortdata ----

  shapefile <- leafletDK::zip

  shapefile <- join_map_data_zip(value = value, id = id, subplot = subplot, mapdata = data, shapefile)

  # Kortlægning

  if(is.null(legendtitle)) legendtitle <- value

  leafletmap <- map_it(shapefile, map = map, legend = legend, pal = pal, logcol = logcol, legendtitle = legendtitle)


  return(leafletmap)
}

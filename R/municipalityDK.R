#' Creates dynamic chloropleth maps of danish municipalities
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
#' folk1 <- readr::read_csv2("http://api.statbank.dk/v1/data/folk1a/CSV?OMR%C3%85DE=*")
#'
#' municipalityDK("INDHOLD", "OMRÅDE", subplot = c("frederiksberg","hvidovre"), data = folk1)
#'
#' @export

municipalityDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                           map = FALSE, legend = FALSE, pal = "YlOrRd", logcol = F,
                           legendtitle = NULL){

  # Kortdata ----

  shapefile <- leafletDK::municipal

  shapefile <- join_map_data(value = value, id = id, subplot = subplot, mapdata = data, shapefile)


  # Kortlægning

  if(is.null(legendtitle)) legendtitle <- value

  leafletmap <- map_it(shapefile, map = map, legend = legend, pal = pal, logcol = logcol, legendtitle = legendtitle)

  return(leafletmap)
}

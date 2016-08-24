#' Creates dynamic chloropleth maps of danish nominating districts
#'
#' @param value is the name of the column in the data where the values are stored
#' @param id is the name of the column in the data where the ids are stored
#' @param subplot is a string of ids you want to keep - excludes all others
#' @param data is the data frame that contains the data to map
#' @param map is a TRUE / FALSE of wether a real map should be plotted underneath
#' @param legend is a TRUE / FALSE of wether the legend should be plotted
#' @param pal is the color palette for the chloropleth
#' @param logcol is a TRUE / FALSE of wether the chloropleth colors should be drawn against the log value
#'
#' @return An interactive Leaflet map
#'
#' @examples
#' library(leafletDK)
#'
#' fv15tot <- readr::read_csv2("http://api.statbank.dk/v1/data/FV15TOT/CSV?VALRES=VAELG%2C13&OMR%C3%85DE=*")
#' fv15tot <- tidyr::spread(fv15tot, VALRES, INDHOLD)
#' fv15tot <- dplyr::select(fv15tot, -TID)
#' names(fv15tot) <- c("area", "writing", "voters")
#' fv15tot <- dplyr::mutate(fv15tot, pertenthousand = writing/(voters/10000))
#'
#' fv15tot <- dplyr::filter(fv15tot, stringr::str_detect(fv15tot$area, "OPSTILLINGSKREDS"))
#' fv15tot <- dplyr::mutate(fv15tot, area = stringr::str_to_lower(stringr::str_replace_all(area, "( OPSTILLINGSKREDS)|(\\d+[.] )", "")))
#'
#' districtDK("pertenthousand", "area", data = fv15tot)
#'
#' @export

districtDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                       map = FALSE, legend = FALSE, pal = "YlOrRd", logcol = F){

  # Kortdata ----

  shapefile <- leafletDK::district

  shapefile <- join_map_data(value = value, id = id, subplot = subplot, mapdata = data, shapefile)

  # Kortlægning

  leafletmap <- map_it(shapefile, map = map, legend = legend, pal = pal, logcol = logcol)

  return(leafletmap)
}

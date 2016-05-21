#' Creates dynamic chloropleth maps of danish municipalities
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
#' folk1 <- read.csv2("http://api.statbank.dk/v1/data/folk1/CSV?OMR%C3%85DE=*",
#'                      stringsAsFactors = FALSE, encoding = "UTF-8")
#'
#' municipalityDK("INDHOLD", "OMRÅDE", subplot = c("frederiksberg","hvidovre"), data = folk1)
#'
#' @export

municipalityDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                           map = F, legend = F){

  require(leaflet)

  # Kortdata ----

  shapefile <- leafletDK::municipal

  # Fix possible encoding issues
  shapefile$name <- fix_names_encoding(shapefile$name)
  shapefile@data$name <- fix_names_encoding(shapefile@data$name)

  shapefile_data <- shapefile@data

  mapdata <- data

  mapdata$joinID <- fix_names_join(fix_names_encoding(mapdata[, id]))

  names(mapdata)[which(names(mapdata) == value)] <- "values"

  mapdata <- mapdata[,c("values", "joinID")]

  shapefile_data <- shapefile_data %>% dplyr::left_join(mapdata)

  shapefile@data <- shapefile_data

  if(!(is.null(subplot))) {
    subplot <- fix_names_join(subplot)
    shapefile <- subset(shapefile, shapefile$joinID %in% subplot)
    }

  missingnames <- unique(shapefile$name[is.na(shapefile$values)])

  if(length(missingnames) != 0){print(paste0("Missing values for ", missingnames))}

  shapefile <- subset(shapefile, !(is.na(shapefile$values)))

  # Farve og popup ----

  colorscale = colorNumeric("YlOrRd", domain = NULL)

  data_popup <- paste0("<strong>", shapefile$name, "</strong>",
                       "<br>", prettyNum(shapefile$values, big.mark = ".", decimal.mark = ","))

  # Kortlægning ----

  leafletmap <- leaflet(shapefile) %>%
    addPolygons(fillColor = ~colorscale(values),
                fillOpacity = 0.8,
                color = "000000",
                stroke = F,
                popup = data_popup)

  if(legend == T) {
    leafletmap <- addLegend(leafletmap, "bottomright", pal = colorscale, values = ~values,
                            title = stringr::str_to_title(value),
                            opacity = 1)
  }

  if(map == T) leafletmap <- addTiles(leafletmap)

  return(leafletmap)
}

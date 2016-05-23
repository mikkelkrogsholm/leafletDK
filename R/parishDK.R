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
#' library(tidyr)
#'
#' km1 <- read.csv2("http://api.statbank.dk/v1/data/KM1/CSV?SOGN=*&FKMED=*",
#'                  stringsAsFactors = FALSE, encoding = "UTF-8")
#' km1 <- km1 %>% spread(FKMED, INDHOLD)
#' km1$pct <- round(km1[,4]/(km1[,3]+km1[,4])*100,1)
#'
#' parishDK("pct", "SOGN", data = km1)
#'
parishDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                     map = FALSE, legend = FALSE){

  # Kortdata ----

  shapefile <- leafletDK::parish

  # Fix possible encoding issues
  shapefile$name <- fix_names_encoding(shapefile$name)
  shapefile@data$name <- fix_names_encoding(shapefile@data$name)

  shapefile_data <- shapefile@data

  mapdata <- data

  mapdata$joinID <- fix_names_join(fix_names_encoding(mapdata[, id]))
  mapdata$joinID <- stringr::str_replace_all(mapdata$joinID, "\\d+", "")

  names(mapdata)[which(names(mapdata) == value)] <- "values"

  mapdata <- mapdata[,c("values", "joinID")]

  shapefile_data <- shapefile_data %>% dplyr::left_join(mapdata)

  shapefile_data$values[is.na(shapefile_data$values)] <- NA

  shapefile@data <- shapefile_data

  if(!(is.null(subplot))) {
    subplot <- fix_names_join(subplot)
    shapefile <- subset(shapefile, shapefile$joinID %in% subplot)
    }

  missingnames <- unique(shapefile$name[is.na(shapefile$values)])

  if(length(missingnames) != 0){print(paste0("Missing values for ", missingnames))}

  # shapefile <- subset(shapefile, !(is.na(shapefile$values)))

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

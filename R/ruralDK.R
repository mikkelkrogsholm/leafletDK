#' Creates dynamic chloropleth maps of danish rural regions
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
#' aus08 <- read.csv2("http://api.statbank.dk/v1/data/AUS08/CSV?OMR%C3%85DE=01%2C02%2C03%2C04%2C05%2C06%2C07%2C08%2C09%2C10%2C11&SAESONFAK=9",
#'                    stringsAsFactors = FALSE, encoding = "UTF-8")
#' aus08$OMRÅDE <- str_replace_all(aus08$OMRÅDE, "Landsdel ", "")
#'
#' ruralDK("INDHOLD", "OMRÅDE", data = aus08)
#'
#' @export

ruralDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                    map = FALSE, legend = FALSE){

  # Kortdata ----

  shapefile <- leafletDK::rural

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

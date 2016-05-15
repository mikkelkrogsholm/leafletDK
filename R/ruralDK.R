#' Creates dynamic chloropleth maps of danish rural regions
#'
#' @param value is the name of the column in the data where the values are stored
#' @param id is the name of the column in the data where the ids are stored
#' @param subplot is a string of ids you want to keep - excludes all others
#' @param data is the data frame that contains the data to map
#'
#' @return An interactive Leaflet map
#'
#' @examples
#' library(leafletDK)
#' aus08 <- read.csv2("http://api.statbank.dk/v1/data/AUS08/CSV?OMR%C3%85DE=01%2C02%2C03%2C04%2C05%2C06%2C07%2C08%2C09%2C10%2C11&SAESONFAK=9",
#'                    stringsAsFactors = F)
#' aus08$OMRÅDE <- str_replace_all(aus08$OMRÅDE, "Landsdel ", "")
#'
#' ruralDK("INDHOLD", "OMRÅDE", data = aus08)
#'
#' @export

ruralDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL){

  require(leaflet)

  # Kortdata ----

  shapefile <- leafletDK::rural

  shapefile_data <- shapefile@data

  mapdata <- data

  # Fix names
  fixNames <- function(x){
    x <- tolower(x)
    x <- gsub("æ", "ae", x)
    x <- gsub("ø", "oe", x)
    x <- gsub("å", "aa", x)
    x <- gsub("-", "", x)
    x <- gsub(" ", "", x)
    return(x)
  }

  mapdata$joinID <- fixNames(mapdata[, id])

  names(mapdata)[which(names(mapdata) == value)] <- "values"

  mapdata <- mapdata[,c("values", "joinID")]

  shapefile_data <- shapefile_data %>% dplyr::left_join(mapdata)

  shapefile@data <- shapefile_data

  if(!(is.null(subplot))) {
    subplot <- fixNames(subplot)
    shapefile <- subset(shapefile, shapefile$joinID %in% subplot)
    }

  missingnames <- unique(shapefile$name[is.na(shapefile$values)])

  if(length(missingnames) != 0){print(paste0("Missing values for ", missingnames))}

  shapefile <- subset(shapefile, !(is.na(shapefile$values)))

  # Kortlægning ----

  colorscale = colorNumeric("YlOrRd", domain = NULL)

  map <- leaflet(shapefile) %>%
    addTiles %>%
    addPolygons(fillColor = ~colorscale(values),
                fillOpacity = 0.8,
                color = "000000",
                stroke = F) %>%
      addLegend("bottomright", pal = colorscale, values = ~values,
                title = stringr::str_to_title(value),
                labFormat = labelFormat(prefix = "",
                                        big.mark = "."),
                opacity = 1)

  return(map)
}

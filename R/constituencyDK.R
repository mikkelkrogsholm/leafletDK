#' Creates dynamic chloropleth maps of danish constituencies
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
#' library(dplyr)
#' library(tidyr)
#' library(stringr)
#'
#' fv15tot <- read.csv2("http://api.statbank.dk/v1/data/FV15TOT/CSV?VALRES=VAELG%2C13&OMR%C3%85DE=*",
#'                      stringsAsFactors = F)
#'
#' fv15tot <- fv15tot %>% spread(VALRES, INDHOLD) %>% select(-TID)
#' names(fv15tot) <- c("area", "writing", "voters")
#' fv15tot <- fv15tot %>% mutate(pertenthousand = writing/(voters/10000))
#' fv15tot <- fv15tot %>% filter(str_detect(fv15tot$area, "STORKREDS")) %>%
#'   mutate(area = str_to_lower(str_replace_all(area, "S STORKREDS", "")))
#'
#' constituencyDK("pertenthousand", "area", data = fv15tot)
#'
#' @export

constituencyDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                           map = F, legend = F){

  require(leaflet)

  # Kortdata ----

  shapefile <- leafletDK::constituency

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

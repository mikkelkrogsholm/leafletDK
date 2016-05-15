#' Creates dynamic chloropleth maps of danish zip codes
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
#' library(dplyr)
#' library(tidyr)
#' library(stringr)
#'
#' ejen4 <- read.csv2("http://api.statbank.dk/v1/data/EJEN4/CSV?OMR%C3%85DE=*&EJENDOMSKATE=2103&BN%C3%98GLE=2&OVERDRAG=1&Tid=2001",
#'                    stringsAsFactors = F)
#'
#' ejen4 <- ejen4 %>% select(OMRÅDE, TID, INDHOLD)
#' ejen4 <- ejen4[str_detect(ejen4$OMRÅDE,"[A-ZÆØÅa-zæøå]"),]
#' ejen4$zip <- unlist(str_extract_all(ejen4$OMRÅDE, "\\d{4}"))
#' ejen4$zip <- ifelse(as.numeric(ejen4$zip) < 2400, round(as.numeric(ejen4$zip)/100)*100, as.numeric(ejen4$zip))
#' ejen4 <- ejen4 %>% group_by(zip) %>% summarise(indhold = sum(INDHOLD))
#' ejen4 <- as.data.frame(ejen4)
#'
#' zipDK("indhold", "zip", data = ejen4)
#'
#' @export

zipDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL){

  require(leaflet)

  # Kortdata ----

  shapefile <- leafletDK::zip

  shapefile_data <- shapefile@data

  mapdata <- data

  mapdata$joinID <- mapdata[, id]

  names(mapdata)[which(names(mapdata) == value)] <- "values"

  mapdata <- mapdata[,c("values", "joinID")]

  shapefile_data <- shapefile_data %>% dplyr::left_join(mapdata)

  shapefile@data <- shapefile_data

  if(!(is.null(subplot))) {
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

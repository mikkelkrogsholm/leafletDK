#' Creates dynamic chloropleth maps of danish zip codes
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
#' ejen4 <- read.csv2("http://api.statbank.dk/v1/data/EJEN4/CSV?OMR%C3%85DE=*&EJENDOMSKATE=2103&BN%C3%98GLE=2&OVERDRAG=1&Tid=2001",
#'                    stringsAsFactors = FALSE, encoding = "UTF-8")
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

zipDK <- function(value = NULL, id = NULL, subplot = NULL, data = NULL,
                  map = FALSE, legend = FALSE){

  # Kortdata ----

  shapefile <- leafletDK::zip

  # Fix possible encoding issues
  shapefile$name <- fix_names_encoding(shapefile$name)
  shapefile@data$name <- fix_names_encoding(shapefile@data$name)

  shapefile_data <- shapefile@data

  mapdata <- data

  mapdata$joinID <- fix_names_join(fix_names_encoding(mapdata[, id]))
  mapdata$joinID <- as.integer(mapdata$joinID)

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

join_map_data <- function(value = NULL, id = NULL, subplot = NULL, mapdata = NULL, shapefile){

  # Fix possible encoding issues
  shapefile$name <- fix_names_encoding(shapefile$name)
  shapefile@data$name <- fix_names_encoding(shapefile@data$name)

  shapefile_data <- shapefile@data

  mapdata$joinID <- fix_names_join(fix_names_encoding(mapdata[id][[1]]))

  names(mapdata)[which(names(mapdata) == value)] <- "values"

  mapdata <- mapdata[,c("values", "joinID")]

  shapefile_data <- dplyr::left_join(shapefile_data, mapdata)

  shapefile@data <- shapefile_data

  if(!(is.null(subplot))) {
    subplot <- fix_names_join(subplot)
    shapefile <- subset(shapefile, shapefile$joinID %in% subplot)
  }

  missingnames <- unique(shapefile$name[is.na(shapefile$values)])

  if(length(missingnames) != 0){message(paste0("Missing values for ", sort(missingnames), "\n"))}

  # shapefile <- subset(shapefile, !(is.na(shapefile$values)))

  return(shapefile)
}

################

join_map_data_zip <- function(value = NULL, id = NULL, subplot = NULL, mapdata = NULL, shapefile){

  # Fix possible encoding issues
  shapefile$name <- fix_names_encoding(shapefile$name)
  shapefile@data$name <- fix_names_encoding(shapefile@data$name)

  shapefile_data <- shapefile@data

  mapdata$joinID <- as.integer(mapdata[id][[1]])

  names(mapdata)[which(names(mapdata) == value)] <- "values"

  mapdata <- mapdata[,c("values", "joinID")]

  shapefile_data <- dplyr::left_join(shapefile_data, mapdata)

  shapefile@data <- shapefile_data

  if(!(is.null(subplot))) {
    shapefile <- subset(shapefile, shapefile$joinID %in% subplot)
  }

  missingnames <- unique(shapefile$name[is.na(shapefile$values)])

  if(length(missingnames) != 0){print(paste0("Missing values for ", missingnames))}

  # shapefile <- subset(shapefile, !(is.na(shapefile$values)))

  return(shapefile)
}

################

map_it <- function(shapefile, map = FALSE, legend = FALSE, pal = "YlOrRd", logcol = F){

  colorscale = colorNumeric(pal, domain = NULL)

  data_popup <- paste0("<strong>", shapefile$name, "</strong>",
                       "<br>", prettyNum(shapefile$values, big.mark = ".", decimal.mark = ","))

  # Kortlægning ----

  leafletmap <- leaflet(shapefile) %>%
    addPolygons(fillColor = ~colorscale({if(logcol)log(values) else values}),
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

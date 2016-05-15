municipal <- rgdal::readOGR("inst/extdata/kommuner/kommuner.shp",
                     layer = "kommuner",
                     encoding="windows-1252")

shapefile <- municipal

shapefile_data <- shapefile@data

names(shapefile_data) <- c("id", "name")

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


shapefile_data$joinID <- fixNames(shapefile_data$name)
shapefile_data$name <- as.character(shapefile_data$name)
shapefile_data$id <- as.numeric(as.character(shapefile_data$id))

shapefile@data <- shapefile_data

municipal <- shapefile

devtools::use_data(municipal, overwrite = TRUE)

################################################################
################################################################

constituency <- rgdal::readOGR("inst/extdata/storkredse/storkredse.shp",
                            layer = "storkredse")

shapefile <- constituency

shapefile_data <- shapefile@data

library(dplyr)

shapefile_data <- shapefile_data %>% select(storkredsn, navn)

names(shapefile_data) <- c("id", "name")

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


shapefile_data$joinID <- fixNames(shapefile_data$name)
shapefile_data$name <- as.character(shapefile_data$name)
shapefile_data$id <- as.numeric(as.character(shapefile_data$id))

shapefile@data <- shapefile_data

constituency <- shapefile

devtools::use_data(constituency, overwrite = TRUE)

################################################################
################################################################

district <- rgdal::readOGR("inst/extdata/opstillingskredse/opstillingskredse.shp",
                               layer = "opstillingskredse")

shapefile <- district

shapefile_data <- shapefile@data

library(dplyr)

shapefile_data <- shapefile_data %>% select(opstilling, navn)

names(shapefile_data) <- c("id", "name")

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

shapefile_data$name <- str_replace_all(shapefile_data$name, "Utterslev", "Bispebjerg")

shapefile_data$joinID <- fixNames(shapefile_data$name)
shapefile_data$name <- as.character(shapefile_data$name)
shapefile_data$id <- as.numeric(as.character(shapefile_data$id))

shapefile@data <- shapefile_data

district <- shapefile

devtools::use_data(district, overwrite = TRUE)

################################################################
################################################################

load("data/zip.rda")

shapefile <- zip

shapefile_data <- shapefile@data

library(dplyr)

names(shapefile_data) <- c("id", "name")

shapefile_data$name <- as.character(shapefile_data$name)
shapefile_data$joinID <- as.numeric(as.character(shapefile_data$id))
shapefile_data$id <- as.numeric(as.character(shapefile_data$id))

shapefile@data <- shapefile_data

zip <- shapefile

devtools::use_data(zip, overwrite = TRUE)

################################################################
################################################################

regional <- region.polygon

shapefile <- regional

shapefile_data <- shapefile@data

shapefile_data <- as.data.frame(shapefile_data)

shapefile_data <- shapefile_data[,-1]

names(shapefile_data) <- c("id", "name")

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


shapefile_data$joinID <- fixNames(shapefile_data$name)
shapefile_data$name <- as.character(shapefile_data$name)
shapefile_data$id <- as.character(shapefile_data$id)

shapefile@data <- shapefile_data

regional <- shapefile

devtools::use_data(regional, overwrite = TRUE)

################################################################
################################################################

rural <- rural.polygon

shapefile <- rural

shapefile_data <- shapefile@data

shapefile_data$name[shapefile_data$name == "København By"] <- "Byen København"

shapefile_data <- as.data.frame(shapefile_data)

shapefile_data <- shapefile_data[,2:1]

names(shapefile_data) <- c("id", "name")

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


shapefile_data$joinID <- fixNames(shapefile_data$name)
shapefile_data$name <- as.character(shapefile_data$name)
shapefile_data$id <- as.character(shapefile_data$id)

shapefile@data <- shapefile_data

rural <- shapefile

devtools::use_data(rural, overwrite = TRUE)

################################################################
################################################################

parish <- parish.polygon

shapefile <- parish

shapefile_data <- shapefile@data

shapefile_data <- as.data.frame(shapefile_data)

names(shapefile_data) <- c("id", "name")

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


shapefile_data$joinID <- fixNames(shapefile_data$name)
shapefile_data$name <- as.character(shapefile_data$name)
shapefile_data$id <- as.character(shapefile_data$id)

shapefile@data <- shapefile_data

parish <- shapefile

devtools::use_data(parish, overwrite = TRUE)

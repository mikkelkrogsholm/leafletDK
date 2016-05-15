library(leafletDK)

folk1 <- read.csv2("http://api.statbank.dk/v1/data/folk1/CSV?OMR%C3%85DE=*",
                     stringsAsFactors = F)

municipalityDK("INDHOLD", "OMRÅDE", data = folk1)

#################################################
#################################################


library(leafletDK)
library(dplyr)
library(tidyr)
library(stringr)

fv15tot <- read.csv2("http://api.statbank.dk/v1/data/FV15TOT/CSV?VALRES=VAELG%2C13&OMR%C3%85DE=*",
                   stringsAsFactors = F)

fv15tot <- fv15tot %>% spread(VALRES, INDHOLD) %>% select(-TID)
names(fv15tot) <- c("area", "writing", "voters")
fv15tot <- fv15tot %>% mutate(pertenthousand = writing/(voters/10000))
fv15tot <- fv15tot %>% filter(str_detect(fv15tot$area, "STORKREDS")) %>%
  mutate(area = str_to_lower(str_replace_all(area, "S STORKREDS", "")))

constituencyDK("pertenthousand", "area", data = fv15tot)

#################################################
#################################################


library(leafletDK)
library(dplyr)
library(tidyr)
library(stringr)

fv15tot <- read.csv2("http://api.statbank.dk/v1/data/FV15TOT/CSV?VALRES=VAELG%2C13&OMR%C3%85DE=*",
                     stringsAsFactors = F)

fv15tot <- fv15tot %>% spread(VALRES, INDHOLD) %>% select(-TID)
names(fv15tot) <- c("area", "writing", "voters")
fv15tot <- fv15tot %>% mutate(pertenthousand = writing/(voters/10000))
fv15tot <- fv15tot %>% filter(str_detect(fv15tot$area, "OPSTILLINGSKREDS")) %>%
  mutate(area = str_to_lower(str_replace_all(area, "( OPSTILLINGSKREDS)|(\\d+[.] )", "")))

districtDK("pertenthousand", "area", data = fv15tot)

#################################################
#################################################


library(leafletDK)
library(dplyr)
library(tidyr)
library(stringr)

fv15tot <- read.csv2("http://api.statbank.dk/v1/data/FV15TOT/CSV?VALRES=VAELG%2C13&OMR%C3%85DE=*",
                     stringsAsFactors = F)

fv15tot <- fv15tot %>% spread(VALRES, INDHOLD) %>% select(-TID)
names(fv15tot) <- c("area", "writing", "voters")
fv15tot <- fv15tot %>% mutate(pertenthousand = writing/(voters/10000))
fv15tot <- fv15tot %>% filter(str_detect(fv15tot$area, "STORKREDS")) %>%
  mutate(area = str_to_lower(str_replace_all(area, "S STORKREDS", "")))

constituencyDK("pertenthousand", "area", data = fv15tot)

#################################################
#################################################

library(leafletDK)
library(dplyr)
library(tidyr)
library(stringr)

ejen4 <- read.csv2("http://api.statbank.dk/v1/data/EJEN4/CSV?OMR%C3%85DE=*&EJENDOMSKATE=2103&BN%C3%98GLE=2&OVERDRAG=1&Tid=2001",
                     stringsAsFactors = F)

ejen4 <- ejen4 %>% select(OMRÅDE, TID, INDHOLD)
ejen4 <- ejen4[str_detect(ejen4$OMRÅDE,"[A-ZÆØÅa-zæøå]"),]
ejen4$zip <- unlist(str_extract_all(ejen4$OMRÅDE, "\\d{4}"))
ejen4$zip <- ifelse(as.numeric(ejen4$zip) < 2400, round(as.numeric(ejen4$zip)/100)*100, as.numeric(ejen4$zip))
ejen4 <- ejen4 %>% group_by(zip) %>% summarise(indhold = sum(INDHOLD))
ejen4 <- as.data.frame(ejen4)

zipDK("indhold", "zip", data = ejen4)

##############

library(leafletDK)

folk1 <- read.csv2("http://api.statbank.dk/v1/data/folk1/CSV?OMR%C3%85DE=*",
                     stringsAsFactors = F)

regionalDK("INDHOLD", "OMRÅDE", data = folk1)

##############

library(leafletDK)
library(tidyr)

km1 <- read.csv2("http://api.statbank.dk/v1/data/KM1/CSV?SOGN=*&FKMED=*",
                   stringsAsFactors = F)
km1 <- km1 %>% spread(FKMED, INDHOLD)
km1$pct <- round(km1[,4]/(km1[,3]+km1[,4])*100,1)

parishDK("pct", "SOGN", data = km1)

##############

library(leafletDK)
library(stringr)

aus08 <- read.csv2("http://api.statbank.dk/v1/data/AUS08/CSV?OMR%C3%85DE=01%2C02%2C03%2C04%2C05%2C06%2C07%2C08%2C09%2C10%2C11&SAESONFAK=9",
                 stringsAsFactors = F)
aus08$OMRÅDE <- str_replace_all(aus08$OMRÅDE, "Landsdel ", "")

ruralDK("INDHOLD", "OMRÅDE", data = aus08)

value = "pct"
id = "SOGN"
mapdata = km1

###

library(rgeos)
library(maptools)
district2 <- thinnedSpatialPoly(district, tolerance = 1)



##############
## Make a table of species occurances for each species seen at each UC Natural Reserve

library(rgbif)
library(rgeos)
library(dplyr)

##############
## Started with Blue Oak, but the WKT seems broken. Try Angelo

angelo.address <- "https://raw.githubusercontent.com/BNHM/spatial-layers/master/wkt/AngeloCoastRangeReserve.wkt"

angelo.wkt <- readChar(angelo.address, nchars= 10e6) # Don't have a good way to get the number of characters beforehandm so I just gave it an arbitrarily big number

occ_search(geometry = angelo.wkt, geom_big = "axe") # WKT format isn't recognized because "MultiPolygon (((" should be "MULTIPOLYGON(((" 

# Reformat the WKT! 
angelo.wkt <- paste("MULTIPOLYGON", substring(angelo.wkt, 14, nchar(angelo.wkt)), sep="")

records <- occ_search(geometry = angelo.wkt, geom_big = "axe", limit = 199999) # limited to the max number of records using this API call

## rGBIF splits them into a list of 4 datasets. Let's glue those together to one large dataset, and cue maniacal laugher.

# Reduce the datasize to only the same columns from each dataset
geom1 <- select(records$geom1$data, scientificName, decimalLatitude, decimalLongitude, basisOfRecord, phylum, order, class, family, eventDate, stateProvince, gbifID, datasetName, institutionID)
geom2 <- select(records$geom2$data, scientificName, decimalLatitude, decimalLongitude, basisOfRecord, phylum, order, class, family, eventDate, stateProvince, gbifID, datasetName, institutionID) # No records for some reason
geom3 <- select(records$geom3$data, scientificName, decimalLatitude, decimalLongitude, basisOfRecord, phylum, order, class, family, eventDate, stateProvince, gbifID, datasetName, institutionID)
geom4 <- select(records$geom4$data, scientificName, decimalLatitude, decimalLongitude, basisOfRecord, phylum, order, class, family, eventDate, stateProvince, gbifID, datasetName, institutionID) # No records for some reason

# Combine them all together
angelo.occurrences <- rbind(geom1, geom3)

## Filter out any ocurrences that aren't in California. As of October over 55k are from Europe, Pennsylvania, etc. As of December, most are from California, but some are NA.

angelo.occurrences <- filter(angelo.occurrences, stateProvince == "California")

# Find the most recent occurences of each taxon
max.dates <- angelo.occurrences %>% group_by(scientificName) %>% summarize(maxdatetime = max(eventDate))



# Find the entire observation of the most recent occurrence of a taxon
most.recent.occurrences <- NULL

for (i in 1:nrow(max.dates)) { 
  spp.i <- filter(angelo.occurrences, scientificName == paste(max.dates[i,1]) & eventDate == paste(max.dates[i,2]))
  most.recent.occurrences <- rbind(most.recent.occurrences, spp.i)
}
    

# Print the reptiles!
reptiles <- filter(most.recent.occurrences, class == "Reptilia")


print(data.frame(ScientificName = reptiles$scientificName, BasisofRecord = reptiles$basisOfRecord, DatasetName = reptiles$datasetName, InstitutionCode = reptiles$institutionID, Link = paste("https://gbif.org/occurence/",reptiles$gbifID, sep="")))

      
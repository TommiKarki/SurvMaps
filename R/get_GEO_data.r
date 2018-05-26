#' Get GEO data from ECDC GIS server
#'
#' This function gets the geodata from ECDC GIS server into R SpatialpolygonsDataFrame or SpatialpointsDataFrame.
#' @param layer Select the layer for Hybrid Layer service: 0 for Points, 1 for Polygons.
#' @param STAT_LEVL Which data level to choose; see details. Defaults to 0 for EU/EEA. Give a single value or vector.
#' @param FIELDS Which data fields to retrieve from the hybrid layer; see details. Defaults to "GEO_ID".
#' Give a single value or character vector of relevant field names.
#' @param isValid isValid=1 removes the obsolete NUTS regions.
#' @details STAT_LEVL; 0 for EU/EEA and candidate country level, 10 for the rest of world country level. 
#' 1,2,3 = NUTS levels; 11,12 = GAUL levels; 21,22,23,24 = GADM levels. 
#' FIELDS; for additional fields to retrieve from the hybrid lay, see:
#' https://gis.ecdc.europa.eu/public/rest/services/UtilitiesServices/EMMA_GoMap_GC_HL_V1_3_WebMerc/MapServer/
#' @author Tommi Karki, with thanks to Silviu Ionescu.
#' @export
#' @examples
#' # get EU/EEA country polygons
#' plgs <- get_GEO_data(layer = 1)
#' 
#' # get EU/EEA country points
#' points <- get_GEO_data(layer = 0)
#' 
#' # use the geodata
#' plot(points, col = "red")
#' plot(plgs, add = TRUE)
#' 
#' # get all the world country polygons and some additional fields
#' plgs <- get_GEO_data(layer = 1, FIELDS = c("GEO_ID", "GEO_NAME", "CONTINENT"), STAT_LEVL=c(0,10))

get_GEO_data <- function(layer,
                              STAT_LEVL = c(0),
                              FIELDS = "GEO_ID",
                              isValid=1){

# ECDC GIS server url
url_ecdcGisSrv <- "https://gis.ecdc.europa.eu/1/query"
url_srvcLocation <- 
  "public/rest/services/UtilitiesServices/EMMA_GoMap_GC_HL_V1_3_WebMerc/MapServer"
url_layer <- layer
url_outFields <- paste(FIELDS, collapse= ",")
STAT_LEVL <- paste(STAT_LEVL, collapse = ",")
url_whereClause <- paste0("isValid=",isValid, " AND STAT_LEVL IN (", STAT_LEVL,")")

# The full url with the layer and where-clause
url_full <- modify_url(url_ecdcGisSrv, 
                       path = list(url_srvcLocation, url_layer, "query"), 
                       query = list(where = url_whereClause, outFields=url_outFields,
                                    f= "geojson"))
suppressWarnings(gjson_data <- geojson_sp(geo_pretty(as.geojson(Dump_From_GeoJson(url_full)))))

return(gjson_data)
}

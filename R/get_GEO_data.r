#' Get GEO data from ECDC GIS server
#'
#' This function gets the geodata from ECDC GIS server into R SpatialpolygonsDataFrame or SpatialpointsDataFrame. 
#' Gives a warning when closing the connection.
#' @param layer Select the layer for Hybrid Layer service: 0 for Points, 1 for Polygons.
#' @param STAT_LEVL Which values: 0,10=Country level(0 for EU/EEA and candidates, 10 for the rest of world); 
#' 1,2,3 = NUTS levels; 11,12 = GAUL levels; 21,22,23,24 = GADM levels. Defaults to 0 for EU/EEA.
#' @param FIELDS = Which "additional" field to retrieve from the hybrid layer, defaults to "GEO_ID". 
#' "GEO_NAME" also available.
#' @param isValid isValid=1 removes the obsolete NUTS regions.
#' @author Tommi Karki; with thanks to Silviu Ionescu.
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

get_GEO_data <- function(layer,
                              STAT_LEVL = c(0),
                              FIELDS = "GEO_ID",
                              isValid=1){
require(httr)
require(geojsonio)
require(geojson)

# ECDC GIS server url
url_ecdcGisSrv <- "https://gis.ecdc.europa.eu/1/query"
url_srvcLocation <- 
  "public/rest/services/UtilitiesServices/EMMA_GoMap_GC_HL_V1_3_WebMerc/MapServer"
url_layer <- layer
url_outFields <- FIELDS
url_whereClause <- paste0("isValid=",isValid, " AND STAT_LEVL IN (", STAT_LEVL,")")

# The full url with the layer and where-clause
url_full <- modify_url(url_ecdcGisSrv, 
                       path = list(url_srvcLocation, url_layer, "query"), 
                       query = list(where = url_whereClause, outFields=url_outFields,
                                    f= "geojson"))
gjson_data <- geojson_sp(geo_pretty(as.geojson(Dump_From_GeoJson(url_full))))

return(gjson_data)
}

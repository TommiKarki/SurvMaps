#' Map your prepared data with SurvMapper2
#'
#' Later version of SurvMapper. Creates surveillance chloropleth maps for data prepared with PrepMap. Note that due to the use of grid for legend and the small inlets for non-visible 
#' countries, mapping is not superswift and elements appear one by one to the graph.
#' Currently uses 'Tahoma' font, but needs care with the registration of fonts with extrafont, and not perhaps ideal. Map function best used with get_GEO_data and PrepMap, i.e. geographical data predefined
#' in a certain way. Now allows adding points, but these need separate data and do not yet follow "size by" arguments.
#'
#' @param data Your spatial data that you want to map, prepared to work with ggplot2, currently only chloropleth available
#' @param fills Your column/variable(s) that your want to map. Preferably a factor in defined order. 
#' @param long Your longitude variable, defaults to 'long'
#' @param lat Your latitude variable, defaults to 'lat'
#' @param id Your spatial id variable, defaults to 'id'
#' @param GEO_ID Your spatial id variable (e.g. country code), defaults to 'GEO_ID'
#' @param bground Your variable with 1/0 to classify the grey background (0's included only in lightgrey), defaults to 'isEEA'
#' @param Legend_title Legend title(s). More than one if more than one fills. Use escape new line to split the legend to several rows.
#' @param col_scale Colour scale, use 'green', 'red', 'blue' or 'qualitative'. Note that the last category is always "No data" grey.
#' More than one if more than one fills.
#' @param fill_levels The order to map the levels in fills; only works with one fills variable.
#' @param reverse_colours Reverse the order of the colour scale. Note that the last category/ies are always "No data" and "Not included" grey (the latter can be omitted).
#' @param not_included Label for the background colour category for the legend, defaults to "Not included". Use NULL to omit the "Not included" category from the legend.
#' @param add_points Add point data on the map, defaults to FALSE
#' @param pointdata If adding the point data, give the points.
#' @param pointsize If adding points, give the point size
#' @param pointshape If adding points, give the symbol for points (pch)
#' @param cex_factor Cex factor for many things, changes sizes depending on the end resolution
#' @keywords map
#' @author Tommi Karki
#' @export
#' @examples
#' 
#' # load the included dummy data
#' load(system.file("extdata", "dummy_data.rds", package = "SurvMaps"))
#' # Get the EU/EEA and candidate country SpatialPolygonsDataframe, including a variable "isEEA"
#' plg_map <- get_GEO_data(layer = 1, STAT_LEVL = c(0), FIELDS = c("isEEA", "GEO_ID"))
#' 
#' # Prepare the data for SurvMapper with PrepMap
#' mymap <- PrepMap(data = dummy_data , geo = plg_map)
#' 
#' # The map is correctly aligned only for selected width/height, so you can plot into a predefined device
#' dev.new(width=11.8,height=8, noRStudioGD = TRUE)
#' 
#' # Simple chloropleth map
#' SurvMapper2(mymap, fills ="Dummy_status", Legend_title = "Testing this", col_scale = "red")
#' 
#' # Simple chloropleth map with other options
#' SurvMapper2(mymap, fills ="Dummy_status", Legend_title = "Testing this", col_scale = "red")
#'
#' # Chloropleth map with some additional options
#' SurvMapper2(mymap, fills ="Dummy_status", Legend_title = "Testing this", 
#'        fill_levels = c("Dummy4",
#'                        "Dummy3",
#'                        "Dummy2", 
#'                        "Dummy1",
#'                        "No data"),
#'        col_scale = "hotcold", reverse_colours = TRUE, not_included = NULL)
#'
#' # Note that you can map at once several columns, but all options are not yet available for this scenario - 
#' # e.g. level order is good to be predefined if plotting several columns. And depends on graphical device (e.g. recording)
#' SurvMapper(mymap, fills = c("Dummy_status", "Dummy2"), Legend_title = c("Testing this", "And also this"),
#'        col_scale = c("blue", "qualitative"))

SurvMapper2 <- function (data, fills, long = "long", lat = "lat", id = "id", 
          GEO_ID = "GEO_ID", bground = "isEEA", Legend_title, col_scale, 
          fill_levels = NULL, reverse_colours = FALSE, not_included = "Not included",
          add_points = FALSE, pointdata = NULL, pointsize = NULL, pointshape = "*",
          cex_factor = 1) 
{
  if(add_points == TRUE & is.null(pointdata)){
    stop("For adding points to the chloropleth map, please include geometries also for the points!")
  }
  
  windowsFonts(Tahoma = windowsFont("Tahoma"))
  require(EcdcColors)
  # for (i in fills) {
    fill <- fills
    Leg_title <- Legend_title
    colour_scale <- col_scale
    if (is.null(fill_levels)) {
      data[[fill]] <- factor(data[[fill]])
      pointdata[[fill]] <- factor(pointdata[[fill]])
    }
    else {
      data[[fill]] <- factor(data[[fill]], levels = fill_levels)
      pointdata[[fill]] <- factor(pointdata[[fill]], levels = fill_levels)
    }
    if (colour_scale == "green") {
      map_cols <- SurvColors(col_scale = "green", n = length(levels(data[[fill]])) - 
                               1)
      map_cols[length(map_cols) + 1] <- SurvColors("grey", 
                                                   grey_shade = "mediumlight")
    }
    else if (colour_scale == "blue") {
      map_cols <- SurvColors(col_scale = "blue", n = length(levels(data[[fill]])) - 
                               1)
      map_cols[length(map_cols) + 1] <- SurvColors("grey", 
                                                   grey_shade = "mediumlight")
    }
    else if (colour_scale == "red") {
      map_cols <- SurvColors(col_scale = "red", n = length(levels(data[[fill]])) - 
                               1)
      map_cols[length(map_cols) + 1] <- SurvColors("grey", 
                                                   grey_shade = "mediumlight")
    }
    else if (colour_scale == "qualitative") {
      map_cols <- SurvColors(col_scale = "qualitative", 
                             n = length(levels(data[[fill]]))-1)
      map_cols[length(map_cols) + 1] <- SurvColors("grey", 
                                                           grey_shade = "mediumlight")
    }
    else if (colour_scale == "hotcold") {
      map_cols <- SurvColors(col_scale = "hotcold", n = length(levels(data[[fill]]))- 
                               1)
      map_cols[length(map_cols) + 1] <- SurvColors("grey", 
                                                           grey_shade = "mediumlight")
    }
    else {
      map_cols <- map_cols
      map_cols[length(map_cols) + 1] <- SurvColors("grey", 
                                                           grey_shade = "mediumlight")
    }
    if (reverse_colours == TRUE) {
      map_cols[1:length(map_cols) - 1] <- rev(map_cols[1:length(map_cols) - 
                                                         1])
    }
    p1 <- ggplot(data = data, aes_string(x = long, y = lat, 
                                         fill = fill)) + geom_map(data = data, map = data, 
                                                                  aes_string(map_id = id), color = SurvColors("grey", 
                                                                                                              grey_shade = "dark"), size = 0.2) + theme_map() + 
      coord_map("azequalarea", xlim = c(-24, 44), ylim = c(34, 
                                                           70), orientation = c(52, 10, 0)) + theme(legend.position = "none") + 
      geom_map(data = data[data[[bground]] == 0, ], map = data[data[[bground]] == 
                                                                 0, ], aes_string(map_id = id), fill = SurvColors("grey", 
                                                                                                                  grey_shade = "light"), color = SurvColors("grey", 
                                                                                                                                                            grey_shade = "dark"), size = 0.2)
    if (length(levels(data[[fill]])) < 9) {
      p1 <- p1 + scale_fill_manual(values = map_cols[1:length(levels(data[[fill]]))])
    }else {
      stop("Too many categories for the map, please re-check and rescale!")
    }
    
    
    # This needs to somehow match the levels that exist for point with the levels for the polygons, in this case -
    # not completely working yet! Cutoff around 90 for rgb works nice though.
    if(add_points == TRUE){
      p1 <- p1 + geom_point(data=pointdata, aes_string(x="coords.x1", y="coords.x2", col = fills),size = 8*cex_factor, shape = pointshape)+
        scale_color_manual(values = unlist(lapply(map_cols[1:length(levels(data[[fill]]))], function(x) ifelse( mean(col2rgb(x)) >  100, "black", "white"))))
    }
    
    if(!is.null(not_included)){
    map_cols[length(map_cols) + 1] <- SurvColors("grey", 
                                                 grey_shade = "light")
    
    nfills <- c(levels(data[[fill]]), not_included)
    }else{
      nfills <- c(levels(data[[fill]]))
    }
    xpos <- 0.01
    xtextpos <- 0.056
    textcex <- 2*cex_factor
    lwd <- 1*cex_factor
    grid.newpage()
    v1 <- viewport(width = 1, height = 1)
    print(p1, vp = v1)
    grid.rect(width = 0.04, height = 0.025, x = xpos + 0.002, 
              y = 0.9-0.03, just = "left", gp = gpar(fill = map_cols[1], 
                                                col = SurvColors("grey", grey_shade = "dark"), 
                                                lwd = lwd))
    grid.rect(width = 0.04, height = 0.025, x = xpos + 0.002, 
              y = 0.865-0.03, just = "left", gp = gpar(fill = map_cols[2], 
                                                  col = SurvColors("grey", grey_shade = "dark"), 
                                                  lwd = lwd))
    grid.text(Leg_title, x = xpos + 0.002, y = 0.93-0.03, just = c("left", "bottom"), 
              vp = v1, gp = gpar(fontsize = 9, fontfamily = "Tahoma", 
                                 cex = textcex))
    grid.text(paste(nfills[1]), x = xtextpos, 
              y = 0.9-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                         fontfamily = "Tahoma", cex = textcex))
    grid.text(paste(nfills[2]), x = xtextpos, 
              y = 0.865-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                           fontfamily = "Tahoma", cex = textcex))
    if (length(nfills) >= 3) {
      grid.rect(width = 0.04, height = 0.025, x = xpos + 
                  0.002, y = 0.83-0.03, just = "left", gp = gpar(fill = map_cols[3], 
                                                            col = SurvColors("grey", grey_shade = "dark"), 
                                                            lwd = lwd))
      grid.text(paste(nfills[3]), x = xtextpos, 
                y = 0.83-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                            fontfamily = "Tahoma", cex = textcex))
    }
    if (length(nfills) >= 4) {
      grid.rect(width = 0.04, height = 0.025, x = xpos + 
                  0.002, y = 0.795-0.03, just = "left", gp = gpar(fill = map_cols[4], 
                                                             col = SurvColors("grey", grey_shade = "dark"), 
                                                             lwd = lwd))
      grid.text(paste(nfills[4]), x = xtextpos, 
                y = 0.795-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                             fontfamily = "Tahoma", cex = textcex))
    }
    if (length(nfills) >= 5) {
      grid.rect(width = 0.04, height = 0.025, x = xpos + 
                  0.002, y = 0.76-0.03, just = "left", gp = gpar(fill = map_cols[5], 
                                                            col = SurvColors("grey", grey_shade = "dark"), 
                                                            lwd = lwd))
      grid.text(paste(nfills[5]), x = xtextpos, 
                y = 0.76-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                            fontfamily = "Tahoma", cex = textcex))
    }
    if (length(nfills) >= 6) {
      grid.rect(width = 0.04, height = 0.025, x = xpos + 
                  0.002, y = 0.725-0.03, just = "left", gp = gpar(fill = map_cols[6], 
                                                             col = SurvColors("grey", grey_shade = "dark"), 
                                                             lwd = lwd))
      grid.text(paste(nfills[6]), x = xtextpos, 
                y = 0.725-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                             fontfamily = "Tahoma", cex = textcex))
    }
    if (length(nfills) >= 7) {
      grid.rect(width = 0.04, height = 0.025, x = xpos + 
                  0.002, y = 0.69-0.03, just = "left", gp = gpar(fill = map_cols[7], 
                                                            col = SurvColors("grey", grey_shade = "dark"), 
                                                            lwd = lwd))
      grid.text(paste(nfills[7]), x = xtextpos, 
                y = 0.69-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                            fontfamily = "Tahoma", cex = textcex))
    }
    if (length(nfills) >= 8) {
      grid.rect(width = 0.04, height = 0.025, x = xpos + 
                  0.002, y = 0.655-0.03, just = "left", gp = gpar(fill = map_cols[8], 
                                                             col = SurvColors("grey", grey_shade = "dark"), 
                                                             lwd = lwd))
      grid.text(paste(nfills[8]), x = xtextpos, 
                y = 0.655-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                             fontfamily = "Tahoma", cex = textcex))
    }
    
    if (length(nfills) >= 9) {
      grid.rect(width = 0.04, height = 0.025, x = xpos + 
                  0.002, y = 0.620-0.03, just = "left", gp = gpar(fill = map_cols[9], 
                                                             col = SurvColors("grey", grey_shade = "dark"), 
                                                             lwd = lwd))
      grid.text(paste(nfills[9]), x = xtextpos, 
                y = 0.620-0.03, just = "left", vp = v1, gp = gpar(fontsize = 9, 
                                                             fontfamily = "Tahoma", cex = textcex))
    }
    
    grid.rect(width = 0.04, height = 0.025, x = xpos + 0.002, 
              y = 0.55-0.03, just = "left", gp = gpar(fill = map_cols[levels(data[[fill]]) == 
                                                                   unique(data[[fill]][data[[GEO_ID]] == "LU"])], 
                                                 col = SurvColors("grey", grey_shade = "dark"), 
                                                 lwd = lwd))
    if(add_points == TRUE){
    grid.text("*", x = xtextpos-0.03, y = 0.55-0.0375, just = "left", 
              vp = v1, gp = gpar(fontsize = 12, fontfamily = "sans",
                                 cex = textcex, col = 
                                   ifelse(mean(col2rgb(map_cols[levels(data[[fill]]) == 
                                                                                     unique(data[[fill]][data[[GEO_ID]] == "LU"])])) >  100,
                                          "black", "white")))
    }
    grid.text("Luxembourg", x = xtextpos, y = 0.55-0.03, just = "left", 
              vp = v1, gp = gpar(fontsize = 9, fontfamily = "Tahoma", 
                                 cex = textcex))
    grid.rect(width = 0.04, height = 0.025, x = xpos + 0.002, 
              y = 0.515-0.03, just = "left", gp = gpar(fill = map_cols[levels(data[[fill]]) == 
                                                                    unique(data[[fill]][data[[GEO_ID]] == "MT"])], 
                                                  col = SurvColors("grey", grey_shade = "dark"), 
                                                  lwd = lwd))
    
    if(add_points == TRUE){
      grid.text("*", x = xtextpos-0.03, y = 0.515-0.0375, just = "left", 
                vp = v1, gp = gpar(fontsize = 12, fontfamily = "sans",
                                   cex = textcex, col = 
                                     ifelse(mean(col2rgb(map_cols[levels(data[[fill]]) == 
                                                                    unique(data[[fill]][data[[GEO_ID]] == "MT"])])) >  100,
                                            "black", "white")))
    }
    
    grid.text("Malta", x = xtextpos, y = 0.515-0.03, just = "left", 
              vp = v1, gp = gpar(fontsize = 9, fontfamily = "Tahoma", 
                                 cex = textcex))
  # }
}

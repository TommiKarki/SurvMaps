#' Map your prepared data with SurvMapper
#'
#' Creates surveillance chloropleth maps for data prepared with PrepMap. Note that due to the use of grid for legend and the small inlets for non-visible 
#' countries, mapping is not superswift and elements appear one by one to the graph. Also, the alignment of the legend, 
#' as well as fontsize depends on the width x height. Current ideal dimensions approximately 1000x680.
#' Currently uses 'Arial' font, to be updated to 'Tahoma'.
#'
#' @param data Your spatial data that you want to map, prepared to work with ggplot2, currently only chloropleth available
#' @param fills Your column/variable(s) that your want to map. Preferably a factor in defined order. 
#' @param long Your longitude variable, defaults to 'long'
#' @param lat Your latitude variable, defaults to 'lat'
#' @param id Your spatial id variable, defaults to 'id'
#' @param GEO_ID Your spatial id variable (e.g. country code), defaults to 'GEO_ID'
#' @param bground Your variable with 1/0 to classify the grey background (0's included only in lightgrey), defaults to 'isEEA'
#' @param Legend_title Legend title(s). More than one if more than one fills.
#' @param col_scale Colour scale, use 'green', 'red', 'blue' or 'qualitative'. Note that the last category is always "No data" grey.
#' More than one if more than one fills.
#' @param fill_levels The order to map the levels in fills; only works with one fills variable.
#' @param reverse_colours Reverse the order of the colour scale. Note that the last category is always "No data" grey.
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
#' SurvMapper(mymap, fills ="Dummy_status", Legend_title = "Testing this", col_scale = "red")
#'
#' # Chloropleth map with some additional options
#' SurvMapper(mymap, fills ="Dummy_status", Legend_title = "Testing this", 
#'        fill_levels = c("Dummy4",
#'                        "Dummy3",
#'                        "Dummy2", 
#'                        "Dummy1",
#'                        "No data"),
#'        col_scale = "red", reverse_colours = TRUE)
#'
#' # Note that you can map at once several columns, but all options are not yet available for this scenario - 
#' # e.g. level order is good to be predefined if plotting several columns. And depends on graphical device (e.g. recording)
#' SurvMapper(mymap, fills = c("Dummy_status", "Dummy2"), Legend_title = c("Testing this", "And also this"),
#'        col_scale = c("blue", "qualitative"))
SurvMapper <- function(data, fills, long = long, lat = lat, id = id, GEO_ID = GEO_ID, bground = isEEA,
                    Legend_title, col_scale,
                    fill_levels = NULL, reverse_colours=FALSE){
  bground <- deparse(substitute(bground))
  GEO_ID <- deparse(substitute(GEO_ID))
  windowsFonts(Arial = windowsFont("TT Arial"))
  require(SurvColors)
  for(i in fills){
    fill <- i
    Leg_title <- Legend_title[fills==i]
    colour_scale <- col_scale[fills==i]
    
    if(is.null(fill_levels)){
      data[[fill]] <- factor(data[[fill]])
    }else{
      data[[fill]] <- factor(data[[fill]], levels = fill_levels)}
    if(colour_scale=="green"){
      map_cols <- SurvColors(col_scale = "green", n=length(levels(data[[fill]]))-1)
      map_cols[length(map_cols)+1] <- SurvColors("grey", grey_shade = "mediumlight")
    }else if(colour_scale=="blue"){
      map_cols <- SurvColors(col_scale = "blue", n=length(levels(data[[fill]]))-1)
      map_cols[length(map_cols)+1] <- SurvColors("grey", grey_shade = "mediumlight")
    }else if(colour_scale=="red"){
      map_cols <- SurvColors(col_scale = "red", n=length(levels(data[[fill]]))-1)
      map_cols[length(map_cols)+1] <- SurvColors("grey", grey_shade = "mediumlight")
    }else if(colour_scale=="qualitative"){
      map_cols <- SurvColors(col_scale = "qualitative", n=length(levels(data[[fill]])))
      map_cols[length(levels(data[[fill]]))] <- SurvColors("grey", grey_shade = "mediumlight")
    }else if(colour_scale=="hotcold"){
      map_cols <- SurvColors(col_scale = "hotcold", n=length(levels(data[[fill]])), hot_cols = 3)
      map_cols[length(levels(data[[fill]]))] <- SurvColors("grey", grey_shade = "mediumlight")
    }else{
      map_cols <- map_cols
      map_cols[length(levels(data[[fill]]))] <- SurvColors("grey", grey_shade = "mediumlight")
    }
    
    if(reverse_colours==TRUE){
      map_cols[1:length(map_cols)-1] <- rev(map_cols[1:length(map_cols)-1])
    }
    # map_cols2 <- map_cols
    
    # The main map without any legend (added using grid below - 
    # unfortunately a bit manual but allows more control)
    p1 <- ggplot(data = data, aes_string(x = "long", y = "lat", fill = fill)) +
      geom_map(data = data, map = data,
               aes_string(map_id = "id"),
               color = SurvColors("grey", grey_shade="dark"), size = 0.2) +
      theme_map() +
      coord_map("azequalarea", xlim = c(-24, 44),ylim = c(34, 70),
                orientation = c(52,10,0)) +
      theme(legend.position="none") +
      geom_map(data = data[data[[bground]]==0,], map = data[data[[bground]]==0,],
               aes_string(map_id = "id"),
               fill = SurvColors("grey", grey_shade="light"),
               color = SurvColors("grey", grey_shade="dark"), size = 0.2) 
    if(length(levels(data[[fill]])) == 2){
      p1 <- p1 + scale_fill_manual(values = map_cols[1:2])
    }else if(length(levels(data[[fill]])) == 3){
      p1 <- p1 + scale_fill_manual(values = map_cols[1:3])
    }else if(length(levels(data[[fill]])) == 4){
      p1 <- p1 + scale_fill_manual(values = map_cols[1:4])
    }else if(length(levels(data[[fill]])) == 5){
      p1 <- p1 + scale_fill_manual(values = map_cols[1:5])
    }else if(length(levels(data[[fill]])) == 6){
      p1 <- p1 + scale_fill_manual(values = map_cols[1:6])
    }else if (length(levels(data[[fill]])) == 7){
      p1 <- p1 + scale_fill_manual(values = map_cols[1:7])
    }else{
      p1 <- p1 + scale_fill_manual(values = map_cols[1:8])
    }
    
    # printing the map and the legend together, and adding small rectangles for non-visible countries
    xpos <- 0.01
    xtextpos <- 0.056
    textcex <- 1.5
    grid.newpage()
    v1 <- viewport(width = 1, height = 1) #plot area for the main map
    # v2 <- viewport(width = 0.057, x = xpos, y = 0.26, just = "left") #plot area for the inset map LU
    # v3 <- viewport(width = 0.057, x = xpos, y = 0.14, just = "left") #plot area for the inset map MT
    print(p1,vp=v1)
    
    grid.rect(width = 0.04, height = 0.025, 
              x = xpos+0.002, y=0.9, just = "left", 
              gp = gpar(fill=map_cols[1],
                        col = SurvColors("grey", grey_shade="dark"),
                        lwd = 0.2))
    
    grid.rect(width = 0.04, height = 0.025, 
              x = xpos+0.002, y=0.865, just = "left", 
              gp = gpar(fill=map_cols[2],
                        col = SurvColors("grey", grey_shade="dark"),
                        lwd = 0.2))
    
    # Legend title
    grid.text(Leg_title, x=xpos+0.002, y=0.93, just = "left", vp = v1, 
              gp = gpar(fontsize = 9, fontfamily = "Arial",
                        cex = textcex))
    
    # Legend text for each key
    grid.text(paste(levels(data[[fill]])[1]), x=xtextpos, y=0.9, just = "left", vp = v1, 
              gp = gpar(fontsize = 9, fontfamily = "Arial",
                        cex = textcex))
    
    grid.text(paste(levels(data[[fill]])[2]), x=xtextpos, y=0.865, just = "left", vp = v1, 
              gp = gpar(fontsize = 9, fontfamily = "Arial",
                        cex = textcex))
    
    
    if(length(levels(data[[fill]])) >= 3){
      
      grid.rect(width = 0.04, height = 0.025, 
                x = xpos+0.002, y=0.83, just = "left", 
                gp = gpar(fill=map_cols[3],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[3]), x=xtextpos, y=0.83, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
      
    }
    if(length(levels(data[[fill]])) >= 4){
      
      grid.rect(width = 0.04, height = 0.025, 
                x = xpos+0.002, y=0.795, just = "left", 
                gp = gpar(fill=map_cols[4],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[4]), x=xtextpos, y=0.795, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
    }
    if(length(levels(data[[fill]])) >= 5){
      
      grid.rect(width = 0.04, height = 0.025, 
                x = xpos+0.002, y=0.76, just = "left", 
                gp = gpar(fill=map_cols[5],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      
      grid.text(paste(levels(data[[fill]])[5]), x=xtextpos, y=0.76, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
      
    }
    if(length(levels(data[[fill]])) >= 6){
      
      grid.rect(width = 0.04, height = 0.025, 
                x = xpos+0.002, y=0.725, just = "left", 
                gp = gpar(fill=map_cols[6],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[6]), x=xtextpos, y=0.725, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
    }
    if (length(levels(data[[fill]])) >= 7){
      
      grid.rect(width = 0.04, height = 0.025, 
                x = xpos+0.002, y=0.69, just = "left", 
                gp = gpar(fill=map_cols[7],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[7]), x=xtextpos, y=0.69, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
    }
    if(length(levels(data[[fill]])) >= 8){
      
      grid.rect(width = 0.04, height = 0.025, 
                x = xpos+0.002, y=0.655, just = "left", 
                gp = gpar(fill=map_cols[8],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[8]), x=xtextpos, y=0.655, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
    }
    
    # Add the small inset rectangles for LU and MT
  
    grid.rect(width = 0.04, height = 0.025, 
              x = xpos+0.002, y=0.55, just = "left", 
              gp = gpar(fill=map_cols[levels(data[[fill]]) == unique(data[[fill]][data[[GEO_ID]]=="LU"])],
                        col = SurvColors("grey", grey_shade="dark"),
                        lwd = 0.2))
    grid.text("Luxembourg", x=xtextpos, y=0.55, just = "left", vp = v1, gp = gpar(fontsize = 9,
                                                                                  fontfamily = "Arial",
                                                                                  cex = textcex))
   
    grid.rect(width = 0.04, height = 0.025, 
              x = xpos+0.002, y=0.515, just = "left", 
              gp = gpar(fill=map_cols[levels(data[[fill]]) == unique(data[[fill]][data[[GEO_ID]]=="MT"])],
                        col = SurvColors("grey", grey_shade="dark"),
                        lwd = 0.2))
    grid.text("Malta", x=xtextpos, y=0.515, just = "left", vp = v1, gp = gpar(fontsize = 9,
                                                                             fontfamily = "Arial",
                                                                             cex = textcex))
    
    
  }
}

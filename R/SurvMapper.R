#' Map your prepared data with SurvMapper
#'
#' Creates surveillance chloropleth maps. Note that due to the use of grid for legend and the small inlets for non-visible 
#' countries, mapping is not superswift and elements appear one by one to the graph. Also, the alignment of the legend, 
#' as well as fontsize depends on the width x height. Current ideal dimensions approximately 1000x680.
#'
#' @param data Your spatial data that you want to map, currently only chloropleth available
#' @param fills Your column/variable(s) that your want to map. Preferably a factor in defined order. 
#' @param long Your longitude variable, defaults to 'long'
#' @param lat Your latitude variable, defaults to 'lat'
#' @param id Your id variable, defaults to 'id'
#' @param isEEA Your isEEA variable, defaults to 'isEEA'
#' @param CNTR_ID Your CNTR_ID variable, defaults to 'CNTR_ID'
#' @param Legend_titles Legend title(s). More than one if more than one fills.
#' @param col_scale Colour scale, use 'green', 'red', 'blue' or 'qualitative'. Note that the last category is always "No data" grey.
#' More than one if more than one fills.
#' @param fill_levels The order to map the levels in fills; only works with one fills variable.
#' @param reverse_colours Reverse the order of the colour scale. Note that the last category is always "No data" grey.
#' @keywords map
#' @export
#' @examples
#' 
#' # load the included dummy data
#' load(system.file("extdata", "dummy_data.rds", package = "SurvMaps"))
#' # load the included EU/EEA SpatialPolygonsDataframe (includes Asia and Africa for background)
#' load(system.file("extdata", "EU_AFR_AS_plgs.rds", package = "SurvMaps"))
#' 
#' # Prepare the data for SurvMapper with PrepMap
#' mymap <- PrepMap(data = dummy_data , geo = plg_map)
#' 
#' # The map is correctly aligned only for selected width/height, so you can plot into a predefined device
#' dev.new(width=11.8,height=8, noRStudioGD = TRUE)
#' 
#' # Simple chloropleth map
#' SurvMapper(mymap, fills ="Dummy_status", Legend_titles = "Testing this", col_scale = "red")
#'
#' # Chloropleth map with some additional options
#' SurvMapper(mymap, fills ="Dummy_status", Legend_titles = "Testing this", 
#'        fill_levels = c("Optimal",
#'                        "Good",
#'                        "Poor", 
#'                        "Very poor",
#'                        "No data"),
#'        col_scale = "red", reverse_colours = TRUE)
#'
#' # Note that you can map at once several columns, but all options are not yet available for this scenario - 
#' # e.g. level order is good to be predefined if plotting several columns. And depends on graphical device (e.g. recording)
#' SurvMapper(mymap, fills = c("Dummy_status", "Dummy2"), Legend_titles = c("Testing this", "And also this"),
#'        col_scale = c("blue", "qualitative"))
SurvMapper <- function(data, fills, long = long, lat = lat, id = id, isEEA = isEEA, CNTR_ID = CNTR_ID,
                    Legend_titles, col_scale,
                    fill_levels = NULL, reverse_colours=FALSE){
  
  windowsFonts(Arial = windowsFont("TT Arial"))
  require(SurvColors)
  for(i in fills){
    fill <- i
    Legend_title <- Legend_titles[fills==i]
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
    }else{
      stop("Specify the colours as 'qualitative', 'green', 'blue' or 'red'!")}
    
    if(reverse_colours==TRUE){
      map_cols[1:length(map_cols)-1] <- rev(map_cols[1:length(map_cols)-1])
    }
    
    
    # The main map without any legend (added using grid below - unfortunately a bit manual but allows more control)
    print("Creating EU/EEA map!")
    p1 <- ggplot(data = data, aes_string(x = "long", y = "lat", fill = fill)) +
      geom_map(data = data, map = data,
               aes_string(map_id = "id"),
               color = SurvColors("grey", grey_shade="dark"), size = 0.2) +
      theme_map() +
      coord_map("azequalarea", xlim = c(-24, 44),ylim = c(34, 70),
                orientation = c(52,10,0)) +
      theme(legend.position="none") +
      geom_map(data = data[data[["isEEA"]]==0,], map = data[data[["isEEA"]]==0,],
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
    
    print("Creating Luxembourg inset map!")
    mymap_lu_mt <- data[data[["isEEA"]]==1,]
    # mymap_lu_mt <- data[data[["CNTR_ID"]]%in%c("LU", "MT", "FR", "BE", "DE"),]
    mp_lu <- ggplot(data = mymap_lu_mt, aes_string(x = "long", y = "lat", fill = fill)) +
      geom_map(data = mymap_lu_mt, map = mymap_lu_mt,
               aes_string(map_id = "id"),
               color = SurvColors("grey", grey_shade="dark"), size = 0.5) +
      theme_map() +
      coord_map("azequalarea", xlim = c(5.59,6.61),ylim = c(49.3, 50.35)) +
      labs(x=NULL, y=NULL) +
      theme(legend.position = "none", plot.margin=unit(c(0,0,0,0),"mm"),
            panel.border = element_rect(fill = NA, colour = SurvColors("grey", grey_shade="dark"), 
                                        size = 0.5))
    if(length(levels(data[[fill]])) == 2){
      mp_lu <- mp_lu + scale_fill_manual(values = map_cols[1:2])
    }else if(length(levels(data[[fill]])) == 3){
      mp_lu <- mp_lu + scale_fill_manual(values = map_cols[1:3])
    }else if(length(levels(data[[fill]])) == 4){
      mp_lu <- mp_lu + scale_fill_manual(values = map_cols[1:4])
    }else if(length(levels(data[[fill]])) == 5){
      mp_lu <- mp_lu + scale_fill_manual(values = map_cols[1:5])
    }else if(length(levels(data[[fill]])) == 6){
      mp_lu <- mp_lu + scale_fill_manual(values = map_cols[1:6])
    }else if (length(levels(data[[fill]])) == 7){
      mp_lu <- mp_lu + scale_fill_manual(values = map_cols[1:7])
    }else{
      mp_lu <- mp_lu + scale_fill_manual(values = map_cols[1:8])
    }
    
    
    
    
    
    print("Creating Malta inset map!")
    # Malta inset map
    mp_mt <- ggplot(data = mymap_lu_mt, aes_string(x = "long", y = "lat", fill = fill)) +
      geom_map(data = mymap_lu_mt, map = mymap_lu_mt,
               aes_string(map_id = "id"),
               color = SurvColors("grey", grey_shade="dark"), size = 0.5) +
      theme_map() +
      coord_map("azequalarea",xlim = c(14.1175,14.6375),ylim = c(35.58, 36.32)) +
      labs(x=NULL, y=NULL) +
      theme(legend.position = "none", plot.margin=unit(c(0,0,0,0),"mm"),
            panel.border = element_rect(fill = NA, colour = SurvColors("grey", grey_shade="dark"), 
                                        size = 0.5))
    if(length(levels(data[[fill]])) == 2){
      mp_mt <- mp_mt + scale_fill_manual(values = map_cols[1:2])
    }else if(length(levels(data[[fill]])) == 3){
      mp_mt <- mp_mt + scale_fill_manual(values = map_cols[1:3])
    }else if(length(levels(data[[fill]])) == 4){
      mp_mt <- mp_mt + scale_fill_manual(values = map_cols[1:4])
    }else if(length(levels(data[[fill]])) == 5){
      mp_mt <- mp_mt + scale_fill_manual(values = map_cols[1:5])
    }else if(length(levels(data[[fill]])) == 6){
      mp_mt <- mp_mt + scale_fill_manual(values = map_cols[1:6])
    }else if (length(levels(data[[fill]])) == 7){
      mp_mt <- mp_mt + scale_fill_manual(values = map_cols[1:7])
    }else{
      mp_mt <- mp_mt + scale_fill_manual(values = map_cols[1:8])
    }
    
    
    print("printing the maps together!")
    xpos <- 0.01
    xtextpos <- 0.071
    textcex <- 1.5
    grid.newpage()
    v1 <- viewport(width = 1, height = 1) #plot area for the main map
    v2 <- viewport(width = 0.057, x = xpos, y = 0.26, just = "left") #plot area for the inset map LU
    v3 <- viewport(width = 0.057, x = xpos, y = 0.14, just = "left") #plot area for the inset map MT
    print(p1,vp=v1)
    
    grid.rect(width = 0.055, height = 0.025, 
              x = xpos+0.002, y=0.9, just = "left", 
              gp = gpar(fill=map_cols[1],
                        col = SurvColors("grey", grey_shade="dark"),
                        lwd = 0.2))
    
    grid.rect(width = 0.055, height = 0.025, 
              x = xpos+0.002, y=0.865, just = "left", 
              gp = gpar(fill=map_cols[2],
                        col = SurvColors("grey", grey_shade="dark"),
                        lwd = 0.2))
    
    # Legend title
    grid.text(Legend_title, x=xpos+0.002, y=0.93, just = "left", vp = v1, 
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
      
      grid.rect(width = 0.055, height = 0.025, 
                x = xpos+0.002, y=0.83, just = "left", 
                gp = gpar(fill=map_cols[3],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[3]), x=xtextpos, y=0.83, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
      
    }
    if(length(levels(data[[fill]])) >= 4){
      
      grid.rect(width = 0.055, height = 0.025, 
                x = xpos+0.002, y=0.795, just = "left", 
                gp = gpar(fill=map_cols[4],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[4]), x=xtextpos, y=0.795, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
    }
    if(length(levels(data[[fill]])) >= 5){
      
      grid.rect(width = 0.055, height = 0.025, 
                x = xpos+0.002, y=0.76, just = "left", 
                gp = gpar(fill=map_cols[5],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      
      grid.text(paste(levels(data[[fill]])[5]), x=xtextpos, y=0.76, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
      
    }
    if(length(levels(data[[fill]])) >= 6){
      
      grid.rect(width = 0.055, height = 0.025, 
                x = xpos+0.002, y=0.725, just = "left", 
                gp = gpar(fill=map_cols[6],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[6]), x=xtextpos, y=0.725, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
    }
    if (length(levels(data[[fill]])) >= 7){
      
      grid.rect(width = 0.055, height = 0.025, 
                x = xpos+0.002, y=0.69, just = "left", 
                gp = gpar(fill=map_cols[7],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[7]), x=xtextpos, y=0.69, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
      
    }
    if(length(levels(data[[fill]])) >= 8){
      
      grid.rect(width = 0.055, height = 0.025, 
                x = xpos+0.002, y=0.655, just = "left", 
                gp = gpar(fill=map_cols[8],
                          col = SurvColors("grey", grey_shade="dark"),
                          lwd = 0.2))
      
      grid.text(paste(levels(data[[fill]])[8]), x=xtextpos, y=0.655, just = "left", vp = v1, 
                gp = gpar(fontsize = 9, fontfamily = "Arial",
                          cex = textcex))
    }
    
    # Add the small inset maps for LU and MT
    print(mp_lu,vp=v2)
    print(mp_mt,vp=v3)
    grid.text("Luxembourg", x=xtextpos, y=0.26, just = "left", vp = v1, gp = gpar(fontsize = 9,
                                                                                  fontfamily = "Arial",
                                                                                  cex = textcex))
    grid.text("Malta", x=xtextpos, y=0.14, just = "left", vp = v1, gp = gpar(fontsize = 9,
                                                                             fontfamily = "Arial",
                                                                             cex = textcex))
    
    
  }
}

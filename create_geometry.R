
####################################################################################################
####################################################################################################
#####                                                                                          #####
#####    Function to read in data from openstreetmap.org and create a bLSmodelR source file    #####
#####                                                                                          #####
####################################################################################################
####################################################################################################

library(bLSmodelR)
library(osmdata)


create_polygons <- function(left,bottom,right,top,feature='building',transform=TRUE){

assign("has_internet_via_proxy", TRUE, environment(curl::has_internet)) # otherwise for unknow reason it does not work.

## extract data according to coordinates
bbox <- opq(c(left,bottom,right,top))
osm_feature <- add_osm_feature(bbox,key = feature)
data_raw <- osmdata_sf(osm_feature)

# Extract polygon data
geometry_polygons <- data_raw$osm_polygons
## transform to Danish coordinate system
if(transform){geometry_polygons$geometry <- st_transform(geometry_polygons$geometry, crs = "+proj=utm +zone=33 +datum=WGS84")}
## extract coordinates
coords <- as.data.table(st_coordinates(geometry_polygons$geometry))
osm_id <- data.table(osmID=geometry_polygons$osm_id,L2=coords[,unique(L2)])
coords[,L2 <- as.character(L2)]

coord_dt <- merge(coords,osm_id,by='L2')

## make bLSmodelR source object
Sources <- genSources(as.data.frame(cbind(coord_dt[,.(osmID,X,Y)],1)))
}


create_points <- function(dt){
Coord_sf <- st_as_sf(dt, coords = c("V2", "V3"), crs = 4326)
Coord_DK <- st_transform(Coord_sf, crs = "+proj=utm +zone=33 +datum=WGS84")
Sampling <- data.table(cbind(Coord_DK$V1,as.data.table(matrix(unlist(Coord_DK$geometry),ncol=2,byrow=TRUE))))
setnames(Sampling,c('Name','x','y'))
}


# drew a grid with wind direction sectors over a plot
draw_grid <- function(point, delta = NULL, n_lines = NULL, line_length = 200, zero = 0, col = 'grey', lwd = NULL) {
  if (is.null(delta) & is.null(n_lines)) stop('Specify either "delta" or "n_lines".')
  if (!is.null(delta) & !is.null(n_lines)) {
    if (delta != 360 / n_lines * 2) stop('Only specify delta or n_lines, but not both.')
  }
  if (is.null(delta)) {
    delta <- 360 / n_lines / 2
  } else {
    n_lines <- 360 / delta / 2
  }
  if (!((360 / delta) %% 1 == 0 && (360 / delta) > 0)) stop('360 is not a multiple of your delta value')

  # Ensure point is a data.frame
  if (is.list(point) && !is.data.frame(point)) {
    point <- data.frame(x = point$x, y = point$y)
  }

  # Generate angles
  angles <- seq(0, 360 - delta, by = delta)

  # Function to draw grid for a single point
  draw_single_grid <- function(x, y) {
    for (angle in angles) {
      angle_rad <- (angle - zero) * pi / 180
      x1 <- x + (line_length/2) * cos(angle_rad)
      x2 <- x - (line_length/2) * cos(angle_rad)
      y1 <- y + (line_length/2) * sin(angle_rad)
      y2 <- y - (line_length/2) * sin(angle_rad)
      segments(x0 = x1, y0 = y1, x1 = x2, y1 = y2, col = col, lwd = lwd)
    }
  }

  # Apply the function to all points
  mapply(draw_single_grid, point$x, point$y)
}



draw_line <- function(point = NULL, angle, length = 500, col = 'grey', lty = 2, x = NULL, y = NULL) {
  if (is.null(point)) {
    x_start <- x
    y_start <- y
  } else {
    x_start <- point$x
    y_start <- point$y
  }
  
  # Convert angle to radians
  angle_rad <- angle * pi / 180
  
  # Calculate end points
  x_end <- x_start + (length / sqrt(2)) * sin(angle_rad)
  y_end <- y_start + (length / sqrt(2)) * cos(angle_rad)
  
  # Create line segments
  segments(x0 = rep(x_start, each = length(angle)),
           y0 = rep(y_start, each = length(angle)),
           x1 = as.vector(x_end),
           y1 = as.vector(y_end),
           col = col,
           lty = lty)
}



# draw_grid <- function(point, delta=NULL, n_lines = NULL, line_length=200, zero=0, col='grey',lwd=NULL) {
# 	# browser()
#   if(is.null(delta) & is.null(n_lines)){stop('Specify either "delta" or "n_lines".')}
#   if(!is.null(delta) & !is.null(n_lines)){if(delta != 360/n_lines*2){stop('Only specify delta or n_lines, but not both.')}}
#   if(is.null(delta)){delta <- 360/n_lines/2} else {n_lines <- 360/delta/2}
#   if(!((360 / delta) %% 1 == 0 && (360 / delta) > 0)){stop('360 is not a multiple of your delta value')}
#   # Convert angle from degrees to radians
#   angle_rad <- delta * pi / 180
#   zero_rad <- zero * pi / 180

#   # Calculate coordinates for each line
# for(j in 1:length(point$x)){
# # Create a matrix to store line coordinates
#   lines_matrix <- matrix(NA, nrow = n_lines, ncol = 4)
#   for(i in 1:n_lines){
#     x1 <- point[j]$x + (line_length/2) * cos(angle_rad-zero_rad)
#     x2 <- point[j]$x - (line_length/2) * cos(angle_rad-zero_rad)
#     y1 <- point[j]$y + (line_length/2) * sin(angle_rad-zero_rad)
#     y2 <- point[j]$y - (line_length/2) * sin(angle_rad-zero_rad)
#     lines_matrix[i, ] <- c(x1,x2,y1,y2)
    
#     # Increment the angle for the next line
#     angle_rad <- angle_rad + (delta * pi / 180)
#   }
  
#   for(i in 1:n_lines){
#     lines(c(lines_matrix[i, 1], lines_matrix[i, 2]), c(lines_matrix[i, 3], lines_matrix[i, 4]),col=col,lwd=lwd)
#   }}
# }

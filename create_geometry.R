
####################################################################################################
####################################################################################################
#####                                                                                          #####
#####    Function to read in data from openstreetmap.org and create a bLSmodelR source file    #####
#####                                                                                          #####
####################################################################################################
####################################################################################################

library(bLSmodelR)
library(osmdata)
library(sf)


create_polygons <- function(bottom, left, top, right, feature = 'building', transform  = TRUE, rename = TRUE){
# browser()
  assign("has_internet_via_proxy", TRUE, environment(curl::has_internet)) # otherwise for unknow reason it does not work.

  ## extract data according to coordinates
  bbox <- opq(c(left, bottom, right, top))
  # browser()
  osm_feature <- add_osm_feature(bbox, key = feature)
  data_raw <- osmdata_sf(osm_feature)

  # Extract polygon data
  geometry_polygons <- data_raw$osm_polygons
  ## transform to Danish coordinate system
  if(transform){geometry_polygons$geometry <- st_transform(geometry_polygons$geometry, crs = "+proj=utm +zone=33 +datum=WGS84")}
  ## extract coordinates

  coords <- as.data.table(st_coordinates(geometry_polygons$geometry))
  osm_id <- data.table(osmID = geometry_polygons$osm_id, L2 = coords[, unique(L2)])
  coords[, L2 <- as.character(L2)]

  coord_dt <- merge(coords, osm_id, by = 'L2')

  ## make bLSmodelR source object
  if(rename){
    Sources <- genSources(as.data.frame(cbind(coord_dt[, .(L2, X, Y)], 1)))
    } else {
    Sources <- genSources(as.data.frame(cbind(coord_dt[, .(osmID, X, Y)], 1)))
    }
}

create_points <- function(dt){
  Coord_sf <- st_as_sf(dt, coords = c("V2", "V3"), crs = 4326)
  Coord_DK <- st_transform(Coord_sf, crs = "+proj=utm +zone=33 +datum=WGS84")
  Sampling <- data.table(cbind(Coord_DK$V1, as.data.table(matrix(unlist(Coord_DK$geometry), ncol = 2, byrow = TRUE))))
  setnames(Sampling, c('Name', 'x', 'y'))
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



draw_line <- function(point = NULL, angle, length = 500, col = 'grey', lty = 2, lwd = 1,
                      x = NULL, y = NULL, draw_arc = FALSE, arc_radius = 15, arc_col = col, reverse = FALSE) {
  if (is.null(point)) {
    x_start <- as.vector(x)
    y_start <- as.vector(y)
  } else {
    x_start <- as.vector(point$x)
    y_start <- as.vector(point$y)
  }
    
  # Convert angle to radians
  angle_rad <- (90 - angle) * pi / 180
  
  # Create all combinations of start points and angles
  combinations <- expand.grid(
    point_index = seq_along(x_start),
    angle_index = seq_along(angle_rad)
  )
  
  # Calculate end points
  x_end <- x_start[combinations$point_index] + length * cos(angle_rad[combinations$angle_index])
  y_end <- y_start[combinations$point_index] + length * sin(angle_rad[combinations$angle_index])
  
  # Create line segments
  segments(x0 = x_start[combinations$point_index],
           y0 = y_start[combinations$point_index],
           x1 = x_end,
           y1 = y_end,
           col = col,
           lty = lty,
           lwd = lwd)
  
  # Draw arcs if requested
  if (draw_arc && length(angle) > 1) {
    for (i in seq_along(x_start)) {
      start_angle <- angle_rad[1]
      end_angle <- angle_rad[2]
      
      # Determine the direction of the arc
      if (reverse) {
        if (end_angle > start_angle) {
          end_angle <- end_angle - 2*pi
        }
        theta <- seq(start_angle, end_angle, length.out = 100)
      } else {
        if (end_angle < start_angle) {
          end_angle <- end_angle + 2*pi
        }
        theta <- seq(start_angle, end_angle, length.out = 100)
      }
      
      # Draw the arc
      arc_x <- x_start[i] + arc_radius * cos(theta)
      arc_y <- y_start[i] + arc_radius * sin(theta)
      lines(arc_x, arc_y, col = arc_col)
    }
  }
}



# Function to categorise wind direction into sectors
WDsector <- function(WD, zero = 0, delta = 22.5) {
  # Calculate the end point of the sector
  end_point <- (zero + delta) %% 360
  if (zero == 0) {
    sector <- ifelse(WD == 0, 999, cut(WD, breaks = c(0:(360/delta)) * delta, labels = 1:(360/delta)))
  } else {
    # Assign sector numbers based on wind direction
    sector <- cut(WD, breaks = c(0:(360/delta)) * delta, labels = 1:(360/delta))
  }
  return(sector)
}


# Define a function to rotate a set of coordinates by a given angle
rot_coord <- function(coords, angle) {
  angle_rad <- angle * (pi / 180)
  rotation_matrix <- matrix(c(cos(angle_rad), -sin(angle_rad), sin(angle_rad), cos(angle_rad)), 
                           nrow = 2, ncol = 2)
  rotated_coords <- t(rotation_matrix %*% t(coords))
  dt <- data.table(rotated_coords)
  setnames(dt, c('x', 'y'))
  return(dt)
}


calc_rot <- function(coords_dt) {
  # Extract the x and y coordinates of the endpoints
  x1 <- as.numeric(coords_dt[1, 1])
  y1 <- as.numeric(coords_dt[1, 2])
  x2 <- as.numeric(coords_dt[2, 1])
  y2 <- as.numeric(coords_dt[2, 2])
  # Calculate the difference in x and y
  dx <- x2 - x1
  dy <- y2 - y1
  # Use atan2 to calculate the angle between the line and the x-axis (in radians)
  angle_rad <- atan2(dy, dx)
  # Convert radians to degrees
  angle_deg <- angle_rad * (180 / pi)
  # Return the angle in degrees
  return(angle_deg)
}


shift_coordinates <- function(dt) {
  # Find the minimum values of x and y
  min_x <- min(dt$x)
  min_y <- min(dt$y)
  
  # Shift the x and y values to make the minimum values 0
  dt[, `:=`(x = x - min_x, y = y - min_y)]
  
  return(dt)
}


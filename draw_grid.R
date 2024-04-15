# drew a grid with wind direction sectors over a plot

draw_grid <- function(point, delta=NULL, n_lines = NULL, line_length=200, zero=0, col='grey',lwd=NULL) {
	# browser()
  if(is.null(delta) & is.null(n_lines)){stop('Specify either "delta" or "n_lines".')}
  if(!is.null(delta) & !is.null(n_lines)){if(delta != 360/n_lines*2){stop('Only specify delta or n_lines, but not both.')}}
  if(is.null(delta)){delta <- 360/n_lines/2} else {n_lines <- 360/delta/2}
  if(!((360 / delta) %% 1 == 0 && (360 / delta) > 0)){stop('360 is not a multiple of your delta value')}
  # Convert angle from degrees to radians
  angle_rad <- delta * pi / 180
  zero_rad <- zero * pi / 180
  # Create a matrix to store line coordinates
  lines_matrix <- matrix(NA, nrow = n_lines, ncol = 4)
  # Calculate coordinates for each line
  for (i in 1:n_lines) {
    x1 <- point[1] + (line_length/2) * cos(angle_rad-zero_rad)
    x2 <- point[1] - (line_length/2) * cos(angle_rad-zero_rad)
    y1 <- point[2] + (line_length/2) * sin(angle_rad-zero_rad)
    y2 <- point[2] - (line_length/2) * sin(angle_rad-zero_rad)
    lines_matrix[i, ] <- c(x1,x2,y1,y2)
    
    # Increment the angle for the next line
    angle_rad <- angle_rad + (delta * pi / 180)
  }
  
  for (i in 1:n_lines) {
    lines(c(lines_matrix[i, 1], lines_matrix[i, 2]), c(lines_matrix[i, 3], lines_matrix[i, 4]),col=col,lwd=lwd)
  }
}

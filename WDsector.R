# Function to categorize wind direction into sectors

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

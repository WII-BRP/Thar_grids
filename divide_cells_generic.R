library(sf)
library(dplyr)
library(ggplot2)

# Function to generate equidistant points between two points
generate_equidistant_points <- function(start, end, n) {
  x <- seq(start[1], end[1], length.out = n)
  y <- seq(start[2], end[2], length.out = n)
  return(cbind(x, y))
}


add_subcell_numbers <- function(subcell_sf, nrows, ncols) {
  subcell_sf$subcell_id <- NA
  
  # Subcell numbering starts from the bottom-left corner and goes up in each column, left to right across columns
  subcell_number <- 1
  
  for (j in 1:ncols) {  # Loop through columns, left to right
    for (i in 1:nrows) {  # Loop through rows, bottom to top within each column
      # Calculate the index for subcell in the data frame
      index <- (nrows - i) * ncols + j  # Reverse row indexing to start from bottom
      subcell_sf$subcell_id[index] <- paste(subcell_number)
      subcell_number <- subcell_number + 1
    }
  }
  
  return(subcell_sf)
}

# Function to divide the cell into subcells
divide_cells_generic <- function(cell, nrows, ncols) {
  num_rows = nrows + 1
  num_cols = ncols + 1
  # Get coordinates of the vertices of the original cell
  coords <- st_coordinates(cell) %>% st_zm()
  
  # Extract vertices: bottom-left, top-left, top-right, and bottom-right
  bl = coords[which.min(coords[, 2]), ]
  tl = coords[which.min(coords[, 1]), ]
  tr = coords[which.max(coords[, 2]), ]
  br = coords[which.max(coords[, 1]), ]
  
  # Generate equidistant points for each edge
  equidistant_points_top <- generate_equidistant_points(tl, tr, num_cols)
  equidistant_points_left <- generate_equidistant_points(tl, bl, num_rows)
  equidistant_points_right <- generate_equidistant_points(br, tr, num_rows)
  equidistant_points_bottom <- generate_equidistant_points(bl, br, num_cols)
  
  # Generate parallel rows of points between left and right edges (inside the polygon)
  parallel_rows <- list()
  for (i in 1:num_rows) {
    left_point <- equidistant_points_left[i, ]
    right_point <- equidistant_points_right[(num_rows + 1) - i, ]
    
    row_points <- generate_equidistant_points(left_point, right_point, num_cols)
    parallel_rows[[i]] <- row_points
  }
  
  # Create subcell polygons by connecting points from consecutive parallel rows and edge points
  subcells <- list()
  
  for (i in 1:(num_rows - 1)) {
    for (j in 1:(num_cols - 1)) {
      pt1 = c(parallel_rows[[i]][j, 1], parallel_rows[[i]][j, 2]) # Row i, point j
      pt2 = c(parallel_rows[[i]][(j + 1), 1], parallel_rows[[i]][(j + 1), 2]) # Row i, point j+1
      pt3 = c(parallel_rows[[i + 1]][(j + 1), 1], parallel_rows[[i + 1]][(j + 1), 2]) # Row i+1, point j+1
      pt4 = c(parallel_rows[[i + 1]][j, 1], parallel_rows[[i + 1]][j, 2]) # Row i+1, point j
      
      # Create the polygon for the subcell
      subcell_points <- matrix(c(pt1, pt2, pt3, pt4, pt1), ncol = 2, byrow = TRUE)
      subcell_polygon <- st_sfc(st_polygon(list(subcell_points)), crs = 4326)
      
      # Add the subcell to the list with a unique identifier
      subcell_name <- paste("Subcell", i, j)
      subcells[[subcell_name]] <- subcell_polygon
    }
  }
  
  # Convert the subcells list into an sf object
  subcell_sf <- do.call(rbind, lapply(subcells, function(x) st_sf(geometry = x)))%>%
    add_subcell_numbers(nrows, ncols)%>%
    st_join(cell)
  
  return(subcell_sf)
}
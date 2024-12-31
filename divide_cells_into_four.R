library(sf)
library(dplyr)
library(tidyr)

divide_cells_into_four <- function(cell) {
  cell = cell %>% st_zm()  # Remove Z and M dimensions if present
  
  # Get the coordinates of the polygon's vertices
  coords <- st_coordinates(cell)
  
  # Create sf object for vertices
  coords_sf <- st_as_sf(data.frame(x = coords[, 1], y = coords[, 2]), coords = c("x", "y"), crs = st_crs(cell))[1:4, ]
  
  # Get the centroid of the cell
  centroid <- st_centroid(cell)
  
  # Calculate midpoints of edges
  midpoints <- data.frame(
    x = (coords[1:4, 1] + coords[2:5, 1]) / 2,  # Midpoints in x (average of consecutive vertices)
    y = (coords[1:4, 2] + coords[2:5, 2]) / 2   # Midpoints in y (average of consecutive vertices)
  ) %>%
    st_as_sf(coords = c("x", "y"), crs = st_crs(cell))
  
  # Create cell_A
  cell_A_pts <- matrix(c(
    st_coordinates(coords_sf[2,])[1], st_coordinates(coords_sf[2,])[2],  # First point (bottom-left)
    st_coordinates(midpoints[2,])[1], st_coordinates(midpoints[2,])[2],  # Second point (bottom-right)
    st_coordinates(centroid)[1], st_coordinates(centroid)[2],  # Third point (top-right)
    st_coordinates(midpoints[1,])[1], st_coordinates(midpoints[1,])[2],  # Fourth point (top-left)
    st_coordinates(coords_sf[2,])[1], st_coordinates(coords_sf[2,])[2]   # Closing the polygon (first point again)
  ), ncol = 2, byrow = TRUE)
  
  cell_A <- st_sfc(st_polygon(list(cell_A_pts)), crs = st_crs(cell)) %>%
    st_sf() %>%
    mutate(subcell = "A")
  
  # Create cell_B
  cell_B_pts <- matrix(c(
    st_coordinates(coords_sf[1,])[1], st_coordinates(coords_sf[1,])[2],  # First vertex (bottom-left)
    st_coordinates(midpoints[1,])[1], st_coordinates(midpoints[1,])[2],  # Midpoint on bottom edge
    st_coordinates(centroid)[1], st_coordinates(centroid)[2],  # Centroid
    st_coordinates(midpoints[4,])[1], st_coordinates(midpoints[4,])[2],  # Midpoint on left edge
    st_coordinates(coords_sf[1,])[1], st_coordinates(coords_sf[1,])[2]   # Closing the polygon (first point again)
  ), ncol = 2, byrow = TRUE)
  
  cell_B <- st_sfc(st_polygon(list(cell_B_pts)), crs = st_crs(cell)) %>%
    st_sf() %>%
    mutate(subcell = "B")
  
  # Create cell_C
  cell_C_pts <- matrix(c(
    st_coordinates(coords_sf[3,])[1], st_coordinates(coords_sf[3,])[2],  # Third vertex (top-right)
    st_coordinates(midpoints[3,])[1], st_coordinates(midpoints[3,])[2],  # Midpoint on top edge
    st_coordinates(centroid)[1], st_coordinates(centroid)[2],  # Centroid
    st_coordinates(midpoints[2,])[1], st_coordinates(midpoints[2,])[2],  # Midpoint on right edge
    st_coordinates(coords_sf[3,])[1], st_coordinates(coords_sf[3,])[2]   # Closing the polygon (third point again)
  ), ncol = 2, byrow = TRUE)
  
  cell_C <- st_sfc(st_polygon(list(cell_C_pts)), crs = st_crs(cell)) %>%
    st_sf() %>%
    mutate(subcell = "C")
  
  # Create cell_D
  cell_D_pts <- matrix(c(
    st_coordinates(coords_sf[4,])[1], st_coordinates(coords_sf[4,])[2],  # Fourth vertex (top-left)
    st_coordinates(midpoints[4,])[1], st_coordinates(midpoints[4,])[2],  # Midpoint on left edge
    st_coordinates(centroid)[1], st_coordinates(centroid)[2],  # Centroid
    st_coordinates(midpoints[3,])[1], st_coordinates(midpoints[3,])[2],  # Midpoint on top edge
    st_coordinates(coords_sf[4,])[1], st_coordinates(coords_sf[4,])[2]   # Closing the polygon (fourth point again)
  ), ncol = 2, byrow = TRUE)
  
  cell_D <- st_sfc(st_polygon(list(cell_D_pts)), crs = st_crs(cell)) %>%
    st_sf() %>%
    mutate(subcell = "D")
  
  # Combine all cells into one sf object
  split_result <- rbind(cell_A, cell_B, cell_C, cell_D) %>%
    mutate(New_CellID = cell$name) %>%
    mutate(ID_6km = paste(paste("JSM",New_CellID,sep=""), subcell, sep = "_"))%>%
    select(-subcell)
  
  return(split_result)
}
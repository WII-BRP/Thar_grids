library(sf)
library(dplyr)

original_grid_sf = st_read("F:/Projects/Active/Khuiyala_mining/Grids/Jaisalmer_12km_WGS84.shp")

# Function to divide a cell into 4 subcells
source("F:/Projects/Active/Khuiyala_mining/Codes/divide_cells_into_four.R")
source("F:/Projects/Active/Khuiyala_mining/Codes/divide_cells_generic.R")

split_cells_list <- list()

# Loop over each cell in the grid (assuming `grid` is an sf object)
for (i in 1:nrow(original_grid_sf)) {
  cell_geometry <- original_grid_sf[i,]  # Extract geometry of the i-th cell
  split_cells <- divide_cells_into_four(cell_geometry)  # Apply divide_cell function to the cell geometry
  split_cells_list[[i]] <- split_cells  # Store the result in the list
}

# Combine all the split cells into a single sf object
grids_6km <- do.call(rbind, split_cells_list)

#st_write(grids_6km, "F:/Projects/Active/Khuiyala_mining/Grids/Jaisalmer_6km_WGS84.shp", append = F)

split_cells_list <- list()
nr = 12
nc = 12
# Loop over each cell in the grid
for (i in 1:nrow(original_grid_sf)) {
  split_cells <- divide_cells_generic(original_grid_sf[i,], nrows = nr, ncols = nc) 
  split_cells_list[[i]] <- split_cells  # Store the result in the list
}

# Combine all the split cells into a single sf object
split_cells_combined <- do.call(rbind, split_cells_list)

grids_1km = split_cells_combined%>%
  select(subcell_id, name)%>%
  rename(minorID = subcell_id)%>%
  mutate(ID = paste(paste("JSM", name, sep=""),minorID, sep="_"))

#st_write(grids_1km, "F:/Projects/Active/Khuiyala_mining/Grids/Jaisalmer_1km_WGS84.shp", append = F)

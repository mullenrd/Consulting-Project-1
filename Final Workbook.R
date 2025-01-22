library(terra)
library(sf)
library(raster)
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(geodata)
library(elevatr)
library(RColorBrewer)
library(viridis)

#IMPORTANT READ
#The URL attached below is the parent directory for the land coverage on NASA. Unfortunately, 
#the files were too large to load to Github, so they an be downloaded here. The two tiles are h32v26 and
#h33v26. Ideally you can search and make sure the file has SA_LC.tif at the end as that is the one 
#for South America. The download requires a NASA Earth Data account, which is free and just requires a quick signup. 
#Once downloaded, make sure to change the pathname for the files to correctly match your specified folder with the data.

#URL:https://e4ftl01.cr.usgs.gov/MEASURES/GLanCE30.001/


#Dataset (Replace with new path to excel file with data)
monkey<-read_xlsx("/Users/reesemullen/Desktop/Statistical Practice/Consulting Projects/Consulting-Project-1/LF_fecal_Samples_2019_2022.xlsx")

#Parse function for Lat/Long
parse_position <- function(position) {
  # Remove the hemisphere markers (N/S/E/W) for now
  clean_position <- gsub("[NSWE]", "", position)
  
  # Match the cleaned format without direction markers
  match <- regexec("([0-9]+)°\\s*([0-9.]+)'\\s*([0-9]+)°\\s*([0-9.]+)'", clean_position)
  parts <- regmatches(clean_position, match)[[1]]
  
  # Debugging output
  cat("Matched parts (numeric only):", parts, "\n")
  
  if (length(parts) == 5) {  # Ensure the correct number of parts is matched
    # Convert matched parts to numeric
    lat_deg <- as.numeric(parts[2])
    lat_min <- as.numeric(parts[3])
    lon_deg <- as.numeric(parts[4])
    lon_min <- as.numeric(parts[5])
    
    # Ensure no NAs after conversion
    if (is.na(lat_deg) || is.na(lat_min) || is.na(lon_deg) || is.na(lon_min)) {
      cat("Numeric conversion failed for:", position, "\n")
      return(c(NA, NA))
    }
    
    # Calculate decimal degrees
    latitude <- lat_deg + lat_min / 60
    longitude <- lon_deg + lon_min / 60
    
    # Reapply hemisphere corrections using the original position
    if (grepl("S", position)) latitude <- -latitude
    if (grepl("W", position)) longitude <- -longitude
    
    return(c(latitude, longitude))
  }
  
  # Log failure to parse
  cat("Failed to parse:", position, "\n")
  return(c(NA, NA))
}


# Apply the function to the Position column
coordinates <- t(sapply(monkey$Position, parse_position))
monkey$Latitude <- coordinates[, 1]
monkey$Longitude <- coordinates[, 2]


# Specify the folder containing your raster files
folder_path <- "/Users/reesemullen/Desktop/Statistical Practice/Consulting Projects/Consulting-Project-1/Land Coverage Data"

# List all .tif files in the folder
file_list <- list.files(folder_path, pattern = "\\.tif$", full.names = TRUE)

# Extract the year from the filenames
# The year is the 4 digits after "A"
file_years <- sapply(file_list, function(x) {
  sub(".*_A(\\d{4}).*", "\\1", x)
})

# Group files by year
file_groups <- split(file_list, file_years)

# Combine the files for each year
#Replace the folder name for saved rasters
output_folder <- "/Users/reesemullen/Downloads/Combined/" # Folder to save combined rasters
dir.create(output_folder, showWarnings = FALSE)

for (year in names(file_groups)) {
  # Get the two files for the current year
  files <- file_groups[[year]]
  
  if (length(files) == 2) {  # Ensure there are exactly two files
    # Load the rasters
    raster1 <- rast(files[1])
    raster2 <- rast(files[2])
    
    # Merge the rasters
    combined_raster <- merge(raster1, raster2)
    
    # Save the combined raster
    output_path <- file.path(output_folder, paste0("Combined_", year, ".tif"))
    writeRaster(combined_raster, output_path, overwrite = TRUE)
    
    # Plot the combined raster
    plot(combined_raster, main = paste("Combined Raster for Year", year))
    
    # Print a message
    cat("Combined files for year", year, "saved to", output_path, "\n")
  } else {
    cat("Skipping year", year, "as it does not have exactly 2 files.\n")
  }
}



# Directory containing the .tif files (Replace with path to data)
tif_folder <- ("/Users/reesemullen/Downloads/Combined")

# Get a list of all .tif files in the folder
tif_files <- list.files(tif_folder, pattern = "\\.tif$", full.names = TRUE)

# Initialize an empty list to store rasters
rasters_list <- list()

# Loop through each file and process
for (file in tif_files) {
  # Extract a meaningful name (e.g., year) from the filename
  file_name <- sub(".*?(\\d{4}).*", "\\1", basename(file))
  
  # Load the raster
  raster <- rast(file)
  
  # Store the raster in the list with the extracted name
  rasters_list[[file_name]] <- raster
  
  # Optional: Uncomment the lines below to plot each raster
  # plot(raster, main = paste("Raster for", file_name))
  # cat("Raster for", file_name, "plotted.\n")
  
  cat("Raster for", file_name, "stored in memory.\n")
}

# Print summary of the raster list
print(rasters_list)

# Reproject each raster in the combined_rasters list to WGS84 (Longitude/Latitude)
combined_rasters_wgs84 <- lapply(rasters_list, function(raster) {
  project(raster, "EPSG:4326")
})

# Confirm the CRS for the first raster in the updated list
crs(combined_rasters_wgs84[[1]])
plot(combined_rasters_wgs84[[1]])

#Assuming `df` contains your latitude, longitude, and site information
representative_points <- monkey %>%
  filter(Site %in% c("El Torro", "Hierba Buena", "Peña Blanca", "San Rafael")) %>%
  filter(!is.na(Latitude) & !is.na(Longitude)) %>%  # Exclude rows with missing coordinates
  group_by(Site) %>%
  slice(1) %>%  # Select the first valid point for each site
  ungroup()

# Remove rows with missing Longitude or Latitude
representative_points <- representative_points[!is.na(representative_points$Longitude) & !is.na(representative_points$Latitude), ]

# Create a spatial object for these points
points_data <- data.frame(
  longitude = representative_points$Longitude,
  latitude = representative_points$Latitude,
  site = representative_points$Site
)

points <- vect(points_data, geom = c("longitude", "latitude"), crs = "EPSG:4326")

# Define category labels and updated colors
category_labels <- c("Water", "Snow/Ice", "Developed", "Barren", "Trees", "Shrubs", "Herbaceous")
updated_colors <- c("#000080", "#0000FF", "#0FFF00", "#FFFF00", 
                    "#228B22", "#9ACD32", "#FFD700")  # Trees = Forest Green, Shrubs = Light Olive Green

# Loop through each raster in combined_rasters_wgs84
for (year in names(combined_rasters_wgs84)) {
  
  # Select and project raster
  combined_raster <- combined_rasters_wgs84[[year]]
  projected_raster <- project(combined_raster, "EPSG:4326")
  
  # Plot the zoomed-in raster WITH LEGEND
  plot(projected_raster, 
       main = paste("Zoomed-in Plot for Year", year), 
       col = updated_colors, 
       xlim = c(zoom_extent["xmin"], zoom_extent["xmax"]),
       ylim = c(zoom_extent["ymin"], zoom_extent["ymax"]),
       axes = TRUE, legend = TRUE)  # Enable legend here
  grid()
  
  # Add points within the zoomed region
  plot(points, add = TRUE, col = c("blue", "purple", "orange", "red"), 
       pch = 20, cex = 3)
  
  # Add manual legend for points (bottom-left)
  legend(#"bottomleft", 
    x = -77.915,  # Adjust X position based on zoom_extent
    y = -5.76,   # Adjust Y position based on zoom_extent
         legend = points_data$site, 
         col = c("blue", "purple", "orange", "red"),
         pch = 20, pt.cex = 2, 
         title = "Sites", 
         bg = "white", bty = "o")
  
  # Add axis labels
  mtext("Longitude", side = 1, line = 4, font = 2)
  mtext("Latitude", side = 2, line = 0.2, font = 2)
  
  # Message for progress
  cat("Finished processing and plotting for year:", year, "\n")
}





# Initialize a vector to store the percentage of 'Trees' for each year
trees_percentage <- c()

# Loop through each raster in combined_rasters_wgs84
for (year in names(combined_rasters_wgs84)) {
  
  # Select the raster for the current year
  selected_raster <- combined_rasters_wgs84[[year]]
  
  # Project raster to EPSG:4326 if not already in that CRS
  projected_raster <- project(selected_raster, "EPSG:4326")
  
  # Extract raster values and calculate the total number of cells
  raster_values <- values(projected_raster)
  total_cells <- length(raster_values)
  
  # Count the cells classified as 'Trees' (assuming Trees = 5 based on earlier ID)
  trees_cells <- sum(raster_values == 5, na.rm = TRUE)
  
  # Calculate the percentage of 'Trees' cells
  percentage <- (trees_cells / total_cells) * 100
  
  # Store the percentage in the vector with the year as the name
  trees_percentage[year] <- percentage
}

# Convert the vector to a data frame for plotting
trees_df <- data.frame(Year = names(trees_percentage), 
                       Trees_Percentage = trees_percentage)

# Plot the percentage of 'Trees' for each year as a bar chart
# Create the improved bar plot
ggplot(trees_df, aes(x = Year, y = Trees_Percentage)) +
  geom_bar(stat = "identity", fill = "forestgreen", color = "black", width = 0.7) +
  
  # Add percentage labels on top of each bar
  geom_text(aes(label = sprintf("%.1f%%", Trees_Percentage)), 
            vjust = -0.5, size = 3.5, color = "black", fontface = "bold") +
  
  # Add horizontal gridlines for better readability
  scale_y_continuous(expand = c(0, 0), limits = c(0, max(trees_df$Trees_Percentage) + 5)) +
  theme_minimal(base_size = 14) +
  
  # Improve axis and titles
  labs(
    title = "Percentage of Trees in Each Raster Plot",
    x = "Year",
    y = "Percentage of Trees (%)"
  ) +
  
  # Adjust theme for cleaner visuals
  theme(
    panel.grid.major.y = element_line(color = "grey80", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", hjust = 0.5),
    axis.title = element_text(face = "bold")
  )







# Define the bounding box of your area (longitude and latitude)
lon_min <- -77.915  # Minimum longitude
lon_max <- -77.785 # Maximum longitude
lat_min <- -5.816    # Minimum latitude
lat_max <- -5.66   # Maximum latitude

# Create a data frame with the coordinates
coords <- data.frame(
  lon = c(lon_min, lon_max, lon_max, lon_min, lon_min),
  lat = c(lat_min, lat_min, lat_max, lat_max, lat_min)
)

# Convert the data frame to an sf polygon
polygon_sf <- st_polygon(list(as.matrix(coords))) %>%
  st_sfc(crs = 4326) %>%
  st_sf()

# Get elevation data
dem_data <- get_elev_raster(
  locations = polygon_sf,
  z = 10,           # Zoom level for resolution; adjust between 1 (low) to 14 (high)
  clip = "locations"
)
# Plot the elevation data
plot(dem_data, main = "Elevation Map")
# Calculate slope
slope_data <- terrain(dem_data, opt = 'slope', unit = 'degrees')

# Plot the slope data
plot(slope_data, main = "Slope Map")

# Calculate aspect
aspect_data <- terrain(dem_data, opt = 'aspect', unit = 'degrees')

# Calculate hillshade
hillshade_data <- hillShade(slope_data, aspect_data, angle = 40, direction = 270)

# Calculate hillshade to add a 3D effect
slope <- terrain(dem_data, opt = "slope")
aspect <- terrain(dem_data, opt = "aspect")
hillshade <- hillShade(slope, aspect, angle = 40, direction = 270)

# Plot hillshade
plot(hillshade_data, col = gray(0:100/100), legend = FALSE, main = "Hillshade Map")

# Overlay elevation contours
contour(dem_data, add = TRUE, col = 'white')

# Define a clearer color palette
palette <- terrain.colors(100)  # Or use RColorBrewer: brewer.pal(n, "Spectral")


palette <- colorRampPalette(brewer.pal(11, "Spectral"))(100)
# Plot the base map with hillshade and elevation
plot(hillshade, col = gray(0:100 / 100), legend = FALSE, 
     main = "Enhanced Elevation Map with Points")

plot(dem_data, col = palette, alpha = 0.6, add = TRUE, legend.args = list(
  text = "Elevation (m)", side = 4, font = 2, line = 3, cex = 1.2
))
contour(dem_data, add = TRUE, col = "black", lwd = 0.6, levels = seq(1800, 3200, 100))

# Add points to the map
points(points_data$longitude, points_data$latitude, 
       pch = 19, col = c("blue","purple","orange", "red"), cex =3)  # Red dots with size 1.2
legend(
  x = -77.915,  # Adjust X position based on zoom_extent
  y = -5.76,   # Adjust Y position based on zoom_extent
  legend = points_data$site,
  col = c("blue", "purple","orange", "red"),
  pch = 20,
  pt.cex = 2,
  bg = "white"  # Add a white background for better visibility
)
mtext("Longitude", side = 1, line = 2, font = 2)
mtext("Latitude", side = 2, line = 2, font = 2)


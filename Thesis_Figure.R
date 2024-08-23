# Packages
library(sampling)
library(sswr)
library(remotes)
library(survey)
library(ggplot2)
library(leaflet)
library(osmdata)
library(ggmap)
library(sf)
library(dplyr)
library(leaps)
library(mase)
library(mgcv)
library(Matrix)
library(geoR)
library(viridis)
library(tidyr)
library(kableExtra)
library(ggcorrplot)
library(reshape2)
library(gridExtra)
######################################################
# Figures
# Figure 1: The Image of Amazon Above-Ground Biomass Distribution
# simple random sampling
data(grdAmazonia)
N <- nrow(grdAmazonia)

# create the basic image
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = AGB)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "AGB")

# plot the image
print(base_plot)
##############################################
# Figure 2: Distribution of Variables in grdAmazonia dataset
data(grdAmazonia)
# delete the coordinate
grdAmazonia <- grdAmazonia %>% select(-x1, -x2)
# 确保Ecoregion和Biome是因子类型
grdAmazonia <- grdAmazonia %>%
  mutate(Ecoregion = as.factor(Ecoregion), 
         Biome = as.factor(Biome))

# Converts data to a long format for plotting
grdAmazonia_long <- melt(grdAmazonia)

# plot the distribution of each variables
ggplot(grdAmazonia_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  facet_wrap(~variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distribution of Variables in grdAmazonia",
       x = "Value",
       y = "Frequency")
####################################3
# Figure 3: Box Plots of all Variables
data(grdAmazonia)
# box plots of each covariates
par(mfrow = c(1, 2))

# AGB
boxplot(grdAmazonia$AGB, 
        main = "Boxplot of AGB",
        ylab = "AGB (Above Ground Biomass)",
        col = "lightblue", 
        border = "darkblue") 

# SWIR2
boxplot(grdAmazonia$SWIR2, 
        main = "Boxplot of SWIR2",
        ylab = "SWIR2",
        col = "lightcoral", 
        border = "darkred")  

# Terra_PP
boxplot(grdAmazonia$Terra_PP, 
        main = "Boxplot of Terra_PP",
        ylab = "Terra_PP",
        col = "lightgreen",  
        border = "darkgreen")  

# Prec_dm
boxplot(grdAmazonia$Prec_dm, 
        main = "Boxplot of Prec_dm",
        ylab = "Prec_dm",
        col = "lightgoldenrodyellow", 
        border = "goldenrod")  

# Elevation
boxplot(grdAmazonia$Elevation, 
        main = "Boxplot of Elevation",
        ylab = "Elevation",
        col = "orange", 
        border = "darkred") 

# Clay
boxplot(grdAmazonia$Clay, 
        main = "Boxplot of Clay",
        ylab = "Clay",
        col = "lightgray", 
        border = "darkgray")
#####################################################
# Figure4: requency Distribution of Categorical Variables

# Frequency Distribution for Categorical Variables
# transform Ecoregion and Biome to factor
data(grdAmazonia)
grdAmazonia <- grdAmazonia %>%
  mutate(Ecoregion = as.factor(Ecoregion), 
         Biome = as.factor(Biome))
# Selecting categorical variables and calculating frequency counts and frequencies
categorical_stats <- grdAmazonia %>%
  select_if(is.factor) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  group_by(variable, value) %>%
  summarise(count = n(), .groups = 'drop') %>%
  mutate(frequency = count / sum(count) * 100)

# Creating frequency distribution tables for categorical variables
categorical_stats_table <- categorical_stats %>%
  kable("html", caption = "Frequency Distribution for Categorical Variables") %>%
  kable_styling(full_width = F)

# printable form of result
print(categorical_stats_table)

# set factor levels
categorical_stats$value <- factor(categorical_stats$value, 
                                  levels = unique(categorical_stats$value))

# Create a bar graph showing frequency counts
custom_colors <- c("#FF5733", "#3357FF")
ggplot(categorical_stats, aes(x = value, y = count, fill = variable)) +
  geom_bar(stat = "identity") +
  coord_flip() +  
  theme_minimal() +
  labs(title = "Frequency Distribution of Categorical Variables",
       x = "Category",
       y = "Count") +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = custom_colors)

##########################################
# Figure5: Biomes Classification Map
# Assuming grdAmazonia is already loaded
# Convert Biome to a factor
grdAmazonia$Biome_f <- as.factor(grdAmazonia$Biome)

# Define a color palette for the biomes
biome_colors<-c("Mangroves" = "#ff7f00",  # Blue for Mangroves
              "Tropical & Subtropical Dry Broadleaf Forests" = "purple",  # Green
              "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "#1f78b4",  # Red
              "Tropical & Subtropical Moist Broadleaf Forests" = "#33a07c")  # Orange

# Simple random sampling
n <- 100
N <- nrow(grdAmazonia)
set.seed(314)
units <- sample(N, size = n, replace = FALSE)
mysample <- grdAmazonia[units, ]

# Create the base plot with Biome as background
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = Biome_f)) +
  scale_fill_manual(values = biome_colors) +
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "Biome") +
  theme(legend.position = "bottom")

# Plot the image
print(base_plot)
#########################################
# Figure 6: Ecoregions Classification Map
# Ecoregion
# Load necessary libraries
library(ggplot2)
library(sf)
library(dplyr)
library(viridis)

# Assuming grdAmazonia is already loaded
# Convert Ecoregion to a factor
grdAmazonia$Ecoregion_f <- as.factor(grdAmazonia$Ecoregion)

# Simple random sampling
n <- 100
N <- nrow(grdAmazonia)
set.seed(314)
units <- sample(N, size = n, replace = FALSE)
mysample <- grdAmazonia[units, ]


# Define a color vector with specific colors for each ecoregion
colors <- c("Uatuma-Trombetas moist forests" = "lightblue",
            "Guianan lowland moist forests" = "blue",
            "Guianan highland moist forests" = "green",
            "Guianan savanna" = "purple",
            "Marajo varzea" = "orange",
            "Amazon-Orinoco-Southern Caribbean mangroves" = "cyan",
            "Monte Alegre varzea" = "yellow",
            "Tocantins/Pindare moist forests" = "brown",
            "Xingu-Tocantins-Araguaia moist forests" = "pink",
            "Gurupa varzea" = "red",
            "Madeira-Tapajos moist forests" = "darkgreen",
            "Tapajos-Xingu moist forests" = "gold",
            "Purus-Madeira moist forests" = "darkblue",
            "Maranhao Babassu forests" = "magenta",
            "Cerrado" = "grey",
            "Mato Grosso tropical dry forests" = "orange3")

# Create the plot with manual colors
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = Ecoregion_f)) +
  scale_fill_manual(values = colors) +  # Apply manual color mapping
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "Ecoregion") +
  theme(legend.position = "bottom")

# Plot the image
print(base_plot)
######################################
#Figure 7: 
# rename the Biome
biome_names <- c(
  "Mangroves" = "Mangroves",
  "Tropical & Subtropical Dry Broadleaf Forests" = "Dry Broadleaf Forests",
  "Tropical & Subtropical Grasslands, Savannas & Shrublands" = "Grasslands, Savannas & Shrublands",
  "Tropical & Subtropical Moist Broadleaf Forests" = "Moist Broadleaf Forests"
)

# add the new name to the dataframe
grdAmazonia$Biome_short <- as.factor(sapply(grdAmazonia$Biome,
                                            function(x) biome_names[x]))

# Set the graphic layout to 3 rows and 2 columns
par(mfrow = c(1, 1))  

# AGB by Biome
boxplot(AGB ~ Biome_short, data = grdAmazonia, 
        main = "Boxplot of AGB by Biome",
        xlab = "Biome",
        ylab = "AGB (Above Ground Biomass)",
        las = 1, 
        col = "lightblue",  
        border = "darkblue")  

# SWIR2 by Biome
boxplot(SWIR2 ~ Biome_short, data = grdAmazonia, 
        main = "Boxplot of SWIR2 by Biome",
        xlab = "Biome",
        ylab = "SWIR2",
        las = 1, 
        col = "lightcoral",  
        border = "darkred")  

# Terra_PP by Biome
boxplot(Terra_PP ~ Biome_short, data = grdAmazonia, 
        main = "Boxplot of Terra_PP by Biome",
        xlab = "Biome",
        ylab = "Terra_PP",
        las = 1, 
        col = "lightgreen",  
        border = "darkgreen")  

# Prec_dm by Biome
boxplot(Prec_dm ~ Biome_short, data = grdAmazonia, 
        main = "Boxplot of Prec_dm by Biome",
        xlab = "Biome",
        ylab = "Prec_dm",
        las = 1, 
        col = "lightgoldenrodyellow", 
        border = "goldenrod") 

# Elevation by Biome
boxplot(Elevation ~ Biome_short, data = grdAmazonia, 
        main = "Boxplot of Elevation by Biome",
        xlab = "Biome",
        ylab = "Elevation",
        las = 1,  
        col = "orange", 
        border = "darkred") 

# Clay by Biome
boxplot(Clay ~ Biome_short, data = grdAmazonia, 
        main = "Boxplot of Clay by Biome",
        xlab = "Biome",
        ylab = "Clay",
        las = 1,  
        col = "lightgray",  
        border = "darkgray") 
######################################################
# Figure 8: Correlation Matrix of Variables
data("grdAmazonia")

# Select the relevant variables
data<- grdAmazonia[, c("AGB","lnSWIR2","Terra_PP","Prec_dm","Elevation","Clay")]

# Calculate the correlation matrix for numeric variables
cor_matrix <- cor(data, use = "complete.obs")

# Plot the correlation matrix using ggcorrplot
ggcorrplot(cor_matrix, 
           method = "circle",  # Use circle method to represent correlations
           type = "lower",  # Display the lower triangle of correlation matrix
           lab = TRUE,  # Show correlation coefficients inside the circles
           lab_size = 3,  # Size of the labels
           colors = c("red", "white", "blue"),  # Color palette for correlations
           title = "Correlation Matrix of Variables")  # Title of the plot
#########################################################
# Figure 9: Map of Simple Random Samples
# simple random sampling
n <- 100
N <- nrow(grdAmazonia)
units <- sample(N, size = n, replace = FALSE)
mysample <- grdAmazonia_1[units, ]


# the World Sinusoidal projection (esri:54008)
sinusoidal_crs <- "+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Transform the data to sf target, 
# specify the original projected coordinate system as World Sinusoidal.
grdAmazonia_sf <- st_as_sf(mysample, coords = c("x1", "x2"),
                           crs = sinusoidal_crs)

# Transform coordinate to WGS84 (EPSG:4326)
grdAmazonia_wgs84 <- st_transform(grdAmazonia_sf, crs = 4326)

# Extract the transformed coordinates
coords <- st_coordinates(grdAmazonia_wgs84)

# Create a new data frame containing latitude and longitude
grdAmazonia_converted <- data.frame(
  longitude = coords[, "X"],  # latitude
  latitude = coords[, "Y"],  # longitude
  AGB = mysample$AGB  # original AGB values
)

# Plot the map
map_SRS <- leaflet() %>%
  addTiles() %>%
  setView(lng = -60, lat = -3, zoom = 4) %>%
  addCircleMarkers(
    lng = grdAmazonia_converted$longitude,
    lat = grdAmazonia_converted$latitude,
    radius = 4,
    color = "red",
    fillOpacity = 0.8,
    stroke = FALSE,
    popup = paste("AGB:", grdAmazonia_converted$AGB)
  )

map_SRS
############################################
# Figure 10: The Image of AGB with Samples Overlay Using SRS
# simple random sampling
data(grdAmazonia)
n <- 100
N <- nrow(grdAmazonia)
set.seed(314)
units <- sample(N, size = n, replace = FALSE)
mysample <- grdAmazonia[units, ]

# create the basic image
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = AGB)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "AGB")

# overlay the samples
final_plot <- base_plot +
  geom_point(data = mysample, color = "red", size = 2) +
  theme(legend.position = "bottom")

# plot the image
print(final_plot)
#####################################
# Figure11:
#####################################
# Figure 12
#####################################
# Figure 13:he Image of AGB with Samples Overlay Using SS
data("grdAmazonia")

# Calculate the total number in each stratum
N_h <- tapply(grdAmazonia$Biome, INDEX = grdAmazonia$Biome, FUN = length)
w_h <- N_h / sum(N_h)
n <- 100

# Set a minimum sample size for each stratum
min_samples <- 5

# Calculate the initial allocation
n_h <- trunc(n * w_h)

# Identify strata with sample sizes less than the minimum sample size
low_sample_layers <- names(n_h[n_h < min_samples])

# Adjust these strata to have the minimum sample size
n_h[low_sample_layers] <- min_samples

# Recalculate the remaining sample size
remaining_samples <- n - sum(n_h)

# Adjust the strata with the maximum sample size to ensure total samples=100
max_stratum <- names(n_h)[which.max(n_h)]
n_h[max_stratum] <- n_h[max_stratum] + remaining_samples
n_h

# Ensure the total sample size is 100
print(sum(n_h))  # This should print 100

# Print the final allocation
print(n_h)

# Sampling
library(sampling)
library(dplyr)
ord <- unique(grdAmazonia$Biome)
set.seed(314)
units <- sampling::strata(
  grdAmazonia, stratanames = "Biome", size = n_h[ord], method = "srswr")
mysample <- getdata(grdAmazonia, units) %>%
  mutate(x1 = x1 %>% jitter(amount = 25 / 2),
         x2 = x2 %>% jitter(amount = 25 / 2))

# image of AGB
library(ggplot2)
# create the basic image
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = AGB)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "AGB")

# overlay the samples
final_plot <- base_plot +
  geom_point(data = mysample, color = "red", size = 2) +
  theme(legend.position = "bottom")

# plot the final image
print(final_plot)
######################################
# Figure 14:Distribution of Biome with Samples Overlay Using SS
# Convert Biome to a factor
grdAmazonia$Biome <- as.factor(grdAmazonia$Biome)

# Define a color palette for the biomes
biome_colors <- c("Mangroves" = "#ff7f00",  
                  "Forest_dry" = "purple", 
                  "Grassland" = "#1f78b4",  
                  "Forest_moist" = "#33a07c")  s

# Create the base plot with Biome as background
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = Biome)) +
  scale_fill_manual(values = biome_colors) +
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "Biome")

# Overlay the samples
final_plot <- base_plot +
  geom_point(data = mysample, aes(x = x1, y = x2), color = "red", size = 2) +
  theme(legend.position = "bottom")

# Plot the final image
print(final_plot)
#######################################
# Figure 15: The Image of AGB with Samples Overlay Using CS in 1
data(grdAmazonia)

# set seed
set.seed(314)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # sample size
N <- nrow(grdAmazonia) # total number of population


# define the radius
radius <- 250000

# function to calculate the distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}
# Choose a random point as the center of the circle
center_index <- sample(N, size = 1)
center_point <- grdAmazonia[center_index, ]

# Find all the points within the circle
distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2,
                                center_point$x1, center_point$x2)
within_circle_indices <- which(distances <= radius)
within_circle <- grdAmazonia[within_circle_indices, ]

# Selecet samples randomly within the given circle area
sample_indices <- sample(nrow(within_circle), size = 100, replace = FALSE)
mysample <- within_circle[sample_indices, ]

# create the basic image
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = AGB)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "AGB")

# overlay the samples
final_plot <- base_plot +
  geom_point(data = mysample, color = "red", size = 2) +
  theme(legend.position = "bottom")

# plot the final image
print(final_plot)


#######################################
# Figure 16: The Image of AGB with Samples Overlay Using CS in 2
Areas
# Load dataset
data(grdAmazonia)

# Set seed
set.seed(321)

# Compute log of SWIR2
grdAmazonia <- grdAmazonia %>%
  mutate(lnSWIR2 = log(SWIR2))

# Define parameters
n <- 100 # Total sample size
N <- nrow(grdAmazonia) # Total number of population
radius <- 250000 # Radius for the circle

# Function to calculate the distance between two points
calculate_distance <- function(x1, y1, x2, y2) {
  sqrt((x1 - x2)^2 + (y1 - y2)^2)
}

# Function to sample within a given circle
sample_within_circle <- function(center_point, size) {
  distances <- calculate_distance(grdAmazonia$x1, grdAmazonia$x2,
                                  center_point$x1, center_point$x2)
  within_circle_indices <- which(distances <= radius)
  within_circle <- grdAmazonia[within_circle_indices, ]
  sample_indices <- sample(nrow(within_circle), size = size, replace = FALSE)
  within_circle[sample_indices, ]
}

# Choose the first random point as the center of the circle
center_index1 <- sample(N, size = 1)
center_point1 <- grdAmazonia[center_index1, ]

# Sample 30 points within the first circle
sample1 <- sample_within_circle(center_point1, size = 30)

# Ensure the second center is not within the first circle
center_index2 <- sample(N, size = 1)
center_point2 <- grdAmazonia[center_index2, ]
while (calculate_distance(center_point1$x1, center_point1$x2,
                          center_point2$x1, center_point2$x2) <= radius * 2) {
  center_index2 <- sample(N, size = 1)
  center_point2 <- grdAmazonia[center_index2, ]
}

# Sample 70 points within the second circle
sample2 <- sample_within_circle(center_point2, size = 70)

# Combine the two samples
mysample <- bind_rows(sample1, sample2)

# create the basic image
base_plot <- ggplot(grdAmazonia, aes(x = x1, y = x2)) +
  geom_raster(aes(fill = AGB)) +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "Easting (km)", y = "Northing (km)", fill = "AGB")

# overlay the samples
final_plot <- base_plot +
  geom_point(data = mysample, color = "red", size = 2) +
  theme(legend.position = "bottom")

# plot the final image
print(final_plot)
###########################################################
# Figure 23:
# data
data <- data.frame(
  SamplingMethod = c("Simple random sampling", "Stratified sampling",
                     "Convenience sampling in one circular area",
                     "Convenience sampling in two circular areas"),
  Bias = c(7.813, 1.115, 46.120, 33.199),
  Coverage = c(0.96, 0.96, 0.20, 0.24)
)

# Plot
ggplot(data, aes(x = SamplingMethod)) +
  # Bar graph for Coverage
  geom_bar(aes(y = Coverage, fill = "Coverage"), stat = "identity",
           position = "dodge", width = 0.5) +
  # Line graph of Bias
  geom_line(aes(y = Bias / max(data$Bias)*1,group = 1,color = "Bias"),size = 1)+
  geom_point(aes(y = Bias / max(data$Bias) * 1, color = "Bias"), size = 3)+
  # y axis
  scale_y_continuous(
    name = "Coverage",
    limits = c(0, 1),  # Coverage's range
    sec.axis = sec_axis(~ . * max(data$Bias), name = "Bias")
  ) +
  labs(title = "Comparison of Sampling Methods", x = "Sampling Method") +
  theme_minimal() +
  coord_flip() +  # Flip the axes
  scale_fill_manual(values = c("Coverage" = "skyblue")) +
  scale_color_manual(values = c("Bias" = "red")) +
  theme(legend.position = "right")
#################################################################
# Figure 24:
# data
data <- data.frame(
  SamplingMethod = c("Simple random sampling (Single regression estimator)",
      "Simple random sampling (Multiple regression estimator)",
      "Stratified sampling (Separate regression estimator)",
      "Convenience sampling in one circular area (Single regression estimator)",
      "Convenience sampling in one circular area (Multiple regression estimator)",
      "Convenience sampling in two circular areas (Single regression estimator)",
      "Convenience sampling in two circular areas (Multiple regression estimator)"),
  Bias = c(0.631, 0.945, 2.84e-4, 18.576, 36.706, 11.879, 15.968),
  Coverage = c(0.96, 0.96, 0.96, 0.49, 0.49, 0.64, 0.54),
  SamplingCategory = c("Simple random sampling", "Simple random sampling",
                       "Stratified sampling",
                       "Convenience sampling in one circular area",
                       "Convenience sampling in one circular area",
                       "Convenience sampling in two circular areas",
                       "Convenience sampling in two circular areas")
)

# Plot
ggplot(data, aes(x = SamplingMethod, fill = SamplingCategory)) +
  # Bar graph for Coverage
  geom_bar(aes(y = Coverage), stat = "identity", 
           
           position = "dodge", width = 0.5) +
  # Line graph of Bias
  geom_line(aes(y = Bias / max(data$Bias) * 1, group = 1),
            color = "red", size = 1) +
  geom_point(aes(y = Bias / max(data$Bias) * 1), color = "red", size = 3) +
  # y axis
  scale_y_continuous(
    name = "Coverage",
    limits = c(0, 1),  # Coverage's range
    sec.axis = sec_axis(~ . * max(data$Bias), name = "Bias")  # Bias 使用次要坐标轴
  ) +
  labs(title = "Comparison of Model-Assisted Sampling Methods",
       x = "Sampling Method") +
  theme_minimal() +
  coord_flip() +  # Flip the axes
  scale_fill_brewer(palette = "Set3") +  # Different colors to distinguish
  theme(legend.position = "right")

#################################################################
# Figure 25: 
# data
data <- data.frame(
  Sampling_Method = rep(c("Simple random sampling", "Stratified sampling", 
                    "Convenience sampling in one circular area", 
                    "Convenience sampling in two circular areas"), each = 4),
  Estimator = rep(c("LM", "LM with interaction", "GLM", "GAM"), times = 4),
  Bias = c(3.797, 3.815, 4.171, 3.094, 
           3.849, 3.742, 3.779, 3.457, 
           27.083, 43.094, 188.964, 42.268, 
           16.393, 26.334, 21.517, 14.157),
  Coverage = c(0.97, 0.94, 0.98, 0.94, 
               0.94, 0.96, 0.97, 0.94, 
               0.43, 0.53, 0.34, 0.84, 
               0.46, 0.47, 0.43, 0.80)
)

# Bias plot
p1 <- ggplot(data, aes(x = Estimator, y = Bias, fill = Sampling_Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = 
         "Comparison of Bias for Different Sampling Methods with Model-Based Approach", 
       y = "Bias", x = "Estimator") +
  theme_minimal()

# Coverage plot
p2 <- ggplot(data, aes(x = Estimator, y = Coverage, fill = Sampling_Method)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = 
         "Comparison of Coverage for Different Sampling Methods with Model-Based Approach", 
       y = "Coverage", x = "Estimator") +
  theme_minimal()

# Plot
grid.arrange(p1, p2, nrow = 2)
#################################################################
# Figure 26: 

# data
data1 <- data.frame(
  Sampling_Method = rep(c("Design-based approach",
                          "Model-assisted approach", 
                          "Model-based approach"), 
                        times = c(1, 2, 5)),
  Estimator = c("SRS estimator", 
                "Single regression estimator", 
                "Multiple regression estimator", 
                "LM", "LM with interaction", 
                "GLM", "GLM with interaction", "GAM"),
  Bias = c(7.813, 0.631, 0.945, 3.797, 3.815, 4.171, 5.053, 3.094),
  Coverage = c(0.96, 0.96, 0.96, 0.97, 0.94, 0.98, 0.95, 0.94)
)

# Create bar charts of coverage and line charts of bias
ggplot(data1, aes(x = Estimator)) +
  geom_bar(aes(y = Coverage, fill = Sampling_Method),
           stat = "identity", position = "dodge") +
  geom_text(aes(y = Coverage, label = round(Coverage, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  geom_line(aes(y = Bias / 50, group = Sampling_Method),
            color = "black", size = 1) +
  geom_point(aes(y = Bias / 50), color = "black", size = 2) +
  geom_text(aes(y = Bias / 50, label = round(Bias, 3)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  scale_y_continuous(
    name = "Coverage",
    sec.axis = sec_axis(~ . * 50, name = "Bias")
  ) +
  labs(title =
        "Comparison of Bias and Coverage for Different Estimation Approaches") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################################################
# Figure 27: 
# data
data2 <- data.frame(
  Approach = c(rep("Design-based approach", 1), 
               rep("Model-assisted approach", 1), 
               rep("Model-based approach", 5)),
  Estimator = c("SRS estimator", "Separate regression estimator", 
                "LM", "LM with interaction", "GLM", 
                "GLM with interaction", "GAM"),
  Bias = c(0.760, 2.84e-4, 3.849, 3.742, 3.779, 5.025, 3.457),
  Coverage = c(0.99, 0.96, 0.94, 0.96, 0.97, 0.94, 0.94)
)

# Creating Stacked Charts
ggplot(data2, aes(x = Estimator)) +
  geom_bar(aes(y = Coverage, fill = Approach),
           stat = "identity", position = "dodge") +
  geom_line(aes(y = Bias * (1 / 6), group = 1), color = "black", size = 1) +
  geom_point(aes(y = Bias * (1 / 6)), color = "black", size = 2) +
  geom_text(aes(y = Coverage, label = round(Coverage, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  geom_text(aes(y = Bias * (1 / 6), label = round(Bias, 3)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +
  scale_y_continuous(
    name = "Coverage",
    limits = c(0, 1),  # Coverage (0-1)
    sec.axis = sec_axis(~ . * (6 / 1), name = "Bias", 
                        breaks = seq(0, 6, by = 1))  # Bias (0-6)
  ) +
  labs(title =
        "Comparison of Bias and Coverage for Different Estimation Approaches") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
#################################################################
# Figure 28:
# data
data3 <- data.frame(
  Approach = c(rep("Design-based approach", 1), 
               rep("Model-assisted approach", 2), 
               rep("Model-based approach", 4)),
  Estimator = c("SRS estimator", 
                "Single regression estimator", 
                "Multiple regression estimator", 
                "LM", "LM with interaction", 
                "GLM", "GAM"),
  Bias = c(46.120, 18.576, 36.706, 27.083, 43.094, 188.964, 42.268),
  Coverage = c(0.20, 0.49, 0.49, 0.43, 0.53, 0.34, 0.84)
)

# Plot
ggplot(data3, aes(x = Estimator)) +
  geom_bar(aes(y = Coverage, fill = Approach), 
           stat = "identity", position = "dodge") +
  geom_text(aes(y = Coverage, label = round(Coverage, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  geom_line(aes(y = Bias * (1 / 200), group = 1), color = "black", size = 1) + 
  geom_point(aes(y = Bias * (1 / 200)), color = "black", size = 2) + 
  geom_text(aes(y = Bias * (1 / 200), label = round(Bias, 3)), 
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  scale_y_continuous(
    name = "Coverage",
    limits = c(0, 1),  # Coverage:(0-1)
    sec.axis = sec_axis(~ . * (200 / 1), name = "Bias",
                        breaks = seq(0, 200, by = 50))  # Bias:(0-200)
  ) +
  labs(title = 
         "Comparison of Bias and Coverage for Different Estimation Approaches")+
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#################################################################
# Figure 29:
# data
data4 <- data.frame(
  Approach = c(rep("Design-based approach", 1), 
               rep("Model-assisted approach", 2), 
               rep("Model-based approach", 4)),
  Estimator = c("SRS estimator", 
                "Single regression estimator", 
                "Multiple regression estimator", 
                "LM", "LM with interaction", "GLM", "GAM"),
  Bias = c(33.199, 11.879, 15.968, 16.393, 26.334, 21.517, 14.157),
  Coverage = c(0.24, 0.64, 0.54, 0.46, 0.47, 0.43, 0.80)
)

# plot
ggplot(data4, aes(x = Estimator)) +
  geom_bar(aes(y = Coverage, fill = Approach),
           stat = "identity", position = "dodge") +
  geom_text(aes(y = Coverage, label = round(Coverage, 2)), 
            position = position_dodge(width = 0.9), vjust = -0.5) +  
  geom_line(aes(y = Bias * (1 / 40), group = 1), color = "black", size = 1) + 
  geom_point(aes(y = Bias * (1 / 40)), color = "black", size = 2) + 
  geom_text(aes(y = Bias * (1 / 40), label = round(Bias, 3)), 
            position = position_dodge(width = 0.9), vjust = -0.5) + 
  scale_y_continuous(
    name = "Coverage",
    limits = c(0, 1),  # Coverage :(0-1)
    sec.axis = sec_axis(~ . * (40 / 1), name = "Bias",
                        breaks = seq(0, 40, by = 10))  # Bias:(0-40)
  ) +
  labs(title =
        "Comparison of Bias and Coverage for Different Estimation Approaches") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
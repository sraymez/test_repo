## Map making for SENZOR


# Environment set up ----

# Libraries
{
    library(pacman)
    p_load(mapview, leaflet, MODIStsp, raster, terra, sf, geodata, tmap, tidyverse)
}

# Shapefile data ----

# Africa
afr <- st_read(here("Shapefiles", "afr_g2014_2013_0.shp"))

# Gambia
gm_adm0 <- st_read(here("Shapefiles", "gmb_admbnda_adm0_ndma_20220901.shp"))
gm_adm1 <- st_read(here("Shapefiles", "gmb_admbnda_adm1_ndma_20220901.shp"))
gm_adm2 <- st_read(here("Shapefiles", "gmb_admbnda_adm2_ndma_20220901.shp"))

# Nigeria
ng_adm0 <- st_read(here("Shapefiles", "gadm41_NGA_0.shp"))
ng_adm1 <- st_read(here("Shapefiles", "gadm41_NGA_1.shp"))
ng_adm2 <- st_read(here("Shapefiles", "gadm41_NGA_2.shp"))

# Environmental road <- st_read(here('Shapefiles', 'GMB_roads.shp')) waterA <-
# st_read(here('Shapefiles', 'GMB_water_areas_dcw.shp')) waterL <- st_read(here('Shapefiles',
# 'GMB_water_lines_dcw.shp'))

# Raster data ----

# Path for storing raster
options(geodata_default_path = "C:/Users/SantiagoRaymentGomez/OneDrive - London School of Hygiene and Tropical Medicine/OneZoo PhD/Data analysis/Raster")

# Saved rasters
pop <- raster(here("Raster", "gmb_pop.grd"))  # DIVA-GIS
landcover <- raster(here("Raster", "gmb_cov.grd"))  # DIVA-GIS

# World pop rasters elev <- elevation_30s(country = 'GMB', mask = T) prec <-
# worldclim_country(country = 'GMB', var = 'prec', mask = TRUE)

# CRS alignment
st_crs(gm_adm0)  # epsg 4326
crs(landcover)  # epsg 9122

gmb_crs <- st_crs(4326)
crs(landcover) <- st_crs(gmb_crs)$wkt
crs(pop) <- st_crs(gmb_crs)$wkt

landcover <- mask(landcover, gm_adm0)
pop <- mask(pop, adm0)
prec <- mask(prec, adm0)

# Quick plots
admPlot <- tm_shape(adm1) + tm_polygons("ADM1_EN", title = "Admin 1 boundries", palette = "Spectral") +
    tm_layout(frame = F, legend.outside = T)

popPlot <- tm_shape(pop) + tm_raster(style = "cont", palette = "Spectral", title = "Population density (km^2)") +
    tm_layout(frame = F)

landPlot <- tm_shape(landcover) + tm_raster(style = "cont", palette = "Spectral", title = "Land cover") +
    tm_layout(frame = F)
landPlot

elevPlot <- tm_shape(elev) + tm_raster(style = "cont", palette = "-Spectral", title = "Elevation (m)") +
    tm_layout(frame = F)

roads <- tm_shape(adm0) + tm_polygons(col = "white", title = "Roads") + tm_layout(frame = F) + tm_shape(road) +
    tm_lines(col = "brown", lwd = 2)

waterBody <- tm_shape(adm0) + tm_polygons(col = "white") + tm_layout(frame = F) + tm_shape(waterL) +
    tm_lines(col = "blue", lwd = 2) + tm_shape(waterA) + tm_polygons(col = "blue", title = "Inland water")

# Extracting rainy and dry months and calculating mean
names(prec)
rainyMonths <- c(6, 7, 8, 9, 10)
dryMonths <- c(1, 2, 3, 4, 5, 11, 12)

rainyPrec <- prec[[rainyMonths]]
dryPrec <- prec[[dryMonths]]

meanRainyPrec <- app(rainyPrec, mean)
meanDryPrec <- app(dryPrec, mean)

tm_shape(prec) + tm_raster(style = "cont", palette = "-Spectral")
rainyPrecPlot <- tm_shape(meanRainyPrec) + tm_raster(style = "cont", palette = "-Spectral", title = "Rainy reason precipitation (mm)") +
    tm_layout(frame = F)

dryPrecPlot <- tm_shape(meanDryPrec) + tm_raster(style = "cont", palette = "-Spectral", title = "Dry reason precipitation (mm)") +
    tm_layout(frame = F)

tmap_arrange(admPlot, popPlot, landPlot, elevPlot, rainyPrecPlot, dryPrecPlot, roads, waterBody, ncol = 3,
    nrow = 3)


# ESA World cover ----

# Create file list for all tifs
tif_files <- list.files("Raster/ESA World Cover", pattern = ".tif$", full.names = TRUE)

# option 1 Create rasters of these files
lc_rasters <- lapply(tif_files, rast)

# Convert rasters to spatraster collection
lc_rasters_sr <- sprc(lc_rasters)

lc_rasters_sr <- merge(lc_rasters)  # crashes - cant assign vector that size

plot(lc_rasters_sr)


# option 3 for loop
base_raster <- raster(tif_files[1])
for (tif_files in tif_files[-1]) {
    raster_layer <- raster(tif_files)
    base_raster <- merge(base_raster, raster_layer)
}


# option 2 with purrr
merged_raster <- reduce(map(tif_files, raster), merge)

# Study site maps ----

### Gambia ###

# Get bounding box
gmb_sites <- gm_adm2 %>%
    filter(ADM1_EN %in% c("Central River North", "Central River South"))
st_bbox(gmb_sites)

# Specify land cover modis download
MODIStsp(gui = FALSE, out_folder = "Raster/MODIS", out_folder_mod = "Raster/MODIS", selprod = "LandCover_Type_Yearly_500m (MCD12Q1)",
    bandsel = "LC1", sensor = "Terra", user = "santirg", password = "Sg09193Sg09193", start_date = "2021.12.31",
    end_date = "2022.12.31", verbose = TRUE, bbox = c(-15.46274, 13.32131, -14.44634, 13.82274), spatmeth = "bbox",
    out_format = "GTiff", compress = "LZW", out_projsel = "User Defined", output_proj = ": +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
    e_hdf = T, lel = T)

# Import data
landcover_files_gmb <- list.files("Raster/Modis/LandCover_Type_Yearly_500m_v61_GMB_site/LC1", "MCD12Q1_LC1_2022_001",
    full.names = T)
landcover_gmb <- terra::rast(landcover_files_gmb)
names(landcover_gmb) <- "landcover_gmb"

# Crop raster to spatial extent of Gambia shapefile
landcover_gmb <- mask(landcover_gmb, gm_adm0)

# Plot raster to see values of landcover - use style = 'cat' to see individual values that are
# present
range(landcover_gmb)

tm_shape(landcover_gmb) + tm_raster(style = "cat") + tm_layout(frame = F, legend.position = c(0, 0.5))

# Rename land cover classifications to categorical
temp_df_gmb <- as.data.frame(cbind(landcover_breaks = c(9, 10, 11, 12, 13, 14, 16, 17), landcover_classes = c("Savannas",
    "Grasslands", "Permanent wetlands", "Croplands", "Urban and Built-up land", "Cropland/Natural Vegetation Mosaic",
    "Barren land", "Water bodies")))

temp_df_gmb$landcover_breaks <- as.numeric(temp_df_gmb$landcover_breaks)

levels(landcover_gmb) <- temp_df_gmb

tm_shape(landcover_gmb) + tm_raster(title = "Land cover categories") + tm_layout(frame = F, legend.position = c(0.75,
    0.8))

### Nigeria ###

# Ebonyi
ebonyi_sites <- tibble(name = c("Abakaliki", "Nwenzenyi"), lat = c(6.32439309645571, 6.57904146726701),
    long = c(8.11400341219584, 8.04773151923299))

# Convert to sf object
ebonyi_sites_sf <- ebonyi_sites %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

# Filter shapefile to just ebonyi state
ebonyi <- ng_adm2 %>%
    filter(NAME_1 == "Ebonyi")

# bbox for each state
st_bbox(ebonyi)

# Specify MODIS download ebonyi
MODIStsp(gui = FALSE, out_folder = "Raster/MODIS", out_folder_mod = "Raster/MODIS", selprod = "LandCover_Type_Yearly_500m (MCD12Q1)",
    bandsel = "LC1", sensor = "Terra", user = "santirg", password = "Sg09193Sg09193", start_date = "2021.12.31",
    end_date = "2022.12.31", verbose = TRUE, bbox = c(7.525859, 5.690326, 8.444068, 6.802191), spatmeth = "bbox",
    out_format = "GTiff", compress = "LZW", out_projsel = "User Defined", output_proj = ": +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
    e_hdf = T, lel = T)

# Import data
landcover_files_ebonyi <- list.files("Raster/Modis/LandCover_Type_Yearly_500m_v61_NGA_E/LC1", "MCD12Q1_LC1_2022_001",
    full.names = T)
landcover_ebonyi <- terra::rast(landcover_files_ebonyi)
# landcover_modal <- modal(landcover_gmb)
names(landcover_ebonyi) <- "landcover_ebonyi"

# Crop raster to spatial extent of Gambia shapefile
landcover_ebonyi <- mask(landcover_ebonyi, ebonyi)

# Plot raster to see values of landcover - use style = 'cat' to see individual values that are
# present
range(landcover_ebonyi)

tm_shape(landcover_ebonyi) + tm_raster(style = "cat") + tm_layout(frame = F, legend.position = c(0, 0.5))

# Define land cover categories using: https://lpdaac.usgs.gov/documents/101/MCD12_User_Guide_V6.pdf
temp_df_ebo <- as.data.frame(cbind(landcover_breaks = c(2, 4, 5, 9, 10, 12, 13, 14, 17), landcover_classes = c("Evergreen Broadleaf Forests",
    "Deciduous Broadleaf Forests", "Mixed Forests", "Savannas", "Grasslands", "Croplands", "Urban and Built-up land",
    "Cropland/Natural Vegetation Mosaic", "Water bodies")))

temp_df_ebo$landcover_breaks <- as.numeric(temp_df_ebo$landcover_breaks)

levels(landcover_ebonyi) <- temp_df_ebo

tm_shape(landcover_ebonyi) + tm_raster(title = "Land cover categories") + tm_layout(frame = F, legend.position = c(0.8,
    0), title = "Ebonyi Study Sites") + tm_shape(ebonyi) + tm_polygons("white", alpha = 0.1) + tm_shape(ebonyi_sites_sf) +
    tm_dots(col = "name", size = 0.5, shape = 24, title = "Study Site", palette = c("darkred", "darkblue"),
        border.col = NA)


# Ondo
ondo_sites <- tibble(name = c("Owo", "Ogbese"), lat = c(7.19890768275836, 7.25810900972096), long = c(5.59326558605457,
    5.37083337447413))

# Convert to sf object
ondo_sites_sf <- ondo_sites %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

# Filter shapefile to just ondo state
ondo <- ng_adm2 %>%
    filter(NAME_1 == "Ondo")

# Get bbox for ondo
st_bbox(ondo)

# Specify MODIS download ebonyi
MODIStsp(gui = FALSE, out_folder = "Raster/MODIS", out_folder_mod = "Raster/MODIS", selprod = "LandCover_Type_Yearly_500m (MCD12Q1)",
    bandsel = "LC1", sensor = "Terra", user = "santirg", password = "Sg09193Sg09193", start_date = "2021.12.31",
    end_date = "2022.12.31", verbose = TRUE, bbox = c(4.35234, 5.85625, 6.028376, 7.770609), spatmeth = "bbox",
    out_format = "GTiff", compress = "LZW", out_projsel = "User Defined", output_proj = ": +init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0",
    e_hdf = T, lel = T)

# Import data
landcover_files_ondo <- list.files("Raster/Modis/LandCover_Type_Yearly_500m_v61_NGA_O/LC1", "MCD12Q1_LC1_2022_001",
    full.names = T)
landcover_ondo <- terra::rast(landcover_files_ondo)
names(landcover_ondo) <- "landcover_ondo"

# Crop raster to spatial extent of Gambia shapefile
landcover_ondo <- mask(landcover_ondo, ondo)

# Plot raster to see values of landcover - use style = 'cat' to see individual values that are
# present
range(landcover_ondo)

tm_shape(landcover_ondo) + tm_raster(style = "cat") + tm_layout(frame = F, legend.position = c(0, 0.5))

# Define land cover categories
temp_df_ond <- as.data.frame(cbind(landcover_breaks = c(2, 4, 8, 9, 10, 11, 12, 13, 14, 16, 17), landcover_classes = c("Evergreen Broadleaf Forests",
    "Deciduous Broadleaf Forests", "Woody Savannas", "Savannas", "Grasslands", "Permanent Wetlands",
    "Croplands", "Urban and Built-up Lands", "Cropland/Natural Vegetation Mosaics", "Barren", "Water Bodies")))

temp_df_ond$landcover_breaks <- as.numeric(temp_df_ond$landcover_breaks)

levels(landcover_ondo) <- temp_df_ond

tm_shape(landcover_ondo) + tm_raster(title = "Land cover categories") + tm_layout(frame = F, legend.position = c(0.8,
    0), title = "Ondo Study Sites") + tm_shape(ondo) + tm_polygons("white", alpha = 0.1) + tm_shape(ondo_sites_sf) +
    tm_dots(col = "name", size = 0.5, shape = 24, title = "Study Site", palette = c("darkred", "darkblue"),
        border.col = NA)

# Protocol study site maps ----

# Africa
tm_shape(afr %>%
    filter(ADM0_NAME %in% c("Burkina Faso", "Benin", "Cabo Verde", "C\xf4te d'Ivoire", "Gambia", "Ghana",
        "Guinea", "Guinea-Bissau", "Liberia", "Mali", "Mauritania", "Niger", "Nigeria", "Senegal", "Sierra Leone",
        "Togo"))) + tm_polygons("white", alpha = 0.1, title = "West Africa") + tm_shape(afr %>%
    filter(ADM0_NAME %in% c("Gambia", "Nigeria"))) + tm_polygons(col = "ADM0_NAME", palette = c("#084887",
    "#909CC2"), border.col = "black", title = "SENZOR Countries") + tm_layout(frame = F, legend.title.size = 1.2,
    legend.text.size = 1)

# Gambia
gmb_sites <- tibble(name = c("Sambel Kunda", "Njoben"), lat = c(13.6649989258516, 13.4829019593275),
    long = c(-14.9760531866003, -14.9448737140685))

gmb_sites_sf <- gmb_sites %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

tm_shape(gm_adm1) + tm_polygons("white", alpha = 0.1) + tm_shape(gm_adm2 %>%
    filter(ADM1_EN %in% c("Central River North", "Central River South"))) + tm_polygons(col = "ADM1_EN",
    palette = c("#084887", "#909CC2"), border.col = "black", title = "Region") + tm_shape(gmb_sites_sf) +
    tm_dots(col = "name", size = 0.5, shape = 24, title = "Study Site", palette = c("#e65100", "#ffc107"),
        border.col = "black") + tm_layout(frame = F, legend.title.size = 1.2, legend.text.size = 1) +
    tm_legend(legend.position = c(0, 0.75))


# Nigeria
ebonyi_sites <- tibble(name = c("Abakaliki", "Nwenzenyi"), lat = c(6.32439309645571, 6.57904146726701),
    long = c(8.11400341219584, 8.04773151923299))

ebonyi_sites_sf <- ebonyi_sites %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

# Ondo
ondo_sites <- tibble(name = c("Owo", "Ogbese"), lat = c(7.19890768275836, 7.25810900972096), long = c(5.59326558605457,
    5.37083337447413))

ondo_sites_sf <- ondo_sites %>%
    st_as_sf(coords = c("long", "lat"), crs = 4326)

tm_shape(ng_adm1) + tm_polygons("white", alpha = 0.1) + tm_shape(ng_adm2 %>%
    filter(NAME_1 %in% c("Ebonyi", "Ondo"))) + tm_polygons(col = "NAME_1", palette = c("#084887", "#909CC2"),
    border.col = "black", title = "State") + tm_shape(ebonyi_sites_sf) + tm_dots(col = "name", size = 0.3,
    shape = 24, title = "Ebonyi Sites", palette = c("#e65100", "#ffc107"), border.col = "black") + tm_shape(ondo_sites_sf) +
    tm_dots(col = "name", size = 0.3, shape = 24, title = "Ondo Sites", palette = c("#e65100", "#ffc107"),
        border.col = "black") + tm_layout(frame = F, legend.title.size = 1.2, legend.text.size = 1) +
    tm_legend(legend.position = c(0.8, 0))

# End ----

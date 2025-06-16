
#########################################################
# script for past land abandonment
#########################################################
install.packages("spData")
library(spData)

# Load the included world dataset
data(world, package = "spData")

#potapov script
list <- list.files("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/potapov/", full.names=T)
crop.change <- rast(list[1:7])

crop.stable <- crop.change[[5]]
crop.loss <- crop.change[[7]]

crop.stable[crop.stable == 0] <- NA
crop.loss[crop.loss == 0] <- NA

# sample values
n_samples <- 20000

#sample datasets
stable.df <- as.data.frame(crop.stable, xy = TRUE, cells = TRUE, na.rm = TRUE)
sampled_rows.stable <- sample(nrow(stable.df), size = n_samples, prob = stable.df$Global_cropland_3km_2019 , replace = F)
stable_cropland <- stable.df[sampled_rows.stable, ]

sampled_rows.stable_unwt <- sample(nrow(stable.df), size = n_samples , replace = F)
stable_cropland_unwt <- stable.df[sampled_rows.stable_unwt, ]

# sample datasets
loss.df <- as.data.frame(crop.loss, xy = TRUE, cells = TRUE, na.rm = TRUE)
sampled_rows <- sample(nrow(loss.df), size = n_samples, prob = loss.df$Global_cropland_3km_netloss, replace = F)
past_lost_cropland <- loss.df[sampled_rows, ]

sampled_rows_unwt <- sample(nrow(loss.df), size = n_samples, prob = loss.df$Global_cropland_3km_netloss, replace = F)
past_lost_cropland_unwt <- loss.df[sampled_rows_unwt, ]


#########################################################
# script for projected future land abandonment
#########################################################
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
map1 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/abandonment data settineri/data/data/shiny_app/global/ssp1_abandonment_global_50km.tif")
map2 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/abandonment data settineri/data/data/shiny_app/global/ssp2_abandonment_global_50km.tif")
map3 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/abandonment data settineri/data/data/shiny_app/global/ssp3_abandonment_global_50km.tif")
map4 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/abandonment data settineri/data/data/shiny_app/global/ssp4_abandonment_global_50km.tif")
map5 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/abandonment data settineri/data/data/shiny_app/global/ssp5_abandonment_global_50km.tif")

all <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/abandonment data settineri/data/data/shiny_app/global/ssp_all_abandonment_global_50km.tif")

r_stack <- c(map1,map2,map3,map4,map5)  # Stack of 5 rasters
plot(mean(r_stack))
plot(all)

SSP1_abandoned <- map1
mean_abandoned <- mean(r_stack)



# future abandonment
future.loss.df <- as.data.frame(mean_abandoned, xy = TRUE, cells = TRUE, na.rm = TRUE)
sampled_rows <- sample(nrow(future.loss.df), size = n_samples, prob = future.loss.df$global_PFT_2015, replace = F)
future_lost_crop <- future.loss.df[sampled_rows, ]

sampled_rows <- sample(nrow(future.loss.df), size = n_samples, replace = F)
future_lost_crop_unwt <- future.loss.df[sampled_rows, ]

hist(future_lost_crop$mean)
hist(future_lost_crop_unwt$mean)


# dataset for density plots

stable_cropland
past_lost_cropland
future_lost_crop

stable_cropland_unwt
past_lost_cropland_unwt
future_lost_crop_unwt

2480/3

#########################################################
# biodiversity value
#########################################################

# natures contribution to people
ncp <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/Neugarten_natcap/NCP_only_2km_sum.tif")
ncp <- project(ncp, crop.stable, method = "bilinear")
ncp.ma <- project(ncp, mean_abandoned, method = "bilinear")


stable_crop <- extract(ncp, stable_cropland[2:3])
past_loss <- extract(ncp, past_lost_cropland[2:3])
future_loss <- extract(ncp.ma, future_lost_crop[2:3])


tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/NCP.tif", width = 1000, height = 1000, res = 300)
par(mar = c(3,4,1,1))
par(cex = 0.8)       # scale everything inside the plot
plot(density(stable_crop$NCP_only_2km_sum, na.rm=T), col="green3", ylim=c(0,0.132), xlim=c(0,19), lwd=2.3, xlab=NA, las=1, main="", xaxt='n', ylab=NA)
lines(density(past_loss$NCP_only_2km_sum, na.rm=T), col="red", lwd=2.3, lty=5)
lines(density(future_loss$NCP_only_2km_sum, na.rm=T), col="blue", lwd=2.3, lty=4)
axis(side = 1, mgp = c(2, 0.5, 0))  # move labels closer (smaller second value)
mtext("Nature's Contribution to People (No. of scenarios)", side=1, line=1.5, cex=0.8)
mtext("Density", side=2, line=2.75, cex=0.8)


#legend(x = 9.6, y = 0.12,
#       legend = c("Historic abandonment 2003-2019", "Stable croplands in 2019", "Projected abandonment to 2050"),
#       col = c("red", "green", "blue"),
#       lty=c(1,5,4),
#       border = "black",
#       bty = "n",
#       cex = 0.9,
#       lwd=2.3,
#       xjust = 0,  # left align text at x position
#       yjust = 1) 


dev.off()

install.packages("kSamples")
library(kSamples)
ad.test(stable_crop$NCP_only_2km_sum, past_loss$NCP_only_2km_sum, future_loss$NCP_only_2km_sum)


#########################################################
# carbon value
#########################################################


carbon <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/carbon_reforestable/young_forest_sequestration_rate_Griscom_extent.tif")
carbon <- project(carbon, crop.stable)
carbon <- resample(carbon, crop.stable, method="bilinear")
carbon.ma <- resample(carbon, mean_abandoned, method="bilinear")


stable_crop <- extract(carbon, stable_cropland[2:3])
past_loss <- extract(carbon, past_lost_cropland[2:3])
future_loss <- extract(carbon, future_lost_crop[2:3])


tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/Carbon.tif", width = 1000, height = 1000, res = 300)
par(mar = c(3,4,1,1))
par(cex = 0.8)       # scale everything inside the plot
plot(density(stable_crop$young_forest_sequestration_rate_Griscom_extent, na.rm=T), col="green3", ylim=c(0,1.28), xlim=c(0,6.7), lwd=2.3, xlab=NA, las=1, main="", xaxt='n', ylab=NA)
lines(density(past_loss$young_forest_sequestration_rate_Griscom_extent, na.rm=T), col="red", lwd=2.3, lty=5)
lines(density(future_loss$young_forest_sequestration_rate_Griscom_extent, na.rm=T), col="blue", lwd=2.3, lty=4)
axis(side = 1, mgp = c(2, 0.5, 0))  # move labels closer (smaller second value)
mtext("Mg carbon/ha/yr", side=1, line=1.5, cex=0.8)
mtext("Density", side=2, line=2.75, cex=0.8)

dev.off()


#########################################################
# jung biodiversity value
#########################################################

jung <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/jung_biodiversity/BiodiversityOnly/BiodiversityOnly/10km/minshort_speciestargets_biome.id__esh10km_repruns10_ranked.tif")
jung <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/jung_biodiversity/BiodiversityOnly/BiodiversityOnly/10km/minshort_speciestargets_esh10km_repruns10_ranked.tif")
plot(jung)

jung <- project(jung, crop.stable)
jung <- resample(jung, crop.stable, method="bilinear")
jung.ma <- resample(jung, mean_abandoned, method="bilinear")


stable_crop <- extract(jung, stable_cropland[2:3])
past_loss <- extract(jung, past_lost_cropland[2:3])
future_loss <- extract(jung.ma, future_lost_crop[2:3])



tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/JUNG.tif", width = 1000, height = 1000, res = 300)
par(mar = c(3,4,1,1))
par(cex = 0.8)  
plot(density(stable_crop$minshort_speciestargets_esh10km_repruns10_ranked, na.rm=T), col="green3", lwd=2.3, xlab=NA, las=1, main="", xaxt='n', ylab=NA)
lines(density(past_loss$minshort_speciestargets_esh10km_repruns10_ranked, na.rm=T), col="red", lwd=2.3, lty=5)
lines(density(future_loss$minshort_speciestargets_esh10km_repruns10_ranked, na.rm=T), col="blue", lwd=2.3, lty=4)
axis(side = 1, mgp = c(2, 0.5, 0))  # move labels closer (smaller second value)
mtext("Ranked importance for biodiversity", side=1, line=1.5, cex=0.8)
mtext("Density", side=2, line=2.75, cex=0.8)

dev.off()


#################################################
# proximity to PAs
install.packages("wdpar")
library(wdpar)

shape1 <- read_sf(dsn = "C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/distance_pa/WDPA_Jun2025_Public_shp/WDPA_Jun2025_Public_shp_0", layer = "WDPA_Jun2025_Public_shp-polygons")
shape2 <- read_sf(dsn = "C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/distance_pa/WDPA_Jun2025_Public_shp/WDPA_Jun2025_Public_shp_1", layer = "WDPA_Jun2025_Public_shp-polygons")
shape3 <- read_sf(dsn = "C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/distance_pa/WDPA_Jun2025_Public_shp/WDPA_Jun2025_Public_shp_2", layer = "WDPA_Jun2025_Public_shp-polygons")

# load and combine Algeria WDPA protected areas
pa <- rbind(shape1,shape2,shape3)
pa_sf_small <- pa[c("geometry")]  # or even just geometry: pa_sf["geometry"]
pa_sf_small <- st_make_valid(pa_sf_small)
sf::sf_use_s2(FALSE)
sf_simple <- st_simplify(pa_sf_small, dTolerance = 0.01, preserveTopology = TRUE)

res_deg <- 0.1

r_template <- rast(
  ncols = 360/res_deg,
  nrows = 180/res_deg,
  xmin = -180, xmax = 180,
  ymin = -90, ymax = 90,
  crs = "EPSG:4326"
)
r_dist <- distance(r_template, sf_simple)

r_metric <- project(r_template, "EPSG:3857")
pa_metric <- project(sf_simple, r_metric)



pa_sf <- pa["WDPAID "]

pa_sf <- st_simplify(pa, dTolerance = 0.01)

# Replace with your actual file path
wdpa <- vect(pa)

# Keep only terrestrial areas (MARINE == "0") and designated
wdpa <- wdpa[wdpa$MARINE == "0" & wdpa$STATUS == "Designated", ]


# Rasterize PAs to template
pa_raster <- rasterize(wdpa, r_template, field=1, background=0, touches=TRUE)
dist_to_pa <- distance(pa_raster)



#########################################################
# landex value
#########################################################

# secure tenure rights 

library(rnaturalearth)
library(dplyr)
ilc_data <- read.csv("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/landex/ilc_data.csv")
colSums(ilc_data[,2:6])
head(ilc_data)
colSums(ilc_data != 0)
# Step 1: Get world country polygons
world <- ne_countries(scale = "medium", returnclass = "sf")

# Step 2: Join your data to the world polygons
world <- world %>%
  left_join(ilc_data, by = c("name" = "Country"))

# Step 3: Convert to SpatVector (terra format)
world_vect <- vect(world)
plot(world_vect, col=values(world_vect)$Overall)

# Step 4: Create a global raster template
template <- rast(res = 0.2, xmin = -180, xmax = 180, ymin = -90, ymax = 90)  # 1° resolution

# Step 5: Rasterize the 'value' column from polygons
r <- rasterize(world_vect, template, field = "Overall.Score")

# Plot result
plot(r, main = "Country-level Value Map")

stable_crop <- extract(r, stable_cropland[2:3])
past_loss <- extract(r, past_lost_cropland[2:3])
future_loss <- extract(r, future_lost_crop[2:3])

stable_crop <- stable_crop[complete.cases(stable_crop) & !apply(stable_crop == 0, 1, any), ]
past_loss <- past_loss[complete.cases(past_loss) & !apply(past_loss == 0, 1, any), ]
future_loss <- future_loss[complete.cases(future_loss) & !apply(future_loss == 0, 1, any), ]

tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/LANDex.tif", width = 1000, height = 1000, res = 300)
par(mar = c(3,4,1,1))
par(cex = 0.8)  
plot(density(stable_crop$Overall.Score, na.rm=T), col="green3", lwd=2.3, xlab=NA, las=1, main="", xaxt='n', ylab="")
lines(density(past_loss$Overall.Score, na.rm=T), col="red", lwd=2.3, lty=5)
lines(density(future_loss$Overall.Score, na.rm=T), col="blue", lwd=2.3, lty=4)
axis(side = 1, mgp = c(2, 0.5, 0))  # move labels closer (smaller second value)
mtext("Global Land Governance Index", side=1, line=1.5, cex=0.8)
mtext("Density", side=2, line=2.75, cex=0.8)

dev.off()



#########################################################
# yield and production 46 crops GAEZ
#########################################################
production <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/spam_yield/46_crops_standarized_production.tif")
production <- resample(production, crop.stable, method="bilinear")
production <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/spam_yield/46_crops_standarized_production.tif")
production.ma <- resample(production, mean_abandoned, method="bilinear")


stable_crop <- extract(production, stable_cropland[2:3])
past_loss <- extract(production, past_lost_cropland[2:3])
future_loss <- extract(production.ma, future_lost_crop[2:3])

tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/production.tif", width = 1000, height = 1000, res = 300)
par(mar = c(3,4,1,1))
par(cex = 0.8) 
plot(density(stable_crop$sum, na.rm=T), col="green3", lwd=2.3, xlab=NA, las=1, main="", xaxt='n', ylim=c(0,0.97), ylab="", xlim=c(0,6))
lines(density(past_loss$sum, na.rm=T), col="red", lwd=2.3, lty=5)
lines(density(future_loss$sum, na.rm=T), col="blue", lwd=2.3, lty=4)
axis(side = 1, mgp = c(2, 0.5, 0))  # move labels closer (smaller second value)
mtext("Standardized Production (46 crop species)", side=1, line=1.5, cex=0.8)
mtext("Density", side=2, line=2.75, cex=0.8)
dev.off()




#########################
# yield
########################

yield <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/spam_yield/46_crops_standarized_yields.tif")
yield <- resample(yield, crop.stable, method="bilinear")
yield <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/spam_yield/46_crops_standarized_yields.tif")
yield.ma <- resample(yield, mean_abandoned, method="bilinear")

stable_crop <- extract(yield, stable_cropland[2:3])
past_loss <- extract(yield, past_lost_cropland[2:3])
future_loss <- extract(yield.ma, future_lost_crop[2:3])

tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/yield.tif", width = 1000, height = 1000, res = 300)
par(mar = c(3,4,1,1))
par(cex = 0.8) 
plot(density(stable_crop$sum, na.rm=T), col="green3", lwd=2.3, xlab=NA, las=1, main="", xaxt='n', ylim=c(0,0.18), ylab="")
lines(density(past_loss$sum, na.rm=T), col="red", lwd=2.3, lty=5)
lines(density(future_loss$sum, na.rm=T), col="blue", lwd=2.3, lty=4)
axis(side = 1, mgp = c(2, 0.5, 0))  # move labels closer (smaller second value)
mtext("Standarized Yield (46 crops)", side=1, line=1.5, cex=0.8)
mtext("Density", side=2, line=2.75, cex=0.8)
dev.off()





#########################################################
# zabel ag suitbility
#########################################################
suitbility.current <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/Zabel_suitibility/hist/overall_suitability_subset_1to17.bil")
suitbility.current <- resample(suitbility.current, crop.stable, method="bilinear")
suitbility.current <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/Zabel_suitibility/hist/overall_suitability_subset_1to17.bil")
suitbility.current.ma <- resample(suitbility.current, mean_abandoned, method="bilinear")

#suitbility.future <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/Zabel_suitibility/future_2040-69rcp2_6/overall_suitability_subset_1to17.bil")

stable_crop <- extract(suitbility.current, stable_cropland[2:3])
past_loss <- extract(suitbility.current, past_lost_cropland[2:3])
future_loss <- extract(suitbility.current.ma, future_lost_crop[2:3])

stable_crop <- stable_crop[stable_crop$overall_suitability_subset_1to17 != 0, ]
past_loss <- past_loss[past_loss$overall_suitability_subset_1to17 != 0, ]
future_loss <- future_loss[future_loss$overall_suitability_subset_1to17 != 0, ]

tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure2/current.crop.suit.tif", width = 1000, height = 1000, res = 300)
par(mar = c(3,4,1,1))
par(cex = 0.8) 
plot(density(stable_crop$overall_suitability_subset_1to17, na.rm=T), col="green3", lwd=2.3, xlab=NA, las=1, main="", xaxt='n', ylim=c(0,0.033), xlim=c(0,100), ylab="")
lines(density(past_loss$overall_suitability_subset_1to17, na.rm=T), col="red", lwd=2.3, lty=5)
lines(density(future_loss$overall_suitability_subset_1to17, na.rm=T), col="blue", lwd=2.3, lty=4)
axis(side = 1, mgp = c(2, 0.5, 0))  # move labels closer (smaller second value)
mtext("Current Cropland Suitibility", side=1, line=1.5, cex=0.8)
mtext("Density", side=2, line=2.75, cex=0.8)
dev.off()





##################################################################
# plotting
####################################################################

library(sf)

crop.stable
crop.loss


all <-crop(all,crop.stable)
all <-resample(all,crop.stable)

# Get min and max values (excluding NAs)
r_min <- global(all, "min", na.rm = TRUE)[1]
r_max <- global(all, "max", na.rm = TRUE)[1]

# Rescale raster values to 0-100
all_rescaled <- ((all - r_min$min) / (r_max$max - r_min$min)) * 100


# alternative method
# Normalize so each pixel sums to 1
r_sum <- crop.stable + crop.loss + all_rescaled
r1n <- crop.loss / r_sum
r2n <- crop.stable / r_sum
r3n <- all_rescaled / r_sum

# Combine into 3-band RGB raster
rgb_stack <- c(r1n, r2n, r3n)


# Reduce resolution by factor of 2 (or 3, etc.)
rgb_smooth <- aggregate(rgb_stack, fact = 10, fun = mean, na.rm = TRUE)
rgb_smooth2 <- project(rgb_smooth, "ESRI:54009")  # Mollweide
world2 <- st_transform(world, crs = "ESRI:54009")


tiff("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/my_ternary_map.tif", width = 3000, height = 2000, res = 300, compression = "lzw")
par(mar = c(4,5,2,1))
plotRGB(rgb_smooth2, r = 1, g = 2, b = 3, scale = 1, axes = FALSE,
        colNA = "gray97", xlim=c(-12634266, 16515888))
lines(world2)


legend(x = -3600000, y = -4350000,
       legend = c("Historic abandonment 2003-2019", "Stable croplands in 2019", "Projected abandonment to 2050"),
       fill = c("red", "green3", "blue"),
       border = "black",
       bty = "n",
       cex = 1,
       xjust = 0,  # left align text at x position
       yjust = 1) 
dev.off()

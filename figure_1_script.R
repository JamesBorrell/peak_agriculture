
library(dplyr)
library(terra)


########################################################
# script for growth in land use extensification through time
########################################################
# HYDE 3.3
########################################################

cropland_r <- read.table("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/hyde_plot/v3.3/cropland_r_base.txt", quote="/")
crp <- as.data.frame(t(cropland_r))
crp <- crp[-1,]

grazing_r <- read.table("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/hyde_plot/v3.3/grazing_r_base.txt", quote="/")
grz <- as.data.frame(t(grazing_r))
grz <- grz[-1,]

pop_r <- read.table("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/hyde_plot/popc_r.txt", quote="/")
pop_r <- as.data.frame(t(pop_r))
pop_r <- pop_r[-1,]
pop_r <- pop_r[-127,]

dat <- cbind(crp[,1:4],grz[,2:4], pop_r[,2])
colnames(dat) <- c("Year", "Cropland", "Crop_upper", "Crop_lower", "Grazing", "Grazing_upper", "Grazing_lower", "Population")


dat <- dat %>%
  mutate(across(where(is.character), as.numeric))

# organise
dat$total <- c(dat$Cropland + dat$Grazing)
dat$total.upper <- c(dat$Crop_upper + dat$Grazing_upper)
dat$total.lower <- c(dat$Crop_lower + dat$Grazing_lower)

dat$percapita <- dat$total / dat$Population 
options(scipen = 999)
dat$total <- round(dat$total)
dat$Cropland <- round(dat$Cropland)
dat$Grazing <- round(dat$Grazing)

t <- dat$Population / dat$total

dat$percapita*100 # means Ha PER PERSON

##################################
# LH2 dataset
##################################

# historical
historical <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/states.nc")
names(historical)

n_layers <- nlyr(historical)  # total number of layers
group_size <- 1166
groups <- split(1:n_layers, ceiling((1:n_layers)/group_size))
historical_split <- lapply(groups, function(i) historical[[i]])

historical_split_pasture <- rast(historical_split[c(12)]) + rast(historical_split[c(11)])
historical_split_crop <- rast(historical_split[c(6)]) + rast(historical_split[c(7)]) + rast(historical_split[c(8)]) + rast(historical_split[c(9)]) + rast(historical_split[c(10)])

a <- cellSize(historical_split_crop, unit = "km")  # use "km" if you want square kilometers

pasture_sum <- global((historical_split_pasture * a), fun = "sum", na.rm = TRUE)
crop_sum <- global((historical_split_crop * a), fun = "sum", na.rm = TRUE)
total_sum <- pasture_sum + crop_sum

total_historic <- cbind.data.frame(seq(850,2015,1), total_sum$sum)
colnames(total_historic) <- c("Year", "Total")

write.csv(total_historic, "C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/ag_land_historic_luh2.csv")
total_historic <- read.csv("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/ag_land_historic_luh2.csv")

############
# future
############

SSP1_2.6 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-f_gn_2015-2100.nc")
SSP4_3.4 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-GCAM-ssp434-2-1-f_gn_2015-2100.nc")
SSP2_4.5 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MESSAGE-ssp245-2-1-f_gn_2015-2100.nc")
SSP4_6.0 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-GCAM-ssp460-2-1-f_gn_2015-2100.nc")
SSP3_7.0 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-AIM-ssp370-2-1-f_gn_2015-2100.nc")
SSP5_8.5 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-f_gn_2015-2100.nc")

n_layers <- nlyr(SSP1_2.6)  # total number of layers
group_size <- 86
groups <- split(1:n_layers, ceiling((1:n_layers)/group_size))

# Create a list of SpatRaster subsets
r_list_SSP1_2.6 <- lapply(groups, function(i) SSP1_2.6[[i]])
r_list_SSP4_3.4 <- lapply(groups, function(i) SSP4_3.4[[i]])
r_list_SSP2_4.5 <- lapply(groups, function(i) SSP2_4.5[[i]])
r_list_SSP4_6.0 <- lapply(groups, function(i) SSP4_6.0[[i]])
r_list_SSP3_7.0 <- lapply(groups, function(i) SSP3_7.0[[i]])
r_list_SSP5_8.5 <- lapply(groups, function(i) SSP5_8.5[[i]])

# pasture
pastureSSP1_2.6 <- rast(r_list_SSP1_2.6[c(12)]) + rast(r_list_SSP1_2.6[c(11)])
pastureSSP4_3.4 <- rast(r_list_SSP4_3.4[c(12)]) + rast(r_list_SSP4_3.4[c(11)])
pastureSSP2_4.5 <- rast(r_list_SSP2_4.5[c(12)]) + rast(r_list_SSP2_4.5[c(11)])
pastureSSP4_6.0 <- rast(r_list_SSP4_6.0[c(12)]) + rast(r_list_SSP4_6.0[c(11)])
pastureSSP3_7.0 <- rast(r_list_SSP3_7.0[c(12)]) + rast(r_list_SSP3_7.0[c(11)])
pastureSSP5_8.5 <- rast(r_list_SSP5_8.5[c(12)]) + rast(r_list_SSP5_8.5[c(11)])

# cropland
cropSSP1_2.6 <- rast(r_list_SSP1_2.6[c(6)]) + rast(r_list_SSP1_2.6[c(7)]) + rast(r_list_SSP1_2.6[c(8)]) + rast(r_list_SSP1_2.6[c(9)]) + rast(r_list_SSP1_2.6[c(10)])
cropSSP4_3.4 <- rast(r_list_SSP4_3.4[c(6)]) + rast(r_list_SSP4_3.4[c(7)]) + rast(r_list_SSP4_3.4[c(8)]) + rast(r_list_SSP4_3.4[c(9)]) + rast(r_list_SSP4_3.4[c(10)])
cropSSP2_4.5 <- rast(r_list_SSP2_4.5[c(6)]) + rast(r_list_SSP2_4.5[c(7)]) + rast(r_list_SSP2_4.5[c(8)]) + rast(r_list_SSP2_4.5[c(9)]) + rast(r_list_SSP2_4.5[c(10)])
cropSSP4_6.0 <- rast(r_list_SSP4_6.0[c(6)]) + rast(r_list_SSP4_6.0[c(7)]) + rast(r_list_SSP4_6.0[c(8)]) + rast(r_list_SSP4_6.0[c(9)]) + rast(r_list_SSP4_6.0[c(10)])
cropSSP3_7.0 <- rast(r_list_SSP3_7.0[c(6)]) + rast(r_list_SSP3_7.0[c(7)]) + rast(r_list_SSP3_7.0[c(8)]) + rast(r_list_SSP3_7.0[c(9)]) + rast(r_list_SSP3_7.0[c(10)])
cropSSP5_8.5 <- rast(r_list_SSP5_8.5[c(6)]) + rast(r_list_SSP5_8.5[c(7)]) + rast(r_list_SSP5_8.5[c(8)]) + rast(r_list_SSP5_8.5[c(9)]) + rast(r_list_SSP5_8.5[c(10)])


# AgGREGATE
a <- cellSize(cropSSP1_2.6, unit = "km")  # use "km" if you want square kilometers

pastureSSP1_2.6_sum <- global((pastureSSP1_2.6 * a), fun = "sum", na.rm = TRUE)
pastureSSP4_3.4_sum <- global((pastureSSP4_3.4 * a), fun = "sum", na.rm = TRUE)
pastureSSP2_4.5_sum <- global((pastureSSP2_4.5 * a), fun = "sum", na.rm = TRUE)
pastureSSP4_6.0_sum <- global((pastureSSP4_6.0 * a), fun = "sum", na.rm = TRUE)
pastureSSP3_7.0_sum <- global((pastureSSP3_7.0 * a), fun = "sum", na.rm = TRUE)
pastureSSP5_8.5_sum <- global((pastureSSP5_8.5 * a), fun = "sum", na.rm = TRUE)

cropSSP1_2.6_sum <- global((cropSSP1_2.6 * a), fun = "sum", na.rm = TRUE)
cropSSP4_3.4_sum <- global((cropSSP4_3.4 * a), fun = "sum", na.rm = TRUE)
cropSSP2_4.5_sum <- global((cropSSP2_4.5 * a), fun = "sum", na.rm = TRUE)
cropSSP4_6.0_sum <- global((cropSSP4_6.0 * a), fun = "sum", na.rm = TRUE)
cropSSP3_7.0_sum <- global((cropSSP3_7.0 * a), fun = "sum", na.rm = TRUE)
cropSSP5_8.5_sum <- global((cropSSP5_8.5 * a), fun = "sum", na.rm = TRUE)

totalSSP1_2.6_sum <- pastureSSP1_2.6_sum + cropSSP1_2.6_sum
totalSSP4_3.4_sum <- pastureSSP4_3.4_sum + cropSSP4_3.4_sum
totalSSP2_4.5_sum <- pastureSSP2_4.5_sum + cropSSP2_4.5_sum
totalSSP4_6.0_sum <- pastureSSP4_6.0_sum + cropSSP4_6.0_sum
totalSSP3_7.0_sum <- pastureSSP3_7.0_sum + cropSSP3_7.0_sum
totalSSP5_8.5_sum <- pastureSSP5_8.5_sum + cropSSP5_8.5_sum

Year <- seq(2015,2100,1)

ag_land_luh2 <- cbind(Year,pastureSSP1_2.6_sum, pastureSSP4_3.4_sum, pastureSSP2_4.5_sum, pastureSSP4_6.0_sum, pastureSSP3_7.0_sum, pastureSSP5_8.5_sum,
                      cropSSP1_2.6_sum, cropSSP4_3.4_sum, cropSSP2_4.5_sum, cropSSP4_6.0_sum, cropSSP3_7.0_sum, cropSSP5_8.5_sum,
                      totalSSP1_2.6_sum, totalSSP4_3.4_sum, totalSSP2_4.5_sum, totalSSP4_6.0_sum, totalSSP3_7.0_sum, totalSSP5_8.5_sum)

colnames(ag_land_luh2) <- c("Year","pastureSSP1_2.6_sum", "pastureSSP4_3.4_sum", "pastureSSP2_4.5_sum", "pastureSSP4_6.0_sum", "pastureSSP3_7.0_sum", "pastureSSP5_8.5_sum",
                            "cropSSP1_2.6_sum", "cropSSP4_3.4_sum", "cropSSP2_4.5_sum", "cropSSP4_6.0_sum", "cropSSP3_7.0_sum", "cropSSP5_8.5_sum",
                            "totalSSP1_2.6_sum", "totalSSP4_3.4_sum", "totalSSP2_4.5_sum", "totalSSP4_6.0_sum", "totalSSP3_7.0_sum", "totalSSP5_8.5_sum")


write.csv(ag_land_luh2, "C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/ag_land_luh2.csv")
ag_land_luh2 <- read.csv("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/ag_land_luh2.csv")



##################################
# LUH2 Future 2100-2300 dataset
##################################

SSP5_3.4_300 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp534-2-1-e_gn_2100-2300.nc")
SSP5_8.5_300 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-MAGPIE-ssp585-2-1-e_gn_2100-2300.nc")
SSP1_2.6_300 <- rast("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/multiple-states_input4MIPs_landState_ScenarioMIP_UofMD-IMAGE-ssp126-2-1-e_gn_2100-2300.nc")

n_layers <- nlyr(SSP5_3.4_300)  # total number of layers
group_size <- 201
groups <- split(1:n_layers, ceiling((1:n_layers)/group_size))

r_list_SSP5_3.4_300 <- lapply(groups, function(i) SSP5_3.4_300[[i]])
r_list_SSP5_8.5_300 <- lapply(groups, function(i) SSP5_8.5_300[[i]])
r_list_SSP1_2.6_300 <- lapply(groups, function(i) SSP1_2.6_300[[i]])

pastureSSP5_3.4_300 <- rast(r_list_SSP5_3.4_300[c(12)]) + rast(r_list_SSP5_3.4_300[c(11)])
pastureSSP5_8.5_300 <- rast(r_list_SSP5_8.5_300[c(12)]) + rast(r_list_SSP5_8.5_300[c(11)])
pastureSSP1_2.6_300 <- rast(r_list_SSP1_2.6_300[c(12)]) + rast(r_list_SSP1_2.6_300[c(11)])

cropSSP5_3.4_300 <- rast(r_list_SSP5_3.4_300[c(6)]) + rast(r_list_SSP5_3.4_300[c(7)]) + rast(r_list_SSP5_3.4_300[c(8)]) + rast(r_list_SSP5_3.4_300[c(9)]) + rast(r_list_SSP5_3.4_300[c(10)])
cropSSP5_8.5_300 <- rast(r_list_SSP5_8.5_300[c(6)]) + rast(r_list_SSP5_8.5_300[c(7)]) + rast(r_list_SSP5_8.5_300[c(8)]) + rast(r_list_SSP5_8.5_300[c(9)]) + rast(r_list_SSP5_8.5_300[c(10)])
cropSSP1_2.6_300 <- rast(r_list_SSP1_2.6_300[c(6)]) + rast(r_list_SSP1_2.6_300[c(7)]) + rast(r_list_SSP1_2.6_300[c(8)]) + rast(r_list_SSP1_2.6_300[c(9)]) + rast(r_list_SSP1_2.6_300[c(10)])

a <- cellSize(cropSSP1_2.6, unit = "km")  # use "km" if you want square kilometers

pastureSSP5_3.4_300_sum <- global((pastureSSP5_3.4_300 * a), fun = "sum", na.rm = TRUE)
pastureSSP5_8.5_300_sum <- global((pastureSSP5_8.5_300 * a), fun = "sum", na.rm = TRUE)
pastureSSP1_2.6_300_sum <- global((pastureSSP1_2.6_300 * a), fun = "sum", na.rm = TRUE)

cropSSP5_3.4_300_sum <- global((cropSSP5_3.4_300 * a), fun = "sum", na.rm = TRUE)
cropSSP5_8.5_300_sum <- global((cropSSP5_8.5_300 * a), fun = "sum", na.rm = TRUE)
cropSSP1_2.6_300_sum <- global((cropSSP1_2.6_300 * a), fun = "sum", na.rm = TRUE)

totalSSP5_3.4_300_sum <- pastureSSP5_3.4_300_sum + cropSSP5_3.4_300_sum
totalSSP5_8.5_300_sum <- pastureSSP5_8.5_300_sum + cropSSP5_8.5_300_sum
totalSSP1_2.6_300_sum <- pastureSSP1_2.6_300_sum + cropSSP1_2.6_300_sum

Year <- seq(2100,2300,1)

ag_land_luh2_future <- cbind(Year,pastureSSP5_3.4_300_sum, pastureSSP5_8.5_300_sum, pastureSSP1_2.6_300_sum,
                             cropSSP5_3.4_300_sum, cropSSP5_8.5_300_sum, cropSSP1_2.6_300_sum,
                             totalSSP5_3.4_300_sum, totalSSP5_8.5_300_sum, totalSSP1_2.6_300_sum)
colnames(ag_land_luh2_future) <- c("Year","pastureSSP5_3.4_300_sum", "pastureSSP5_8.5_300_sum", "pastureSSP1_2.6_300_sum",
                            "cropSSP5_3.4_300_sum", "cropSSP5_8.5_300_sum", "cropSSP1_2.6_300_sum",
                            "totalSSP5_3.4_300_sum", "totalSSP5_8.5_300_sum", "totalSSP1_2.6_300_sum")

write.csv(ag_land_luh2_future, "C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/ag_land_future_luh2.csv")
ag_land_luh2_future <- read.csv("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/LUH2/ag_land_future_luh2.csv")


###########################################################
# Population data
###########################################################

population <- read.csv("~/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/land_use_project/population/population.csv")
population$Population <- population$Population*1000
my_ids <- ag_land_luh2$Year
population_subset <- population[population$Year %in% my_ids, ]


population$percapitaSSP1_2.6_sum <- ag_land_luh2$totalSSP1_2.6_sum/population_subset$Population
population$percapitaSSP4_3.4_sum <- ag_land_luh2$totalSSP4_3.4_sum/population_subset$Population
population$percapitaSSP2_4.5_sum <- ag_land_luh2$totalSSP2_4.5_sum/population_subset$Population
population$percapitaSSP4_6.0_sum <- ag_land_luh2$totalSSP4_6.0_sum/population_subset$Population
population$percapitaSSP3_7.0_sum <- ag_land_luh2$totalSSP3_7.0_sum/population_subset$Population
population$percapitaSSP5_8.5_sum <- ag_land_luh2$totalSSP5_8.5_sum/population_subset$Population





#######################################################
# test
######################################################

par(mar = c(4,5,2,1))
plot(dat$Year,dat$total, type="o", lwd=1.3, cex=0.6) #hyde
lines(total_historic$Year,total_historic$Total, type="o", lwd=1.3, cex=0.6, col="red") #luh2h

plot(dat$Year,dat$total, type="o", lwd=1.3, cex=0.6, xlim=c(1500,2024)) #hyde
lines(total_historic$Year,total_historic$Total, type="o", lwd=1.3, cex=0.6, col="red", xlim=c(1500,2024)) #luh2h
lines(ag_land_luh2$Year,ag_land_luh2$totalSSP1_2.6_sum , pch=16, type="o", lwd=1.3, cex=0.3, xlim=c(1700,2100), ylim=c(0,60000000), las=1, yaxt='n', xlab="")




#######################################################
# complete assembly
######################################################
n <- 100  # Number of gradient steps
cols0 <- colorRampPalette(c("brown",  "white"))(n)
cols <- colorRampPalette(c("white",  "skyblue"))(n)
cols1 <- colorRampPalette(c("skyblue",  "darkgreen"))(n)


# Top plot
par(mar = c(4,5,2,1))
par(fig = c(0, 0.35, 0.4, 1))  # x1, x2, y1, y2
plot(1, type = "n", xlim=c(-10000,1350), ylim=c(0,65000000), ylab="", yaxt='n', xlab="", xaxt="n" )
x_vals <- seq(-10000,1350, length.out = n + 1)
for (i in 1:n) {
  rect(x_vals[i], 0, x_vals[i + 1], 65000000,
       col = cols0[i], border = NA)
}
par(new = TRUE)
plot(dat$Year[1:28],dat$total[1:28], pch=16,   type="l", lwd=1.3, cex=0.6, xlim=c(-10000,1350), ylim=c(0,65000000), las=1, yaxt='n', ylab = expression("Mkm"^2), xlab="")
lines(dat$Year[1:28],dat$total.upper[1:28], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(-10000,1350), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n")
lines(dat$Year[1:28],dat$total.lower[1:28], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(-10000,1350), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n")

axis(side = 2, at = seq(0, 65000000, by = 10000000), labels = paste0(seq(0, 60, by = 10)), las=2)
axis(side = 1, at = seq(-10000, 0, by = 2000), labels = paste0(seq(-10000, 0, by = 2000)), las=1)
#axis(side = 1, at = 1851, labels = paste0("1700"), las=1)
par(new = TRUE)
plot(dat$Year[1:28],dat$percapita[1:28],  pch=16, type="l", lwd=1.3, cex=0.6, axes=F, xlim=c(-10000,1350), col="blue", xlab="", ylab="", ylim=c(0,0.02))


par(mar = c(4,0,2,1))
par(fig = c(0.35, 1, 0.4, 1), new = TRUE)
plot(1, type = "n", xlim=c(1710,2100), ylim=c(0,65000000), ylab="", yaxt='n', xlab="", xaxt="n" )
x_vals <- seq(1700, 2000, length.out = n + 1)
for (i in 1:n) {
  rect(x_vals[i], 0, x_vals[i + 1], 65000000,
       col = cols[i], border = NA)
}
x_vals <- seq(2000, 2100, length.out = n + 1)
for (i in 1:n) {
  rect(x_vals[i], 0, x_vals[i + 1], 65000000,
       col = cols1[i], border = NA)
}
par(new = TRUE)
plot(dat$Year[28:127],dat$total[28:127],  pch=16, type="l", lwd=1.3, cex=0.6, xlim=c(1710,2100), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n")
axis(side = 1, at = seq(1700, 2100, by = 100), labels = paste0(seq(1700, 2100, by = 100)), srt = 45)
lines(dat$Year[28:127],dat$total.upper[28:127], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(1710,2100), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n")
lines(dat$Year[28:127],dat$total.lower[28:127], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(1710,2100), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n")
segments(x0 = 2015, y0 = 0, x1 = 2015, y1 = 65000000, col = "black", lty = 2, lwd = 3)


#lines(total_historic$Year,total_historic$Total, pch=16, type="l", lwd=1.2, cex=0.6, xlim=c(1960,2100), ylim=c(0,60000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n")

par(new = TRUE)
plot(dat$Year[28:127],dat$percapita[28:127], pch=16, type="l", lwd=1.3, cex=0.6, axes=F, xlim=c(1710,2100), col="blue", ylim=c(0,0.02), xlab="")

par(new = TRUE)
plot(ag_land_luh2$Year,ag_land_luh2$totalSSP1_2.6_sum-48671084+47708768 , pch=16, type="l", lwd=1.3, cex=0.3, xlim=c(1710,2100), ylim=c(0,65000000), las=1, yaxt='n', xlab="")
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP4_3.4_sum-48671084+47708768, pch=16, type="l", lwd=1.3, cex=0.3)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP2_4.5_sum-48671084+47708768, pch=16, type="l", lwd=1.3, cex=0.3)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP4_6.0_sum-48671084+47708768, pch=16, type="l", lwd=1.3, cex=0.3)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP3_7.0_sum-48671084+47708768, pch=16, type="l", lwd=1.3, cex=0.3)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP5_8.5_sum-48671084+47708768, pch=16, type="l", lwd=1.3, cex=0.3)
#axis(side = 1, at = seq(2020, 2100, by = 20), labels = paste0(seq(2020, 2100, by = 20)), las=1)

par(new = TRUE)
plot(population$Year, population$percapitaSSP1_2.6_sum, pch=16, type="l", lwd=1.3, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(1710,2100), xlab="", xaxt='n')
lines(population$Year, population$percapitaSSP4_3.4_sum, pch=16, type="l", lwd=1.3, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(1700,2100), xlab="", xaxt='n')
lines(population$Year, population$percapitaSSP2_4.5_sum, pch=16, type="l", lwd=1.3, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(1700,2100), xlab="", xaxt='n')
lines(population$Year, population$percapitaSSP4_6.0_sum, pch=16, type="l", lwd=1.3, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(1700,2100), xlab="", xaxt='n')
lines(population$Year, population$percapitaSSP3_7.0_sum, pch=16, type="l", lwd=1.3, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(1700,2100), xlab="", xaxt='n')
lines(population$Year, population$percapitaSSP5_8.5_sum, pch=16, type="l", lwd=1.3, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(1700,2100), xlab="", xaxt='n')

# 47708768 is the end of one datast
# was the start of the next 48671084
(48671084/47708768)*100




#######################################################
# three compartments
######################################################
svg("C:/Users/jb83kg/OneDrive - The Royal Botanic Gardens, Kew/Documents/PERSPECTIVE_papers/2025_PROCB_peak_agriculture/1_analyses/figure_1v2.svg", width = 15, height = 12)  # size in inches


n <- 200  # Number of gradient steps
cols0 <- colorRampPalette(c("brown",  "white"))(n)
cols <- colorRampPalette(c("white",  "skyblue"))(n)
cols1 <- colorRampPalette(c("skyblue",  "darkgreen"))(n)


# Top plot
par(mar = c(4,5,2,0.5))
par(fig = c(0, 0.4, 0.4, 1))  # x1, x2, y1, y2
plot(1, type = "n", xlim=c(-10000,1700), ylim=c(0,65000000), xaxs = "i",yaxs="i", ylab="", yaxt='n', xlab="", xaxt="n" )
x_vals <- seq(-10000,1700, length.out = n + 1)
for (i in 1:n) {
  rect(x_vals[i], 0, x_vals[i + 1], 65000000,
       col = cols0[i], border = NA)
}
par(new = TRUE)
plot(dat$Year[1:28],dat$total[1:28], pch=16,   type="l", lwd=2.3, cex=0.6, xlim=c(-10000,1700), ylim=c(0,65000000), xaxs = "i",yaxs="i", las=1, yaxt='n', ylab="", xlab="")
lines(dat$Year[1:28],dat$total.upper[1:28], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(-10000,1700), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n", lty=3)
lines(dat$Year[1:28],dat$total.lower[1:28], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(-10000,1700), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n", lty=3)
text(-200, 35000000, labels = "Land per capita", pos = 2, col="blue")  # pos = 3 means "above" the point
text(-10000, 62000000, labels = "(a) Holocene", pos = 4, col="black", cex=1.3)  # pos = 3 means "above" the point
mtext(expression("Million Km"^2), side=2.5, line=2, cex=1.5)

axis(side = 2, at = seq(0, 65000000, by = 10000000), labels = paste0(seq(0, 60, by = 10)), las=2)
axis(side = 1, at = seq(-10000, 0, by = 2000), labels = paste0(seq(-10000, 0, by = 2000)), las=1)
#axis(side = 1, at = 1851, labels = paste0("1700"), las=1)
par(new = TRUE)
plot(dat$Year[1:28],dat$percapita[1:28],  pch=16, type="l", lwd=2.3, cex=0.6, axes=F, xlim=c(-10000,1700), col="blue", xaxs = "i",yaxs="i", xlab="", ylab="", ylim=c(0,0.02))

# second segment
par(mar = c(4,0,2,0.5))
par(fig = c(0.4, 0.75, 0.4, 1), new = TRUE)
plot(1, type = "n", xlim=c(1710,2015), ylim=c(0,65000000), xaxs = "i",yaxs="i", ylab="", yaxt='n', xlab="", xaxt="n" )
x_vals <- seq(1715, 2015, length.out = n + 1)
for (i in 1:n) {
  rect(x_vals[i], 0, x_vals[i + 1], 64900000,
       col = cols[i], border = NA)
}
par(new = TRUE)
plot(dat$Year[28:118],dat$total[28:118],  pch=16, type="l", lwd=2.3, cex=0.6, xlim=c(1700,2015), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n", xaxs = "i",yaxs="i")
axis(side = 1, at = seq(1700, 2015, by = 100), labels = paste0(seq(1700, 2015, by = 100)), srt = 45)
lines(dat$Year[28:118],dat$total.upper[28:118], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(1700,2015), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n", lty=3)
lines(dat$Year[28:127],dat$total.lower[28:127], pch=16, type="l", lwd=0.6, cex=0.6, xlim=c(1700,2015), ylim=c(0,65000000), las=1, ylab="", yaxt='n', xlab="", xaxt="n", lty=3)
segments(x0 = 2005, y0 = 0, x1 = 2005, y1 = 65000000, col = "black", lty = 2, lwd = 1)
text(1850, 14000000, labels = "Total agricultural land", pos = 4)  # pos = 3 means "above" the point
text(1700, 62000000, labels = "(b) Transition to Anthropocene", pos = 4, col="black", cex=1.3)  # pos = 3 means "above" the point


par(new = TRUE)
plot(dat$Year[28:118],dat$percapita[28:118], pch=16, type="l", lwd=2.3, cex=0.6, axes=F, xlim=c(1700,2015), col="blue", ylim=c(0,0.02), xlab="", xaxs = "i",yaxs="i")
mtext("Year", side = 1.3, line = 2.7, adj=0.42, cex=1.5)  # side 1 = bottom


# third segment
par(mar = c(4,0,2,4.2))
par(fig = c(0.75, 1, 0.4, 1), new = TRUE)
plot(1, type = "n", xlim=c(2015,2100), ylim=c(0,65000000), xaxs = "i",yaxs="i", ylab="", yaxt='n', xlab="", xaxt="n" )
x_vals <- seq(2015, 2100, length.out = n + 1)
for (i in 1:n) {
  rect(x_vals[i], 0, x_vals[i + 1], 64900000,
       col = cols1[i], border = NA)
}

par(new = TRUE)
plot(ag_land_luh2$Year,ag_land_luh2$totalSSP1_2.6_sum-48671084+47708768 , pch=16, type="l", lwd=1.8, cex=0.3, lty=1, xlim=c(2015,2100), ylim=c(0,65000000), las=1, yaxt='n', xlab="", xaxt='n', xaxs = "i",yaxs="i")
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP4_3.4_sum-48671084+47708768, pch=16, type="l", lwd=1.8, cex=0.3, lty=2)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP2_4.5_sum-48671084+47708768, pch=16, type="l", lwd=1.8, cex=0.3, lty=3)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP4_6.0_sum-48671084+47708768, pch=16, type="l", lwd=1.8, cex=0.3, lty=4)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP3_7.0_sum-48671084+47708768, pch=16, type="l", lwd=1.8, cex=0.3, lty=5)
lines(ag_land_luh2$Year, ag_land_luh2$totalSSP5_8.5_sum-48671084+47708768, pch=16, type="l", lwd=1.8, cex=0.3, lty=6)
axis(side = 1, at = seq(2000, 2100, by = 20), labels = paste0(seq(2000, 2100, by = 20)), las=1)
text(2015, 62000000, labels = "(c) Novel emerging trends", pos = 4, col="black", cex=1.3)  # pos = 3 means "above" the point

# real extended value
lines(dat$Year,dat$total,  pch=16, type="l", lwd=2.3, cex=0.6, xlim=c(2015,2100), ylim=c(0,65000000), las=1, col="white")

par(new = TRUE)
plot(population$Year, population$percapitaSSP1_2.6_sum-0.006515111+0.006552389, pch=16, type="l", lwd=1.5, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(2015,2100), xlab="", xaxt='n', xaxs = "i",yaxs="i", lty=1)
lines(population$Year, population$percapitaSSP4_3.4_sum-0.006515111+0.006552389, pch=16, type="l", lwd=1.5, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(2015,2100), xlab="", xaxt='n', lty=2)
lines(population$Year, population$percapitaSSP2_4.5_sum-0.006515111+0.006552389, pch=16, type="l", lwd=1.5, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(2015,2100), xlab="", xaxt='n', lty=3)
lines(population$Year, population$percapitaSSP4_6.0_sum-0.006515111+0.006552389, pch=16, type="l", lwd=1.5, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(2015,2100), xlab="", xaxt='n', lty=4)
lines(population$Year, population$percapitaSSP3_7.0_sum-0.006515111+0.006552389, pch=16, type="l", lwd=1.5, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(2015,2100), xlab="", xaxt='n', lty=5)
lines(population$Year, population$percapitaSSP5_8.5_sum-0.006515111+0.006552389, pch=16, type="l", lwd=1.5, cex=0.6, col="blue", ylim=c(0,0.02), yaxt='n', xlim=c(2015,2100), xlab="", xaxt='n', lty=6)

axis(4, at = seq(0, 0.018, by = 0.004), labels=seq(0*100, 0.018*100, by = 0.004*100) , col.axis = "blue", las = 1)
mtext("Hectares per person", side=4, line=2.5, cex=1.5, col = "blue")


par(fig = c(0, 1, 0.37, 0.4),        # left, right, bottom, top in [0,1] of figure region
    mar = c(0, 2, 0, 1),          # no margins
    new = TRUE)

plot(0, 0, type = "n", axes = FALSE, xlab = "", ylab = "")
legend("center",
       legend = c("SSP1 RCP2.6", "SSP4 RCP3.4","SSP2 RCP4.5","SSP4 RCP6.0","SSP3 RCP7.0","SSP5 RCP8.5"),
       lty = 1:6,
       horiz = TRUE,
       bty = "n", lwd=1.5)

dev.off()
# Summer school iDiv: "Ecology and evolution of morphological fruit traits" (Group 2)

## *Determining colorimetric measures from spectrometric reflectance measurements of fruits*

### Important before starting the measurement:

# -   Always use a consistent name labeling system in your measurements
# 
# -   It should include the species name, the individual number and perhaps the number of the repeated measurement 
#     (e.g., Sp1-i1_r1 for species 1, individual 1 and replicate 1)
#     Note: I use -- and \_ as separators in the same string so that we can easily split up the names to make new columns for grouping, 
#     e.g., the replicate number from sp-individual ID and also the species itself
# 
# -   Adjust the name of each measurement before starting each individual (replicate) measurement
# 
# -   A tutorial for the package we will be using can be found here: <https://rafaelmaia.net/pavo/articles/pavo.html>
# 
# -   Before starting to work with the data, we have to create a new folder where we paste all measurement files 
#     that contain "*Reflection*" in their file name. Those are our calibrated measurements 
#     (there are four kinds of files in the folder where you saved your measurements: 
#     *Background* = dark measurement, 
#     *Ref* = light measurement, 
#     *FLMS*.. = uncorrected measurement file,
#     *Reflection* = corrected measurement file).
#
# -   We will read all files into R simultaneously just by the indication of this folder, so make sure you can find it easily


species_names <- read.csv2("Thursday_data/Species_names_phylo_color.csv")
rownames(species_names) <- species_names$x
#Utility function:
# Function to merge data frames in list
multi_join <- function(list_of_loaded_data, join_func, ...)
{
  require("dplyr")
  output <- Reduce(function(x, y) {join_func(x, y, ...)}, list_of_loaded_data)
  return(output)
}    



### Installing and loading packages =======================================

# install.packages("pavo", dependencies = T)
# install.packages("dplyr")
# install.packages("stringr")
# install.packages("tidyr")
#install.packages("reshape2")

library(pavo)
library(dplyr)
library(stringr)
library(tidyr)
library(reshape2)

### 1. Reading in the reflection measurements  =======================================

# We want to read in all measurements at the same time, this is done using the "*getspec()*" function from 'pavo'. 
# You have to indicate the folder with the measurement files that have "*Reflection*" in their name, 
# and since we are reading in ".txt" files, we need to indicate this in the argument *ext = "txt".\
# *With) we cut out spectral noise.


specs <- getspec("SummerSchool/Reflectance/", ext = "txt", lim = c(300, 680))

par(mfrow=c(8,4))  
explorespec(specs, by = 3, lwd = 2) # by = indicates how many replicates should be plotted into one window

# let's smooth the curves to exclude calculation errors due to noise in the measurement

specs2 <- procspec(specs, opt = "smooth")




## A) Calculating brightness, hue and chroma  =======================================

### 1) For replicates individually ====

# First, we have to subset the reflection data into species data frames for the separate calculation of the metrics. 
# Pavo has a modified version of subset() which uses partial matching of strings. 
# You can indicate any sequence of letter/signs/numbers that you would like to filter for in the column names of your reflection measurement.


## Automated in loop:
name_specs <- colsplit(names(specs[,-1]), "-", c("species", "replicate", " "))[,-3]
name_specs <- unique(name_specs$species)

# raw
list_sp_df <- list()
for (i in seq_along(name_specs)) {
  x <- subset(specs, name_specs[[i]])
  list_sp_df[[i]] <- x
}

# smoothed
list_sp_df2 <- list()
for (i in seq_along(name_specs)) {
  x <- subset(specs2, name_specs[[i]])
  list_sp_df2[[i]] <- x
}


## Manually per hand
# refl_sp1 <- subset(specs, "sp1") # based on species id in names in reflectance df
# refl_sp2 <- subset(specs, "sp2")
# refl_sp3 <- subset(specs, "sp3")


# Next we will calculate brightness, hue and chroma using the *summary()* function 
# for each replicate (we can also do it for each species or each individual tree that was sampled, see below). \
# If we use the *subset* *= T* argument inside the function, it will automatically calculate the three desired variables. 
# There is a diversity of other measures the function can calculate but those three are the most commonly used in comparative studies.

# again we cut out some more of the spectral noise (needed here. may not be needed for your data. Remove wlmin/wlmax arguments if not needed)


## automated in loop:
# raw
list_colvars_df <- list()
for (i in seq_along(list_sp_df)){
  
  col_vars <- summary(list_sp_df[[i]], subset=T)
  list_colvars_df[[i]] <- col_vars
}

# smoothed
## automated in loop:
list_colvars_df2 <- list()
for (i in seq_along(list_sp_df2)){
  
  col_vars <- summary(list_sp_df2[[i]], subset=T)
  list_colvars_df2[[i]] <- col_vars
}


## make look nice:
# raw
list_df_final <- list()
for (i in seq_along(list_colvars_df)){
  df <- data.frame(
    species = rownames(list_colvars_df[[i]]),
    meanBrightness = list_colvars_df[[i]]$B2,
    Chroma = list_colvars_df[[i]]$S8,
    Hue = list_colvars_df[[i]]$H1)
    
    list_df_final[[i]] <- df
}

# smoothed
list_df_final2 <- list()
for (i in seq_along(list_colvars_df2)){
  df <- data.frame(
    species = rownames(list_colvars_df2[[i]]),
    meanBrightness = list_colvars_df2[[i]]$B2,
    Chroma = list_colvars_df2[[i]]$S8,
    Hue = list_colvars_df2[[i]]$H1)
  
  list_df_final2[[i]] <- df
}

## Manually per hand:
# col_sp1 <- summary(refl_sp1, subset =T, wlmin = 300, wlmax= 680)
# col_sp2 <- summary(refl_sp2, subset =T, wlmin = 300, wlmax= 680)
# col_sp3 <- summary(refl_sp3, subset =T, wlmin = 300, wlmax= 680)


## make new dfs manually per hand:
# Refl_sp1 <- data.frame( 
#   species = rownames(col_sp1), 
#   meanBrightness = col_sp1$B2, 
#   Chroma = col_sp1$S8, 
#   Hue = col_sp1$H1)
# 
# Refl_sp3 <- data.frame(
#   species = rownames(col_sp3), 
#   meanBrightness = col_sp3$B2, 
#   Chroma = col_sp3$S8, 
#   Hue = col_sp3$H1)
# 
# Refl_sp2 <- data.frame(
#   species = rownames(col_sp2), 
#   meanBrightness = col_sp2$B2, 
#   Chroma = col_sp2$S8, 
#   Hue = col_sp2$H1)
# 
# Refl <- merge(Refl_sp1, Refl_sp3, all=T)
# Refl <- merge(Refl, Refl_sp2, all=T)




# lets make our data frame look nicer by creating a new data frame.
## automated in loop:
# raw:
list_split_df <- list()
for (i in seq_along(list_df_final)){
  x <- separate(list_df_final[[i]], species, into = c("species", "data", "time"), sep = "_")
  x <- separate(x, species, into = c("species", "replicate"), sep = "-")
  list_split_df[[i]] <- x
  }
split_df <- multi_join(list_split_df, full_join)
df2 <- merge(split_df[-c(3)], species_names, by.x="species", by.y="x")

# smoothed:
list_split_df2 <- list()
for (i in seq_along(list_df_final2)){
  x <- separate(list_df_final2[[i]], species, into = c("species", "data", "time"), sep = "_")
  x <- separate(x, species, into = c("species", "replicate"), sep = "-")
  list_split_df2[[i]] <- x
}
split_df2 <- multi_join(list_split_df2, full_join)
df3 <- merge(split_df2[-c(3)], species_names, by.x="species", by.y="x")

## manually by hand:
# Refl2 <- Refl %>% separate(species, into = c("sp-id", "replicate", "data", "time"), sep = "_")
# Refl3 <- Refl2 %>% separate("sp-id", into = c("species", "individual"), sep = "-")
# unique_id <- paste(Refl2$`sp-id`, Refl2$replicate)
# Refl3$unique_id <- unique_id
# 
# Reflection_data_final <- Refl3 %>% select(unique_id, species, individual, replicate, meanBrightness, Chroma, Hue)





### 2. Summarized for species or individuals

# here follows the approach how to do it for species / 
# For this, we will first calculate average reflection spectra for each species and then perform the calculation of metrics.
# raw:
spp2 <- split_df$species 
sppspec <- aggspec(specs, by=spp2, FUN = mean)
species_metrics <- summary(sppspec, subset = T)
species_metrics$sp_id <- rownames(species_metrics)

# smoothed:
sppspec2 <- aggspec(specs2, by=spp2, FUN = mean)
species_metrics2 <- summary(sppspec2, subset = T)
species_metrics2$sp_id <- rownames(species_metrics2)


species_metrics <- merge(species_metrics, species_names, by.x="sp_id", by.y="x" )
species_metrics2 <- merge(species_metrics2, species_names, by.x="sp_id", by.y="x" )

  df4 <- data.frame(species = species_metrics$species,
    sp_id = species_metrics$sp_id,
    meanBrightness = species_metrics$B2,
    Chroma = species_metrics$S8,
    Hue = species_metrics$H1)
  df4
  
  write.csv(df4, "Thursday_data/Colorimetric_variables_SS22.csv")
  
  df5 <- data.frame(species = species_metrics2$species,
                   sp_id = species_metrics2$sp_id,
                   meanBrightness = species_metrics2$B2,
                   Chroma = species_metrics2$S8,
                   Hue = species_metrics2$H1)
  df5
  write.csv(df5, "Thursday_data/Colorimetric_variables_smoothed_SS22.csv")



# with [-1] we remove the name of the wavelength column
# with [-2] we remove the replicate number (and appendix from software)

# # Now we calculate the mean across individuals (1.) and look at averaged curves: 
# (not relevant here since we did not take samples for different individuals)

# mspecs <- aggspec(specs, by = spp2, FUN = mean)
# round(mspecs[1:5, ], 2)
# par(mfrow=c(3,2)) #
# peakshape(mspecs, plot = TRUE)
# 
# individual_metrics <- summary(mspecs, subset = T, wlmin=300, wlmax=680)
# 
# # 2. for palm species
# spp3 <- unique(spp2)
# spp3 <- str_split_fixed((spp3), "-", 2)[,-2]
# sppspec <- aggspec(mspecs, by = spp3, FUN = mean)
# round(sppspec[1:3, ], 2)
# par(mfrow=c(2,2)) #
# peakshape(sppspec, plot = TRUE)
# 
# species_metrics <- summary(sppspec, subset = T, wlmin=300, wlmax=680)
# 
# # make new dfs:
# Refl_individuals <- data.frame( 
#   species = rownames(individual_metrics), 
#   meanBrightness = individual_metrics$B2, 
#   Chroma = individual_metrics$S8, 
#   Hue = individual_metrics$H1)
# 
# Refl_species <- data.frame(
#   species = rownames(species_metrics), 
#   meanBrightness = species_metrics$B2, 
#   Chroma = species_metrics$S8, 
#   Hue = species_metrics$H1)



## B)  Visualization of reflection curves

# There are a couple of other useful functions in the pavo package, for example for the visualization of reflectance curves. 
# Below follows an additional part with some useful plotting techniques from the package.

# Overlay curves:
par(mfrow=c(8,4))
# raw data
explorespec(specs, by = spp2, lwd = 2) # raw measurement per species curves
# smoothed
explorespec(specs2, by = spp2, lwd = 2) # by = indicates how many replicates should be plotted into one window


# Peak curves:
par(mfrow=c(1,1))

# per replicate
peakshape(specs, plot = TRUE, lim=c(300, 680)) # peak for each raw measurement
peakshape(specs2, plot = TRUE, lim=c(300, 680))

# per species
peakshape(sppspec, plot = TRUE)   # Peak for each measurement per species
peakshape(sppspec2, plot = TRUE)   # Peak for each measurement per species

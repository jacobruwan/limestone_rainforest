# Data
# Use the choose files function to get the location of the datasets on your computer
choose.files()

# Should get an output like:
# "C:\\Users\\Jacob\Project\\Data\\biotic_leaftraits.RData"

# Copy the folder spot below, and copy the """".RData file into the load function below

location <- "C:\\Users\\Jacob\\Dropbox\\Semester 2 2017\\MXB344\\Project\\Data\\"

# Load the datasets
load(paste0(location, "biotic_leaftraits.RData"))
load(paste0(location, "abiotic_filterpaper.RData"))
# load(paste0(location, "abiotic_canopycover.RData"))
load(paste0(location, "abiotic_penetrometer.RData"))
load(paste0(location, 'domseedlingcover_spread.RData'))

library(ggplot2)
library(dplyr)



####################################################
## Abiotic
# Decopmosition
ggplot(aes(x=site, y=decomposition),
       data = abiotic_filterpaper.df) + 
  geom_boxplot() +
  ggtitle("Abiotic Comparison") 

# LAI
ggplot(aes(x=site, y=lai, col = site),
       data = abiotic_canopycover.df) + 
  geom_boxplot() +
  ggtitle("Abiotic Comparison") 

# Gap fResction THreshold
ggplot(aes(x=site, y=gft, col = site),
       data = abiotic_canopycover.df) + 
  geom_boxplot() +
  ggtitle("Abiotic Comparison") 

# Compaction
ggplot(aes(x=site, y=compaction, col = site),
       data = abiotic_penetrometer.df) + 
  geom_boxplot() +
  ggtitle("Abiotic Comparison") 
























####################################################
## Leaf Traits

biotic_leaftraits.df %>%
  filter(!is.na(leaf_arrangement)) %>%
  ggplot(aes(x = av_trans_elevation, y = leaf_arrangement,
             col = site)) +
  geom_point() +
  theme_bw()


#########################3
## Dominant Seedling Coverage
domseedlingcover_spread.df %>% head()










################################################
###### Look at correlations
## Add/remove variables from the select(var1, var2, etc.) section
domseedlingcover_spread.df %>%
  select(dom_1_abundance, dom_2_abundance, dom_3_abundance,
         dom_1_seedling_cover, dom_2_seedling_cover, dom_3_seedling_cover) %>%
  cor()



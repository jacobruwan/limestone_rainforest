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
load(paste0(location, "abiotic_canopycover.RData"))
load(paste0(location, "abiotic_penetrometer.RData"))



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

count_data <- biotic_leaftraits.df %>%
  filter(species_presence == 1) %>%
  group_by(site, apex_shape) %>% # CHANGE LEAF TRAIT
  summarise(number_observations = n())

# CHANGE THE X VARIABLE LEAF TRAIT TO THE ONE ABOVE
ggplot(aes(x=apex_shape, y=number_observations, fill = site),
       data = count_data) + 
  geom_bar(stat = "identity", position = "dodge") +
  coord_flip() +
  ggtitle("Leaf Trait Comparison") 




###########################3
# Model example

fit <- glm(lai ~ site,
           family = "gaussian",
           data = abiotic_canopycover.df)

# In the output below, you want a significant siteP Estimate
summary(fit)

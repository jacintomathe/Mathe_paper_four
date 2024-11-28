#Clean the environment
rm(list=ls()); ls()  			# tidying up the work space

#Claen plot panel
clear_plot_panel <- function() {
  if (dev.cur() > 1) {
    while (dev.cur() > 1) dev.off()
  }
}
clear_plot_panel()
##Load packages
library(rio)
library(tidyverse)
library(sf)

#Set your workijg directory
setwd("/Users/jacintomathe/Documents/Mathe/Projects/GitHub/Mathe_papers/Mathe_paper0")

# Themes ------------------------------------------------------------------
#Proper function in R:
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

#Set theme for your plots
mathe_theme_normal=theme(axis.title = element_text(face = "bold",size = 9,colour="black"),
                         axis.text = element_text(vjust=0.6,colour = "black",size = 8),
                         legend.text = element_text(size = 8,colour="black"),
                         legend.title = element_text(size=11,colour="black"),
                         panel.background = element_rect(fill = "white"),
                         panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                                         colour = "gray100"),
                         panel.grid.minor = element_line(size = 0.25, linetype = 'dashed',
                                                         colour = "gray80"),
                         plot.background = element_rect(fill = "white"))
theme_set(mathe_theme_normal)

# loading Bones data --------------------------------------------------------------
data <- import("paper_zero_raw_data/mathe_primate_carcass.csv")
gnp_landscape=read_sf("paper_zero_raw_data/paper_zero_data_cleaning/landscapes_parkbnd_2010.shp")
dim(data)
names(data) <- proper(names(data))#Proper the manes (captilize the first letter)

#Write a shapefile from the data to measure the distances and integrate spatial data
carcass_sf <- data %>% 
  st_as_sf(coords = c("Longitude","Latitude"),
           crs=4127)

st_crs(gnp_landscape)=st_crs(carcass_sf)
#Distance calculation
#To roads
roads=read_sf("paper_zero_raw_data/paper_zero_data_cleaning/Gorongosa updated roads_2020a.shp")
roads <- roads %>% st_transform(crs = 4127)
plot(roads[1])

roads=st_intersection(roads,gnp_landscape)
mDist_to_road <- as.data.frame(st_distance(carcass_sf, roads))
names(mDist_to_road)=paste("dist",1:ncol(mDist_to_road),sep = "_")
carcass_sf$mDist_to_road=apply(mDist_to_road, MARGIN =  1, FUN = min, na.rm = T)
dim(carcass_sf)

#To rivers
rivers=read_sf("paper_zero_raw_data/paper_zero_data_cleaning/gnp_main_rivers_latlong_a.shp")
# rivers <- rivers %>% st_transform(crs = 4127)
st_crs(rivers)
plot(rivers)

st_crs(rivers)=st_crs(carcass_sf)

rivers=st_intersection(rivers,gnp_landscape)
mDist_to_river <- as.data.frame(st_distance(carcass_sf, rivers))
names(mDist_to_river)=paste("dist",1:ncol(mDist_to_river),sep = "_")
carcass_sf$mDist_to_river=apply(mDist_to_river, MARGIN =  1, FUN = min, na.rm = T)
dim(mDist_to_river);dim(carcass_sf)

#To water==Urema
Urema=read_sf("paper_zero_raw_data/paper_zero_data_cleaning/lake_urema_satimg_lla.shp")
st_crs(Urema)
Urema <- st_cast(Urema, "MULTILINESTRING")

# Urema <- Urema %>% st_transform(crs = 4127)
st_crs(Urema)
plot(Urema)
plot(gnp_landscape[1])

mDist_to_UREMA <- as.data.frame(st_distance(carcass_sf, Urema))
names(mDist_to_UREMA)=paste("dist",1:ncol(mDist_to_UREMA),sep = "_")
carcass_sf$mDist_to_UREMA=apply(mDist_to_UREMA, MARGIN =  1, FUN = min, na.rm = T)

#To Lions
lion <- import("paper_zero_raw_data/lions1.csv")
# lions=read_sf("/Users/jacintomathe/Documents/Mathe/JAM/Database/GIS/Shapefiles/Gorongosa/DPhil/Data_points/track_lion_list_points.shp")

lions_sf <- lion %>% 
  st_as_sf(coords = c("Longitude","Latitude"),
           crs=4127)

lions=st_intersection(lions_sf,gnp_landscape)
mDist_to_lions <- as.data.frame(st_distance(carcass_sf, lions))
carcass_sf$mDist_to_lions=apply(mDist_to_lions, MARGIN =  1, FUN = min, na.rm = T)
dim(mDist_to_lions)
dim(carcass_sf)

#To Wilddogs
wild_dogs <- import("paper_zero_raw_data/wild_dogs1.csv")
# wilddogs=read_sf("/Users/jacintomathe/Documents/Mathe/JAM/Database/GIS/Shapefiles/Gorongosa/DPhil/Data_points/track_wd_list_points.shp")
dim(wild_dogs)

wild_dogs_sf <- wild_dogs %>% 
  st_as_sf(coords = c("Longitude","Latitude"),
           crs=4127)

wilddogs=st_intersection(wild_dogs_sf,gnp_landscape)
mDist_to_wilddogs <- as.data.frame(st_distance(carcass_sf, wilddogs))
carcass_sf$mDist_to_wilddogs=apply(mDist_to_wilddogs, MARGIN =  1, FUN = min, na.rm = T)
dim(mDist_to_wilddogs)
dim(carcass_sf)

#To Leopards
leopards <- import("paper_zero_raw_data/leopards1.csv")
# leopards=read_sf("/Users/jacintomathe/Documents/Mathe/JAM/Database/GIS/Shapefiles/Gorongosa/DPhil/Data_points/track_lp_list_points.shp")
dim(leopards)

leopards_sf <- leopards %>% 
  st_as_sf(coords = c("Longitude","Latitude"),
           crs=4127)

leopards <- leopards_sf %>% st_transform(crs = 4127)
st_crs(leopards)

leopards=st_intersection(leopards,gnp_landscape)
mDist_to_leopards <- as.data.frame(st_distance(carcass_sf, leopards))
carcass_sf$mDist_to_leopards=apply(mDist_to_leopards, MARGIN =  1, FUN = min, na.rm = T)
dim(mDist_to_leopards);dim(carcass_sf)#View the dimension of the dataframes

#To gnp_pans
gnp_pans=read_sf("paper_zero_raw_data/paper_zero_data_cleaning/pans_gnp.shp")
dim(gnp_pans)

gnp_pans <- gnp_pans %>% st_transform(crs = 4127)
st_crs(gnp_pans)

gnp_pans=st_intersection(gnp_pans,gnp_landscape)
mDist_to_gnp_pans <- as.data.frame(st_distance(carcass_sf, gnp_pans))
carcass_sf$mDist_to_gnp_pans=apply(mDist_to_gnp_pans, MARGIN =  1, FUN = min, na.rm = T)
dim(mDist_to_gnp_pans)
dim(carcass_sf)

#Final data for modeling
data <- carcass_sf
data %>% 
  dplyr::group_by(Landscape) %>% 
  dplyr::summarise(total=sum(Mne)) %>% 
  ggplot()+
  geom_col(aes(x=Landscape,y=total))+
  coord_flip()+theme_classic()

data %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(total=sum(Mne)) %>% 
  ggplot()+
  geom_col(aes(x=Species,y=total))+
  coord_flip()+theme_classic()

data %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(total=sum(n())) %>% 
  ggplot()+
  geom_col(aes(x = reorder(Species, total), y = total)) +
  coord_flip()+
  theme(axis.title = element_text(colour = "black",face = "bold"),
        axis.text =  element_text(colour = "black"))+
  labs(x="Species",y="Carcass abundance")

ggsave("paper_zero_graphs/carcass_abundance.PNG",width = 4,height = 4)

#sample description
data %>% 
  filter(Species=="Baboon") %>% 
  pull(Species) %>% 
  length()

data %>% 
  subset(Species=="Other primate") %>% 
  pull(Species) %>% 
  length()

#assign all Primates to the designated column.
data <- data %>% 
  mutate(Primate=case_when(Species=="Baboon"~"Yes",
                           Species=="Samango monkey"~"Yes",
                           Species=="Other primate"~"Yes",
                           TRUE ~ "No"))
data <- data %>% 
  mutate(Category=case_when(Species=="Baboon"~"Primate",
                            Species=="Samango monkey"~"Primate",
                            Species=="Other primate"~"Primate",
                            TRUE ~ "Non-Primate"))
View(data)
anyNA(data$Primate)#check if there is any NA

#Check data before performing GLM
data %>% 
  subset(Primate=="Yes") %>% 
  pull(Mne) %>% 
  sum()

data %>% 
  subset(Primate!="Yes") %>% 
  pull(Mne) %>% 
  length()

#Non-primate taxa
data %>% 
  subset(Primate!="Yes") %>% 
  distinct(Species,.keep_all = T) %>% 
  pull(Species) %>% 
  length()

#Primate taxa
data %>% 
  subset(Primate=="Yes") %>% 
  distinct(Species,.keep_all = T) %>% 
  pull(Species) %>% 
  length()

length(unique(data$Species))

dim(data)
table(data$Primate)
data$geometry=NULL
data$Primate=as.factor(data$Primate)


#Primate vs non-primates
data %>%
  group_by(Category) %>% 
  summarise(N_carcasses=sum(n())) %>% 
  mutate(Percent=paste0(round(N_carcasses/sum(N_carcasses)*100,0),"%")) %>% 
  # Creating the pie chart
  ggplot(aes(x = "", y = N_carcasses, fill = Category, label = Percent)) +
  geom_bar(stat = "identity", width = 1, colour = "grey32") +
  coord_polar("y", start = 0) +
  geom_text(position = position_stack(vjust = 0.5), size = 6,color="white") +  # Add text labels
  theme_void() +
  scale_fill_manual("Carcass",values = c("dodgerblue4", "coral2")) +  # Customize fill colors as needed
  theme(legend.position = "right",legend.title = element_text(face = "bold"))  # Optional: Adjust legend position

ggsave("paper_zero_graphs/Primates_vs_non_primates.PNG",width = 6,height = 5,bg="white")

library(car)
levels(data$Primate);str(data)

#GLM
model1 <- glm(Primate ~ Landscape +mDist_to_road + mDist_to_road+ mDist_to_leopards+ mDist_to_lions+mDist_to_wilddogs+mDist_to_UREMA+mDist_to_river+mDist_to_river, family = binomial, data = data)
summary(model1)
vif(model1)
#removed wild dogs due to multicolinearity

model2 <- glm(Primate ~ mDist_to_road + mDist_to_road+ mDist_to_leopards+ mDist_to_lions+mDist_to_wilddogs+mDist_to_UREMA+mDist_to_river+mDist_to_river, family = binomial, data = data)
summary(model2)
vif(model2)

model2 %>% tidy(conf.int = T)
#Removed landscapes due to insignificancy

model3 <- glm(Primate ~  mDist_to_road + mDist_to_road+ mDist_to_leopards+ mDist_to_lions+mDist_to_UREMA+mDist_to_river+mDist_to_river, family = binomial, data = data)
summary(model3)
vif(model3)

model3_print <- model3 %>% tidy(conf.int = T,exponentiate = T);model3_print#Final model
model3_print %>% 
  mutate(across(where(is.numeric), round, digits = 5)) %>% 
  export("paper_zero_data/model3_print_chp0.csv")
# Model comparison
data.frame(M=c("Model1","Model2","Model3"),
           AIC=c(model1$aic,model2$aic,model3$aic))

# Perform Chi-Square Test on the simplified data
carcass_data <- data %>% 
  mutate(Mne=as.numeric(Mne)) %>% 
  group_by(Landscape,Primate) %>% 
  summarise(Total_bones=sum(Mne)) %>% 
  pivot_wider(names_from = Primate,values_from = Total_bones,values_fill = 0)

carcass_data$Landscape=NULL

# Or, if small sample size
fisher.test(carcass_data)


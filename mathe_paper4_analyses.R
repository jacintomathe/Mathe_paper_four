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

# Themes ------------------------------------------------------------------
#Proper function in R:
proper=function(x) paste0(toupper(substr(x, 1, 1)), tolower(substring(x, 2)))

# defining the themes ------------------------------------------------------------------
mathe_theme_normal <- theme(
  axis.title = element_text(face = "bold", size = 12, colour = "black", family = "Times New Roman"),
  axis.text = element_text(vjust = 0.6, colour = "black", size = 12, family = "Times New Roman"),
  legend.text = element_text(size = 12, colour = "black", family = "Times New Roman"),
  legend.title = element_text(size = 12, colour = "black", face = "bold", family = "Times New Roman"),
  panel.grid.major = element_line(linewidth = 0.01, linetype = NULL, colour = "gray90"),
  panel.background = element_rect(fill = "white"),
  panel.grid.minor = element_line(size = 0.15, linetype = 'dashed', colour = "gray40"),
  plot.background = element_rect(fill = "white", colour = "white")
)

theme_set(mathe_theme_normal)

# loading Bones data --------------------------------------------------------------
data<- import("paper_zero_raw_data/mathe_primate_carcass_v.2.csv")  
unique(data$Species)
data$Species[data$Species=="Hystricidae"]="Porcupine"

data %>% 
  dplyr::group_by(Species) %>% 
  dplyr::summarise(total=sum(n())) %>% 
  ggplot()+
  geom_col(aes(x = reorder(Species, total), y = total),fill="dodgerblue4") +
  coord_flip()+
  theme(axis.title = element_text(colour = "black",face = "bold"),
        axis.text =  element_text(colour = "black"))+
  labs(x="Species",y="Carcass abundance")

ggsave("paper_zero_graphs/carcass_abundance.PNG",width = 6,height = 6)

#sample description
data %>% 
  pull(Species) %>% 
  length()

data %>% 
  pull(Species) %>% 
  unique() %>% 
  length()

data %>% 
  filter(Species=="Baboon") %>% 
  pull(Species) %>% 
  length()

data %>% 
  filter(Species=="Baboon") %>% 
  pull(Species) %>% 
  unique() %>% 
  length()

data %>% 
  filter(Species!="Baboon") %>% 
  pull(Species) %>% 
  length()

data %>% 
  filter(Species!="Baboon") %>% 
  pull(Species) %>% 
  unique() %>% 
  length()

data %>% 
  subset(Species=="Other primate") %>% 
  pull(Species) %>% 
  length()

#assign all Primates to the designated column.
data <- data %>% 
  mutate(Primate=case_when(Species=="Baboon"~1,
                           Species=="Samango monkey"~1,
                           Species=="Other primate"~1,
                           TRUE ~ 0))
data <- data %>% 
  mutate(Category=case_when(Species=="Baboon"~"Primate",
                            Species=="Samango monkey"~"Primate",
                            Species=="Other primate"~"Primate",
                            TRUE ~ "Non-Primate"))
View(data)
anyNA(data$Primate)#check if there is any NA

#Check data before performing GLM
data %>% 
  subset(Primate==1) %>% 
  pull(Mne) %>% 
  sum()

data %>% 
  subset(Primate!=1) %>% 
  pull(Mne) %>% 
  length()

#Non-primate taxa
data %>% 
  subset(Primate!=1) %>% 
  distinct(Species,.keep_all = T) %>% 
  pull(Species) %>% 
  length()

#Primate taxa
data %>% 
  subset(Primate==1) %>% 
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
library(broom)
levels(data$Primate);str(data)


names(data)
View(data)

data1 <- data %>% 
  select(Primate,Category,Landscape,mDist_to_river,mDist_to_UREMA,mDist_to_gnp_pans,mDist_to_road,Mne) %>% 
  mutate(across(starts_with("mDist"), scale),
        landscape= 
                  case_when(
                    Landscape == "Rift Valley Riverine & Floodplain" ~ "Annually flooding area",
                    TRUE ~ "Non-annually flooding area"
                  )) %>% 
  mutate(flooding= 
           case_when(
             landscape == "Annually flooding area" ~ 1,
             landscape == "Non-annually flooding area"~0,
             TRUE ~ 0
           ))
table(data1$landscape)

View(data1)

#GLM
model1 <- glm(Primate ~ landscape + mDist_to_UREMA+ mDist_to_road+mDist_to_gnp_pans+mDist_to_river, family = binomial, data = data1)
summary(model1)
vif(model1)

##MODEL 2
model2 <- glm(Primate ~  landscape*(mDist_to_UREMA+ mDist_to_road+mDist_to_gnp_pans+mDist_to_river), family = binomial, data = data1)
summary(model2)
vif(model2)

model2_print <- model2 %>% tidy(exponentiate = T) %>% 
  View()

##MODEL 3
model3 <- glm(Primate ~  flooding*(mDist_to_UREMA+ mDist_to_road+mDist_to_gnp_pans+mDist_to_river), family = binomial, data = data1)
summary(model3)
vif(model3)

model2_print <- model2 %>% tidy(exponentiate = T) %>% 
  mutate(p.value=round(p.value,4));model2_print#Final model

View(model2_print)

model2_print %>% 
  mutate(across(where(is.numeric), round, digits = 3),
         Site=proper(term)) %>% 
  export("paper_zero_data/model2_print_chp0.csv")

# Model comparison
data.frame(M=c("Model1","Model2"),
           AIC=c(model1$aic,model2$aic))

data1 %>% 
  group_by(landscape,Category) %>% 
  summarise(Total=sum(Mne)) %>% 
  ggplot()+
  geom_col(aes(x=landscape,y=Total,fill=Category),position = position_dodge())+
  geom_text(aes(label = Total, x = landscape, y = Total,fill=Category), 
            size = 3, 
            position = position_dodge(width = 1), 
            vjust = -0.5)+  # Adjusts text position)+
  scale_fill_manual(values = c("dodgerblue4","coral2"))+
  labs(x="Landscape",y="Number of bones")
  
ggsave("paper_zero_graphs/Primates_vs_non_primates_bone_density.PNG",width = 7,height = 5,bg="white")

#the Fisher’s Exact Test 

shapiro.test(data1$Mne)

data2 <- data1 %>% 
  group_by(Category,landscape) %>% 
  summarise(Total=sum(Mne)) %>% 
  pivot_wider(names_from = landscape,values_from = Total)

# Perform Fisher's Exact Test
fisher.test(as.matrix(data2[,-1]))



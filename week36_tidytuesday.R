
# libraries

library(tidyverse)
library(ggrepel)
library(patchwork)

# load the data

tuesdata <- tidytuesdayR::tt_load('2020-09-01')
tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

#View(tuesdata)

# yield vs fertilizer

fert<-tuesdata[["cereal_crop_yield_vs_fertilizer_application"]]

fert1<-fert%>%
  drop_na()%>%
  filter(Year == 2017)%>%
  mutate(fertilizer_tonnes = `Nitrogen fertilizer use (kilograms per hectare)`*0.001)

# Relationship

# Add the regression line
p1<-ggplot(fert1, aes(x=`Nitrogen fertilizer use (kilograms per hectare)`, 
                      y=`Cereal yield (tonnes per hectare)`),
           label = Entity) + 
  geom_point()+
  geom_smooth(method=lm)+
  geom_label_repel(aes(label=ifelse(`Nitrogen fertilizer use (kilograms per hectare)`>180,
                                    as.character(Entity),'')),
                   hjust=0,vjust=0, color = "purple",
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "y")+
  geom_label_repel(aes(label=ifelse(`Cereal yield (tonnes per hectare)`>10,
                                    as.character(Entity),'')),
                   hjust=0,vjust=0, color = "purple",
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "y")+
  labs(
    x = "Nitrogen fertilizer use (tonnes per hectare)",
    y = "Cereal yield (tonnes per hectare)",
    title = "Cereal Yield vs Nitrogen Fertilzer Use, World Data, 2017",
    subtitle = "Cereal yield is measured in tonnes per hectare and Nitrogen fertilizer use in kilograms per hectare",
    caption = "Data Source: Our World in Data | Code: @magwanjiru "
  )+
  theme_classic() +
  theme(legend.position = "right")+
  theme(plot.title = element_text(color = "black", size = 16 
  ),
  plot.subtitle = element_text(color = "gray", 
                               size = 14))


p1

fert11<-fert%>%
  drop_na()%>%
  filter(Year == 2007)%>%
  mutate(fertilizer_tonnes = `Nitrogen fertilizer use (kilograms per hectare)`*0.001)


# Relationship

# Add the regression line
p2<-ggplot(fert11, aes(x=`Nitrogen fertilizer use (kilograms per hectare)`, 
                      y=`Cereal yield (tonnes per hectare)`),
           label = Entity) + 
  geom_point()+
  geom_smooth(method=lm)+
  geom_label_repel(aes(label=ifelse(`Nitrogen fertilizer use (kilograms per hectare)`>180,
                                    as.character(Entity),'')),
                   hjust=0,vjust=0, color = "purple",
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "y")+
  geom_label_repel(aes(label=ifelse(`Cereal yield (tonnes per hectare)`>7.5,
                                    as.character(Entity),'')),
                   hjust=0,vjust=0, color = "purple",
                   segment.size  = 0.2,
                   segment.color = "grey50",
                   direction     = "y")+
  labs(
    x = "Nitrogen fertilizer use (tonnes per hectare)",
    y = "Cereal yield (tonnes per hectare)",
    title = "Cereal Yield vs Nitrogen Fertilzer Use, World Data, 2007",
    subtitle = "Cereal yield is measured in tonnes per hectare and Nitrogen fertilizer use in kilograms per hectare",
    caption = "Data Source: Our World in Data | Code: @magwanjiru "
  )+
  theme_classic() +
  theme(legend.position = "right")+
  theme(plot.title = element_text(color = "black", size = 16 
  ),
  plot.subtitle = element_text(color = "gray", 
                               size = 14))

p2

# patchwork in action
# idea is to just compare 10 years down the line
(p2)/(p1)



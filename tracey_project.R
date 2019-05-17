library(tidyverse)
library(emmeans)
library(multcompView)
library(lmerTest)
library(janitor)

cotton <- read_csv('tracey_BiologicalTrialData1.csv') %>% 
  clean_names('snake')
cotton$weekf <- cotton$week
str(cotton)
factcols <- c('plot', 'transect','map_kg_ha','nitrogen_kg_ha', 'weekf')
cotton[factcols] <- lapply(cotton[factcols], factor)

plantcols <- c('plant_1', 'plant_2','plant_3', 'plant_4','plant_5','plant_6','plant_7')
with(cotton, table(map_kg_ha, nitrogen_kg_ha, treatment))

cotton2 <- gather(cotton, key = 'plant_number', value = 'plant_height', plantcols)

ggplot(cotton2, aes(weekf, plant_height, colour = treatment)) + 
  geom_boxplot()

ggplot(cotton2, aes(week, plant_height, colour = treatment)) + 
  geom_point() +
  geom_smooth(alpha=.2)

ggplot(cotton2, aes(week, plant_height, colour = treatment)) + 
  geom_point() +
  geom_smooth(alpha=.2)+
  facet_wrap(~nitrogen_kg_ha*map_kg_ha)

lm1 <- lmer(plant_height~treatment+map_kg_ha+nitrogen_kg_ha+(1|transect) + (1|plot), cotton2)
anova(lm1)
summary(lm1)

lm2 <- lmer(plant_height~map_kg_ha+nitrogen_kg_ha+treatment+weekf+
              treatment*weekf + treatment*nitrogen_kg_ha +
              treatment*map_kg_ha+(1|transect) + (1|plot), cotton2)
anova(lm2)

# A shorter way of the lm2
lm3 <- lmer(plant_height~weekf*(nitrogen_kg_ha + treatment + map_kg_ha)+
              (1|transect) + (1|plot), cotton2)
anova(lm3)

emmeans(lm3, pairwise~nitrogen_kg_ha|weekf)


resultsplot <- summary(emmeans(lm3, ~treatment*weekf))

ggplot(resultsplot, aes(weekf, emmean, colour = treatment)) + 
  geom_point(position = position_dodge(width=.2))+
  geom_errorbar(aes(ymin=emmean-SE, ymax= emmean+SE), width = .2, position = position_dodge(width = .2))+
  geom_smooth()

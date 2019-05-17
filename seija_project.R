library(tidyverse)
library(emmeans)
library(multcompView)
library(lmerTest)
library(janitor)

# Import Seija's data
N1 <- read_csv('Comparison_Experiment_Kcl_v_K2SO4.csv')
N1 <- clean_names(N1, case = "snake")
N1$extractant <- str_replace_all(N1$extractant,' ','')


# Add in the treatment (fertiliser) intially omited from the dataset
N1$treatment <- rep(c("urea", 'water'), each = 12, times=3)
N1$rep_id <- with(N1, interaction(soil, treatment, extractant))
str(N1)

# Gather the adjusted data to make it tidy
N2 <- N1 %>% gather(key=n_type, value = n_amount, c(7,9,11))

ggplot(N1, aes(soil, log(nh3n_adjusted), colour=extractant))+
  geom_boxplot() +
  facet_wrap(~treatment)

ggplot(N1, aes(treatment, log(nh3n_adjusted), colour=extractant))+
  geom_boxplot() +
  facet_wrap(~soil)


ggplot(N2, aes(soil, log(n_amount), colour = extractant)) +
  geom_boxplot() +
  facet_wrap(~n_type)

# Remove water from original data to look at the two extractant treatments without the confounding of the water

N3 <- subset(N1, extractant != 'H2O')

#Additive model
lm2 <- lmer(log(nh3n_adjusted)~extractant+treatment+soil+(1|rep_id), N3)
anova(lm2)
summary(lm2)

#Factorial model (treatment exluded as additive model showed little effect)
lm3 <- lmer(log(nh3n_adjusted)~extractant*soil+(1|rep_id), N3)
anova(lm3)
summary(lm3)
emmeans(lm3, pairwise~extractant|soil, type = 'response')
emmeans(lm3, revpairwise~extractant|soil, type = 'response') #Reverse pairwise will swap the results over (ie, show an increase instead of a decrease)

plot(lm3)

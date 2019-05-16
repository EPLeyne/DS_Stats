library(tidyverse)
library(emmeans)
library(multcompView)
library(lmerTest)

# Import Seija's data

N_experiment <- read_csv('Comparison_Experiment_Kcl_v_K2SO4.csv')

N_experiment_2 <- N_experiment %>% gather(key=N_type, value = amount, c(7,9,11))

ggplot(N_experiment_2, aes(Soil, amount, colour = Extractant)) +
  geom_boxplot() +
  facet_wrap(~N_type, scales = "free")

ggplot(N_experiment, aes(Soil, NH3N_adjusted, colour = Extractant)) +
  geom_boxplot()

ggplot(N_experiment, aes(Soil, log(NH3N_adjusted), colour = Extractant)) +
  geom_boxplot()

# Remove water

N_experiment_3 <- subset(N_experiment, Extractant != 'H2O')

lm1 <- lm(log(NH3N_adjusted)~Soil*Extractant, N_experiment_3)
anova(lm1)
emmeans(lm1, pairwise~Extractant|Soil, type = 'response')
emmeans(lm1, revpairwise~Extractant|Soil, type = 'response')

plot(lm1, 1:2)

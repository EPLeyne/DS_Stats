library(tidyverse)
library(emmeans)
library(multcompView)
library(lmerTest)

#lwr - full factorial model
lwr <- read_csv('Prac 3 mock LWR.csv')

ggplot(lwr, aes(x=GeneB, y= LWR, colour = GeneA)) + 
  geom_boxplot() + geom_point()

lm1 <- lm(LWR~GeneA*GeneB, data=lwr)
anova(lm1)

# Analysis of Variance Table
# 
# Response: LWR
# Df  Sum Sq Mean Sq F value    Pr(>F)    
# GeneA        1  86.436  86.436 15.3181 0.0003874 ***
#   GeneB        1 208.849 208.849 37.0121 5.372e-07 ***
#   GeneA:GeneB  1  24.336  24.336  4.3128 0.0450232 *  
#   Residuals   36 203.138   5.643                      
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

summary(lm1)

# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      58.9800     0.7512  78.516  < 2e-16 ***
#   GeneAAA          -4.5000     1.0623  -4.236 0.000151 ***
#   GeneBBB          -6.1300     1.0623  -5.770 1.41e-06 ***
#   GeneAAA:GeneBBB   3.1200     1.5024   2.077 0.045023 *  

## Interaction mean is (58.98 - 4.5 - 6.13 + 3.12)
## GeneAAA:GeneBBB Estimate is the measure of the effect between GeneA and GeneB and SE is the measure of the 
## uncertainty of that estimate.

emmeans(lm1,pairwise~GeneA|GeneB)

emmeans(lm1,pairwise~GeneB|GeneA)
plot(lm1, which=1:2)


#Cabbage - additive model
# Is an additive model as not really worried about the effect of days, so only want to measure Cultivar effect

cabbage <- read_csv('Prac 3 cabbage data.csv')
str(cabbage)
ggplot(cabbage, aes(Date, VitC, colour = Cult)) +
  geom_boxplot() + geom_point()

# Model the data

lm2 <- lm(VitC~Cult*Date, data = cabbage) # Full factorial to see if there is an effect from date or Cult*Date
anova(lm2)
summary (lm2)
lm3 <- lm(VitC~Cult+Date, data = cabbage) # Additive model as Cult*Date had no effect
anova(lm3)
emmeans(lm3, pairwise~Cult)
plot (lm3, which=1:2)



# Barley

barley <- read_csv('Prac 3 barley yield.csv')
str(barley)
barley2 <- gather(barley, key=Year, value = Yield, 4:5)

ggplot(barley2, aes(Var, Yield, colour=Year)) +
  geom_boxplot()
ggplot(barley2, aes(Var, Yield, colour=Year)) +
  facet_wrap('Loc') + geom_point()
ggplot(barley2, aes(Var, Yield, colour=Loc)) +
  geom_boxplot()

lm4 <- lm(Yield~Var+Loc, data = barley2)
anova(lm4)
lm5 <- lm(Yield~Var+Loc+Year, data = barley2)
anova(lm5)
emmeans (lm4, pairwise~Var)

plot(lm4, which = 1:2)
means1 <- emmeans(lm5, ~Var)
cld(means1)
pwpp(means1)

plot(means1)


# Forest

forest <- read_csv('Prac 3 forest.csv')

str(forest)

ggplot(forest, aes(Density, QuadDiam, colour = StandType))+
  geom_point() + 
  geom_smooth(method = 'lm', alpha = 0.2)

forest_lm <- lm(Density ~ QuadDiam * StandType, forest)
anova(forest_lm)
summary(forest_lm)
emmeans(forest_lm, pairwise~StandType)

forest_lm1 <- lm(Density ~ QuadDiam + StandType, forest)
anova(forest_lm1)
summary(forest_lm1)
emmeans(forest_lm1, pairwise~StandType)

plot(forest_lm, which = 1:2)
plot(forest_lm1, which = 1:2)

# Prac4 Photosynthesis

photo <- read_csv('Prac 4 photosynthesis.csv')
str(photo)
photo$Position <- factor(photo$Position)
photo$Temp <- factor(photo$Temp)

ggplot(photo, aes(Temp, PhotoRate, colour = Position)) +
  geom_point() + labs(x = "Temperature")

ggplot(photo, aes(Position, PhotoRate, colour = Temp)) +
  geom_point() + labs(x = "Position")

photo_lmer <- lmer(PhotoRate~Temp+(1|Position), photo)
anova(photo_lmer)
summary(photo_lmer)
emmeans(photo_lmer, pairwise~Temp)

plot(photo_lmer)


# Prac 4 Drought

drought <- read_csv('Prac 4 drought data.csv')

str(drought)
drought$plant <- factor(drought$plant)
drought$Genotype <- factor(drought$Genotype)
drought$Genotype <- relevel(drought$Genotype, ref="WT")
drought$WaterCondition <- factor(drought$WaterCondition)
drought$WaterCondition <- relevel(drought$WaterCondition, ref = "Normal")

drought$cond <- with(drought, interaction(WaterCondition, Genotype))
ggplot(drought, aes(cond, Temperature, colour = plant)) + geom_point()

# Naive method, doesn't factor in the number of plants, thinks there is more independant data
#  Therefore returns lower p-values
drought_lm <- lm(Temperature~Genotype*WaterCondition, drought)
anova(drought_lm)
emmeans(drought_lm, ~Genotype*WaterCondition)

# Proper method factors in the same plants as less than independant and returns a higher p-value
drought_lmer <- lmer(Temperature~Genotype*WaterCondition + (1|plant), drought)
anova(drought_lmer)
emmeans(drought_lmer, ~Genotype*WaterCondition)


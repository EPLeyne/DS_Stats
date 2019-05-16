library(tidyverse)
library(emmeans)

wheat <- read_csv('wheat yield.csv')

wheat_high <- filter(wheat, Variation == 'High')
wheat_low <- filter(wheat, Variation == 'Low')

ggplot(wheat, aes(Variety, Yield, colour = Variety)) + geom_point() + facet_wrap(~Variation)

#T test
t.test(Yield~Variety, data = wheat_high, var.equal=TRUE)
t.test(Yield~Variety, data = wheat_low, var.equal=TRUE)


# linear model
lm1 <- lm(Yield ~ Variety, data = wheat_low)
#ANOVA of the linear model
anova (lm1)
#summary of the linear model
summary (lm1)
#The means of the varieties 
emmeans(lm1, ~Variety)

#ANOVA analysis for single experiemntal value (yield) with more than 1 mean (multiple varieties)
wheat2 <- read_csv('wheat yield PLUS.csv')
## To change the order of the order of the output
wheat2$Variety <- factor(wheat2$Variety, c("Standard", "New", "NewPlus"))

str(wheat2)
ggplot(wheat2, aes(Variety, Yield, colour = Variety)) + geom_point()

lm2 <- lm(Yield ~ Variety, data = wheat2)
anova (lm2)
summary (lm2)
# To show what the p-values are between the varieties
emmeans(lm2, pairwise~Variety)

plot (lm2, which = c(1,2))


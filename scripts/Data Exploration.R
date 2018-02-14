#Tachinid Competitive and Development Project - Data Exploration
#Keaton Wilson
#2018-02-13
#keatonwilson@me.com

#Packages
library(tidyverse)

#Importing the master data file
tach_master = read_csv(file = "Data/TachDataFull.csv")

#Cleaning up a bit
tach_master = tach_master %>%
  select(-SiblingNumber, -NutritionIndex)
tach_master$Sex = factor(tach_master$Sex)

#First steps: 1. Let's get sibling numbers for each group so we can calculate the nutritional index.
siblings = tach_master %>%
  group_by(CaterpillarID) %>%
  summarize(sib_number = max(FlyID))

#left_Join this
tach_master = tach_master %>%
  left_join(siblings)

#Generating a Nutritional Index that is the head capsule width divided by the number of siblings in a cohort
tach_master = tach_master %>%
  mutate(nut_index = HeadCapsuleWidth/sib_number)

#Let's impute NA values. 
library(caret)
install.packages("VIM")
library(VIM)


tach_master_impute = as.tibble(kNN(tach_master, variable = colnames(tach_master[8:13])))
tach_master_impute$FlyWeight = as.numeric(tach_master_impute$FlyWeight)



#Exploratory plots!

#Fly weight as a function of sibling number
ggplot(tach_master_impute, aes(x = sib_number, y = FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6, position = "jitter") +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1))

#Fly weight as a function of head capsule size
ggplot(tach_master_impute, aes(x = HeadCapsuleWidth, y = FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6, position = "jitter") +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1))

#best model seems to be one that includes both, additively.
lm1 = lm(FlyWeight ~ sib_number + HeadCapsuleWidth, data = tach_master_impute)
summary(lm1)
#Also interesting to note how sex doesn't come into play here... it makes the model worse. Indicating that this relationship isn't
#different among the sexes


#Ok, now the fun stuff.

ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(HeadWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(ThoraxWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(AbWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(WingWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(as.numeric(LegsWeight)), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()


#Ok, these are rad, and Goggy will love them, I think. 

#Didn't he do relative body part size plotted against size?

ggplot(tach_master_impute, aes(x = FlyWeight, y = HeadWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

ggplot(tach_master_impute, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

ggplot(tach_master_impute, aes(x = FlyWeight, y = AbWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

ggplot(tach_master_impute, aes(x = FlyWeight, y = WingWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()

#Seems like there might a wing-weight that is fucked up

ggplot(tach_master_impute, aes(x = FlyWeight, y = as.numeric(LegsWeight)/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic()


#Comparing body parts
ggplot(tach_master_impute, aes(x = ThoraxWeight/FlyWeight, y = AbWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm")





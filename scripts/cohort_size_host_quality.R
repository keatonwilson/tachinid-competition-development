#Script for Building cohort and size figures and analysis
#Keaton Wilson
#keatonwilson@me.com
#2018-09-06
#Packages
library(tidyverse)
library(caret)
library(VIM)
library(ggpubr)
library(car)

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

#Let's impute missing values with a weighted mean
tach_master_impute = as.tibble(kNN(tach_master, variable = colnames(tach_master[8:13]), numFun = "weightedMean"))
tach_master_impute$FlyWeight = as.numeric(tach_master_impute$FlyWeight)

#Number of imputations
tach_master_impute %>%
  select(UniqueFlyID, FlyWeight_imp:LegsWeight_imp) %>%
  summarize(sum_fw = sum(FlyWeight_imp),
            sum_hw = sum(HeadWeight_imp),
            sum_thw = sum(ThoraxWeight_imp),
            sum_abw = sum(AbWeight_imp),
            sum_ww = sum(WingWeight_imp),
            sum_lw = sum(LegsWeight_imp))

#Total observations 
tach_master_impute %>%
  select(FlyWeight, HeadWeight, ThoraxWeight, AbWeight, WingWeight, LegsWeight) %>%
  summarize(n())


##PLOTS##
#Fly weight as a function of sibling number
Fig.1 = ggplot(tach_master_impute, aes(x = sib_number, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 4, alpha = 0.7, aes(shape = FlyWeight_imp)) +
  theme_classic(base_size = 20) +
  geom_smooth(method = "lm", aes(group = 1), color = "black") +
  xlab("Cohort Size") +
  ylab("Adult Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male")) +
  scale_shape_discrete("Imputed")

#ggsave(Fig.1, file = "./output/Fig.1.pdf", device = "pdf", width = 10, height = 8, units = "in")

#Summary table
tach_master_impute %>%
  group_by(CaterpillarID) %>%
  summarize(sib_number = mean(sib_number),
            avg_fly_weight = mean(FlyWeight, na.rm = TRUE), 
            n = n()) %>%
  arrange(desc(sib_number))

#Fly weight as a function of head capsule size
Fig.2 = ggplot(tach_master_impute, aes(x = HeadCapsuleWidth, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 4, alpha = 0.7, aes(shape = FlyWeight_imp)) +
  theme_classic(base_size = 20) +
  geom_smooth(method = "lm", aes(group = 1), color = "black") +
  xlab("Host head-capsule width (mm)") +
  ylab("Adult Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male")) +
  scale_shape_discrete("Imputed")

fig_2_panel = ggarrange(Fig.1, Fig.2, labels = "auto", label.x = 0.85, common.legend = TRUE, nrow = 2)

ggsave(fig_2_panel, file = "./output/Fig.2.pdf", device = "pdf", width = 8.5, height = 11, units = "in")

#best model seems to be one that includes both cohort size and headcapsule width, additively.
lm0 = lm(FlyWeight ~ sib_number, data = tach_master_impute)
lm1 = lm(FlyWeight ~ sib_number + HeadCapsuleWidth, data = tach_master_impute)
lm2 = lm(FlyWeight ~ sib_number*HeadCapsuleWidth, data = tach_master_impute)
lm3 = lm(FlyWeight ~ sib_number*HeadCapsuleWidth*Sex, data = tach_master_impute)

summary(lm0)
summary(lm1)
summary(lm2)
summary(lm3)

AIC(lm0)
AIC(lm1)
AIC(lm2)
AIC(lm3)

#EVERYTHING IS THE SAME AS ABOVE EXCEPT NO IMPUTATION
tach_master = tach_master %>%
  mutate(FlyWeight = as.numeric(FlyWeight))

#Fly weight as a function of sibling number
Fig.1 = ggplot(tach_master, aes(x = sib_number, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 4, alpha = 0.7) +
  theme_classic(base_size = 20) +
  geom_smooth(method = "lm", aes(group = 1), color = "black") +
  xlab("Cohort Size") +
  ylab("Adult Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male"))

#ggsave(Fig.1, file = "./output/Fig.1.pdf", device = "pdf", width = 10, height = 8, units = "in")

#Summary table
tach_master %>%
  group_by(CaterpillarID) %>%
  summarize(sib_number = mean(sib_number),
            avg_fly_weight = mean(FlyWeight, na.rm = TRUE), 
            n = n()) %>%
  arrange(desc(sib_number))

#Fly weight as a function of head capsule size
Fig.2 = ggplot(tach_master, aes(x = HeadCapsuleWidth, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 4, alpha = 0.7) +
  theme_classic(base_size = 20) +
  geom_smooth(method = "lm", aes(group = 1), color = "black") +
  xlab("Host head-capsule width (mm)") +
  ylab("Adult Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male"))

fig_2_panel = ggarrange(Fig.1, Fig.2, labels = "auto", label.x = 0.85, common.legend = TRUE, nrow = 2)

ggsave(fig_2_panel, file = "/Users/KeatonWilson/Documents/Writing/Tachinid Development/Figures/Nature Figures/no_impute/Fig2Panel.pdf", device = "pdf", width = 8.5, height = 11, units = "in")

#best model seems to be one that includes both cohort size and headcapsule width, additively.
lm0 = lm(FlyWeight ~ sib_number, data = tach_master)
lm1 = lm(FlyWeight ~ sib_number + HeadCapsuleWidth, data = tach_master)
lm2 = lm(FlyWeight ~ sib_number*HeadCapsuleWidth, data = tach_master)
lm3 = lm(FlyWeight ~ sib_number*HeadCapsuleWidth*Sex, data = tach_master)

summary(lm0)
summary(lm1)
summary(lm2)
summary(lm3)

AIC(lm0)
AIC(lm1)
AIC(lm2)
AIC(lm3)

#Residual plot
tach_master_no_na = tach_master %>%
  filter(!is.na(FlyWeight) & !is.na(HeadCapsuleWidth))

lm_resid = lm(FlyWeight ~ HeadCapsuleWidth, data = tach_master_no_na)

tach_master_no_na %>%
  mutate(resid = residuals(lm_resid)) %>%
  ggplot(aes(x = resid, y = FlyWeight)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

#Checking average differences between males and females #IMPUTED
tach_master_impute %>%
  group_by(Sex) %>%
  summarize(avg_weight = mean(FlyWeight, na.rm = TRUE),
            sd = sd(FlyWeight, na.rm = TRUE),
            n = n())

lm.sex = lm(FlyWeight ~ Sex, data = tach_master_impute)
summary(lm.sex)

#Checking average differences between males and females #NON-IMPUTED
tach_master %>%
  group_by(Sex) %>%
  summarize(avg_weight = mean(FlyWeight, na.rm = TRUE),
            sd = sd(FlyWeight, na.rm = TRUE),
            n = n())

lm.sex = lm(FlyWeight ~ Sex, data = tach_master)
summary(lm.sex)

tach_master_impute %>%
  filter(!is.na(FlyWeight)) %>%
  mutate(residuals = lm0$residuals) %>%
  ggplot(aes(x = residuals, y = HeadCapsuleWidth)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_classic()

#Also interesting to note how sex doesn't come into play here... it makes the model worse. Indicating that this relationship isn't
#different among the sexes

#Also need to show that there is a positive correlation between head-capsule width and weight @ wander
#Read in bigger data set

head_cap_wander = read_csv(file = "Data/Parasitized Caterpillar Data.csv")

head_cap_wander %>%
  filter(is.na(Parasitized)) %>%
  filter(HeadCapsuleWidth > 5) %>%
ggplot(aes(x = HeadCapsuleWidth, y = WanderWeight)) +
  geom_jitter(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm") +
  xlab("Head Capsule Width (mm)") +
  ylab("Weight at wandering (g)")

d1 = head_cap_wander %>%
  filter(is.na(Parasitized)) %>%
  filter(HeadCapsuleWidth > 5)

lm_test = lm(WanderWeight ~ HeadCapsuleWidth, data = d1)

lm_head_cap = lm(WanderWeight ~ HeadCapsuleWidth, data = head_cap_wander %>%
                   filter(is.na(Parasitized)))
summary(lm_head_cap)

#Not great - but there are a lot of other variables embedded in that - perhaps it's better to use other examples from the literature.


old_data_size = tibble(CaterpillarID = c(3, 6, 7, 14, 18, 19, 23, 24, 25, 26, 34, 37, 38, 40), sib_number = c(13, 8, 47, 50, 12, 5, 30, 17, 26, 31, 30, 22, 30, 35))

cohort_size = bind_rows(old_data_size, siblings)

ggplot(cohort_size, aes(x = sib_number)) +
  geom_histogram(binwidth = 5, aes(y = ..density..)) +
  geom_density() +
  theme_classic()

#Trying stuff out
tach_master

ggplot(tach_master, aes(x = WanderWeight, y = FlyWeight, color = Sex)) +
  geom_point() +
  theme_classic() 
ggplot(tach_master, aes(y = HeadCapsuleWidth, x = WanderWeight)) +
  geom_point() +
  theme_classic()

summary(lm(HeadCapsuleWidth ~ WanderWeight, data = tach_master))

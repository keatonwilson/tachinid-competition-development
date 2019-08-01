#Tachinid LME and New Figs
#Keaton Wilson 
#keatonwilson@me.com
#2019-07-30

#packages
library(tidyverse)
library(caret)
library(VIM)
library(ggpubr)
library(car)
library(nlme)
library(lme4)
library(car)
library(MASS)
library(lmerTest)

#Importing the master data file
tach_master = read_csv(file = "Data/TachDataFull.csv")

#Cleaning up a bit
tach_master = tach_master %>%
  dplyr::select(-SiblingNumber, -NutritionIndex)
tach_master$Sex = factor(tach_master$Sex)

#First steps: 1. Let's get sibling numbers for each group so we can calculate the nutritional index.
siblings = tach_master %>%
  group_by(CaterpillarID) %>%
  summarize(sib_number = max(FlyID))

#left_Join this
tach_master = tach_master %>%
  left_join(siblings)

#Fixing some data errors
tach_master[tach_master$CaterpillarID == "9",]$HeadCapsuleWidth = 6.03
tach_master[tach_master$CaterpillarID == "14",]$HeadCapsuleWidth = 5.97

#Checking
tach_master %>%
  group_by(CaterpillarID, sib_number, HeadCapsuleWidth) %>%
  summarize(n = n())

#Converting to numeric
tach_master$FlyWeight = as.numeric(tach_master$FlyWeight)

#Drop NAs
tach_master_no_na = tach_master %>%
  dplyr::select(CaterpillarID, Sex, FlyWeight, sib_number, HeadCapsuleWidth) %>%
  drop_na()

#Models
lm1 = lmer(FlyWeight ~ sib_number + HeadCapsuleWidth + (1|CaterpillarID), data = tach_master_no_na)
lm1_2 = lme(FlyWeight ~ sib_number + HeadCapsuleWidth, 
            random=(~1|CaterpillarID), data = tach_master_no_na)
lm1_2
summary(lm1_2)

coef(summary(lm1))[, "Estimate"]

library(effects)
newdat <- expand.grid(sib_number=c(min(tach_master$sib_number),
                                   max(tach_master$sib_number)),
                      HeadCapsuleWidth=c(min(tach_master$HeadCapsuleWidth),
                                         max(tach_master$HeadCapsuleWidth)))

pe_sib = predictorEffect("sib_number", lm1_2)
pe_head_cap = predictorEffect("HeadCapsuleWidth", lm1_2)

pe_sib_line = data.frame(x = pe_sib$x, y = pe_sib$fit, 
                         ymin = pe_sib$upper, ymax = pe_sib$lower)

pe_headcap_line = data.frame(x = pe_head_cap$x, y = pe_head_cap$fit, 
                         ymin = pe_head_cap$upper, ymax = pe_head_cap$lower)


Fig.1a = tach_master_no_na %>%
  mutate(pred = predict(lm1_2, level = 0),
         fitted = fitted(lm1_2, level = 0)) %>%
  ggplot(aes(x = sib_number, y = FlyWeight)) +
  geom_point(aes(color = Sex), size = 2) +
  geom_point(aes(y = pred), size = 3) +
  geom_line(data = pe_sib_line, aes(x = sib_number, y = y)) +
  geom_ribbon(data = pe_sib_line, aes(x = sib_number, ymin = ymin, ymax = ymax, y = y), 
              alpha = 0.2) +
  theme_classic(base_size = 20) +
  xlab("Cohort Size") +
  ylab("Adult Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male"))

Fig.1b = tach_master_no_na %>%
  mutate(pred = predict(lm1_2, level = 0),
         fitted = fitted(lm1_2, level = 0)) %>%
  ggplot(aes(x = HeadCapsuleWidth, y = FlyWeight)) +
  geom_point(aes(color = Sex), size = 2) +
  geom_point(aes(y = pred), size = 3) +
  geom_line(data = pe_headcap_line, aes(x = HeadCapsuleWidth, y = y)) +
  geom_ribbon(data = pe_headcap_line, aes(x = HeadCapsuleWidth, ymin = ymin, ymax = ymax, y = y), 
              alpha = 0.2) +
  theme_classic(base_size = 20) +
  xlab("Head Capsule Width (mm)") +
  ylab("Adult Fly Weight (mg)") +
  scale_color_discrete(labels = c("Female", "Male"))

fig_2_panel = ggarrange(Fig.1a, Fig.1b, labels = "auto", label.x = 0.85, common.legend = TRUE, nrow = 2)

ggsave(fig_2_panel, file = "/Users/KeatonWilson/Documents/Writing/Tachinid Development/Figures/Nature Figures/no_impute/Fig2Panel_V2.pdf", device = "pdf", width = 8.5, height = 11, units = "in")

ggsave(fig_2_panel, file = "./output/Fig2Panel_V2.pdf", device = "pdf", width = 8.5, height = 11, units = "in")


#Model Testing
lme_1 = lme(FlyWeight ~ sib_number, 
            random=(~1|CaterpillarID), data = tach_master_no_na)

lme_2 = lme(FlyWeight ~ sib_number + HeadCapsuleWidth, 
            random=(~1|CaterpillarID), data = tach_master_no_na)

lme_3 = lme(FlyWeight ~ sib_number*HeadCapsuleWidth, 
            random=(~1|CaterpillarID), data = tach_master_no_na)

lme_4 = lme(FlyWeight ~ sib_number*HeadCapsuleWidth*Sex, 
            random=(~1|CaterpillarID), data = tach_master_no_na)

anova(lme_1)
summary(lme_1)
rsquared(lme_1)
AIC(lme_1)

anova(lme_2)
summary(lme_2)
rsquared(lme_2)
AIC(lme_2)

anova(lme_3)
summary(lme_3)
rsquared(lme_3)
AIC(lme_3)

anova(lme_4)
summary(lme_4)
rsquared(lme_4)
AIC(lme_4)

#Summary statistics
std <- function(x) sd(x)/sqrt(length(x))
tach_master %>%
  group_by(Sex) %>%
  summarize(mean = mean(FlyWeight, na.rm = TRUE), 
            se = std.error(FlyWeight, na.rm = TRUE))

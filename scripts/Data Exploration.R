#Tachinid Competitive and Development Project - Data Exploration
#Keaton Wilson
#2018-02-13
#keatonwilson@me.com

#Packages
library(tidyverse)
library(purrr)
library(broom)
library(ggpubr)

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
#install.packages("VIM")
#install.packages("VIM")
library(VIM)


tach_master_impute = as.tibble(kNN(tach_master, variable = colnames(tach_master[8:13]), numFun = "weightedMean"))
tach_master_impute$FlyWeight = as.numeric(tach_master_impute$FlyWeight)



#Exploratory plots!

#Fly weight as a function of sibling number
Fig.1 = ggplot(tach_master_impute, aes(x = sib_number, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 3, alpha = 0.7) +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1)) +
  xlab("Cohort Size") +
  ylab("Adult Fly Weight (mg)")

ggsave(Fig.1, file = "./output/Fig.1.pdf", device = "pdf", width = 10, height = 8, units = "in")

#Summary table
tach_master_impute %>%
  group_by(CaterpillarID) %>%
  summarize(sib_number = mean(sib_number),
            avg_fly_weight = mean(FlyWeight, na.rm = TRUE), 
            n = n()) %>%
  arrange(desc(sib_number))



#Fly weight as a function of head capsule size
Fig.2 = ggplot(tach_master_impute, aes(x = HeadCapsuleWidth, y = FlyWeight, color = Sex)) +
  geom_jitter(size = 3, alpha = 0.7) +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1)) +
  xlab("Host head-capsule width (mm)") +
  ylab("Adult Fly Weight (mg)")

ggsave(Fig.2, file = "./output/Fig.2.pdf", device = "pdf", width = 10, height = 8, units = "in")

#best model seems to be one that includes both, additively.
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

#Checking average differences between males and females

tach_master_impute %>%
  group_by(Sex) %>%
  summarize(avg_weight = mean(FlyWeight, na.rm = TRUE),
            sd = sd(FlyWeight, na.rm = TRUE),
            n = n())

lm.sex = lm(FlyWeight ~ Sex, data = tach_master_impute)

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
ggplot(head_cap_wander, aes(x = HeadCapsuleWidth, y = WanderWeight)) +
  geom_jitter(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm") +
  xlab("Head Capsule Width (mm)") +
  ylab("Weight at wandering (g)")

lm_head_cap = lm(WanderWeight ~ HeadCapsuleWidth, data = head_cap_wander)

#Not great - but there are a lot of other variables embedded in that - perhaps it's better to use other examples from the literature.

#Ok, now the fun stuff.

#Head weight versus body weight 
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(HeadWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(-1.5,3)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Head Weight (mg)")
  
#No difference between males and females
lmhvb = lm(log(HeadWeight) ~ log(FlyWeight), data = tach_master_impute)
summary(lmhvb)
confint(lmhvb)
#Let's do a Wald's test to check and see if the slope is sig. diff from 1
library(car)
linearHypothesis(lmhvb, "log(FlyWeight) = 1")

#Slope is significantly less than 1. 


#Thorax versus Body  
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(ThoraxWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(-1.5,3)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Thorax Weight (mg)")

#Differences between males and females
lmtvbvs = lm(log(ThoraxWeight) ~ Sex/log(FlyWeight) -1, data = tach_master_impute)
summary(lmtvbvs)
#Testing differences from a slope of 1
linearHypothesis(lmtvbvs, "SexF:log(FlyWeight) = 1")
linearHypothesis(lmtvbvs, "SexM:log(FlyWeight) = 1")


#Yep, Males are significantly higher, no difference in slope though.

#Abdomen versus Body
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(AbWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-1.5, 3), ylim = c(-1.5,3)) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Abdomen Weight (mg)")

#Things are reversed. Females are significantly higher, no difference in slopes. 
lmavbvs = lm(log(AbWeight) ~ log(FlyWeight) + Sex, data = tach_master_impute)


#Wings versus Body
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(WingWeight), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3,3)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Wing Weight (mg)")

#No difference between males and females

#Legs versus Body
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(as.numeric(LegsWeight)), color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(-3, 3), ylim = c(-3,3)) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Legs Weight (mg)")


#Ok, these are rad, and Goggy will love them, I think. 


#Can we do them all on one plot!?
ggplot(tach_master_impute, aes(x = log(FlyWeight), y = log(HeadWeight), shape = Sex)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, aes(group = 1), color = "black") +
  geom_point(aes(x = log(FlyWeight), y = log(ThoraxWeight)), size = 2, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, aes(x = log(FlyWeight), y = log(ThoraxWeight))) +
  geom_point(aes(x = log(FlyWeight), y = log(AbWeight)), size = 2, alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE, aes(x = log(FlyWeight), y = log(AbWeight))) +
  geom_point(aes(x = log(FlyWeight), y = log(WingWeight)), size = 2, alpha = 0.5, shape = 4) +
    geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1, x = log(FlyWeight), y = log(WingWeight))) +
  geom_point(aes(x = log(FlyWeight), y = log(as.numeric(LegsWeight))), size = 2, alpha = 0.5, shape = 3) +
    geom_smooth(method = "lm", se = FALSE, color = "black", aes(group = 1, x = log(FlyWeight), y = log(as.numeric(LegsWeight)))) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 2) +
  coord_cartesian(xlim = c(0, 3), ylim = c(-3,3)) +
  xlab("log10 Body Weight (mg)") +
  ylab("log10 Body Part Weight (mg)") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 2.2, label = "Thorax", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 1.5, label = "Abdomen", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 0.5, label = "Head", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = 0.1, label = "Legs", size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 3, y = -1.3, label = "Wings", size = 10), color = "black")


#Better way to make the plot above - but with changes described by Robbie
tach_master_impute
tach_master_impute_long = tach_master_impute %>%
  select(Sex, FlyWeight:WingIntact) %>%
  gather(key = "organ", value = "organ_weight", -Sex, -FlyWeight, -WingIntact)
tach_master_impute_long$organ = factor(tach_master_impute_long$organ)
tach_master_impute_long$organ_weight = as.numeric(tach_master_impute_long$organ_weight)

#The interesting body parts
Fig.3 = tach_master_impute_long %>%
  filter(organ == "ThoraxWeight" | organ == "AbWeight") %>%
ggplot(aes(x = log(FlyWeight), y = log(organ_weight), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", aes(lty = Sex)) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 3) +
  coord_cartesian(xlim = c(0.5, 3), ylim = c(-3,3)) +
  scale_color_discrete(guide = FALSE) +
  scale_shape_discrete(labels = c("Female", "Male")) +
  scale_linetype_discrete(guide = FALSE) +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 2.2, label = "Thorax",
                                      size = 9), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 1.5, label = "Abdomen",
                                      size = 9), color = "black") +
  ylab("log10 Weight (mg)") +
  xlab("log10 Body Weight (mg)")

ggsave(Fig.3, file = "./output/Fig.3.pdf", device = "pdf", width = 10, height = 8, units = "in")


#The rest
Fig.4 = tach_master_impute_long %>%
  filter(organ %in% c("HeadWeight", "WingWeight", "LegsWeight")) %>%
  ggplot(aes(x = log(FlyWeight), y = log(organ_weight), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", aes(group = organ)) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 3) +
  coord_cartesian(xlim = c(0.5, 3), ylim = c(-3,3)) +
  scale_color_discrete(guide = FALSE) +
  scale_shape_discrete(labels = c("Female", "Male")) +
  scale_linetype_discrete(guide = FALSE) +
  ylab("log10 Weight (mg)") +
  xlab("log10 Body Weight (mg)") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 0.5, label = "Head", 
                                      size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 0.1, label = "Legs", 
                                      size = 10), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = -1.4, label = "Wings", 
                                      size = 10), color = "black")
  
ggsave(Fig.4, file = "./output/Fig.4.pdf", device = "pdf", width = 10, height = 8, units = "in")

#Summary Table of slopes
lm_summaries_1 = tach_master_impute_long %>%
  select(-WingIntact) %>%
  split(.$organ) %>%
  map(~ lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = .x)) %>%
  map(summary) %>%
  map(~ tidy(.x))

lm_summaries_2 = tach_master_impute_long %>%
  select(-WingIntact) %>%
  split(.$organ) %>%
  map(~ lm(organ_weight ~ FlyWeight*Sex, data = .x)) %>%
  map(summary) %>%
  map(~ glance(.x))

test = lm_summaries_1[[1]]

slope_grabber = function(x) {
  female_slope = x[2,2]
  male_slope = female_slope + x[4,2]
  print(c(female_slope, male_slope))
}

slope_grabber(test)
lapply(lm_summaries_1, slope_grabber)


#Testing the linear models - doesn't make sense with the figure above. 
#
lm_test = tach_master_impute_long %>%
  filter(organ == "AbWeight") %>%
lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = .)

#Didn't he do relative body part size plotted against size?

Fig.5 = ggplot(tach_master_impute, aes(x = FlyWeight, y = HeadWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), aes(group = 1), color = "black") +
  xlab("Fly weight (mg)") +
  ylab("Head weight (mg) / Body weight (mg)")

ggsave(Fig.5, file = "./output/Fig.5.pdf", device = "pdf", width = 10, height = 8, units = "in")

#modeling

lm.0.head = lm(HeadWeight/FlyWeight ~poly(FlyWeight, 2), data = tach_master_impute %>%
                 filter(!is.na(FlyWeight)))
summary(lm.0.head)

Fig.6 = ggplot(tach_master_impute, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  xlab("Fly weight (mg)") +
  ylab("Thorax weight (mg) / Body weight (mg)")

ggsave(Fig.6, file = "./output/Fig.6.pdf", device = "pdf", width = 10, height = 8, units = "in")


#Modeling

lm.0.thor = lm(ThoraxWeight/FlyWeight ~poly(FlyWeight, 2) + Sex, data = tach_master_impute %>%
                             filter(!is.na(FlyWeight)))
summary(lm.0.thor)

tach_master_pred = tach_master_impute %>%
  select(Sex, FlyWeight, ThoraxWeight)
preds = predict(lm.0.thor, tach_master_pred, se.fit = TRUE, interval = "confidence", level = 0.95)
tach_master_pred$fit = preds[[1]][,1]
tach_master_pred$lwr = preds[[1]][,2]
tach_master_pred$upr = preds[[1]][,3]

 Fig.6 = ggplot(tach_master_impute, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  #geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  geom_line(data = tach_master_pred, aes(y = fit)) +
  geom_ribbon(data = tach_master_pred, aes(ymin = lwr, ymax = upr, group = Sex), alpha = 0.2, color = NA) +
  xlab("Fly weight (mg)") +
  ylab("Thorax weight (mg) / Body weight (mg)")


Fig.7 = ggplot(tach_master_impute, aes(x = FlyWeight, y = AbWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm") +
  xlab("Fly weight (mg)") +
  ylab("Abdomen weight (mg) / Body weight (mg)")

ggsave(Fig.7, file = "./output/Fig.7.pdf", device = "pdf", width = 10, height = 8, units = "in")

#Modeling
lm.0.ab = lm(AbWeight/FlyWeight ~ FlyWeight + Sex, data = tach_master_impute %>%
                           filter(!is.na(FlyWeight)))
summary(lm.0.ab)

Fig.8 = ggplot(subset(tach_master_impute, WingWeight < 0.5), aes(x = FlyWeight, y = WingWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", aes(group = 1), color = "black") +
  xlab("Fly weight (mg)") +
  ylab("Wings weight (mg) / Body weight (mg)")

ggsave(Fig.8, file = "./output/Fig.8.pdf", device = "pdf", width = 10, height = 8, units = "in")

#modeling
lm.0.wing = lm(WingWeight/FlyWeight ~ FlyWeight+Sex, data = tach_master_impute %>%
                 filter(!is.na(FlyWeight)))
summary(lm.0.wing)

lm.0.legs = lm(as.numeric(LegsWeight)/FlyWeight ~ FlyWeight+Sex, data = tach_master_impute %>%
                 filter(!is.na(FlyWeight) & !is.na(LegsWeight)))
summary(lm.0.legs)

Fig.9 = ggplot(tach_master_impute, aes(x = FlyWeight, y = as.numeric(LegsWeight)/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", aes(group = 1), color = "black") +
  xlab("Fly weight (mg)") +
  ylab("Legs weight (mg) / Body weight (mg)")

ggsave(Fig.9, file = "./output/Fig.9.pdf", device = "pdf", width = 10, height = 8, units = "in")


#Seems like there might a wing-weight that is fucked up - outlier removed - should probably remove all data where the wing category is False
ggplot((tach_master_impute %>% filter(WingWeight/FlyWeight < 0.1)), aes(x = FlyWeight, y = WingWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", aes(group = 1), color = "black")


ggplot(tach_master_impute, aes(x = FlyWeight, y = as.numeric(LegsWeight)/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", color = "black", aes(group = 1))


#Multipanel plot
gfinal = ggarrange(Fig.5, Fig.8, Fig.9, Fig.6, Fig.7, common.legend = TRUE, ncol = 2, nrow = 3)
ggsave(gfinal, file = "./output/Fig6(paneled).pdf", device = "pdf", width = 8.5, height = 8, units = "in")

#Using the calorimetry data to compute energy measurements

#install.packages("stringr")
library(stringr)
lm_head_summ
lm_thorax_summ
lm_ab_summ

tach_master_impute_long$organ = str_replace(tach_master_impute_long$organ, "Weight", "")

 tach_master_impute_long = tach_master_impute_long %>%
  mutate(calories = ifelse(organ == "Head", organ_weight*lm_head_summ[[2]],
                           ifelse(organ == "Ab", organ_weight*lm_ab_summ[[2]],
                                  ifelse(organ == "Thorax", organ_weight*lm_thorax_summ[[2]], NA))))
  

ggplot(tach_master_impute_long, aes(x = log(FlyWeight), y = log(calories), color = Sex, pch = organ)) +
  geom_point(size = 3, alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0) +
  geom_smooth(method = "lm", se = FALSE) +
  theme_classic()


tach_master_cal = tach_master %>%
  select(UniqueFlyID, Sex, FlyWeight, HeadWeight, ThoraxWeight, AbWeight) %>%
  mutate(head_cal = (HeadWeight*lm_head_summ[[2]]),
         thorax_cal = ThoraxWeight*lm_thorax_summ[[2]], 
         ab_cal = AbWeight*lm_ab_summ[[2]])

tach_master_impute_long

#A better figure
##The interesting body parts
Fig.10 = tach_master_impute_long %>%
  filter(organ == "Thorax" | organ == "Ab") %>%
  ggplot(aes(x = log(FlyWeight), y = log(calories), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", aes(lty = Sex)) +
  theme_classic() +
  geom_abline(slope = 1, intercept = 0, lty = 3) +
  coord_cartesian(xlim = c(0.5, 3), ylim = c(-2,4)) +
  geom_point(data = tach_master_impute_long %>%
            filter(organ == "Head"),
               size = 2, alpha = 0.5) +
  geom_smooth(data = tach_master_impute_long %>%
                filter(organ == "Head"), method = "lm", aes(group = 1)) +
  scale_color_discrete(guide = FALSE) +
  scale_shape_discrete(labels = c("Female", "Male")) +
  scale_linetype_discrete(guide = FALSE) +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 3.55, label = "Thorax",
                                      size = 9), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 2.7, label = "Abdomen",
                                      size = 9), color = "black") +
  geom_label(show.legend = FALSE, aes(x = 2.95, y = 1, label = "Head",
                                     size = 9), color = "black") +
  ylab("log calories") +
  xlab("log10 Body Weight (mg)")

ggsave(Fig.10, file = "./output/Fig.10.pdf", device = "pdf", width = 10, height = 8, units = "in")




Fig_7_cal = ggplot(tach_master_cal, aes(x = thorax_cal, y = ab_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2) +
  theme_classic() +
  xlab("Thorax Calories") +
  ylab("Abdomen Calories") +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))

#Stats
lm_cal_1 = lm(ab_cal ~ thorax_cal*Sex, data = tach_master_cal)

Fig_8_cal = ggplot(tach_master_cal, aes(x = head_cal, y = ab_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.6) +
  geom_smooth(method = "lm", alpha = 0.2) +
  theme_classic() +
  xlab("Head Calories") +
  ylab("Abdomen Calories") +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))


Fig_9_cal = ggplot(tach_master_cal, aes(x = head_cal, y = thorax_cal, color = Sex)) +
  geom_point(aes(size = as.numeric(FlyWeight)), alpha = 0.6) +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), alpha = 0.2) +
  theme_classic() +
  xlab("Head Calories") +
  ylab("Thorax Calories") +
  scale_size_continuous(name = "Fly Weight (mg)", range = c(1,5)) +
  scale_color_discrete(labels = c("Female", "Male")) +
  guides(color=guide_legend(override.aes=list(fill=NA)))



tach_master_cal %>%
  mutate(head_energy_dens = head_cal/HeadWeight,
         thorax_energy_dens = thorax_cal/ThoraxWeight,
         ab_energy_dens = ab_cal/AbWeight) %>%
  select(Sex, FlyWeight, head_energy_dens, thorax_energy_dens, ab_energy_dens)






#Testing RMA shit

install.packages("smatr")
library(smatr)

tach_master_impute

sma.1 = sma(log(HeadWeight) ~ log(FlyWeight), data = tach_master_impute)
ols.1 = lm(log(HeadWeight) ~ log(FlyWeight), data = tach_master_impute)


tach_master_impute %>%
  ggplot(aes(x = log(FlyWeight), y = log(HeadWeight))) +
  geom_point() +
  geom_abline(slope = 0.6887, intercept = -1.5383) +
  geom_abline(slope = 0.7644, intercept = -1.6936, lty = 2)

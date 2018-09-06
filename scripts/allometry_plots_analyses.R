#Allometry plots and analyses
#Keaton Wilson
#keatonwilson@me.com
#2018-09-06

#Packages
library(tidyverse)
library(car)
library(ggpubr)

#You will need to make sure tach_master_impute is present in the environment from the cohort_size_host_quality script
glimpse(tach_master_impute)

#Turning the data into a long dataframe
tach_master_impute_long = tach_master_impute %>%
  gather(key = "organ", value = "organ_weight", HeadWeight:LegsWeight) %>%
  select(-CaterpillarID, -FlyID, -WanderWeight, -HeadCapsuleWidth, -WanderDate, -Comments, -sib_number, -nut_index)
tach_master_impute_long$organ = factor(tach_master_impute_long$organ)
tach_master_impute_long$organ_weight = as.numeric(tach_master_impute_long$organ_weight)

#The interesting body parts (ab and thorax)
Fig.3 = tach_master_impute_long %>%
  filter(organ == "ThoraxWeight" | organ == "AbWeight") %>%
  mutate(imp = ifelse(ThoraxWeight_imp == TRUE, TRUE, 
                      ifelse(AbWeight_imp == TRUE, TRUE, 
                             ifelse(FlyWeight_imp == TRUE, TRUE, FALSE)))) %>%
  ggplot(aes(x = log(FlyWeight), y = log(organ_weight), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_point(data = tach_master_impute_long %>%
               filter(organ == "ThoraxWeight" | organ == "AbWeight") %>%
               mutate(imp = ifelse(ThoraxWeight_imp == TRUE, TRUE, 
                                   ifelse(AbWeight_imp == TRUE, TRUE, 
                                          ifelse(FlyWeight_imp == TRUE, TRUE, FALSE)))) %>%
               filter(imp == TRUE), size = 3) +
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

#ggsave(Fig.3, file = "./output/Fig.3.pdf", device = "pdf", width = 10, height = 8, units = "in")


#The rest
Fig.4 = tach_master_impute_long %>%
  filter(organ != "ThoraxWeight" & organ != "AbWeight") %>%
  ggplot(aes(x = log(FlyWeight), y = log(organ_weight), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_point(data = tach_master_impute_long %>%
               filter(organ != "ThoraxWeight" & organ != "AbWeight") %>%
               mutate(imp = ifelse(HeadWeight_imp == TRUE, TRUE, 
                                   ifelse(LegsWeight_imp == TRUE, TRUE, 
                                          ifelse(WingWeight_imp == TRUE, TRUE,
                                                ifelse(FlyWeight_imp == TRUE, TRUE, FALSE))))) %>%
               filter(imp == TRUE), size = 3) +
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

#ggsave(Fig.4, file = "./output/Fig.4.pdf", device = "pdf", width = 10, height = 8, units = "in")

#STATS for the two figures above
#Summary Table of slopes
lm_summaries_1 = tach_master_impute_long %>%
  select(-WingIntact) %>%
  split(.$organ) %>%
  map(~ lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = .x)) %>%
  map(summary) %>%
  map(~ tidy(.x))


#PLOTS#
#Making a series of smaller figures that compare relative body weight investment as a function of body size. 

#Normalized Head weight as a function of body size
Fig.5 = ggplot(tach_master_impute, aes(x = FlyWeight, y = HeadWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), aes(group = 1), color = "black") +
  xlab("Fly weight (mg)") +
  ylab("Head weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))

#modeling
lm.0.head = lm(HeadWeight/FlyWeight ~poly(FlyWeight, 2), data = tach_master_impute %>%
                 filter(!is.na(FlyWeight)))
summary(lm.0.head)

#Normalized thorax weight as a function of body-size
Fig.6 = ggplot(tach_master_impute, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  xlab("Fly weight (mg)") +
  ylab("Thorax weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Modeling

lm.0.thor = lm(ThoraxWeight/FlyWeight ~poly(FlyWeight, 2) + Sex, data = tach_master_impute %>%
                 filter(!is.na(FlyWeight)))
summary(lm.0.thor)

#Have to make prediction bands and lines manually because the additive model is best
tach_master_pred = tach_master_impute %>%
  select(Sex, FlyWeight, ThoraxWeight)
preds = predict(lm.0.thor, tach_master_pred, se.fit = TRUE, interval = "confidence", level = 0.95)
tach_master_pred$fit = preds[[1]][,1]
tach_master_pred$lwr = preds[[1]][,2]
tach_master_pred$upr = preds[[1]][,3]

#Overwriting the first figure with the second
Fig.6 = ggplot(tach_master_impute, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  #geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  geom_line(data = tach_master_pred, aes(y = fit)) +
  geom_ribbon(data = tach_master_pred, aes(ymin = lwr, ymax = upr, group = Sex), alpha = 0.2, color = NA) +
  xlab("Fly weight (mg)") +
  ylab("Thorax weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Normalized Abdomen weight as a function of body size
Fig.7 = ggplot(tach_master_impute, aes(x = FlyWeight, y = AbWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm") +
  xlab("Fly weight (mg)") +
  ylab("Abdomen weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Modeling
lm.0.ab = lm(AbWeight/FlyWeight ~ FlyWeight + Sex, data = tach_master_impute %>%
               filter(!is.na(FlyWeight)))
summary(lm.0.ab)

#Normalized wing weight as a function of body size
Fig.8 = ggplot(subset(tach_master_impute, WingWeight < 0.5), aes(x = FlyWeight, y = WingWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", aes(group = 1), color = "black") +
  xlab("Fly weight (mg)") +
  ylab("Wings weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))



#modeling
lm.0.wing = lm(WingWeight/FlyWeight ~ FlyWeight+Sex, data = tach_master_impute %>%
                 filter(!is.na(FlyWeight)))
summary(lm.0.wing)

lm.0.legs = lm(as.numeric(LegsWeight)/FlyWeight ~ FlyWeight+Sex, data = tach_master_impute %>%
                 filter(!is.na(FlyWeight) & !is.na(LegsWeight)))
summary(lm.0.legs)

#Normalized leg weight as a function of body size
Fig.9 = ggplot(tach_master_impute, aes(x = FlyWeight, y = as.numeric(LegsWeight)/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", aes(group = 1), color = "black") +
  xlab("Fly weight (mg)") +
  ylab("Legs weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Multipanel plot
gfinal = ggarrange(Fig.5, Fig.8, Fig.9, Fig.6, Fig.7, common.legend = TRUE, ncol = 2, nrow = 3)
ggsave(gfinal, file = "./output/Fig6(paneled).pdf", device = "pdf", width = 8.5, height = 8, units = "in")

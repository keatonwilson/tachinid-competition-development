#Allometry plots and analyses
#Keaton Wilson
#keatonwilson@me.com
#2018-09-06

#Packages
library(tidyverse)
library(car)
library(ggpubr)

#You will need to make sure tach_master_impute and tach_master are present in the environment from the cohort_size_host_quality script
glimpse(tach_master_impute)

#Turning the data into a long dataframe
tach_master_impute_long = tach_master_impute %>%
  gather(key = "organ", value = "organ_weight", HeadWeight:LegsWeight) %>%
  dplyr::select(-CaterpillarID, -FlyID, -WanderWeight, -HeadCapsuleWidth, -WanderDate, -Comments, -sib_number, -nut_index)
tach_master_impute_long$organ = factor(tach_master_impute_long$organ)
tach_master_impute_long$organ_weight = as.numeric(tach_master_impute_long$organ_weight)

tach_master_long = tach_master %>%
  gather(key = "organ", value = "organ_weight", HeadWeight:LegsWeight) %>%
  dplyr::select(-CaterpillarID, -FlyID, -WanderWeight, -HeadCapsuleWidth, -WanderDate, -Comments, -sib_number, -nut_index)
tach_master_long$organ = factor(tach_master_long$organ)
tach_master_long$organ_weight = as.numeric(tach_master_long$organ_weight)

#ALLOMETRY FIGURES
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

ggsave(Fig.3, file = "./output/Fig.3.pdf", device = "pdf", width = 10, height = 8, units = "in")


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

Fig_4_paneled = ggarrange(Fig.3, Fig.4, common.legend = TRUE, labels = "auto", label.x = 0.85, nrow = 2)
ggsave(filename = "./output/Fig_4_paneled.pdf", plot = Fig_4_paneled, device = "pdf", width = 8, height = 11, units = "in")

#STATS for the two figures above
#Summary Table of slopes
lm_summaries_1 = tach_master_impute_long %>%
  select(-WingIntact) %>%
  split(.$organ) %>%
  map(~ lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = .x)) %>%
  map(summary) %>%
  map(~ tidy(.x))


#Because it's additive and not interactive, we need to generate predictions ahead of time and bind them to the data frame

Fig_3_df_thorax = tach_master_long %>%
  filter(organ == "ThoraxWeight")

model_thorax = lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = Fig_3_df_thorax)
preds_thorax = as.tibble(predict(model_thorax, newdata = Fig_3_df_thorax, interval = "confidence"))

Fig_3_df_thorax = bind_cols(preds_thorax, Fig_3_df_thorax)

Fig_3_df_ab = tach_master_long %>%
  filter(organ == "AbWeight")

model_ab = lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = Fig_3_df_ab)
preds_ab = as.tibble(predict(model_ab, newdata = Fig_3_df_ab, interval = "confidence"))

Fig_3_df_ab = bind_cols(preds_ab, Fig_3_df_ab)


#The interesting body parts (ab and thorax)
Fig.3 = ggplot(data = Fig_3_df_thorax, aes(x = log(FlyWeight), y = log(organ_weight), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.7, color = "#F8766D") +
  geom_line(aes(x = log(FlyWeight), y = fit, lty = Sex)) +
  geom_ribbon(aes(ymax = upr, ymin = lwr), alpha = 0.3, color = NA, fill = "#F8766D") +
  geom_point(data = Fig_3_df_ab, size = 2, alpha = 0.7, color = "#00BFC4") +
  geom_line(data = Fig_3_df_ab, aes(y = fit, lty = Sex)) +
  geom_ribbon(data = Fig_3_df_ab, aes(ymax = upr, ymin = lwr), alpha = 0.3, color = NA, fill = "#00BFC4") +
  theme_classic(base_size = 20) +
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
  xlab("")

ggsave(Fig.3, file = "./output/Fig.3.pdf", device = "pdf", width = 10, height = 8, units = "in")


#The rest
Fig.4 = tach_master_long %>%
  filter(organ != "ThoraxWeight" & organ != "AbWeight") %>%
  ggplot(aes(x = log(FlyWeight), y = log(organ_weight), shape = Sex, color = organ)) +
  geom_point(size = 2, alpha = 0.5) +
  geom_smooth(method = "lm", aes(group = organ)) +
  theme_classic(base_size = 20) +
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

Fig_4_paneled = ggarrange(Fig.3, Fig.4, common.legend = TRUE, labels = "auto", nrow = 2)
ggsave(filename = "/Users/KeatonWilson/Documents/Writing/Tachinid Development/Figures/Nature Figures/no_impute/Supp_fig_1.pdf", plot = Fig_4_paneled, device = "pdf", width = 8, height = 11, units = "in")

#STATS for the two figures above
#Summary Table of slopes
lm_summaries_1 = tach_master_long %>%
  select(-WingIntact) %>%
  split(.$organ) %>%
  map(~ lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = .x)) %>%
  map(summary) %>%
  map(~ tidy(.x))

lm.thorax = tach_master_long %>%
  filter(organ == "ThoraxWeight") %>%
  lm(log(organ_weight) ~ log(FlyWeight) + Sex, data = .)

preds = predict(lm.thorax, interval = "predict")


#PLOTS#
#Making a series of smaller figures that compare relative body weight investment as a function of body size. 
#Removed imputed values and made plots without all the imputations

#Normalized Head weight as a function of body size
Fig.5 = ggplot(tach_master, aes(x = FlyWeight, y = HeadWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2), aes(group = 1), color = "black") +
  xlab("Body weight (mg)") +
  ylab("Head weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))

#modeling
lm.sex.head = lm(HeadWeight/FlyWeight ~ poly(FlyWeight, 2)+Sex, data = tach_master %>%
                   filter(!is.na(FlyWeight)))
lm.0.head = lm(HeadWeight/FlyWeight ~FlyWeight, data = tach_master %>%
                 filter(!is.na(FlyWeight)))
lm.final.head = lm(HeadWeight/FlyWeight ~poly(FlyWeight, 2), data = tach_master %>%
                     filter(!is.na(FlyWeight)))

summary(lm.0.head)
AIC(lm.0.head)
summary(lm.sex.head)
AIC(lm.sex.head)
summary(lm.final.head)
AIC(lm.final.head)

#Normalized thorax weight as a function of body-size
Fig.6 = ggplot(tach_master, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  xlab("Body weight (mg)") +
  ylab("Thorax weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Modeling
lm.0.thor = lm(ThoraxWeight/FlyWeight ~FlyWeight, data = tach_master %>%
                 filter(!is.na(FlyWeight)))
lm.1.thor = lm(ThoraxWeight/FlyWeight ~poly(FlyWeight, 2), data = tach_master %>%
                 filter(!is.na(FlyWeight)))

lm.final.thor = lm(ThoraxWeight/FlyWeight ~poly(FlyWeight, 2) + Sex, data = tach_master %>%
                 filter(!is.na(FlyWeight)))
summary(lm.0.thor)
AIC(lm.0.thor)
summary(lm.final.thor)

#Have to make prediction bands and lines manually because the additive model is best
tach_master_pred = tach_master %>%
  select(Sex, FlyWeight, ThoraxWeight)
preds = predict(lm.0.thor, tach_master_pred, se.fit = TRUE, interval = "confidence", level = 0.95)
tach_master_pred$fit = preds[[1]][,1]
tach_master_pred$lwr = preds[[1]][,2]
tach_master_pred$upr = preds[[1]][,3]

#Overwriting the first figure with the second
Fig.6 = ggplot(tach_master, aes(x = FlyWeight, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  #geom_smooth(method = "glm", formula = y ~ poly(x, 2)) +
  geom_line(data = tach_master_pred, aes(y = fit)) +
  geom_ribbon(data = tach_master_pred, aes(ymin = lwr, ymax = upr, group = Sex), alpha = 0.2, color = NA) +
  xlab("Body weight (mg)") +
  ylab("Thorax weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Normalized Abdomen weight as a function of body size
Fig.7 = ggplot(tach_master, aes(x = FlyWeight, y = AbWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm") +
  xlab("Body weight (mg)") +
  ylab("Abdomen weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Modeling
lm.0.ab = lm(AbWeight/FlyWeight ~ FlyWeight, data = tach_master %>%
               filter(!is.na(FlyWeight)))
lm.1.ab = lm(AbWeight/FlyWeight ~ FlyWeight + Sex, data = tach_master %>%
               filter(!is.na(FlyWeight)))
lm.2.ab = lm(AbWeight/FlyWeight ~ FlyWeight*Sex, data = tach_master %>%
               filter(!is.na(FlyWeight)))
summary(lm.2.ab)

#Normalized wing weight as a function of body size
Fig.8 = ggplot(subset(tach_master, WingWeight < 0.5), aes(x = FlyWeight, y = WingWeight/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm", aes(group = 1), color = "black") +
  xlab("Body weight (mg)") +
  ylab("Wings weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))



#modeling
lm.0.wing = lm(WingWeight/FlyWeight ~ FlyWeight+Sex, data = tach_master %>%
                 filter(!is.na(FlyWeight)))
lm.1.wing = lm(WingWeight/FlyWeight ~ FlyWeight, data = tach_master %>%
                 filter(!is.na(FlyWeight)))
summary(lm.0.wing)

lm.0.legs = lm(as.numeric(LegsWeight)/FlyWeight ~ FlyWeight, data = tach_master %>%
                 filter(!is.na(FlyWeight) & !is.na(LegsWeight)))
summary(lm.0.legs)
lm.1.legs = lm(as.numeric(LegsWeight)/FlyWeight ~ FlyWeight+Sex, data = tach_master %>%
                 filter(!is.na(FlyWeight) & !is.na(LegsWeight)))

lm.2.legs = lm(as.numeric(LegsWeight)/FlyWeight ~ FlyWeight*Sex, data = tach_master %>%
                 filter(!is.na(FlyWeight) & !is.na(LegsWeight)))

summary(lm.2.legs)

#Normalized leg weight as a function of body size
Fig.9 = ggplot(tach_master, aes(x = FlyWeight, y = as.numeric(LegsWeight)/FlyWeight, color = Sex)) +
  geom_point(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "glm") +
  xlab("Body weight (mg)") +
  ylab("Legs weight (mg) / Body weight (mg)") +
  theme(text = element_text(size = 10))


#Multipanel plot
gfinal = ggarrange(Fig.5, Fig.8, Fig.9, Fig.6, Fig.7, common.legend = TRUE, ncol = 2, nrow = 3)
ggsave(gfinal, file = "/Users/KeatonWilson/Documents/Writing/Tachinid Development/Figures/Nature Figures/no_impute/Figure3panel.pdf", device = "pdf", width = 8.5, height = 8, units = "in")


#Testing out some new stuff

fig_4_a = ggplot(tach_master_impute, aes(x = sib_number, y = HeadWeight/FlyWeight, color = Sex)) +
  geom_jitter(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm", aes(group = 1), color = "black") +
  xlab("Cohort Size") +
  ylab("Head Weight/Body Weight") 

lm_rel_head = lm(HeadWeight/FlyWeight ~ sib_number, data = tach_master_impute)
summary(lm_rel_head)

fig_4_b = ggplot(tach_master_impute, aes(x = sib_number, y = ThoraxWeight/FlyWeight, color = Sex)) +
  geom_jitter(size = 3, alpha = 0.6) +
  theme_classic() +
  geom_smooth(method = "lm") +
  xlab("Cohort Size") +
  ylab("Thorax Weight/Body Weight")

lm_rel_thorax = lm(ThoraxWeight/FlyWeight ~ sib_number*Sex, data = tach_master_impute)
summary(lm_rel_thorax)

fig_4_c = ggplot(tach_master_impute, aes(x = sib_number, y = AbWeight/FlyWeight, color = Sex)) +
  geom_jitter(alpha = 0.6, size = 3) +
  theme_classic() +
  geom_smooth(method = "lm") +
  xlab("Cohort Size") +
  ylab("Abdomen Weight/ Body Weight")

lm_rel_abdomen = lm(AbWeight/FlyWeight ~ sib_number*Sex, data = tach_master_impute)
summary(lm_rel_abdomen)

Fig_4_paneled = ggarrange(fig_4_a, fig_4_b, fig_4_c, common.legend = TRUE, labels = c("a", "b", "c"),
                          label.x = 0.85)

#ggsave(plot = Fig_4_paneled, filename = "./output/Fig4(paneled).pdf", device = "pdf")

ggplot(tach_master_impute, aes(x = sib_number, y = as.numeric(LegsWeight)/FlyWeight, color = Sex)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm")

lm_rel_legs = tach_master_impute %>%
  mutate(LegsWeight = as.numeric(LegsWeight))

lm_test = lm(LegsWeight/FlyWeight ~ sib_number, data = lm_rel_legs)

ggplot(tach_master_impute, aes(x = sib_number, y = as.numeric(WingWeight)/FlyWeight, color = Sex)) +
  geom_point() +
  theme_classic() +
  geom_smooth(method = "lm")


ggplot(tach_master_impute, aes(x = FlyWeight, fill = Sex)) +
  geom_histogram(position = "dodge") +
  theme_classic()

tach_master_impute %>%
  group_by(Sex) %>%
  summarize(n())

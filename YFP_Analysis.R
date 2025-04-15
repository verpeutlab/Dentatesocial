library(caret)
library(ggpubr)
library(broom)
library(lme4)
YFP_Neuron <- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data_Final.xlsx", 
                          sheet = "YFP_ACC_Complexity")

YFP_Neurons<-YFP_Neuron[!YFP_Neuron$Sex=='Female', ]

YFP_Clean <- YFP_Neuron %>%
  drop_na(Intersections)

YFP_ave_Clean <- YFP_Clean[YFP_Clean$Distance>70 & YFP_Clean$Distance<200, ]

YFP_Final <- YFP_ave_Clean %>%
  select(Mouse, Sex, Group, Group2, Distance, Intersections)

Proximal_Intersections_sum <- YFP_Intersections_Sum  %>% filter(Distance > 70)

##########Repeated Measures ANOVA########
modelV1 <- aov(Intersections ~ Group + Sex + Distance + Error(Mouse/Distance), data = YFP_Clean) %>%
  tukey_hsd()
summary(modelV1)

##############Fit the linear mixed-effects model (Subject as random effect, Distance and Sex as fixed effects)
library(nlme)
library(emmeans)

lme_model <- lme(Intersections ~ Group2 + Sex + Distance, 
                 random = ~ 1 | Mouse,  
                 data = YFP_Clean) # Random effect for each subject
summary(lme_model)


####Averaging Intersections by Mouse and Distance####
YFP_ave <- YFP_Clean %>%
  group_by(Mouse, ) %>%
  mutate(ave1 = mean(Distance_Peak_Intersections)) %>%
  mutate(ave2 = mean(Total_Intersections)) %>%
  mutate(ave3 = mean(Peak_Intersections)) %>%
  mutate(ave4 = mean(Half_Peak_Distance))



YFP_Final <- YFP_ave_Clean %>%
  select(Mouse, Sex, Group, Group2, Distance, Intersections) #%>% distinct(Mouse, Sex, Group, Group2, ave1, ave2, ave3, ave4)

YFP_ave_ <- Proximal_Intersections_sum %>% distinct(Mouse, Sex, Group, Group2, Distance, Intersections, sum, .keep_all = TRUE)

####Selecting Sample from average####
YFP_ave_Clean <- YFP_ave[YFP_ave$Sample>0 & YFP_ave$Sample<2, ]

YFP_coefficients <- YFP_ave_Clean %>%
  select(Mouse, Sex, Group, Group2, Term, Coefficients) %>% 
  drop_na()

test1<- aov(ave1 ~ Group2 + Sex, data = YFP_ave_Clean) %>%
  tukey_hsd()
summary(test1)

###Graphing_Intersections_by_Distance########
ggplot(YFP_Neuron, aes(x = Distance, y = Intersections, group = Sex, color = Sex)) +
  #geom_point(alpha=0.5)+
  #stat_summary(geom = "line", fun = "mean") +
  stat_summary(fun.data = mean_se) + 
  geom_smooth(method = lm, se = TRUE, fill=NA, formula = y ~ poly(x,3, raw=TRUE)) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  labs( x = "Distance from Soma (µm)", y = "Average Intersections", title = "Sex ACC Pyramidal Neuron Dedritic Complexity") +
  #scale_color_discrete(limits = c("Untreated", "CNO", "Gq", "Gi")) +
  #scale_color_manual(values=c("Control"="black", "Gq" = "forest green", "Gi" = "Magenta")) +
  scale_color_manual(values=c("Male"="black", "Female"="red")) +
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0, 15) +
  #scale_y_continuous(breaks = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)) +
  xlim(0,200) +
  facet_grid(.~Sex) 



###Reading_In_YFP_Spine_Count_Data########

YFP_Spines <- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data.xlsx", 
                          sheet = "YFP_ACC_Basal_Spines")

YFP_Spine <-YFP_Spines[!YFP_Spines$Sex=='Male', ]

YFP_Spine <-YFP_Spines_SocialP[!YFP_Spines_SocialP$Group=='Gq', ]
YFP_Spines <-YFP_Spine[!YFP_Spine$Group=='Gi', ]

YFP_Spines_Clean <- drop_na(YFP_Spines)

YFP_Spines_SocialP <- merge(cortical_Fos2, Social5) %>%
  distinct()

modelV1 <- lm(formula = Intersections ~ 0 + Group*Sex + Distance, data = YFP_Neuron)
summary(modelV1)

cor.test(YFP_Spines$ave, YFP_Spines$Social_Preference_Index...11, method = "pearson")

####Averaging YFP Spines by Mouse####
YFP_Spines_ave <- YFP_Neuron %>%
  group_by(Mouse, ) %>%
  mutate(ave = mean(Peak_Intersections))

YFP_Spines_Final <- YFP_Spines_Clean  %>%
  select(Mouse, Sex, Group, Group2, ave) %>% distinct(Mouse, Sex, Group, Group2, ave)

YFP_Spines_Clean <- YFP_Spines_ave[YFP_Spines_ave$Sample>0 & YFP_Spines_ave$Sample<2, ]

ggplot(YFP_ave_Clean, aes(x = Sex, y = ave1, color = Sex)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  geom_boxplot() + 
  #stat_compare_means(method = "t.test", ref.group = "Male", label = "p.signif", label.y = 50) +
  #stat_pvalue_manual(test1, label = "p.adj.signif", y.position = 50) +
  #geom_beeswarm() +
  geom_point() +
  scale_color_manual(values = c("Male" = "black", "Female" = "red")) +
  #scale_color_manual(values=c("Control" = "black", "Gq" = "dark green", "Gi" = "magenta")) +
  labs(y = "Normalized Spine Count", title = "Ave Count") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0, 1) +
  facet_grid(.~Group2)

test1<- aov(ave ~ Group2 + Sex, data = YFP_Spines_Final) %>%
  tukey_hsd()
summary(test1)

########Area Under the Curve#########
grouped_data <- split(YFP_Neuron, YFP_Neuron$Mouse) #Split by Group

calculate_auc <- function(Distance, Intersections) {
  return(sum(diff(Distance) * (head(Intersections, -1) + tail(Intersections, -1)) / 2)) #Function to calculate AUC using the trapezoidal rule
}
auc_results <- sapply(grouped_data, function(grouped_data) {
  calculate_auc(grouped_data$Distance, grouped_data$Intersections)}) # Calculate AUC for each group

print(auc_results)

auc_data <- data.frame(
  Group = names(auc_results), #Create data phrame with AUC values per group
  AUC = auc_results)

pairwise_results <- pairwise.t.test(auc_data$AUC, auc_data$Group, p.adjust.method = "bonferroni")
print(pairwise_results)

######Graphing AUC per Group#####

YFP_AUC <- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data.xlsx", #Reading in Data
                          sheet = "YFP_AUC_Data")

YFP_AUCSex <-YFP_AUC[!YFP_AUC$Sex=='Male', ]

ggplot(YFP_ave_Clean, aes(x = Sex, y = ave1, fill = Sex)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(width = .8), width = .5) + 
  stat_summary(fun.data = mean_se, color = "dark grey", position = position_dodge(width = .8), width = .8) +
  #stat_compare_means(label = "p.format", method = "t.test", ref.group = "Male") +
  geom_point(color = "blue") +
  #stat_pvalue_manual(test1, label = "p.adj.signif", y.position = 2000) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values=c("Male" = "black", "Female" = "red")) +
  #scale_fill_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "magenta")) +
  labs(y = "Distance Peak Intersections", title = "ACC Group2 Distance to Peak Intersections") +
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0,200)

test1<- aov(ave1 ~ Group2 + Sex, data = YFP_ave_Clean) %>%
  tukey_hsd()
summary(test1)


##########Sphrecity Test#######
install.packages("ez")
library(ez)

sphericity_test <- ezMixed(
  data = YFP_Final,
  dv = .(Intersections),  # Dependent variable
  random = .(1 |Mouse),  # Random intercept for each subject (Mouse)
  fixed = .(Distance * Group)  # Fixed effects: Distance, Group, and their interaction
)

Social_Analysis<- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data_Final.xlsx", 
                           sheet = "Habituation")

Social <- Social_S.2[!Social_S.2$Group =='Vector', ]
Social2.1 <- Social2[!Social2$Group2 =='Control', ]
SocialFemale <- SocialGQ[!SocialGQ$Sex=='Male', ]
Social5 <- SocialObject %>%
  drop_na(Time)

SocialGQ <- Social2.1[!Social2.1$Group=='Gq', ]
SocialGi <- SocialGQ[!SocialGQ$Group=='Gq', ]
Social3 <- Social5[!Social5$Group=='Vector', ]
Social_S.2 <- Social_S[!Social_S$Group=='ChABC', ]
Social_S <- Social_S.1[!Social_S.1$Chamber=='Middle', ]
Social_Middle <- SocialObject[!SocialObject$Chamber=='Social', ]

Social_S.1 <- Social_Analysis %>%
  select(Mouse, Sex, Group, Group2, Chamber, Time_S, Distance_S, Cup, Time_Cup, Distance_Cup, Social_Preference_Index...11)
Social5 <- Social3 %>%
  drop_na(Social_Preference_Index...11)

Social_Data <- merge(Habituation, Social)
ACC_Social <- merge(Social_Data, cFos_Count) %>%
  distinct()
OFC_Social <- merge(ACC_Social, ORB)
Somato_Social <- merge(OFC_Social, SomatoMotor)
VTAcFos_Social <- merge(Somato_Social, VTAcFos)



Chambers <- factor(Social3$Cup, levels = c("Social", "Object"))
Groups <- factor(Social3$Group2, levels = c("Control", "Gq", "Gi"))

#########Distance Traveled in Social Chamber

ggbarplot(Social5, x = Chambers, y = "Distance", add = "mean_se", color = Group2) +
  theme_bw(base_size = 12, base_family = "TT Arial") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_pvalue_manual(stat.test, label = "p.adj.signif", y.position = 30) +
  #geom_point(position = "identity") +
  theme(axis.title.x = element_blank()) +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "magenta")) +
  labs(y = "Time (s)", title = "Time Spent in Social Chamber - Gi") + 
  theme(plot.title = element_text(hjust = .5)) +
  facet_grid(.~Sex)

stat.test <- aov(Time ~ Chamber, data = SocialFemale) %>%
  tukey_hsd()
summary(stat.test)
#######Time Spent in Social Chamber
 
ggbarplot(Social5, x = "Cup", y = "Time_Cup", add = "mean_se", group = "Group2") + 
  theme_bw(base_size = 12, base_family = "serif") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_pvalue_manual(stat.test.1, label = "p.adj.signif", y.position = 250)+
  geom_point() +
  labs(y = "Average Distance (s)", title = "Time spent in Social Chamber") +
  theme(axis.title.x = element_blank()) +
  theme(plot.title = element_text(hjust = .5)) +
  facet_grid(.~Mouse)

stat.test.1 <- aov(Time_Cup ~ Cup, data = Social3) %>%
  tukey_hsd()
  
#####Distance Traveled in Object Chamber############

ggbarplot(Social5, x = "Cup", y = "Time_Cup", add = "mean_se", group = Group2) +
  theme_bw(base_size = 12, base_family = "serif") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_compare_means(label = "p.signif", method = "t.test", ref.group = "Social", y.position = 30) +
  theme(axis.title.x = element_blank()) +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green")) +
  labs(y = "Average Distance (m)", title = "Distance traveled in Object Chamber") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0, 250) +
  facet_grid(.~Group2+Sex)


####Distance Traveld in Middel Chamber###

ggbarplot(Social2, x = "Group2", y = "C2_D_m", add = "mean_se") +
  theme_bw(base_size = 12, base_family = "serif") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_pvalue_manual(stat.test.3, label = "p.adj.signif", y.position = 50)+
  geom_point(position = "identity")+
  theme(axis.title.x = element_blank()) +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green")) +
  labs(y = "Average Distance (m)", title = "Distance traveled in Object Chamber") + 
  theme(plot.title = element_text(hjust = .5))

stat.test.3 <- aov(C2_D_m ~ Group2, data = Social2) %>%
  tukey_hsd()

######Time Spent in Middle Chamber####

ggbarplot(Social2, x = "Group2", y = "C2_Time_s", add = "mean_se") +
  theme_bw(base_size = 12, base_family = "serif") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_pvalue_manual(stat.test.4, label = "p.adj.signif", y.position = 150)+
  geom_point(position = "identity")+
  theme(axis.title.x = element_blank()) +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green")) +
  labs(y = "Average Distance (m)", title = "Distance traveled in Object Chamber") + 
  theme(plot.title = element_text(hjust = .5))


stat.test.4 <- aov(C2_Time_s ~ Group2, data = Social2) %>%
  tukey_hsd()

######Time Spent around social cup####

ggbarplot(Social2, x = "Group2", y = "Cp3_Time_s", add = "mean_se") +
  theme_bw(base_size = 12, base_family = "serif") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_pvalue_manual(stat.test.5, label = "p.adj.signif", y.position = 350)+
  geom_point(position = "identity")+
  theme(axis.title.x = element_blank()) +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green")) +
  labs(y = "Average Distance (m)", title = "Distance traveled in Object Chamber") + 
  theme(plot.title = element_text(hjust = .5))

stat.test.5 <- aov(Cp3_Time_s ~ Group2, data = Social2) %>%
  tukey_hsd()

######Distance Traveled around social cup####

ggbarplot(SocialGQ, x = "Group2", y = "Distance_Cup", add = "mean_se") +
  theme_bw(base_size = 12, base_family = "serif") + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  stat_pvalue_manual(stat.test.6, label = "p.adj.signif", y.position = 75)+
  geom_point(position = "identity")+
  theme(axis.title.x = element_blank()) +
  scale_color_manual(values=c("Control" = "black", "Gi" = "forest green")) +
  labs(y = "Average Distance (m)", title = "Distance traveled around Social Cup") + 
  theme(plot.title = element_text(hjust = .5)) +
  facet_wrap(~Sex)

stat.test.6 <- aov(Distance_Cup ~ Group2, data = SocialGQ) %>%
  tukey_hsd()

######Social Preference Comaprison between Controls (Collapsed; CNO, Untreated) vs Gq and Gi####

ggplot(SocialGi, aes(x = Group2, y = Social_Preference_Index...11, group = Group2)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  geom_beeswarm() +
  stat_summary(fun.data = "mean_sdl", geom = "errorbar", color = "red", width = .2) +
  stat_summary(fun.y=mean, geom = "point", color = "red") +
  stat_pvalue_manual(stat.test.7, label = "p.adj.signif", y.position = 1) +
  #stat_pvalue_manual(stat.test.8, label = "p.adj.signif", y.position = .8)+
  #stat_pvalue_manual(stat.test.9, label = "p.adj.signif", y.position = .9) +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "magenta")) +
  labs(y = "Social Preference Index", title = "Social Preference Disruption - Females") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0,1) + 
  facet_grid(.~Sex)

stat.test.7 <- aov(Social_Preference_Index...11 ~ Group2, data = SocialGi) %>%
  tukey_hsd()
stat.test.8 <- aov(Time_Cup ~ Cup, data = SocialMale) %>%
  tukey_hsd()
stat.test.9 <- aov(Time_Cup ~ Cup, data = Social4) %>%
  tukey_hsd()


######Bargraph of Time spent in each chamber by Group####

ggplot(SocialMale, aes(x = Groups, y = Social_Preference_Index...12)) +
  stat_summary(geom = "bar", fun = "mean", position = position_dodge(width = .8), width = .5) + 
  stat_summary(fun.data = mean_se, position = position_dodge(width = .8), width = .8) +
  stat_compare_means(label = "p.format", method = "t.test", ref.group = "Social") +
  #stat_pvalue_manual(stat.test.9, label = "p.adj.signif", y.position = 200) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  #scale_fill_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "red")) +
  labs(y = "Distance Traveled (m)", title = "Distance spent near cup - Females") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0,30) +
  facet_grid(.~Group2)

ggplot(Social, aes(x = Cup, y = Time_Cup, group = Group2, color = Group2)) +
  stat_summary(fun.data = mean_se, geom = "bar", position = position_dodge(width = .8), width = .5) +
  theme_bw(base_size = 12, base_family = "serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  #stat_pvalue_manual(stat.test.11, label = "p.adj.signif", y.position = 220) +
  scale_fill_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "red")) +
  labs(y = "Distance (m)", title = "Sex Differences in Time spent Near Social Partner -  (GQ)") + 
  theme(plot.title = element_text(hjust = .5)) +
  facet_grid(.~Sex)


SocialGQ1.1 <- SocialMale[!SocialMale$Cup=='Social', ]
 
stat.test.11 <- aov(Time_Cup ~ Cup, data = SocialMale) %>%
  tukey_hsd()

######Bargraph of Time spent in each chamber by Group Habituation####

Habituation_Analysis<- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data.xlsx", 
                              sheet = "Sleap_S")

Social_Clean <- Habituation_Analysis %>%
  drop_na(Social_Preference_Index...11)


Habituation <- Habituation_Analysis[c(1:06),c(1:8)]
Habituation2 <- drop_na(SocialGi)
SocialGQ <- Habituation_Analysis[!Habituation_Analysis$Sex=='Female', ]
SocialGi <- SocialGQ[!SocialGQ$Group=='Vector', ]
Social3 <- Social2[!Social2$Group2=='Control', ]

Chambers <- factor(Social4$Chamber, levels = c("Social", "Middle", "Object"))
Groups <- factor(Male.group.dfR$Group2, levels = c("Control", "Gq", "Gi"))


ggplot(SocailGi, aes(x = Group, y = Day, group = Group, color = Group)) +
  stat_summary(geom = "bar", fun = "max") + 
  geom_beeswarm(color = "dark gray") +
  #stat_summary(fun.data = mean_se) + 
  stat_pvalue_manual(chambers_test, label = "p.adj.signif", y.position = 40) +
  theme_bw(base_size = 12, base_family = "sans serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(axis.title.x = element_blank()) +
  scale_fill_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "magenta")) +
  labs(y = "Days to Criteria (70%)", title = "Group Reversal - Days to Criteria by Females") + 
  theme(plot.title = element_text(hjust = .5)) + 
  ylim(0,40) +
  facet_grid(.~Sex) +
  facet_grid(.~Group)


dfr3 <- GiR %>%
  select(Mouse, Sex, Group, Group2, ave, Social_Preference_Index...12) %>%
  mutate(Days = max(Day))

chambers_test <- aov(Social_Preference_Index...11 ~ Group2, data = SocialGi) %>%
  tukey_hsd()
summary(chambers_test)

aov(AUC ~ Group, data = auc_data) %>%
  tukey_hsd()

kruskal_test_result <- kruskal.test(AUC ~ Group, data = auc_data)
print(kruskal_test_result)

model1 <- lm(Social_Preference_Index...12 ~ Group2, data = SocialMale)
summary(model1)


# Calculate the mean value for each group
mean_values <- aggregate(AUC_Time_100_150_BaselineRatio_over_CNORatio ~ Group, data = Untreated_Scores, FUN = mean)

# Calculate fold change relative to "Group A"
group_a_mean <- mean_values$AUC_Time_100_150_BaselineRatio_over_CNORatio[mean_values$Group == "Control"]
mean_values$fold_change <- mean_values$AUC_Time_100_150_BaselineRatio_over_CNORatio / group_a_mean
mean_values$log2_fold_change <- log2(mean_values$fold_change)

print(mean_values)


ggplot(mean_values[mean_values$Group != "Control",], aes(x = Group, y = log2_fold_change, fill = Group)) +
  theme_bw(base_size = 12, base_family = "sans serif") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Fold Change Relative to Controls - Zscore AUC", y = "Fold Change") +
  theme_minimal() +
  scale_fill_manual(values = c("Gq" = "forest green", "Gi" = "magenta")) +
  ylim(-2,2)

######Reading in the Data############
DREADD_cFos <- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data_Final.xlsx", 
                           sheet = "LCN_DREADD_Verification")

dfR1 <- data.frame(Mouse, Sex, Group, Sample, Side, Normalized_CellCount, cFos_Count)
DREADD_cFos1 <- drop_na(Expression1)
DREADD_cFos2 <-DREADD_cFos1.1[!DREADD_cFos1.1$Sex=='Female', ]
mCherry_count <- DREADD_cFos.9[!DREADD_cFos.9$Group=='Control', ]

####Averaging Sample per Mouse####

DREADD_cFos3 <- DREADD_cFos %>%
  group_by(Mouse, Region ) %>%
  mutate(ave = mean(Normalized_mCherrybyArea))

Expression1 <- DREADD_cFos3 %>%
  distinct()

####Selecting Sample from average####
DREADD_cFos4 <- DREADD_cFos1[DREADD_cFos1$Sample>0 & DREADD_cFos1$Sample<2, ]

#########BoxPlotGraph#########


ggplot(DREADD_cFos4, aes(x = Region, y = ave, color = Region)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  geom_boxplot() + 
  #stat_compare_means(method = "anova", label.y = 100) +
  #stat_compare_means(method = "t.test", ref.group = "Untreated", label = "p.signif", label.y = 100) +
  #stat_pvalue_manual(test1, label = "p.adj.signif", y.position = 1) +
  #geom_beeswarm() +
  geom_point() +
  #scale_color_manual(values = c("Male" = "black", "Female" = "red"))+
  #scale_color_discrete(limits = c("Male", "Female")) +
  #scale_color_manual(values=c("Gq" = "dark green", "Gi" = "magenta")) +
  labs(y = "mCherry/Area", title = "DREADD Expression in CN") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0, 1) +
  facet_grid(.~Sex)

test1<- aov(ave ~ Region, data = DREADD_cFos4) %>%
  tukey_hsd()
summary(test1)


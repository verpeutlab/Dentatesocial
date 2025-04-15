cFos_Count.1 <- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data_Final.xlsx", 
                           sheet = "VTA")
cortical_Fos <-cFos_Count[c(1:85),c(1:9)]
cFos_Count.2 <- cFos_Count.1[!cFos_Count.1$Sex=='Female', ]
cFos_Count.3 <- cFos_Count.2[!cFos_Count.2$Group=='CNO', ]
cFos_Count <- cFos_Count.3[!cFos_Count.3$Group=='Gq', ]


VTA_cFos <- cFos_Count %>%
  drop_na(TH_cFos)

NC_Fos1 <- VTA_cFos %>%
  group_by(Mouse, ) %>%
  mutate(ave_normalizedcFos = mean(TH_cFos))

VTAcFos.1 <- NC_Fos1[NC_Fos1$Sample>0 & NC_Fos1$Sample<2, ]
Core <- VTAcFos.1 %>%
  select(Mouse, Sex, Group, Group2, ave_normalizedcFos)

cortical_Fos3 <- drop_na(cFos_Count)
Groups <- factor(cortical_Fos2$Group2, levels = c("Control", "Gq", "Gi"))

ggplot(cortical_Fos2, aes(x = Sex, y = ave, color = Sex)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  theme(legend.position = "none") + 
  theme(axis.title.x = element_blank()) +
  geom_boxplot() + 
  #stat_compare_means(method = "anova", label.y = 75) +
  #stat_compare_means(method = "t.test", ref.group = "Untreated", label = "p.signif", label.y = 100) +
  #stat_pvalue_manual(test1, label = "p.adj.signif", y.position = 5) +
  #geom_beeswarm() +
  geom_point() +
  scale_color_manual(values = c("Male" = "black", "Female" = "red"))+
  #scale_color_discrete(limits = c("Male", "Female")) +
  #scale_color_manual(values=c("Control" = "black", "Gq" = "dark green", "Gi" = "magenta")) +
  labs(y = "cFos+TH", title = "SomatoMotor - cFos/Area Normalized") + 
  theme(plot.title = element_text(hjust = .5)) +
  ylim(0, 5) +
  facet_grid(.~Sex)


test1<- aov(ave_normalizedcFos ~ Group2*Sex, data = Core) %>%
  tukey_hsd()
summary(test1)

cort3 <- cortical_Fos2 %>%
  select(Mouse, Sex, Group, Group2, ave) %>% distinct()


# Load necessary libraries
library(corrplot) 
library(reshape2)
library(ggcorrplot)

# Compute Spearman's rank2correlation
 corr_data <- cFos_Count.3 %>%
   select(Time_SS, Time_SO, Distance_SS, Distance_SO, Cup_S, Cup_O, 
          Distance_S, Distance_O, Social_Preference_Index, 
          ACC_cFos, ORB_cFos, SomatoMotor_cFos, VTA_cFos, VTA_TH, Core_cFos, Shell_cFos, Total_cFos, AUC_full, 
          AUC_proximal, AUC_distal, Basal_Spines, Apical_Spines)
 data <- corr_data%>%
   drop_na(Time_SS)
   
 Cdata <- data[, sapply(data, is.numeric)]
 
 cor_matrix <- cor(Cdata, method = "spearman")
 
 
 printVar = function(Cdata){
   vals = cor.test(Cdata,
                   method="spearman")[c("estimate","p.value")]
   names(vals) = c("rho","p")
   paste(names(vals),signif(unlist(vals),2),collapse="\n")
 }
 
 cor_with_pvalues <- function(data, method = "spearman") {
   n <- ncol(data)
   cor_matrix <- matrix(NA, n, n)
   pvalue_matrix <- matrix(NA, n, n)
   colnames(cor_matrix) <- colnames(pvalue_matrix) <- colnames(data)
   rownames(cor_matrix) <- rownames(pvalue_matrix) <- colnames(data)
   
   for (i in 1:n) {
     for (j in i:n) {
       cor_test <- cor.test(data[[i]], data[[j]], method = method)
       cor_matrix[i, j] <- cor_test$estimate
       pvalue_matrix[i, j] <- cor_test$p.value
       if (i != j) {
         cor_matrix[j, i] <- cor_matrix[i, j]
         pvalue_matrix[j, i] <- pvalue_matrix[i, j]
       }
     }
   }
   return(list(cor_matrix = cor_matrix, pvalue_matrix = pvalue_matrix))
 }
 
 result <- cor_with_pvalues(Cdata, method = "spearman")
 
 result$cor_matrix
 result$pvalue_matrix
 
 
 p_matrix <- get_pvalue_matrix(Cdata)
 
 cor_melted <- melt(result$cor_matrix)
 pvalue_melted <- melt(result$pvalue_matrix)
 
 
 # Combine the correlation and p-value data frames
 combined_data <- data.frame(
   Var1 = cor_melted$Var1,
   Var2 = cor_melted$Var2,
   Correlation = cor_melted$value,
   PValue = pvalue_melted$value
 )
 
 combined_data <- combined_data[combined_data$PValue < 0.05, ]

# Set the threshold for displaying coefficients (absolute value greater than 0.3)
threshold <- 0.3
combined_data[abs(combined_data) < threshold] <- NA

# Display the correlation matrix
print(cor_matrix)
print(significance_matrix)

# Add significance level
combined_data$Significance <- ifelse(combined_data$PValue < 0.001, "***",
                                     ifelse(combined_data$PValue < 0.01, "**",
                                            ifelse(combined_data$PValue < 0.05, "*", "ns")))
# Plot the heatmap with correlation values and significance
ggplot(combined_data, aes(x = Var1, y = Var2, fill = Correlation)) +
  geom_tile() +
  geom_text(aes(label = paste0(round(Correlation, 2), "\n", Significance)), color = "black", size = 1.75) +
  scale_fill_gradient2(high = "red", low = "blue", midpoint = 0, limits = c(-1, 1)) +
  theme_minimal() +
  #sig.level = 0.05,
  theme(axis.text.x = element_text(angle = 45, hjust = 1), axis.title = element_blank()) +
  labs(title = "Spearman Correlation Matrix with Significance - Controls")


# Create the heatmap
corrplot(cor_matrix, 
         method = "color", 
         col = colorRampPalette(c("white", "red"))(100), 
         type = "upper", 
         tl.col = "black", 
         addCoef.col = "black", # Display correlation coefficients
         number.cex = .32,      # Size of coefficient text (increase as needed)
         mar = c(.0001, .001, .0001, .00001))




corrplot(combined_data, 
         method = "color", 
         col = colorRampPalette(c("white", "red"))(100), 
         type = "upper", 
         tl.col = "black", 
         addCoef.col = "black",  # Display correlation coefficients
         number.cex = .32,       # Size of coefficient text
         addCoefasPercent = TRUE,
         p.mat = pvalue_melted,       # Add p-values to the plot
         sig.level = 0.05,       # Set significance level threshold
         insig = "blank",        # Do not display non-significant p-values
         mar = c(.0001, .001, .0001, .00001))

# Function to compute correlation and p-values
cor_with_pvalues <- function(data, method = "spearman") {
  n <- ncol(data)
  cor_matrix <- matrix(NA, n, n)
  pvalue_matrix <- matrix(NA, n, n)
  colnames(cor_matrix) <- colnames(pvalue_matrix) <- colnames(data)
  rownames(cor_matrix) <- rownames(pvalue_matrix) <- colnames(data)
  
  for (i in 1:n) {
    for (j in i:n) {
      cor_test <- cor.test(data[[i]], data[[j]], method = method)
      cor_matrix[i, j] <- cor_test$estimate
      pvalue_matrix[i, j] <- cor_test$p.value
      if (i != j) {
        cor_matrix[j, i] <- cor_matrix[i, j]
        pvalue_matrix[j, i] <- pvalue_matrix[i, j]
      }
    }
  }
  return(list(cor_matrix = cor_matrix, pvalue_matrix = pvalue_matrix))
}

# Get correlation and p-values
result <- cor_with_pvalues(Cdata, method = "spearman")

# Apply False Discovery Rate correction to the p-values
# Reshape the p-value matrix to a vector for FDR correction
pvalue_vector <- as.vector(result$pvalue_matrix)
# Apply FDR adjustment
fdr_adjusted_pvalues <- p.adjust(pvalue_vector, method = "fdr")

# Reshape the adjusted p-values back to a matrix
fdr_matrix <- matrix(fdr_adjusted_pvalues, ncol = ncol(result$pvalue_matrix))
rownames(fdr_matrix) <- colnames(fdr_matrix) <- colnames(result$pvalue_matrix)

# Add the FDR adjusted p-values to the result data frame
combined_data <- data.frame(
  Var1 = melt(result$cor_matrix)$Var1,
  Var2 = melt(result$cor_matrix)$Var2,
  Correlation = melt(result$cor_matrix)$value,
  PValue = melt(result$pvalue_matrix)$value,
  FDR = c(fdr_matrix[lower.tri(fdr_matrix)], fdr_matrix[upper.tri(fdr_matrix)])  # Add adjusted p-values
)

# Filter only significant correlations with FDR < 0.05
combined_data_significant <- combined_data[combined_data$FDR < 0.05, ]

corrplot(result$cor_matrix, 
         method = "color",              # Use colored tiles
         col = colorRampPalette(c("blue", "white", "red"))(200),  # Color gradient
         type = "upper",                # Show only the upper triangle
         addCoef.col = "black",         # Add correlation values in black text
         tl.cex = 0.75,                  # Axis label text size
         tl.col = "black",              # Axis label color
         tl.srt = 45,                   # Axis label rotation
         p.mat = result$pvalue_matrix,  # Add p-values matrix
         sig.level = 0.03,              # Show significance level markers
         insig = "blank",               # Remove labels for non-significant correlations
         diag = FALSE,                  # Don't display diagonal
         title = "Correlation Heatmap with FDR Adjustment",  # Title
         mar = c(0, 0, 1, 0)           # Adjust margins
)

# Print out the significant correlations after FDR adjustment
print(combined_data_significant)



# Now let's find the new alpha value (i.e., the largest FDR-adjusted p-value < 0.05)
# Flatten the FDR matrix to a vector
fdr_vector <- as.vector(fdr_matrix)

# Identify the new alpha value (the largest FDR-adjusted p-value below 0.05)
new_alpha_value <- max(fdr_vector[fdr_vector < 0.05])

# Print the new alpha value after FDR correction
cat("The new alpha value after FDR correction is:", new_alpha_value, "\n")

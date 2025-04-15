# Load required libraries
library(zoo)  # for rolling functions
library(ggplot2)  # for plotting
library(runner)

Untreated_Scores<- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data_Final.xlsx", 
                               sheet = "Fiber_Photometry_Data")



# Function to compute running percentile
running_percentile <- function(x, win, p, NaN_threshold = NULL) {
  if (is.null(NaN_threshold)) {
    NaN_threshold <- floor(win / 2)
  }
  
  n <- length(x)
  y <- rep(NA, n)
  
  for (i in seq_len(n)) {
    start <- max(1, i - floor(win / 2))
    end <- min(n, i + floor(win / 2))
    window_data <- x[start:end]
    
    if (sum(is.na(window_data)) > NaN_threshold) {
      y[i] <- NA
    } else {
      y[i] <- quantile(window_data, p / 100, na.rm = TRUE)
    }
  }
  
  return(y)
}
# Function to apply consistent plot settings
prepfig <- function() {
  theme_minimal() +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      axis.line = element_line(color = "black"),
      text = element_text(size = 12)
    )
}
# Load data (assuming the file is in CSV format)
Untreated_Scores<- read_excel ("C:/Users/ttlyle/Desktop/Social_Manuscript_Data_Final.xlsx", 
                               sheet = "Fiber_Photometry_Data")
fps <- 30
# Detrend signal
s_idx <- which(diff(Untreated_Scores$time) < -50)
s_idx <- c(s_idx, nrow(Untreated_Scores))
for (i in seq_along(s_idx)) {
  str <- ifelse(i == 1, 1, s_idx[i - 1] + 1)
  stp <- s_idx[i]
  
  Untreated_Scores$Gcamp_Baseline[str:(stp-1)] <- residuals(lm(Untreated_Scores$Gcamp_Baseline[str:(stp-1)] ~ seq_along(str:(stp-1))))
  Untreated_Scores$Isobestic_Baseline[str:(stp-1)] <- residuals(lm(Untreated_Scores$Isobestic_Baseline[str:(stp-1)] ~ seq_along(str:(stp-1))))
  Untreated_Scores$Gcamp_CNO[str:(stp-1)] <- residuals(lm(Untreated_Scores$Gcamp_CNO[str:(stp-1)] ~ seq_along(str:(stp-1))))
  Untreated_Scores$Isobestic_CNO[str:(stp-1)] <- residuals(lm(Untreated_Scores$Isobestic_CNO[str:(stp-1)] ~ seq_along(str:(stp-1))))
}
# Compute dF/F0
Untreated_Scores$GcampF0 <- running_percentile(Untreated_Scores$Gcamp_Baseline, 3 * fps, 5)
Untreated_Scores$Gcamp_dff <- (Untreated_Scores$Gcamp_Baseline - Untreated_Scores$GcampF0) / Untreated_Scores$GcampF0
Untreated_Scores$IsobesticF0 <- running_percentile(Untreated_Scores$Isobestic_Baseline, 10 * fps, 5)
Untreated_Scores$Isobestic_dff <- (Untreated_Scores$Isobestic_Baseline - Untreated_Scores$IsobesticF0) / Untreated_Scores$IsobesticF0

Untreated_Scores$Gcamp_CNO_F0 <- running_percentile(Untreated_Scores$Gcamp_CNO, 3 * fps, 5)
Untreated_Scores$Gcamp_CNO_dff <- (Untreated_Scores$Gcamp_CNO - Untreated_Scores$Gcamp_CNO_F0) / Untreated_Scores$Gcamp_CNO_F0
Untreated_Scores$Isobestic_CNO_F0 <- running_percentile(Untreated_Scores$Isobestic_CNO, 10 * fps, 5)
Untreated_Scores$Isobestic_CNO_dff <- (Untreated_Scores$Isobestic_CNO - Untreated_Scores$Isobestic_CNO_F0) / Untreated_Scores$Isobestic_CNO_F0



# Smooth and compute ratio
Untreated_Scores$Ratio <- rollapply(Untreated_Scores$Gcamp_dff / rollapply(Untreated_Scores$Isobestic_dff, 2 * fps, mean, fill = NA), fps / 2, mean, fill = NA)
Untreated_Scores$Ratio_CNO <- rollapply(Untreated_Scores$Gcamp_CNO_dff / rollapply(Untreated_Scores$Isobestic_CNO_dff, 2 * fps, mean, fill = NA), fps / 2, mean, fill = NA)
Untreated_Scores$B_CNO_Ratio <- rollapply(Untreated_Scores$Ratio / rollapply(Untreated_Scores$Ratio_CNO, 2 * fps, mean, fill = NA), fps / 2, mean, fill = NA)

Untreated_Scores$zscoredR <- scale(Untreated_Scores$B_CNO_Ratio)

# Plot results

ggplot(Photometry, aes(x = time, y = Gcamp_Baseline, color = Group)) +
  theme_bw(base_size = 12, base_family = "TT Arial") + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  geom_line() +
  labs(x = "Time", y = "Z-score") +
  scale_color_manual(values=c("Control" = "black", "Gq" = "forest green", "Gi" = "magenta")) +
  labs(y = "Z-score", title = "LCN Gcamp Gi Z-score (Baseline_Ratio/CNO_Ratio") +
  ylim(-1, 1) +
  facet_grid(.~Group)


Photometry.1 <- Untreated_Scores[Untreated_Scores$time>99 & Untreated_Scores$time<151, ]
Photometry <- Photometry.1 %>%
  drop_na(Gcamp_Baseline)


########Area Under the Curve#########
grouped_data <- split(Photometry, Photometry$Mouse) #Split by Group

calculate_auc <- function(time, zscoredR) {
  return(sum(diff(time) * (head(zscoredR, -1) + tail(zscoredR, -1)) / 2))      #Function to calculate AUC using the trapezoidal rule
}

auc_results <- sapply(grouped_data, function(grouped_data) {
  calculate_auc(grouped_data$time, grouped_data$zscoredR)}) # Calculate AUC for each group

print(auc_results)

auc_data <- data.frame(
  Group = names(auc_results), #Create data phrame with AUC values per group
  AUC = auc_results)

pairwise_results <- pairwise.t.test(auc_data$AUC, auc_data$Group, p.adjust.method = "bonferroni")
print(pairwise_results)



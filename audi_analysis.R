# ===============================================================================
# AUDI DATASET ANALYSIS - DATA ANALYTICS ASSIGNMENT
# Course: 7COM1079

# SECTION 1: LOAD REQUIRED LIBRARIES
# ===============================================================================
# Install packages if not already installed
required_packages <- c("ggplot2", "dplyr", "tidyr", "corrplot", "car", 
                       "moments", "gridExtra", "scales")

for(package in required_packages) {
  if(!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# ===============================================================================
# SECTION 2: DATA LOADING AND INITIAL EXPLORATION
# ===============================================================================

# Load the dataset
audi_data <- read.csv("audi.csv", stringsAsFactors = FALSE)

# Display basic information about the dataset
cat("=== DATASET OVERVIEW ===\n")
cat("Number of observations:", nrow(audi_data), "\n")
cat("Number of variables:", ncol(audi_data), "\n\n")

cat("=== DATASET STRUCTURE ===\n")
str(audi_data)

cat("\n=== FIRST FEW ROWS ===\n")
head(audi_data)

cat("\n=== SUMMARY STATISTICS ===\n")
summary(audi_data)

cat("\n=== MISSING VALUES ===\n")
colSums(is.na(audi_data))

# ===============================================================================
# SECTION 3: DATA CLEANING AND PREPARATION
# ===============================================================================

# Remove any duplicate rows
audi_data <- audi_data %>% distinct()

# Check for outliers in price (using IQR method for reference)
Q1_price <- quantile(audi_data$price, 0.25)
Q3_price <- quantile(audi_data$price, 0.75)
IQR_price <- Q3_price - Q1_price

cat("\n=== OUTLIER DETECTION (Price) ===\n")
cat("Q1:", Q1_price, "\n")
cat("Q3:", Q3_price, "\n")
cat("IQR:", IQR_price, "\n")
cat("Lower bound:", Q1_price - 1.5*IQR_price, "\n")
cat("Upper bound:", Q3_price + 1.5*IQR_price, "\n")

# Create categorical variables for analysis
audi_data <- audi_data %>%
  mutate(
    age = 2024 - year,
    mileage_category = cut(mileage, 
                           breaks = c(0, 20000, 50000, 100000, Inf),
                           labels = c("Low", "Medium", "High", "Very High")),
    price_category = cut(price,
                         breaks = c(0, 15000, 25000, 40000, Inf),
                         labels = c("Budget", "Mid-Range", "Premium", "Luxury")),
    transmission_type = as.factor(transmission),
    fuel_type = as.factor(fuelType)
  )

# ===============================================================================
# SECTION 4: RESEARCH QUESTION AND HYPOTHESIS
# ===============================================================================

cat("\n")
cat("=====================================================================\n")
cat("RESEARCH QUESTION:\n")
cat("Is there a significant difference in mean prices between\n")
cat("Automatic and Manual transmission Audi vehicles?\n")
cat("=====================================================================\n")
cat("\n")
cat("NULL HYPOTHESIS (H0):\n")
cat("There is no significant difference in the mean prices of Automatic\n")
cat("and Manual transmission Audi vehicles.\n")
cat("(μ_automatic = μ_manual)\n")
cat("\n")
cat("ALTERNATIVE HYPOTHESIS (H1):\n")
cat("There is a significant difference in the mean prices of Automatic\n")
cat("and Manual transmission Audi vehicles.\n")
cat("(μ_automatic ≠ μ_manual)\n")
cat("=====================================================================\n\n")

# ===============================================================================
# SECTION 5: EXPLORATORY DATA ANALYSIS
# ===============================================================================

# Filter to focus on main transmission types (Automatic and Manual)
audi_main <- audi_data %>%
  filter(transmission %in% c("Automatic", "Manual"))

cat("=== DATA SUBSET FOR ANALYSIS ===\n")
cat("Automatic transmission vehicles:", 
    sum(audi_main$transmission == "Automatic"), "\n")
cat("Manual transmission vehicles:", 
    sum(audi_main$transmission == "Manual"), "\n")

# Descriptive statistics by transmission type
cat("\n=== DESCRIPTIVE STATISTICS BY TRANSMISSION TYPE ===\n")
desc_stats <- audi_main %>%
  group_by(transmission) %>%
  summarise(
    n = n(),
    mean_price = mean(price),
    median_price = median(price),
    sd_price = sd(price),
    min_price = min(price),
    max_price = max(price),
    Q1 = quantile(price, 0.25),
    Q3 = quantile(price, 0.75)
  )
print(desc_stats)

# ===============================================================================
# SECTION 6: DATA VISUALIZATION
# ===============================================================================

# Visualization 1: Histogram of Price Distribution by Transmission
p1 <- ggplot(audi_main, aes(x = price, fill = transmission)) +
  geom_histogram(alpha = 0.6, position = "identity", bins = 50, color = "black") +
  scale_fill_manual(values = c("Automatic" = "#FF6B6B", "Manual" = "#4ECDC4")) +
  labs(title = "Price Distribution by Transmission Type",
       subtitle = "Histogram showing frequency distribution of prices",
       x = "Price (£)",
       y = "Frequency",
       fill = "Transmission") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "top")

# Visualization 2: Box Plot for Price by Transmission
p2 <- ggplot(audi_main, aes(x = transmission, y = price, fill = transmission)) +
  geom_boxplot(alpha = 0.7, outlier.color = "red", outlier.shape = 16) +
  stat_summary(fun = mean, geom = "point", shape = 23, size = 3, 
               fill = "white", color = "black") +
  scale_fill_manual(values = c("Automatic" = "#FF6B6B", "Manual" = "#4ECDC4")) +
  labs(title = "Price Comparison: Automatic vs Manual Transmission",
       subtitle = "Box plot with mean (diamond) and outliers (red dots)",
       x = "Transmission Type",
       y = "Price (£)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "none")

# Visualization 3: Violin Plot with overlaid box plot
p3 <- ggplot(audi_main, aes(x = transmission, y = price, fill = transmission)) +
  geom_violin(alpha = 0.5, trim = FALSE) +
  geom_boxplot(width = 0.2, alpha = 0.7, outlier.shape = NA) +
  scale_fill_manual(values = c("Automatic" = "#FF6B6B", "Manual" = "#4ECDC4")) +
  labs(title = "Violin Plot: Price Distribution by Transmission Type",
       subtitle = "Shows density distribution and quartiles",
       x = "Transmission Type",
       y = "Price (£)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "none")

# Visualization 4: Density Plot
p4 <- ggplot(audi_main, aes(x = price, fill = transmission)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = desc_stats, aes(xintercept = mean_price, color = transmission),
             linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Automatic" = "#FF6B6B", "Manual" = "#4ECDC4")) +
  scale_color_manual(values = c("Automatic" = "#FF6B6B", "Manual" = "#4ECDC4")) +
  labs(title = "Density Plot: Price Distribution by Transmission Type",
       subtitle = "Dashed lines indicate mean prices",
       x = "Price (£)",
       y = "Density",
       fill = "Transmission",
       color = "Mean") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10),
        legend.position = "top")

# Display all plots
print(p1)
print(p2)
print(p3)
print(p4)

# Supplementary Visualizations
# Visualization 5: Scatter plot - Price vs Mileage colored by Transmission
p5 <- ggplot(audi_main, aes(x = mileage, y = price, color = transmission)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, alpha = 0.2) +
  scale_color_manual(values = c("Automatic" = "#FF6B6B", "Manual" = "#4ECDC4")) +
  labs(title = "Price vs Mileage by Transmission Type",
       subtitle = "With linear regression lines",
       x = "Mileage",
       y = "Price (£)",
       color = "Transmission") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 10))

print(p5)

# Visualization 6: Bar plot showing count by fuel type and transmission
p6 <- ggplot(audi_main, aes(x = fuelType, fill = transmission)) +
  geom_bar(position = "dodge", color = "black", alpha = 0.7) +
  scale_fill_manual(values = c("Automatic" = "#FF6B6B", "Manual" = "#4ECDC4")) +
  labs(title = "Vehicle Count by Fuel Type and Transmission",
       x = "Fuel Type",
       y = "Count",
       fill = "Transmission") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        legend.position = "top")

print(p6)

# ===============================================================================
# SECTION 7: ASSUMPTION CHECKING FOR TWO-SAMPLE T-TEST
# ===============================================================================

cat("\n=== ASSUMPTION CHECKING ===\n\n")

# 1. Independence - satisfied by design (different cars)
cat("1. INDEPENDENCE: Satisfied (different vehicles in dataset)\n\n")

# 2. Normality Test
cat("2. NORMALITY TEST (Shapiro-Wilk Test):\n")
cat("H0: Data is normally distributed\n")
cat("H1: Data is not normally distributed\n\n")

automatic_prices <- audi_main %>% 
  filter(transmission == "Automatic") %>% 
  pull(price)

manual_prices <- audi_main %>% 
  filter(transmission == "Manual") %>% 
  pull(price)

# Shapiro-Wilk test (use sample if n > 5000)
if(length(automatic_prices) > 5000) {
  set.seed(123)
  automatic_sample <- sample(automatic_prices, 5000)
  shapiro_auto <- shapiro.test(automatic_sample)
} else {
  shapiro_auto <- shapiro.test(automatic_prices)
}

if(length(manual_prices) > 5000) {
  set.seed(123)
  manual_sample <- sample(manual_prices, 5000)
  shapiro_manual <- shapiro.test(manual_sample)
} else {
  shapiro_manual <- shapiro.test(manual_prices)
}

cat("Automatic transmission:\n")
cat("  W =", shapiro_auto$statistic, ", p-value =", shapiro_auto$p.value, "\n")
if(shapiro_auto$p.value < 0.05) {
  cat("  Result: Reject H0 - Data is NOT normally distributed\n")
} else {
  cat("  Result: Fail to reject H0 - Data IS normally distributed\n")
}

cat("\nManual transmission:\n")
cat("  W =", shapiro_manual$statistic, ", p-value =", shapiro_manual$p.value, "\n")
if(shapiro_manual$p.value < 0.05) {
  cat("  Result: Reject H0 - Data is NOT normally distributed\n")
} else {
  cat("  Result: Fail to reject H0 - Data IS normally distributed\n")
}

# Q-Q plots for visual assessment of normality
par(mfrow = c(1, 2))
qqnorm(automatic_prices, main = "Q-Q Plot: Automatic Transmission")
qqline(automatic_prices, col = "red", lwd = 2)
qqnorm(manual_prices, main = "Q-Q Plot: Manual Transmission")
qqline(manual_prices, col = "red", lwd = 2)
par(mfrow = c(1, 1))

# 3. Homogeneity of Variance (Levene's Test)
cat("\n3. HOMOGENEITY OF VARIANCE (Levene's Test):\n")
cat("H0: Variances are equal\n")
cat("H1: Variances are not equal\n\n")

levene_test <- leveneTest(price ~ transmission, data = audi_main)
print(levene_test)

if(levene_test$`Pr(>F)`[1] < 0.05) {
  cat("\nResult: Reject H0 - Variances are NOT equal\n")
  cat("Action: Will use Welch's t-test (does not assume equal variances)\n")
  use_welch <- TRUE
} else {
  cat("\nResult: Fail to reject H0 - Variances ARE equal\n")
  cat("Action: Can use standard t-test\n")
  use_welch <- FALSE
}

# ===============================================================================
# SECTION 8: HYPOTHESIS TESTING
# ===============================================================================

cat("\n")
cat("=====================================================================\n")
cat("HYPOTHESIS TESTING: TWO-SAMPLE T-TEST\n")
cat("=====================================================================\n\n")

# Perform appropriate t-test based on assumption checking
if(use_welch) {
  t_test_result <- t.test(price ~ transmission, data = audi_main, 
                          var.equal = FALSE, alternative = "two.sided")
  cat("Using: Welch's Two-Sample t-test (unequal variances)\n\n")
} else {
  t_test_result <- t.test(price ~ transmission, data = audi_main, 
                          var.equal = TRUE, alternative = "two.sided")
  cat("Using: Student's Two-Sample t-test (equal variances)\n\n")
}

print(t_test_result)

# Extract key values
t_statistic <- t_test_result$statistic
p_value <- t_test_result$p.value
conf_int <- t_test_result$conf.int
mean_automatic <- t_test_result$estimate[1]
mean_manual <- t_test_result$estimate[2]
mean_diff <- mean_automatic - mean_manual

cat("\n=== INTERPRETATION ===\n")
cat("Mean price (Automatic): £", round(mean_automatic, 2), "\n")
cat("Mean price (Manual): £", round(mean_manual, 2), "\n")
cat("Difference in means: £", round(mean_diff, 2), "\n")
cat("95% Confidence Interval: [£", round(conf_int[1], 2), ", £", 
    round(conf_int[2], 2), "]\n")
cat("t-statistic:", round(t_statistic, 4), "\n")
cat("p-value:", format(p_value, scientific = FALSE), "\n\n")

# Decision
alpha <- 0.05
cat("Significance level (α): ", alpha, "\n\n")

if(p_value < alpha) {
  cat("DECISION: Reject the null hypothesis (p < α)\n\n")
  cat("CONCLUSION:\n")
  cat("There IS a statistically significant difference in mean prices\n")
  cat("between Automatic and Manual transmission Audi vehicles.\n")
  cat("Automatic transmission vehicles are on average £", 
      round(abs(mean_diff), 2), 
      ifelse(mean_diff > 0, " MORE expensive", " LESS expensive"),
      " than Manual transmission vehicles.\n")
} else {
  cat("DECISION: Fail to reject the null hypothesis (p ≥ α)\n\n")
  cat("CONCLUSION:\n")
  cat("There is NO statistically significant difference in mean prices\n")
  cat("between Automatic and Manual transmission Audi vehicles.\n")
}

cat("=====================================================================\n")

# ===============================================================================
# SECTION 9: EFFECT SIZE (Cohen's d)
# ===============================================================================

cat("\n=== EFFECT SIZE (Cohen's d) ===\n")

# Calculate pooled standard deviation
n1 <- length(automatic_prices)
n2 <- length(manual_prices)
sd1 <- sd(automatic_prices)
sd2 <- sd(manual_prices)

pooled_sd <- sqrt(((n1-1)*sd1^2 + (n2-1)*sd2^2) / (n1+n2-2))
cohens_d <- mean_diff / pooled_sd

cat("Cohen's d:", round(cohens_d, 4), "\n")
cat("Interpretation: ")
if(abs(cohens_d) < 0.2) {
  cat("Negligible effect\n")
} else if(abs(cohens_d) < 0.5) {
  cat("Small effect\n")
} else if(abs(cohens_d) < 0.8) {
  cat("Medium effect\n")
} else {
  cat("Large effect\n")
}

# ===============================================================================
# SECTION 10: ADDITIONAL STATISTICAL ANALYSIS (CORRELATION)
# ===============================================================================

cat("\n")
cat("=====================================================================\n")
cat("SUPPLEMENTARY ANALYSIS: CORRELATION\n")
cat("=====================================================================\n\n")

# Correlation between numerical variables
numerical_vars <- audi_data %>%
  select(year, price, mileage, tax, mpg, engineSize, age) %>%
  na.omit()

cat("=== CORRELATION MATRIX ===\n")
cor_matrix <- cor(numerical_vars)
print(round(cor_matrix, 3))

# Visualize correlation matrix
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numerical Variables",
         mar = c(0,0,2,0))

# Test specific correlations
cat("\n=== CORRELATION TEST: Price vs Mileage ===\n")
cor_test_mileage <- cor.test(audi_data$price, audi_data$mileage, 
                              method = "pearson")
print(cor_test_mileage)

cat("\n=== CORRELATION TEST: Price vs Age ===\n")
cor_test_age <- cor.test(audi_data$price, audi_data$age, 
                         method = "pearson")
print(cor_test_age)

# ===============================================================================
# SECTION 11: CONTINGENCY TABLE ANALYSIS (PROPORTION TEST)
# ===============================================================================

cat("\n")
cat("=====================================================================\n")
cat("SUPPLEMENTARY ANALYSIS: CHI-SQUARE TEST OF INDEPENDENCE\n")
cat("=====================================================================\n\n")

cat("Research Question: Is there an association between transmission type\n")
cat("and fuel type in Audi vehicles?\n\n")

# Create contingency table
contingency_table <- table(audi_main$transmission, audi_main$fuelType)
cat("=== CONTINGENCY TABLE ===\n")
print(contingency_table)

cat("\n=== PROPORTIONS (Row Percentages) ===\n")
prop_table <- prop.table(contingency_table, margin = 1) * 100
print(round(prop_table, 2))

# Chi-square test
cat("\n=== CHI-SQUARE TEST OF INDEPENDENCE ===\n")
chi_test <- chisq.test(contingency_table)
print(chi_test)

cat("\n=== EXPECTED FREQUENCIES ===\n")
print(round(chi_test$expected, 2))

if(chi_test$p.value < 0.05) {
  cat("\nDECISION: Reject H0 (p < 0.05)\n")
  cat("CONCLUSION: There IS a significant association between\n")
  cat("transmission type and fuel type.\n")
} else {
  cat("\nDECISION: Fail to reject H0 (p ≥ 0.05)\n")
  cat("CONCLUSION: There is NO significant association between\n")
  cat("transmission type and fuel type.\n")
}

# ===============================================================================
# SECTION 12: REGRESSION ANALYSIS
# ===============================================================================

cat("\n")
cat("=====================================================================\n")
cat("SUPPLEMENTARY ANALYSIS: LINEAR REGRESSION\n")
cat("=====================================================================\n\n")

cat("Model: Predicting Price based on Mileage, Year, and Transmission\n\n")

# Build regression model
model <- lm(price ~ mileage + year + transmission, data = audi_main)
cat("=== REGRESSION MODEL SUMMARY ===\n")
summary(model)

# Model diagnostics
cat("\n=== MODEL DIAGNOSTICS ===\n")
par(mfrow = c(2, 2))
plot(model)
par(mfrow = c(1, 1))

# ===============================================================================
# SECTION 13: SUMMARY AND EXPORT RESULTS
# ===============================================================================

cat("\n")
cat("=====================================================================\n")
cat("ANALYSIS COMPLETE\n")
cat("=====================================================================\n\n")

# Create summary data frame
summary_results <- data.frame(
  Metric = c("Sample Size (Automatic)", 
             "Sample Size (Manual)",
             "Mean Price (Automatic)",
             "Mean Price (Manual)",
             "Difference in Means",
             "t-statistic",
             "p-value",
             "Cohen's d",
             "95% CI Lower",
             "95% CI Upper"),
  Value = c(n1,
            n2,
            round(mean_automatic, 2),
            round(mean_manual, 2),
            round(mean_diff, 2),
            round(t_statistic, 4),
            format(p_value, scientific = FALSE, digits = 6),
            round(cohens_d, 4),
            round(conf_int[1], 2),
            round(conf_int[2], 2))
)

cat("=== SUMMARY OF KEY RESULTS ===\n")
print(summary_results)

# Save summary to CSV
write.csv(summary_results, "analysis_summary.csv", row.names = FALSE)
cat("\nSummary results saved to: analysis_summary.csv\n")

# Save descriptive statistics to CSV
write.csv(desc_stats, "descriptive_statistics.csv", row.names = FALSE)
cat("Descriptive statistics saved to: descriptive_statistics.csv\n")

# Save all plots to separate files
# 1. Price Distribution Histogram
ggsave("plot1_price_distribution.png", p1, width = 10, height = 6)

# 2. Box Plot
ggsave("plot2_price_boxplot.png", p2, width = 10, height = 6)

# 3. Violin Plot
ggsave("plot3_price_violin.png", p3, width = 10, height = 6)

# 4. Density Plot
ggsave("plot4_price_density.png", p4, width = 10, height = 6)

# 5. Scatter Plot
ggsave("plot5_price_mileage_scatter.png", p5, width = 10, height = 6)

# 6. Bar Plot
ggsave("plot6_fuel_transmission_bar.png", p6, width = 10, height = 6)

# 7. Correlation Matrix
png("plot7_correlation_matrix.png", width = 800, height = 800)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.7,
         tl.col = "black", tl.srt = 45,
         title = "Correlation Matrix of Numerical Variables",
         mar = c(0,0,2,0))
dev.off()

# 8. Regression Model Diagnostics (Separate files)
# Reset par just in case
par(mfrow = c(1, 1))

png("plot8_residuals_vs_fitted.png", width = 800, height = 600)
plot(model, which = 1)
dev.off()

png("plot9_qq_residuals.png", width = 800, height = 600)
plot(model, which = 2)
dev.off()

png("plot10_scale_location.png", width = 800, height = 600)
plot(model, which = 3)
dev.off()

png("plot11_residuals_leverage.png", width = 800, height = 600)
plot(model, which = 5)
dev.off()

cat("\nAll visualizations saved as separate PNG files\n")

cat("\n=====================================================================\n")
cat("END OF ANALYSIS\n")
cat("=====================================================================\n")

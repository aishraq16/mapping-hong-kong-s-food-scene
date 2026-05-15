# ==============================================================================
# SCRIPT: Research Question Diagnostic Audit
# PURPOSE: Extract statistical context to determine the best modeling approach
# ==============================================================================

library(dplyr)

message("--- Diagnostic Audit for Research Questions ---")

# 1. Check Distributions of Key Variables (Price & Rating)
# This helps decide if we need linear regression or classification.
cat("\n[1] Descriptive Statistics for Model Targets:\n")
print(summary(df_price_complete[, c("rating", "price_level", "dist_to_mtr_m")]))

# 2. Correlation Matrix
# This answers Question 1 (How geography shapes the landscape)
cat("\n[2] Correlation between Geography, Price, and Rating:\n")
# We only calculate this for numeric columns
numeric_cols <- df_price_complete %>% select(rating, price_level, dist_to_mtr_m)
print(cor(numeric_cols, use = "complete.obs"))

# 3. Cuisine Density (Top 10)
# This identifies if we have enough "Cuisine Type" data to use as a predictor.
cat("\n[3] Top 10 Cuisine Keywords (First Tag Identified):\n")
# Assuming type is a string, we extract the first word for a quick count
cuisine_counts <- table(gsub("[^[:alnum:] ]", "", sapply(strsplit(df_price_complete$type, ","), `[`, 1)))
print(head(sort(cuisine_counts, decreasing = TRUE), 10))

# 4. District Variance
# This helps decide if 'District' should be a fixed effect or a random effect.
cat("\n[4] Average Rating and Price by District:\n")
district_summary <- df_price_complete %>%
  group_by(dist_name) %>%
  summarize(
    avg_rating = mean(rating, na.rm = TRUE),
    avg_price = mean(price_level, na.rm = TRUE),
    sample_size = n()
  ) %>%
  arrange(desc(avg_rating))

print(district_summary)
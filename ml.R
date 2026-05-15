# ==============================================================================
# SCRIPT 3: Predictive Modeling (High-Fidelity Results)
# ==============================================================================
library(dplyr)
library(tidyr)        # REQUIRED for pivot_longer
library(caret)        
library(randomForest) 
library(ggplot2)

# Clear old models from memory to prevent level ghosting
rm(list = ls(pattern = "rf_model|lm_model"))

message("--- Running Predictive Models ---")

# ------------------------------------------------------------------------------
# 1. Prepare Data & Fix Factor Levels
# ------------------------------------------------------------------------------
df_model <- df_price_complete %>%
  select(rating, price_level, dist_to_mtr_m, dist_name, cuisine_type, is_chain) %>%
  filter(!is.na(rating)) %>%
  mutate(across(where(is.character), as.factor))

# CRITICAL: Identify categories with only 1 restaurant. 
# caret::createDataPartition will fail if it can't put at least one in both train/test.
singletons <- df_model %>%
  group_by(cuisine_type) %>%
  tally() %>%
  filter(n < 2) %>%
  pull(cuisine_type)

df_model <- df_model %>%
  filter(!(cuisine_type %in% singletons)) %>%
  droplevels() # Remove the empty levels from the factor dictionary

# Train/Test Split
set.seed(42)
train_index <- createDataPartition(df_model$rating, p = 0.8, list = FALSE)
train_data  <- df_model[train_index, ]
test_data   <- df_model[-train_index, ]

# ------------------------------------------------------------------------------
# 2. Model Training
# ------------------------------------------------------------------------------
lm_model <- lm(rating ~ ., data = train_data)
rf_model <- randomForest(rating ~ ., data = train_data, ntree = 100, importance = TRUE)

# ------------------------------------------------------------------------------
# 3. Handle Prediction (Final Level Sync)
# ------------------------------------------------------------------------------
# Force test_data to have the EXACT same levels as train_data
for(f in names(train_data)) {
  if(is.factor(train_data[[f]])) {
    test_data[[f]] <- factor(test_data[[f]], levels = levels(train_data[[f]]))
  }
}

# Remove any NAs introduced if a level was only in test (unlikely now)
test_data <- na.omit(test_data)

# Generate Predictions
lm_preds <- predict(lm_model, test_data)
rf_preds <- predict(rf_model, test_data)

# ------------------------------------------------------------------------------
# VISUAL 1: Actual vs. Predicted
# ------------------------------------------------------------------------------
plot_results <- data.frame(
  Actual = test_data$rating,
  Linear_Regression = lm_preds,
  Random_Forest = rf_preds
) %>%
  pivot_longer(cols = c(Linear_Regression, Random_Forest), 
               names_to = "Model", values_to = "Predicted")

pres_accuracy_plot <- ggplot(plot_results, aes(x = Actual, y = Predicted, color = Model)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", size = 1) +
  facet_wrap(~ Model) +
  scale_color_manual(values = c("#3498db", "#e67e22")) +
  labs(title = "Model Prediction Accuracy",
       subtitle = "Comparing Predicted vs. Actual Google Ratings (Red line = Perfect Prediction)",
       x = "Actual Google Rating", y = "Predicted Rating") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# VISUAL 2: Professional Feature Importance
# ------------------------------------------------------------------------------
imp_data <- as.data.frame(importance(rf_model))
imp_data$Feature <- rownames(imp_data)

pres_importance_plot <- ggplot(imp_data, aes(x = reorder(Feature, IncNodePurity), y = IncNodePurity, fill = IncNodePurity)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_fill_viridis_c(option = "magma") +
  labs(title = "Key Drivers of Restaurant Success",
       subtitle = "Ranking features by their impact on predicting Google Ratings",
       x = "Restaurant Attribute", y = "Relative Importance (Node Purity)") +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------------------------
# SAVE RESULTS
# ------------------------------------------------------------------------------
ggsave("ml_prediction_accuracy.png", plot = pres_accuracy_plot, width = 12, height = 6, dpi = 400)
ggsave("ml_feature_importance_cool.png", plot = pres_importance_plot, width = 10, height = 7, dpi = 400)

metrics <- data.frame(
  Model = c("Linear Regression", "Random Forest"),
  RMSE = c(RMSE(lm_preds, test_data$rating), RMSE(rf_preds, test_data$rating)),
  MAE = c(MAE(lm_preds, test_data$rating), MAE(rf_preds, test_data$rating)),
  R_Squared = c(R2(lm_preds, test_data$rating), R2(rf_preds, test_data$rating))
)
write.csv(metrics, "ml_performance_metrics.csv", row.names = FALSE)

message("--- SUCCESS: Professional ML visuals and metrics saved ---")
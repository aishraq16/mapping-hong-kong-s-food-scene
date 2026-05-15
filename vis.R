# ==============================================================================
# SCRIPT 4: High-Fidelity Visualizations (Updated)
# ==============================================================================
library(dplyr)
library(ggplot2)
library(viridis)
library(tidyr)

# ------------------------------------------------------------------------------
# NEW DIAGRAM 5: Distribution of Numerical Features (Boxplots)
# ------------------------------------------------------------------------------
# Reshaping data to plot distributions of different scales together
plot_boxplots <- df_price_complete %>%
  select(rating, price_level, dist_to_mtr_m, name_len) %>%
  pivot_longer(cols = everything(), names_to = "feature", values_to = "value") %>%
  ggplot(aes(x = feature, y = value, fill = feature)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 1) +
  facet_wrap(~ feature, scales = "free", ncol = 2) +
  scale_fill_viridis_d(option = "mako") +
  labs(title = "Distribution of Key Numerical Features",
       subtitle = "Boxplots showing spread, median, and outliers for modeling variables",
       x = "Feature", y = "Value") +
  theme_minimal() +
  theme(legend.position = "none", strip.text = element_text(face = "bold"))

# ------------------------------------------------------------------------------
# NEW DIAGRAM 6: Total Restaurant Count by Cuisine (All Districts)
# ------------------------------------------------------------------------------
plot_cuisine_total <- ggplot(df_price_complete, aes(x = reorder(cuisine_type, cuisine_type, function(x) -length(x)), fill = cuisine_type)) +
  geom_bar() +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Market Share: Total Restaurant Count by Cuisine",
       subtitle = "Consolidated data from all 4 targeted districts",
       x = "Cuisine Category", y = "Number of Restaurants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# ------------------------------------------------------------------------------
# NEW DIAGRAM 7: Cuisine Counts by District (4 Separate Panels)
# ------------------------------------------------------------------------------
plot_cuisine_districts <- ggplot(df_price_complete, aes(x = reorder(cuisine_type, cuisine_type, function(x) -length(x)), fill = cuisine_type)) +
  geom_bar() +
  facet_wrap(~ dist_name, scales = "free_y") +
  scale_fill_viridis_d(option = "turbo") +
  labs(title = "Regional Cuisine Breakdown",
       subtitle = "Comparative restaurant counts across the 4 study districts",
       x = "Cuisine Category", y = "Number of Restaurants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8),
        legend.position = "none",
        strip.background = element_rect(fill = "gray90"))

# ------------------------------------------------------------------------------
# EXISTING DIAGRAMS (Retained from previous version)
# ------------------------------------------------------------------------------

# DIAGRAM 1: The Geographic & Cultural Landscape
plot_landscape <- ggplot(df_price_complete, aes(x = dist_name, fill = cuisine_type)) +
  geom_bar(position = "fill", color = "white", linewidth = 0.3) +
  scale_fill_viridis_d(option = "turbo") + 
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "The Culinary Map: Cuisine Distribution by District",
       x = "District", y = "Percentage of Total Restaurants", fill = "Cuisine Type") +
  theme_minimal(base_size = 14) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"))

# DIAGRAM 2: The Economic Landscape 
plot_econ_geo <- ggplot(df_price_complete, aes(x = reorder(dist_name, -rating), y = rating, fill = dist_name)) +
  geom_boxplot(alpha = 0.6, outlier.shape = NA) + 
  geom_jitter(aes(color = as.factor(price_level)), width = 0.2, alpha = 0.7, size = 1.5) +
  scale_fill_viridis_d(option = "mako") +
  scale_color_viridis_d(option = "magma", direction = -1) +
  labs(title = "Quality and Cost by Geography", x = "District", y = "Google Rating", color = "Price Level") +
  theme_minimal(base_size = 14) +
  guides(fill = "none") 

# DIAGRAM 4: The MTR Proximity Effect
plot_mtr <- ggplot(df_price_complete, aes(x = dist_to_mtr_m, y = rating, color = dist_name)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "loess", se = FALSE, size = 1.2) + 
  scale_color_viridis_d(option = "plasma") +
  labs(title = "The Transit Effect: MTR Proximity vs. Rating", x = "Distance to MTR (meters)", y = "Google Rating") +
  theme_minimal(base_size = 14)

# ------------------------------------------------------------------------------
# SAVE ALL PLOTS
# ------------------------------------------------------------------------------
# New plots
ggsave("feature_distributions.png", plot = plot_boxplots, width = 10, height = 8, dpi = 300)
ggsave("cuisine_counts_total.png", plot = plot_cuisine_total, width = 10, height = 6, dpi = 300)
ggsave("cuisine_counts_by_district.png", plot = plot_cuisine_districts, width = 12, height = 8, dpi = 300)

# Original plots
ggsave("cuisine_landscape.png", plot = plot_landscape, width = 10, height = 6, dpi = 300)
ggsave("price_rating_dist.png", plot = plot_econ_geo, width = 10, height = 6, dpi = 300)
ggsave("mtr_proximity_analysis.png", plot = plot_mtr, width = 10, height = 6, dpi = 300)

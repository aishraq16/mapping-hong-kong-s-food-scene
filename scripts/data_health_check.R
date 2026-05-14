library(tidyverse)

# Load your combined data
df <- read_csv("final_hk_food_scene_data.csv")

# 1. DATA DENSITY & MISSINGNESS
data_quality <- df %>%
  summarise(
    total_obs = n(),
    na_ratings = sum(is.na(rating)) / n() * 100,
    na_price = sum(is.na(price_level)) / n() * 100,
    na_mtr = sum(is.na(dist_to_mtr_m)) / n() * 100
  )

# 2. SPATIAL DISTRIBUTIONS
spatial_summary <- df %>%
  group_by(dist_name) %>%
  summarise(
    count = n(),
    avg_rating = mean(rating, na.rm = TRUE),
    med_mtr_dist = median(dist_to_mtr_m, na.rm = TRUE)
  )

# 3. CUISINE FREQUENCY (To see if we can extract better tags)
# This splits the 'food, restaurant' strings to see what's common
common_tags <- df %>%
  separate_rows(cuisine_tags, sep = ", ") %>%
  count(cuisine_tags, sort = TRUE) %>%
  head(10)

# PRINT RESULTS FOR SUMMARY
print("--- DATA QUALITY (%) ---")
print(data_quality)
print("--- DISTRICT STATS ---")
print(spatial_summary)
print("--- TOP CUISINE TAGS ---")
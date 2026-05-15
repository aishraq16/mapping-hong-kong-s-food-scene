# ==============================================================================
# SCRIPT: Data Refinement and Subset Generation
# PROJECT: Mapping Hong Kong’s Food Scene
# AUTHOR: Ishraq Afzal
# PURPOSE: Create two specific data subsets for spatial and economic analysis:
#          1) Price-focused analysis (Economic modeling)
#          2) General spatial analysis (Geospatial density modeling)
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: DATA AGGREGATION
# ------------------------------------------------------------------------------

data_path <- "scripts"
files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)

if (length(files) == 0) {
  stop("CRITICAL: No CSV data files detected in path: ", data_path)
}

df_list  <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
combined <- do.call(rbind, df_list)

# RENAME 'cuisine_tags' to 'type' for a more professional schema
if ("cuisine_tags" %in% names(combined)) {
  names(combined)[names(combined) == "cuisine_tags"] <- "type"
}

# Define column names based on the updated schema
price_col    <- "price_level"
distance_col <- "dist_to_mtr_m"
rating_col   <- "rating"
district_col <- "dist_name"
tags_col     <- "type"  # Updated to match the new column name

# ------------------------------------------------------------------------------
# SECTION 1.5: KEYWORD & DISTANCE FILTERING
# ------------------------------------------------------------------------------

# Filter A: Only keep rows where 'type' contains "restaurant" or "food"
target_keywords <- "restaurant|food"
is_relevant <- grepl(target_keywords, combined[[tags_col]], ignore.case = TRUE)
combined <- combined[is_relevant, ]

# Filter B: Remove restaurants over 1km (1000m) from the nearest MTR station
# This removes geographical outliers and focuses on urban clusters.
combined <- combined[!is.na(combined[[distance_col]]) & combined[[distance_col]] <= 1000, ]

# Log results for audit trail
message(sprintf("Filtering complete: Records within 1km of MTR: %d", nrow(combined)))

# ------------------------------------------------------------------------------
# SECTION 2: SUBSET 1 - PRICE-AVAILABLE ANALYSIS
# Goal: Only restaurants with economic data (price_level).
# ------------------------------------------------------------------------------

# Step A: Filter for rows where price_level is strictly NOT NA
df_with_price <- combined[!is.na(combined[[price_col]]), ]

# Step B: Remove rows that have missing values in any remaining columns
df_price_complete <- na.omit(df_with_price)

# ------------------------------------------------------------------------------
# SECTION 3: SUBSET 2 - GENERAL SPATIAL ANALYSIS
# Goal: Maximize sample size for non-economic mapping.
# ------------------------------------------------------------------------------

# Step A: Remove the 'price_level' column entirely first
df_spatial_only <- combined[, !(names(combined) %in% price_col)]

# Step B: Remove rows with missing values in remaining columns (lat, lng, rating)
df_spatial_complete <- na.omit(df_spatial_only)

# ------------------------------------------------------------------------------
# SECTION 4: DIAGNOSTIC REPORTING
# ------------------------------------------------------------------------------

message("--- Data Processing Complete ---")
message(sprintf("Subset 1 (Economic): %d restaurants", nrow(df_price_complete)))
message(sprintf("Subset 2 (Spatial):  %d restaurants", nrow(df_spatial_complete)))

# Sanity check for district counts in the spatial set
print(table(df_spatial_complete[[district_col]]))

# ------------------------------------------------------------------------------
# SECTION 5: UNIQUE VALUE EXPLORATION
# ------------------------------------------------------------------------------

message("--- Unique Values Per Column ---")

for (col_name in names(combined)) {
  uniques <- unique(combined[[col_name]])
  unique_count <- length(uniques)
  
  cat("\n==========================================\n")
  cat(sprintf("COLUMN: %s | UNIQUE COUNT: %d\n", col_name, unique_count))
  cat("==========================================\n")
  
  if (unique_count > 20) {
    cat("Top 20 unique values:\n")
    print(head(uniques, 20))
  } else {
    cat("All unique values:\n")
    print(uniques)
  }
}
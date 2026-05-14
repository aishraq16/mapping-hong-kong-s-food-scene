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

# Define the directory containing the district-level CSV files
data_path <- "scripts"

# Locate all CSV files matching the naming pattern
files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)

# Error handling: Terminate if no data is found to prevent downstream failures
if (length(files) == 0) {
  stop("CRITICAL: No CSV data files detected in path: ", data_path)
}

# Consolidate individual district files into a single master dataframe
# 'lapply' reads each file; 'do.call(rbind, ...)' stacks them vertically
df_list  <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
combined <- do.call(rbind, df_list)

# Define column names based on the established data schema
# Note: 'dist_to_mtr_m' is the distance metric and 'dist_name' is the district
price_col    <- "price_level"
distance_col <- "dist_to_mtr_m"
rating_col   <- "rating"
district_col <- "dist_name"
tags_col     <- "cuisine_tags" # Explicitly defining the tags column

# ------------------------------------------------------------------------------
# SECTION 1.5: KEYWORD FILTERING (RELEVANCE CHECK)
# ------------------------------------------------------------------------------
# Define the keywords before using them
target_keywords <- "restaurant|food"

# Apply string detection on 'cuisine_tags'
# We use grepl to return a logical vector of rows containing the keywords.
# ignore.case = TRUE handles variations like 'Restaurant' or 'Food'.
is_relevant <- grepl(target_keywords, combined[[tags_col]], ignore.case = TRUE)

# Filter the master dataframe
combined_filtered <- combined[is_relevant, ]

# Log the filtering results for project documentation
original_count <- nrow(combined)
filtered_count <- nrow(combined_filtered)
removed_count  <- original_count - filtered_count

message(sprintf("Filtering complete: Removed %d non-relevant rows.", removed_count))
message(sprintf("Current pool size: %d entries.", filtered_count))

# Update the variable name so the rest of your script uses the filtered data
combined <- combined_filtered

# ------------------------------------------------------------------------------
# SECTION 2: SUBSET 1 - PRICE-AVAILABLE ANALYSIS
# Goal: Only restaurants with economic data (price_level).
#       Other missing values (NAs) are removed to ensure model integrity.
# ------------------------------------------------------------------------------

# Step A: Filter for rows where price_level is strictly NOT NA
df_with_price <- combined[!is.na(combined[[price_col]]), ]

# Step B: Remove rows that have missing values in any remaining columns
# 'na.omit' ensures the resulting set is "complete" for statistical modeling
df_price_complete <- na.omit(df_with_price)

# ------------------------------------------------------------------------------
# SECTION 3: SUBSET 2 - GENERAL SPATIAL ANALYSIS
# Goal: Maximize sample size for non-economic mapping.
#       Price is removed first to prevent it from filtering out rows.
# ------------------------------------------------------------------------------

# Step A: Create a copy and remove the 'price_level' column entirely
# Using negative indexing to drop the column by name
df_spatial_only <- combined[, !(names(combined) %in% price_col)]

# Step B: Remove rows with missing values in the remaining columns
# (e.g., restaurants missing only lat/long or rating)
df_spatial_complete <- na.omit(df_spatial_only)

# ------------------------------------------------------------------------------
# SECTION 4: DIAGNOSTIC REPORTING
# ------------------------------------------------------------------------------

message("--- Data Processing Complete ---")
message(sprintf("Subset 1 (Economic): %d restaurants", nrow(df_price_complete)))
message(sprintf("Subset 2 (Spatial):  %d restaurants", nrow(df_spatial_complete)))

# Final sanity check: Ensure district names are consistent in the spatial set
print(table(df_spatial_complete[[district_col]]))

# ------------------------------------------------------------------------------
# SECTION 5: UNIQUE VALUE EXPLORATION
# ------------------------------------------------------------------------------

message("--- Unique Values Per Column ---")

# Iterate through all columns in the original combined dataframe
for (col_name in names(combined)) {
  
  # Extract unique values
  uniques <- unique(combined[[col_name]])
  unique_count <- length(uniques)
  
  cat("\n==========================================\n")
  cat(sprintf("COLUMN: %s | UNIQUE COUNT: %d\n", col_name, unique_count))
  cat("==========================================\n")
  
  # Logic: If there are many unique values (like names or coordinates), 
  # just show the first few to avoid flooding the console.
  if (unique_count > 20) {
    cat("Top 20 unique values:\n")
    print(head(uniques, 20))
  } else {
    cat("All unique values:\n")
    print(uniques)
  }
}
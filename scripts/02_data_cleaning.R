# ==============================================================================
# SCRIPT 1: Data Refinement and Subset Generation
# ==============================================================================

# ------------------------------------------------------------------------------
# SECTION 1: DATA AGGREGATION
# ------------------------------------------------------------------------------
data_path <- "data" # Best practice: store raw data in a folder named 'data'
files <- list.files(path = data_path, pattern = "\\.csv$", full.names = TRUE)

if (length(files) == 0) {
  stop("CRITICAL: No CSV data files detected in path: ", data_path)
}

df_list  <- lapply(files, function(f) read.csv(f, stringsAsFactors = FALSE))
combined <- do.call(rbind, df_list)

# RENAME 'cuisine_tags' to 'type'
if ("cuisine_tags" %in% names(combined)) {
  names(combined)[names(combined) == "cuisine_tags"] <- "type"
}

# Define column names based on the schema
price_col    <- "price_level"
distance_col <- "dist_to_mtr_m"
rating_col   <- "rating"
district_col <- "dist_name"
tags_col     <- "type" 

# FIX: Calculate chain status on the RAW data before we start deleting rows
combined$is_chain <- ifelse(combined$name %in% combined$name[duplicated(combined$name)], "Yes", "No")

# ------------------------------------------------------------------------------
# SECTION 2: KEYWORD & DISTANCE FILTERING
# ------------------------------------------------------------------------------
# Filter A: Only keep rows where 'type' contains "restaurant" or "food"
is_relevant <- grepl("restaurant|food", combined[[tags_col]], ignore.case = TRUE)
combined <- combined[is_relevant, ]

# Filter B: Remove restaurants over 1km (1000m) from MTR
combined <- combined[!is.na(combined[[distance_col]]) & combined[[distance_col]] <= 1000, ]

message(sprintf("Filtering complete: Records within 1km of MTR: %d", nrow(combined)))

# ------------------------------------------------------------------------------
# SECTION 3: SUBSET 1 - PRICE-AVAILABLE ANALYSIS (Economic)
# ------------------------------------------------------------------------------
df_with_price <- combined[!is.na(combined[[price_col]]), ]

# FIX: Only drop rows if they are missing essential modeling columns, not ANY column.
essential_cols_econ <- c(price_col, distance_col, rating_col, district_col, tags_col, "is_chain")
df_price_complete <- df_with_price[complete.cases(df_with_price[, essential_cols_econ]), ]

# ------------------------------------------------------------------------------
# SECTION 4: SUBSET 2 - GENERAL SPATIAL ANALYSIS
# ------------------------------------------------------------------------------
df_spatial_only <- combined[, !(names(combined) %in% price_col)]

essential_cols_spatial <- c(distance_col, rating_col, district_col, tags_col, "is_chain")
df_spatial_complete <- df_spatial_only[complete.cases(df_spatial_only[, essential_cols_spatial]), ]

message("--- Data Processing Complete ---")
message(sprintf("Subset 1 (Economic): %d restaurants", nrow(df_price_complete)))
message(sprintf("Subset 2 (Spatial):  %d restaurants", nrow(df_spatial_complete)))
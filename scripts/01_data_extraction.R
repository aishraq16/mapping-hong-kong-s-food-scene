# Setting up

library(tidyverse)
library(xml2)
library(dplyr)
library(googleway)
library(dotenv)
library(ggplot2)
library(purrr)
library(geosphere) 

# 1. LOAD GOOGLE MAPS API KEY
# Ensure your .env file is in the same folder as this script
if(file.exists(".env")) {
    load_dot_env() 
  } else {
    stop("Error: .env file not found. Please set your working directory.")
  }
google_key <- Sys.getenv("GOOGLE_MAPS_API_KEY")

# 2. LOADING RESTAURANT DATA
# Adjust this file path if your working directory is different

xml_file <- read_xml("~/programming/data sc + ml/mapping-hk-food-scene/data/LP_Restaurants_EN.XML")

nodes <- xml_find_all(xml_file, ".//LP")
restaurants <- data.frame(
  name = xml_find_all(nodes, "./SS") %>% xml_text(),
  address = xml_find_all(nodes, "./ADR") %>% xml_text(),
  dist_code = xml_find_all(nodes, "./DIST") %>% xml_text(), 
  stringsAsFactors = FALSE
  )

code_nodes <- xml_find_all(xml_file, "//DIST_CODE/CODE")
dist_mapping <- data.frame(
  dist_code = xml_attr(code_nodes, "ID"),
  dist_name = xml_text(code_nodes),
  stringsAsFactors = FALSE
)

restaurants <- restaurants %>%
  left_join(dist_mapping, by = "dist_code")

# 3. FILTERING & SEARCH QUERY PREPARATION
target_districts <- c("Central/Western", "Mong Kok", "Sha Tin", "Eastern")

unique_requests <- restaurants %>% 
  filter(dist_name %in% target_districts) %>%
  distinct(name, address, dist_name, .keep_all = TRUE) %>%
  mutate(
    # FIX: Activated and improved regex to clean floor/unit info for better API matching
    clean_address = str_remove(address, "^(FLAT|RM|SHOP|UNIT|OFFICE|G/F|LG/F|UG/F|\\d+/F|\\d+ 樓).*?,\\s*"),
    search_query = paste(name, clean_address, "Hong Kong") # Added "Hong Kong" for global disambiguation
  ) %>%
  mutate(search_query = str_squish(str_trim(search_query)))

# 4. MTR STATIONS DATA
mtr_stations <- data.frame(
  district = c(
    rep("Central/Western", 6), rep("Mong Kok", 2),
    rep("Eastern", 8), rep("Sha Tin", 6)
  ),
  station = c(
    "Central", "Admiralty", "Sheung Wan", "Sai Ying Pun", "HKU", "Kennedy Town",
    "Mong Kok", "Mong Kok East",
    "North Point", "Tin Hau", "Fortress Hill", "Quarry Bay", 
    "Tai Koo", "Sai Wan Ho", "Shau Kei Wan", "Chai Wan",
    "Tai Wai", "Che Kung Temple", "Sha Tin Wai", "City One",
    "Shek Mun", "Sha Tin"
  ),
  lat = c(22.2819, 22.2789, 22.2865, 22.2859, 22.2837, 22.2816, 22.3193, 22.3226, 22.2917, 22.2824, 22.2881, 22.2869, 22.2848, 22.2817, 22.2790, 22.2648, 22.3728, 22.3747, 22.3763, 22.3837, 22.3877, 22.3833),
  lng = c(114.1581, 114.1650, 114.1516, 114.1420, 114.1351, 114.1287, 114.1694, 114.1722, 114.2004, 114.1918, 114.1931, 114.2097, 114.2165, 114.2229, 114.2264, 114.2379, 114.1784, 114.1860, 114.1910, 114.2000, 114.2086, 114.1887)
)

# 5. FUNCTIONS
calc_mtr_dist <- function(res_lat, res_lng, current_district) {
  if(is.na(res_lat) | is.na(res_lng)) return(NA)
  
  # Note: Filtering by district might miss closer stations across borders.
  # To be more accurate for your project, you could compare against all stations.
  relevant_stations <- mtr_stations %>% filter(district == current_district)
  
  if(nrow(relevant_stations) == 0) return(NA)
  
  distances <- distHaversine(
    c(res_lng, res_lat), 
    relevant_stations[, c("lng", "lat")]
  )
  return(min(distances))
}
get_coords <- function(search_string, index, total, district_name) {
  # Standard progress tracker
  message(paste0("\n[", index, "/", total, "] Processing: ", search_string))
  
  # 1. Find Place
  find_res <- tryCatch(
    google_find_place(input = search_string, fields = c("place_id"), key = google_key),
    error = function(e) { message("   ! API Connection Error"); return(NULL) }
  )
  
  if (is.null(find_res) || find_res$status != "OK") {
    message(" x No results found.")
    return(data.frame(lat=NA, lng=NA, rating=NA, price_level=NA, cuisine_tags=NA, dist_to_mtr_m=NA))
  }
  
  # 2. Get Details
  pid <- find_res$candidates$place_id[1]
  details <- tryCatch(
    google_place_details(place_id = pid, key = google_key),
    error = function(e) { return(NULL) }
  )
  
  if (!is.null(details) && details$status == "OK") {
    res_data <- details$result
    loc <- res_data$geometry$location
    
    # Safely extract values
    rating_val <- purrr::pluck(res_data, "rating", .default = NA)
    price_val  <- purrr::pluck(res_data, "price_level", .default = NA)
    type_tags  <- paste(purrr::pluck(res_data, "types", .default = "unknown"), collapse = ", ")
    mtr_dist   <- calc_mtr_dist(loc$lat, loc$lng, district_name)
    
    # 3. CONSTRUCT THE ROW
    row_to_save <- data.frame(
      lat = loc$lat,
      lng = loc$lng,
      rating = rating_val,
      price_level = price_val,
      cuisine_tags = type_tags,
      dist_to_mtr_m = mtr_dist,
      stringsAsFactors = FALSE
    )
    
    # 4. PRINT THE SAVED DATA (Clean Console Output)
    message(sprintf(
      "   -> SAVED: Lat: %.4f | Lng: %.4f | Rating: %s | MTR: %.0fm",
      row_to_save$lat, 
      row_to_save$lng, 
      ifelse(is.na(row_to_save$rating), "N/A", row_to_save$rating),
      row_to_save$dist_to_mtr_m
    ))
    
    return(row_to_save)
  }
  
  message("   x Failed to retrieve details.")
  return(data.frame(lat=NA, lng=NA, rating=NA, price_level=NA, cuisine_tags=NA, dist_to_mtr_m=NA))
}

# 6. EXECUTION LOOP
district_list <- unique_requests %>% 
  group_split(dist_name) %>% 
  set_names(map_chr(., ~ .x$dist_name[1]))

for (d_name in names(district_list)) {
  current_df <- district_list[[d_name]]
  total_n <- nrow(current_df)
  
  message(paste("\n>>> PROCESSING:", d_name, "(", total_n, "entries ) <<<"))
  
  # Use possibly() to wrap our fixed function
  safe_get_coords <- possibly(get_coords, otherwise = data.frame(
    lat = NA, lng = NA, rating = NA, price_level = NA, 
    cuisine_tags = NA, dist_to_mtr_m = NA
  ))
  
  # Execute API calls
  results <- imap_dfr(current_df$search_query, ~safe_get_coords(.x, .y, total_n, d_name)) 
  
  # Combine and save
  combined_data <- bind_cols(current_df, results)
  safe_name <- gsub("/", "_", d_name)
  write_csv(combined_data, paste0("results_", safe_name, ".csv"))
  
  Sys.sleep(0.5) # Gentle throttle
}
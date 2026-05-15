# ==============================================================================
# SCRIPT 2: Cuisine Classification and Feature Engineering
# ==============================================================================
library(dplyr)
library(stringr)

# (Patterns list remains the same as your exhaustive version)
patterns <- list(
  Chinese_Gen      = "Kee|Wai|Lau|Chuen|Restaurant|House|Garden|Kitchen|Dim Sum|Cantonese|Seafood|Palace|Court|Pavilion|記|樓|村|粵|小菜|酒家|軒|坊|苑|廚|園|閣|廳|館|海鮮|酒樓|小聚",
  Chinese_Regional = "Sichuan|Shanghai|Hunan|Peking|Beijing|Chiu Chow|Shunde|Northeast|Xi'an|滬|川|京|湘|潮|順|陝|鼎|京川滬|老上海|湘菜|蜀",
  Hotpot_Claypot   = "Hotpot|Pot|Claypot|Boiler|Drunken|Shabu|火鍋|煲|打邊爐|雞煲|甂爐|涮|小肥|湊湊|海底撈",
  Noodle_Congee    = "Noodle|Congee|Rice|Mixian|Cart Noodle|Vermicelli|粥|麵|飯|米線|粉|車仔麵|撈麵|雲吞|腩|酸辣",
  Japanese         = "Sushi|Ramen|Izakaya|Genki|Don|Yakiniku|Shabu|Japanese|Oma|Toro|Sake|Wagyu|Katsu|Udon|Tempura|Teppanyaki|Omakase|Yakitori|Sashimi|壽司|拉麵|居酒屋|燒肉|丼|屋|旨|旬|麵|刺身|割烹|串燒",
  Korean           = "Korean|Bbq|Bibimbap|Chicken|Kimchi|Jjigae|Galbi|Pocha|Soju|K-BBQ|炸雞|韓|漢城|郊外|友利|家家|明洞",
  SE_Asian         = "Thai|Vietnam|Pho|Malay|Singapore|Laksa|Satay|Indones|Bali|Cambodia|Filipino|Lemongrass|越|泰|星馬|東南亞|金邊|曼谷|越式|泰國",
  South_Asian      = "Rajdoot|Zaitun|Nepal|Bangladesh|Priyoshaad|India|Pakistan|Momo|Timur|Tandoori|Jojo|Claypot|Bombay|Indian|Curry|Masala|Tandoori|Naan|Himalaya|Nepal|Pakistan|Desi|Lankan|Spice|咖哩|印|印度|尼泊爾|香料",
  Middle_East      = "Kebab|Turkish|Lebanese|Mediterranean|Middle East|Halal|Falafel|Hummus|Persian|Shisha|Moroccan|土耳其|中東|地中海",
  Western          = "Pizza|Burger|Pasta|Steak|Bistro|Grill|Italian|French|American|Spanish|Tapas|Mexican|Taco|German|British|Fine Dining|Oyster|西式|扒房|意粉|比薩|漢堡|生蠔",
  Bar_Pub          = "Bar|Pub|Club|Brewery|Taps|Lounge|Craft|Speakeasy|Wine|Cellar|Bistro|酒吧|酒屋|啤|威士忌",
  Cha_Chaan_Teng   = "Tea|Milk|Toast|Cha Chaan|Coffee|Cafe|Sutt|Bing|冰室|茶餐廳|餐室|冰廳|咖啡|多士|茶記",
  Fast_Food        = "Shake|Burger|Five|Fried|McD|KFC|Jollibee|Subway|Pizza Hut|Yoshinoya|Fairwood|Cafe de Coral|Maxim|TamJai|SamGor|Pepper Lunch|Mos|Popeyes|大快活|大家樂|譚仔|三哥|美心|吉野家",
  Street_Food      = "Snack|Fish Ball|Egg Waffle|Street|Bean Curd|Tofu|小食|魚蛋|雞蛋仔|豆腐花|串|煎釀|車仔",
  Bakery_Dessert   = "Cake|Dessert|Sweet|Bakery|Pastry|Bread|Sugar|Creamery|Gelato|Mochi|Donut|Waffle|Yogurt|Tiramisu|Matcha|糖水|餅店|西餅|麵包|蛋糕|甜品|湯丸|窩夫",
  Beverages        = "Milk Tea|Boba|Bubble|Tea|Juice|Smoothie|Starbucks|Pacific|HEYTEA|Gong Cha|Sharetea|Chicha|Silk|奶茶|茶飲|手搖|萃茶|珍珠",
  Healthy_Veg      = "Vegan|Vegetarian|Healthy|Salad|Green|Organic|Poke|Raw|Plant|Keto|素|素食|健康|沙律|齋|原素"
)

# ------------------------------------------------------------------------------
# STEP 1: APPLY CLASSIFICATION
# ------------------------------------------------------------------------------
df_price_complete <- df_price_complete %>%
  mutate(
    cuisine_type = case_when(
      # 1. Brands and specific chains first (to avoid generic overlap)
      str_detect(name, regex(patterns$Fast_Food, ignore_case = TRUE))      ~ "Fast Food",
      
      # 2. Specific Cuisines
      str_detect(name, regex(patterns$Japanese, ignore_case = TRUE))       ~ "Japanese",
      str_detect(name, regex(patterns$Korean, ignore_case = TRUE))         ~ "Korean",
      str_detect(name, regex(patterns$SE_Asian, ignore_case = TRUE))       ~ "South East Asian",
      str_detect(name, regex(patterns$South_Asian, ignore_case = TRUE))    ~ "South Asian",
      str_detect(name, regex(patterns$Middle_East, ignore_case = TRUE))    ~ "Middle Eastern",
      
      # 3. Local Specialty Categories
      str_detect(name, regex(patterns$Cha_Chaan_Teng, ignore_case = TRUE)) ~ "Cha Chaan Teng/Cafe",
      str_detect(name, regex(patterns$Street_Food, ignore_case = TRUE))    ~ "Street Food",
      str_detect(name, regex(patterns$Beverages, ignore_case = TRUE))      ~ "Beverages",
      str_detect(name, regex(patterns$Bakery_Dessert, ignore_case = TRUE)) ~ "Dessert/Bakery",
      
      # 4. Chinese Sub-types (grouped into one for the final column, or kept separate)
      str_detect(name, regex(paste(patterns$Chinese_Regional, 
                                   patterns$Hotpot_Claypot, 
                                   patterns$Noodle_Congee, 
                                   patterns$Chinese_Gen, sep="|"), ignore_case = TRUE)) ~ "Chinese",
      
      # 5. International & Lifestyle
      str_detect(name, regex(patterns$Western, ignore_case = TRUE))        ~ "Western",
      str_detect(name, regex(patterns$Bar_Pub, ignore_case = TRUE))        ~ "Bar/Pub",
      str_detect(name, regex(patterns$Healthy_Veg, ignore_case = TRUE))    ~ "Healthy/Veg",
      
      TRUE ~ "General/Other"
    ),
    # Feature Engineering
    name_len = nchar(name)
  ) 

# Check the results
print(table(df_price_complete$cuisine_type))

# ------------------------------------------------------------------------------
# STEP 2: EXPORT FINAL DATASET
# ------------------------------------------------------------------------------
write.csv(df_price_complete, "final_hk_restaurants_dataset.csv", row.names = FALSE)
message("--- Success! Final dataset saved as 'final_hk_restaurants_dataset.csv' ---")
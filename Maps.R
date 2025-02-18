

title: "Data Comparison"
author: "Yui Fujii"
date: "2025-1-21"


pacman::p_load(
  tidyverse,
  rio,                                     # to import data
  here,                                     # to locate files 
  readxl,
  dplyr,
  ggplot2,
  knitr,
  gt,
  scales,
  skimr,                                   # Provides summary statistics, Data type info, visual summaries
  sf,
  tigris,
  patchwork,
  moments,
  purrr,
  cowplot
)


# Define a named color palette for quintiles
quintile_colors <<- c("#BDD7E7", "#579DCF", "#1662A1", "#003B6B", "#DCDCDC")


# Load Data ---------------------------------------------------------------

 fp_str <- as.character(getwd())
 pos <- regexpr("3yr vs 1yr estimates", fp_str)
 fp.mod <- substr(fp_str, 1, pos + attr(pos, "match.length") - 1)

 cv3.fp <- paste0(fp.mod, "/Data/allcvd_20_22.xlsx", sep = "")
 str3.fp <- paste0(fp.mod, "/Data/allstr_20_22.xlsx", sep = "")
 ami3.fp <- paste0(fp.mod, "/Data/ami_20_22.xlsx", sep = "")
 cv1.fp <- paste0(fp.mod, "/Data/allcvd_22_medians.csv", sep = "")
 str1.fp <- paste0(fp.mod, "/Data/allstr_22_medians.csv", sep = "")
 ami1.fp <- paste0(fp.mod, "/Data/ami_22_medians.csv", sep = "")
 

 cv3 <- read_xlsx(cv3.fp, col_names = TRUE, trim_ws = FALSE)
 str3 <- read_xlsx(str3.fp, col_names = TRUE, trim_ws = FALSE)
 ami3<- read_xlsx(ami3.fp, col_names = TRUE, trim_ws = FALSE)

 cv1 <- read_csv(cv1.fp, col_names = TRUE, trim_ws = FALSE)
 str1 <- read_csv(str1.fp, col_names = TRUE, trim_ws = FALSE)
 ami1 <- read_csv(ami1.fp, col_names = TRUE, trim_ws = FALSE)

 cv3 <- cv3 %>% select(stcty_fips, sort(names(cv3)[-1]))
 str3 <- str3 %>% select(stcty_fips, sort(names(str3)[-1]))
 ami3 <- ami3 %>% select(stcty_fips, sort(names(ami3)[-1]))
 
 cv1 <- cv1 %>% select(stcty_fips, sort(names(cv1)[-1]))
 str1 <- str1 %>% select(stcty_fips, sort(names(str1)[-1]))
 ami1 <- ami1 %>% select(stcty_fips, sort(names(ami1)[-1]))

# Clean Data --------------------------------------------------------------
 
 modify_column_names <- function(data) {
   # Retain only columns starting with "all", "34up", "65up", or "35-64"
   valid_prefixes <- c("stcty_fips", "all", "35up", "65up", "35-64")
   pattern <- paste0("^(", paste(valid_prefixes, collapse = "|"), ")")
   data <- data[, grepl(pattern, names(data))]
   # Remove dashes from column names
   names(data) <- gsub("-", "", names(data))
   
   # Swap prefix and suffix in column names except for "stcty_fips"
   names(data) <- ifelse(
     names(data) == "stcty_fips",
     names(data), # Keep "stcty_fips" unchanged
     sub("^([^_]+)_(.*)", "\\2_\\1", names(data)) # Swap prefix and suffix for other columns
   )
   return(data)
 }
 
 # Apply to your dataframes
 cv1 <- modify_column_names(cv1)
 str1 <- modify_column_names(str1)
 ami1 <- modify_column_names(ami1)
 
 
 
 # Define the abbreviation mapping function
 replace_abbreviations <- function(name) {
   names_map <- c("aia" = "aian", "asn" = "asian", "blk" = "black",
                  "his" = "hispanic", "nhp" = "nhopi", "mor" = "multi",
                  "wht" = "white", "a" = "all")  
   
   # Apply the abbreviation replacements first
   for (abbreviation in names(names_map)) {
     name <- gsub(paste0("^", abbreviation, "_"), paste0(names_map[abbreviation], "_"), name)
   }
   name <- gsub("_a_", "_all_", name)
   return(name)
 }
 
 
 # Apply the function to your dataframes
 cv3 <- cv3 %>%
   rename_with(~ replace_abbreviations(tolower(.)), .cols = -stcty_fips) %>%
   mutate(across(everything(), ~ replace(., . == -1, NA)))
 
 str3 <- str3 %>%
   rename_with(~ replace_abbreviations(tolower(.)), .cols = -stcty_fips) %>%
   mutate(across(everything(), ~ replace(., . == -1, NA)))
 
 ami3 <- ami3 %>%
   rename_with(~ replace_abbreviations(tolower(.)), .cols = -stcty_fips) %>%
   mutate(across(everything(), ~ replace(., . == -1, NA)))
 
 
 
 # Merge Data --------------------------------------------------------------
 # Define the custom subtraction function
 custom_subtraction <- function(x, y) {
   if (is.na(x) & !is.na(y)) {
     return(9999)  # NA - x case
   } else if (!is.na(x) & is.na(y)) {
     return(0000)  # x - NA case
   } else if (is.na(x) & is.na(y)) {
     return(-1111)  # NA - NA case
   } else {
     return(x - y)  # Regular subtraction
   }
 }
 
 

# CV Difference -----------------------------------------------------------

  cv_joined <- cv1 %>%
    left_join(cv3, by = "stcty_fips", suffix = c("cv1", "cv3")) %>%
   select(stcty_fips, sort(names(.)[-1]))  # Sort all columns except stcty_fips
 
 # Perform the subtraction and create the diff columns
 cv_joined <- cv_joined %>%
   mutate(across(ends_with("cv1"), 
                 ~ mapply(custom_subtraction, ., pull(cv_joined, sub("cv1$", "cv3", cur_column()))),
                 .names = "{col}_diff"))
 
 cv_diff <- cv_joined %>%
   select(stcty_fips, ends_with("_diff")) %>%
   select(stcty_fips, sort(names(.)[-1])) %>%
   rename_with(~ gsub("cv1_diff", "", .), -stcty_fips) # Sort all columns except stcty_fips
 
 
 # STR Difference -----------------------------------------------------------
 
 str_joined <- str1 %>%
   left_join(str3, by = "stcty_fips", suffix = c("str1", "str3")) %>%
   select(stcty_fips, sort(names(.)[-1]))  # Sort all columns except stcty_fips
 
 # Perform the subtraction and create the diff columns
 str_joined <- str_joined %>%
   mutate(across(ends_with("str1"), 
                 ~ mapply(custom_subtraction, ., pull(str_joined, sub("str1$", "str3", cur_column()))),
                 .names = "{col}_diff"))
 
 str_diff <- str_joined %>%
   select(stcty_fips, ends_with("_diff")) %>%
   select(stcty_fips, sort(names(.)[-1])) %>%
   rename_with(~ gsub("str1_diff", "", .), -stcty_fips)  # Sort all columns except stcty_fips
 
 # AMI Difference -----------------------------------------------------------
 
 ami_joined <- ami1 %>%
   left_join(ami3, by = "stcty_fips", suffix = c("ami1", "ami3")) %>%
   select(stcty_fips, sort(names(.)[-1]))  # Sort all columns except stcty_fips
 
 # Perform the subtraction and create the diff columns
 ami_joined <- ami_joined %>%
   mutate(across(ends_with("ami1"), 
                 ~ mapply(custom_subtraction, ., pull(ami_joined, sub("ami1$", "ami3", cur_column()))),
                 .names = "{col}_diff"))
 
 ami_diff <- ami_joined %>%
   select(stcty_fips, ends_with("_diff")) %>%
   select(stcty_fips, sort(names(.)[-1])) %>%
   rename_with(~ gsub("ami1_diff", "", .), -stcty_fips)  # Sort all columns except stcty_fips
 




 # Step 2: Create a column indicating if the specific variable is NA
 suppressed.tab <- function(data) {
   data <- data %>%
     mutate(across(-stcty_fips, ~ ifelse(is.na(.), ., -2)))
   return(data)
 }
 
 sup.cv3 <- suppressed.tab(cv3)
 sup.str3 <- suppressed.tab(str3)
 sup.ami3 <- suppressed.tab(ami3)
 sup.cv1 <- suppressed.tab(cv1)
 sup.str1 <- suppressed.tab(str1)
 sup.ami1 <- suppressed.tab(ami1)
 
 write.csv(cv_diff, file = "cv_diff.csv")
 write.csv(str_diff, file = "str_diff.csv")
 write.csv(ami_diff, file = "ami_diff.csv")
 
 write.csv(sup.cv3, file = "sup.cv3.csv")
 write.csv(sup.str3, file = "sup.str3.csv")
 write.csv(sup.ami3, file = "sup.ami3.csv")
 write.csv(sup.cv1, file = "sup.cv1.csv")
 write.csv(sup.str1, file = "sup.str1.csv")
 write.csv(sup.ami1, file = "sup.ami1.csv")

 # Default breaks and labels using quantiles
 maj.cat.limits <- quantile(map_data[[var]], probs = seq(0, 1, by = 0.25), na.rm = TRUE)
 
 cut.labels <<- c(
   paste(maj.cat.limits[1], maj.cat.limits[2],
         sep = " - "),
   paste(maj.cat.limits[2]+0.1, maj.cat.limits[3],
         sep = " - "),
   paste(maj.cat.limits[3]+0.1, maj.cat.limits[4],
         sep = " - "),
   paste(maj.cat.limits[4]+0.1, maj.cat.limits[5],
         sep = " - "),
   paste(maj.cat.limits[5]+0.1, maj.cat.limits[6],
         sep = " - "))
 
 # Add categorized variable using quantiles
 map_data <- map_data %>%
   mutate(
     quintile = cut(
       as.numeric(map_data[[var]]),
       breaks = maj.cat.limits,
       include.lowest = TRUE,
       right = FALSE
     )
   )

 path = "Map"
 data = sup.cv1
 var = "all_all_3564"
 
 sup.map("cv", sup.cv1, "all_all_3564", path) 
 
 sup.map <- function (outcome, data, var, path) {
   
   # Step 1: Load spatial shapefiles
   options(tigris_use_cache = TRUE)
   county_shapes <- counties(cb = TRUE, year = 2020, class = "sf")
   state_shapes <- states(cb = TRUE, year = 2020, class = "sf")    
   
   # Step 3: Perform a left join with the county shapefile
   map_data <- county_shapes %>%
     left_join(data, by = c("GEOID" = "stcty_fips")) %>%
     mutate(variable = .data[[var]])  
   
   map_data <- map_data %>%
     mutate(variable_cat = ifelse(is.na(variable), "NA", "Not NA"))
   
   main_map <- ggplot(data = map_data) +
     geom_sf(aes(fill = variable_cat), color = "grey", size = 0) +
     geom_sf(data = state_shapes, fill = NA, color = "black", size = 0.3) +
     scale_fill_manual(
       values = c("NA" = "#DCDCDC", "Not NA" = "white"),
       name = "Variable"
     ) +
     coord_sf(
       crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
       xlim = c(-2500000, 2500000),  # Adjusted range for contiguous US
       ylim = c(-2000000, 1500000),  # Adjusted range for contiguous US
       expand = FALSE                # Prevent extra padding
     ) +
     theme_void() +
     theme(
       plot.title = element_text(size = 10, face = "bold", , hjust = 0.7),
       plot.title.position = "panel",
       legend.position = "right",
       legend.text = element_text(size = 8),            # Adjust text size in legend
       legend.title = element_text(size = 9, face = "bold"),  # Adjust legend title size
       legend.key.size = unit(0.5, "cm")               # Adjust the size of legend keys
     )
   
   # Alaska map
   alaska_map <- ggplot(data = map_data %>% filter(STATEFP == "02")) +
     geom_sf(aes(fill = variable_cat), color = NA, size = 0.01) +
     geom_sf(data = state_shapes, fill = NA, color = "black", size = 0, lwd = 0.3) +
     scale_fill_manual(
       values = c("NA" = "#DCDCDC", "Not NA" = "white"),
       name = "Variable", guide = "none"
     ) +
     coord_sf(
       crs = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
       xlim = c(-2000000, 1550000),  # Adjusted range for Alaska in Albers Equal Area
       ylim = c(50, 2500000),    # Adjusted range for Alaska in Albers Equal Area
       expand = FALSE
     ) +
     theme_void() +
     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
   
   # Hawaii map
   hawaii_map <- ggplot(data = map_data %>% filter(STATEFP == "15")) +
     geom_sf(aes(fill = variable_cat), color = "grey", size = 0.01) +
     geom_sf(data = state_shapes, fill = NA, color = "black", size = 0, lwd = 0.3) +
     scale_fill_manual(
       values = c("NA" = "#DCDCDC", "Not NA" = "white"),
       name = "Variable", guide = "none"
     ) +
     coord_sf(
       xlim = c(-161, -154),  # Adjust to Hawaii's longitude range
       ylim = c(18, 23),      # Adjust to Hawaii's latitude range
       expand = FALSE
     ) +
     theme_void() +
     theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
   
   # Combine maps using patchwork
   final_map <- main_map +
     inset_element(alaska_map, left = 0.05, bottom = 0.05, right = 0.25, top = 0.25) + # Alaska in the bottom-left corner
     inset_element(hawaii_map, left = 0.26, bottom = 0.05, right = 0.46, top = 0.25)  # Hawaii next to Alaska
   
   
   # Display final map
   final_map <- main_map +
     inset_element(alaska_map, left = 0.0, bottom = -0.3, right = 0.25, top = 0.55) +
     theme(
       plot.background = element_rect(fill = "white", color = "black", size = .07)
     ) +
     inset_element(hawaii_map, left = 0.3, bottom = -0.09, right = 0.45, top = 0.25) +
     theme(
       plot.background = element_rect(fill = "white", color = "black", size = .07)
     )
   
   
   desired_width <- 2560 # Desired width in pixels
   desired_height <- 1862 # Desired height in pixels
   
   #path <- "Map"
   
   # Save the plot using ggsave with high DPI to ensure high quality
   #output_path <- paste0(path, "/", var, "_", outcome, ".jpeg")
   #ggsave(output_path, device = "jpeg", plot = final_map, width = desired_width / 300, height = desired_height / 300, units = "in", dpi = 300)
   print(final_map)
 }
 
# Filter Diff ----------------------------------------------------------

map <- function (outcome, data, var, path) {
  
  quintile_colors <- c("#D32F2F", "#FF7043", "#1976D2", "#0D47A1")
   
   data.only.na <- data %>%
    mutate(across(-"stcty_fips", ~ ifelse(. %in% c(9999, 0000, -1111), NA, .)))
    
    # Filter rows for specific cases
   NaX <- data %>%
     filter(as.numeric(.data[[var]]) == 9999)
    
   XNa <- data %>%
     filter(as.numeric(.data[[var]]) == 0000)
    
   NaNa <- data %>%
     filter(as.numeric(.data[[var]]) == -1111)
    
    
    # Step 1: Load spatial shapefiles
    options(tigris_use_cache = TRUE)
    county_shapes <- counties(cb = TRUE, year = 2020, class = "sf")
    state_shapes <- states(cb = TRUE, year = 2020, class = "sf")    
    
    # Step 3: Perform a left join with the county shapefile
    map_data <- county_shapes %>%
      left_join(data.only.na, by = c("GEOID" = "stcty_fips"))
    NaX <- county_shapes %>%
      inner_join(NaX, by = c("GEOID" = "stcty_fips"))
    XNa <- county_shapes %>%
      inner_join(XNa, by = c("GEOID" = "stcty_fips"))
    NaNa <- county_shapes %>%
      inner_join(NaNa, by = c("GEOID" = "stcty_fips"))
  
  
    # Define custom breakpoints
    # Define custom breakpoints
    manual_breaks <- c(-Inf, -20, 0, 0.001, 20, Inf)
    
    # Define custom labels for the categories
    cut_labels <- c("<= -20", "-20 to 0", "0", "0 to 20", "> 20")
    
    # Apply categorization using custom breakpoints and assign labels directly
    map_data <- map_data %>%
      mutate(
        quintile = case_when(
          .data[[var]] <= -20 ~ "<= -20",
          .data[[var]] > -20 & .data[[var]] <= 0 ~ "-20 to 0",
          .data[[var]] == 0 ~ "0",
          .data[[var]] > 0 & .data[[var]] <= 20 ~ "0 to 20",
          .data[[var]] > 20 ~ "> 20",
          TRUE ~ NA_character_  # Handling NAs, if necessary
        )
      )
  
  
  main_map <- ggplot(data = map_data) +
    geom_sf(aes(fill = quintile), color = NA, size = 0) +
    geom_sf(data = NaX, fill = "#66B366", size = 0) +
    geom_sf(data = XNa, fill = "#7E57C2", size = 0) +
    geom_sf(data = NaNa, fill = "#DCDCDC", size = 0) +
    geom_sf(data = state_shapes, fill = NA, color = "black", size = 0, lwd = 0.3) +
    scale_fill_manual(values = quintile_colors, name = var) +
    coord_sf(
      crs = "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      xlim = c(-2500000, 2500000),  # Adjusted range for contiguous US
      ylim = c(-2000000, 1500000),  # Adjusted range for contiguous US
      expand = FALSE                # Prevent extra padding
    ) +
    labs(
      #title = tit
      ) +
    theme_void() +  # Remove panel background, gridlines, and borders
    theme(
      plot.title = element_text(size = 10, face = "bold", , hjust = 0.7),
      plot.title.position = "panel",
      legend.position = "right",
      legend.text = element_text(size = 8),            # Adjust text size in legend
      legend.title = element_text(size = 9, face = "bold"),  # Adjust legend title size
      legend.key.size = unit(0.5, "cm")               # Adjust the size of legend keys
    )
  
  # Alaska map
  alaska_map <- ggplot(data = map_data %>% filter(STATEFP == "02")) +
    geom_sf(data = NaX, fill = "#66B366", size = 0) +
    geom_sf(data = XNa, fill = "#7E57C2", size = 0) +
    geom_sf(data = NaNa, fill = "#DCDCDC", size = 0) +
    geom_sf(aes(fill = quintile), color = NA, size = 0.01) +
    geom_sf(data = state_shapes, fill = NA, color = "black", size = 0, lwd = 0.3) +
    scale_fill_manual(values = quintile_colors, guide = "none") +
    coord_sf(
      crs = "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs",
      xlim = c(-2000000, 1550000),  # Adjusted range for Alaska in Albers Equal Area
      ylim = c(50, 2500000),    # Adjusted range for Alaska in Albers Equal Area
      expand = FALSE
    ) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
  
  # Hawaii map
  hawaii_map <- ggplot(data = map_data %>% filter(STATEFP == "15")) +
    geom_sf(aes(fill = quintile), color = "grey", size = 0.01) +
    geom_sf(data = NaX, fill = "#66B366", size = 0) +
    geom_sf(data = XNa, fill = "#7E57C2", size = 0) +
    geom_sf(data = NaNa, fill = "#DCDCDC", size = 0) +
    geom_sf(data = state_shapes, fill = NA, color = "black", size = 0, lwd = 0.3) +
    scale_fill_manual(values = quintile_colors, guide = "none") +
    coord_sf(
      xlim = c(-161, -154),  # Adjust to Hawaii's longitude range
      ylim = c(18, 23),      # Adjust to Hawaii's latitude range
      expand = FALSE
    ) +
    theme_void() +
    theme(panel.border = element_rect(color = "black", fill = NA, size = 0.5))
  
  # Combine maps using patchwork
  final_map <- main_map +
    inset_element(alaska_map, left = 0.05, bottom = 0.05, right = 0.25, top = 0.25) + # Alaska in the bottom-left corner
    inset_element(hawaii_map, left = 0.26, bottom = 0.05, right = 0.46, top = 0.25)  # Hawaii next to Alaska
  
  
  # Display final map
  final_map <- main_map +
    inset_element(alaska_map, left = 0.0, bottom = -0.3, right = 0.25, top = 0.55) +
    theme(
      plot.background = element_rect(fill = "white", color = "black", size = .07)
    ) +
    inset_element(hawaii_map, left = 0.3, bottom = -0.09, right = 0.45, top = 0.25) +
    theme(
      plot.background = element_rect(fill = "white", color = "black", size = .07)
    )
  

  desired_width <- 2560 # Desired width in pixels
  desired_height <- 1862 # Desired height in pixels
  
  #path <- "Map"
  
  # Save the plot using ggsave with high DPI to ensure high quality
  #output_path <- paste0(path, "/", var, "_", outcome, ".jpeg")
  #ggsave(output_path, device = "jpeg", plot = final_map, width = desired_width / 300, height = desired_height / 300, units = "in", dpi = 300)
  print(final_map)
  }


  
  path = "Map"
  data = cv_diff
  var = "all_all_3564"
  

# CV ----------------------------------------------------------------------

  
  map("cv", cv_diff, "all_all_3564", "Map")
  map("cv", cv_diff, "all_all_35up", "Map")
  map("cv", cv_diff, "all_all_65up", "Map")
  map("cv", cv_diff, "all_f_3564", "Map")
  map("cv", cv_diff, "all_f_35up", "Map")
  map("cv", cv_diff, "all_f_65up", "Map")
  map("cv", cv_diff, "all_m_3564", "Map")
  map("cv", cv_diff, "all_m_35up", "Map")
  map("cv", cv_diff, "all_m_65up", "Map")
  
  map("cv",cv_diff, "white_all_3564", "Map")
  map("cv",cv_diff, "white_all_35up", "Map")
  map("cv",cv_diff, "white_all_65up", "Map")
  map("cv",cv_diff, "white_f_3564", "Map")
  map("cv",cv_diff, "white_f_35up", "Map")
  map("cv",cv_diff, "white_f_65up", "Map")
  map("cv",cv_diff, "white_m_3564", "Map")
  map("cv",cv_diff, "white_m_35up", "Map")
  map("cv",cv_diff, "white_m_65up", "Map")
  
  map("cv",cv_diff, "black_all_3564", "Map")
  map("cv",cv_diff, "black_all_35up", "Map")
  map("cv",cv_diff, "black_all_65up", "Map")
  map("cv",cv_diff, "black_f_3564", "Map")
  map("cv",cv_diff, "black_f_35up", "Map")
  map("cv",cv_diff, "black_f_65up", "Map")
  map("cv",cv_diff, "black_m_3564", "Map")
  map("cv",cv_diff, "black_m_35up", "Map")
  map("cv",cv_diff, "black_m_65up", "Map")
  
  map("cv",cv_diff, "asian_all_3564", "Map")
  map("cv",cv_diff, "asian_all_35up", "Map")
  map("cv",cv_diff, "asian_all_65up", "Map")
  map("cv",cv_diff, "asian_f_3564", "Map")
  map("cv",cv_diff, "asian_f_35up", "Map")
  map("cv",cv_diff, "asian_f_65up", "Map")
  map("cv",cv_diff, "asian_m_3564", "Map")
  map("cv",cv_diff, "asian_m_35up", "Map")
  map("cv",cv_diff, "asian_m_65up", "Map")
  
  map("cv",cv_diff, "aian_all_3564", "Map")
  map("cv",cv_diff, "aian_all_35up", "Map")
  map("cv",cv_diff, "aian_all_65up", "Map")
  map("cv",cv_diff, "aian_f_3564", "Map")
  map("cv",cv_diff, "aian_f_35up", "Map")
  map("cv",cv_diff, "aian_f_65up", "Map")
  map("cv",cv_diff, "aian_m_3564", "Map")
  map("cv",cv_diff, "aian_m_35up", "Map")
  map("cv",cv_diff, "aian_m_65up", "Map")
  
  map("cv",cv_diff, "nhopi_all_3564", "Map")
  map("cv",cv_diff, "nhopi_all_35up", "Map")
  map("cv",cv_diff, "nhopi_all_65up", "Map")
  map("cv",cv_diff, "nhopi_f_3564", "Map")
  map("cv",cv_diff, "nhopi_f_35up", "Map")
  map("cv",cv_diff, "nhopi_f_65up", "Map")
  map("cv",cv_diff, "nhopi_m_3564", "Map")
  map("cv",cv_diff, "nhopi_m_35up", "Map")
  map("cv",cv_diff, "nhopi_m_65up", "Map")

  map("cv",cv_diff, "multi_all_3564", "Map")
  map("cv",cv_diff, "multi_all_35up", "Map")
  map("cv",cv_diff, "multi_all_65up", "Map")
  map("cv",cv_diff, "multi_f_3564", "Map")
  map("cv",cv_diff, "multi_f_35up", "Map")
  map("cv",cv_diff, "multi_f_65up", "Map")
  map("cv",cv_diff, "multi_m_3564", "Map")
  map("cv",cv_diff, "multi_m_35up", "Map")
  map("cv",cv_diff, "multi_m_65up", "Map") 
  
  

# Str ---------------------------------------------------------------------

  
  
  map("str", str_diff, "all_all_3564", "Map")
  map("str", str_diff, "all_all_35up", "Map")
  map("str", str_diff, "all_all_65up", "Map")
  map("str", str_diff, "all_f_3564", "Map")
  map("str", str_diff, "all_f_35up", "Map")
  map("str", str_diff, "all_f_65up", "Map")
  map("str", str_diff, "all_m_3564", "Map")
  map("str", str_diff, "all_m_35up", "Map")
  map("str", str_diff, "all_m_65up", "Map")
  
  map("str",str_diff, "white_all_3564", "Map")
  map("str",str_diff, "white_all_35up", "Map")
  map("str",str_diff, "white_all_65up", "Map")
  map("str",str_diff, "white_f_3564", "Map")
  map("str",str_diff, "white_f_35up", "Map")
  map("str",str_diff, "white_f_65up", "Map")
  map("str",str_diff, "white_m_3564", "Map")
  map("str",str_diff, "white_m_35up", "Map")
  map("str",str_diff, "white_m_65up", "Map")
  
  map("str",str_diff, "black_all_3564", "Map")
  map("str",str_diff, "black_all_35up", "Map")
  map("str",str_diff, "black_all_65up", "Map")
  map("str",str_diff, "black_f_3564", "Map")
  map("str",str_diff, "black_f_35up", "Map")
  map("str",str_diff, "black_f_65up", "Map")
  map("str",str_diff, "black_m_3564", "Map")
  map("str",str_diff, "black_m_35up", "Map")
  map("str",str_diff, "black_m_65up", "Map")
  
  map("str",str_diff, "asian_all_3564", "Map")
  map("str",str_diff, "asian_all_35up", "Map")
  map("str",str_diff, "asian_all_65up", "Map")
  map("str",str_diff, "asian_f_3564", "Map")
  map("str",str_diff, "asian_f_35up", "Map")
  map("str",str_diff, "asian_f_65up", "Map")
  map("str",str_diff, "asian_m_3564", "Map")
  map("str",str_diff, "asian_m_35up", "Map")
  map("str",str_diff, "asian_m_65up", "Map")
  
  map("str",str_diff, "aian_all_3564", "Map")
  map("str",str_diff, "aian_all_35up", "Map")
  map("str",str_diff, "aian_all_65up", "Map")
  map("str",str_diff, "aian_f_3564", "Map")
  map("str",str_diff, "aian_f_35up", "Map")
  map("str",str_diff, "aian_f_65up", "Map")
  map("str",str_diff, "aian_m_3564", "Map")
  map("str",str_diff, "aian_m_35up", "Map")
  map("str",str_diff, "aian_m_65up", "Map")
  
  map("str",str_diff, "nhopi_all_3564", "Map")
  map("str",str_diff, "nhopi_all_35up", "Map")
  map("str",str_diff, "nhopi_all_65up", "Map")
  map("str",str_diff, "nhopi_f_3564", "Map")
  map("str",str_diff, "nhopi_f_35up", "Map")
  map("str",str_diff, "nhopi_f_65up", "Map")
  map("str",str_diff, "nhopi_m_3564", "Map")
  map("str",str_diff, "nhopi_m_35up", "Map")
  map("str",str_diff, "nhopi_m_65up", "Map")
  
  map("str",str_diff, "multi_all_3564", "Map")
  map("str",str_diff, "multi_all_35up", "Map")
  map("str",str_diff, "multi_all_65up", "Map")
  map("str",str_diff, "multi_f_3564", "Map")
  map("str",str_diff, "multi_f_35up", "Map")
  map("str",str_diff, "multi_f_65up", "Map")
  map("str",str_diff, "multi_m_3564", "Map")
  map("str",str_diff, "multi_m_35up", "Map")
  map("str",str_diff, "multi_m_65up", "Map")
  
  
  

# AMI ---------------------------------------------------------------------

  
  map("ami", ami_diff, "all_all_3564", "Map")
  map("ami", ami_diff, "all_all_35up", "Map")
  map("ami", ami_diff, "all_all_65up", "Map")
  map("ami", ami_diff, "all_f_3564", "Map")
  map("ami", ami_diff, "all_f_35up", "Map")
  map("ami", ami_diff, "all_f_65up", "Map")
  map("ami", ami_diff, "all_m_3564", "Map")
  map("ami", ami_diff, "all_m_35up", "Map")
  map("ami", ami_diff, "all_m_65up", "Map")
  
  map("ami",ami_diff, "white_all_3564", "Map")
  map("ami",ami_diff, "white_all_35up", "Map")
  map("ami",ami_diff, "white_all_65up", "Map")
  map("ami",ami_diff, "white_f_3564", "Map")
  map("ami",ami_diff, "white_f_35up", "Map")
  map("ami",ami_diff, "white_f_65up", "Map")
  map("ami",ami_diff, "white_m_3564", "Map")
  map("ami",ami_diff, "white_m_35up", "Map")
  map("ami",ami_diff, "white_m_65up", "Map")
  
  map("ami",ami_diff, "black_all_3564", "Map")
  map("ami",ami_diff, "black_all_35up", "Map")
  map("ami",str_diff, "black_all_65up", "Map")
  map("ami",ami_diff, "black_f_3564", "Map")
  map("ami",ami_diff, "black_f_35up", "Map")
  map("ami",ami_diff, "black_f_65up", "Map")
  map("ami",ami_diff, "black_m_3564", "Map")
  map("ami",ami_diff, "black_m_35up", "Map")
  map("ami",ami_diff, "black_m_65up", "Map")
  
  map("ami",ami_diff, "asian_all_3564", "Map")
  map("ami",ami_diff, "asian_all_35up", "Map")
  map("ami",ami_diff, "asian_all_65up", "Map")
  map("ami",ami_diff, "asian_f_3564", "Map")
  map("ami",ami_diff, "asian_f_35up", "Map")
  map("ami",ami_diff, "asian_f_65up", "Map")
  map("ami",ami_diff, "asian_m_3564", "Map")
  map("ami",ami_diff, "asian_m_35up", "Map")
  map("ami",ami_diff, "asian_m_65up", "Map")
  
  map("ami",ami_diff, "aian_all_3564", "Map")
  map("ami",ami_diff, "aian_all_35up", "Map")
  map("ami",ami_diff, "aian_all_65up", "Map")
  map("ami",ami_diff, "aian_f_3564", "Map")
  map("ami",ami_diff, "aian_f_35up", "Map")
  map("ami",ami_diff, "aian_f_65up", "Map")
  map("ami",ami_diff, "aian_m_3564", "Map")
  map("ami",ami_diff, "aian_m_35up", "Map")
  map("ami",ami_diff, "aian_m_65up", "Map")
  
  map("ami",ami_diff, "nhopi_all_3564", "Map")
  map("ami",ami_diff, "nhopi_all_35up", "Map")
  map("ami",ami_diff, "nhopi_all_65up", "Map")
  map("ami",ami_diff, "nhopi_f_3564", "Map")
  map("ami",ami_diff, "nhopi_f_35up", "Map")
  map("ami",ami_diff, "nhopi_f_65up", "Map")
  map("ami",ami_diff, "nhopi_m_3564", "Map")
  map("ami",ami_diff, "nhopi_m_35up", "Map")
  map("ami",ami_diff, "nhopi_m_65up", "Map")
  
  map("ami",ami_diff, "multi_all_3564", "Map")
  map("ami",ami_diff, "multi_all_35up", "Map")
  map("ami",ami_diff, "multi_all_65up", "Map")
  map("ami",ami_diff, "multi_f_3564", "Map")
  map("ami",ami_diff, "multi_f_35up", "Map")
  map("ami",ami_diff, "multi_f_65up", "Map")
  map("ami",ami_diff, "multi_m_3564", "Map")
  map("ami",ami_diff, "multi_m_35up", "Map")
  map("ami",ami_diff, "multi_m_65up", "Map")
  
  
  

  datasets <- list(cv_diff = cv_diff, str_diff = str_diff, ami_diff = ami_diff)  # Replace with your datasets
  # Automatically extract variable names from the first dataset
  vars <- setdiff(colnames(datasets[[1]]), "stcty_fips")  # Exclude stcty_fips from the list of variables
  
  # Loop through each dataset and each variable dynamically
  for (i in 1:length(datasets)) {
    # Extract the dataset name dynamically
    dataset_name <- names(datasets)[i]  # Get the name of the current dataset (e.g., "cv3")
    
    for (var in vars) {
      # Create a path with dataset name, variable, and iteration number
      path <- paste0("map_", dataset_name, "_", var, "_", i, ".png")  # Include dataset name in file path
      
      # Run the map function with each dataset, variable, and path
      map(datasets[[i]], var, path)
    }
  }
  

  
  
  
  sup.cv3.sp <- spatial.tab(sup.cv3)
  sup.str3.sp <- spatial.tab(sup.str3)
  sup.ami3.sp <- spatial.tab(sup.ami3)
  
  sup.cv1.sp <- spatial.tab(sup.cv1)
  sup.str1.sp <- spatial.tab(sup.str1)
  sup.ami1.sp <- spatial.tab(sup.ami1)
  
  diff.cv.sp <- spatial.tab(ami_diff)
  diff.str.sp <- spatial.tab(ami_diff)
  diff.ami.sp <- spatial.tab(ami_diff)
  
  
  
  
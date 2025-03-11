# Load necessary libraries
library(tidyverse)

if (!require(janitor)) install.packages("janitor")
library(janitor)

# Import the dataset with the specific file path
file_path <- "C:/Users/ASUSCenter/Desktop/crimedata.csv"
crime_data <- read.csv(file_path, na.strings = c("", "NA", "?"))

# Basic information about the dataset
cat("Original dataset dimensions:", dim(crime_data), "\n")

# Clean column names - ensure consistent formatting
crime_data_clean <- crime_data %>%
  clean_names()

# Examine data structure
str(crime_data_clean)

# Check for missing values in each column
missing_values <- colSums(is.na(crime_data_clean))
print("Columns with missing values:")
print(missing_values[missing_values > 0])

# Handle missing values appropriately based on column type
crime_data_clean <- crime_data_clean %>%
  # For demographic percentages
  mutate(across(where(~is.numeric(.) && 
                        (grepl("pct", names(.)) || 
                           grepl("race", names(.)) || 
                           grepl("age", names(.))), 
                      ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
         
  # In a separate step for counts
  crime_data_clean <- crime_data_clean %>%
  # For counts
  mutate(across(where(~is.numeric(.) && 
                                 (grepl("population", names(.)) || 
                                    grepl("urban", names(.)) || 
                                    grepl("income", names(.)) || 
                                    grepl("cap", names(.)) || 
                                    grepl("pov", names(.)) || 
                                    grepl("immig", names(.)) || 
                                    grepl("shelter", names(.)) || 
                                    grepl("street", names(.)) || 
                                    grepl("drug", names(.))), 
                               ~ifelse(is.na(.), median(., na.rm = TRUE), .)))
  # Set logical bounds for percentage variables (0-100)
  crime_data_clean <- crime_data_clean %>%
  mutate(across(contains("pct"), ~ifelse(. > 100, 100, ifelse(. < 0, 0, .))))

  # Create a function to detect outliers using IQR method
  detect_outliers <- function(x) {
  if (!is.numeric(x) || all(is.na(x))) return(rep(FALSE, length(x)))
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  lower_bound <- q1 - 1.5 * iqr
  upper_bound <- q3 + 1.5 * iqr
  return(x < lower_bound | x > upper_bound)
}

  # Define crime variables using the exact column names from your data
  crime_vars <- c("murders", "murdPerPop", "rapes", "rapesPerPop",
                  "robberies", "robbbPerPop", "assaults", "assaultPerPop",
                  "burglaries", "burglPerPop", "larcenies", "larcPerPop",
                  "autoTheft", "autoTheftPerPop", "arsons", "arsonsPerPop",
                  "ViolentCrimesPerPop", "nonViolPerPop")
  
  # Check which of these columns actually exist in the data
  existing_crime_vars <- intersect(crime_vars, names(crime_data_clean))
  print("Existing crime variables:")
  print(existing_crime_vars)
  
  # Now use only the existing variables for outlier detection
  if(length(existing_crime_vars) > 0) {
    outlier_counts <- sapply(crime_data_clean[existing_crime_vars], function(x) sum(detect_outliers(x), na.rm = TRUE))
    print("Number of outliers in crime variables:")
    print(outlier_counts)
    
    # Create a flag for records with extreme outliers
    crime_data_clean <- crime_data_clean %>%
      mutate(has_outliers = rowSums(across(all_of(existing_crime_vars), detect_outliers)) > 0)
  } else {
    print("No matching crime variables found in the dataset.")
    crime_data_clean$has_outliers <- FALSE
  }

# Check for duplicate records based on community identifiers
duplicates <- crime_data_clean %>%
  group_by(community_name, state, county_code, community_code) %>%
  filter(n() > 1) %>%
  ungroup()

cat("Number of duplicate community records:", nrow(duplicates), "\n")

# Remove duplicates if they exist
if(nrow(duplicates) > 0) {
  crime_data_clean <- crime_data_clean %>%
    distinct(community_name, state, county_code, community_code, .keep_all = TRUE)
}

# Create some derived variables that might be useful for analysis
crime_data_clean <- crime_data_clean %>%
  mutate(
    # Total crime rate per population
    total_crime_per_pop = violent_crimes_per_pop + non_viol_per_pop,
    
    # Proportion of violent vs non-violent crime
    pct_violent_crime = violent_crimes_per_pop / (violent_crimes_per_pop + non_viol_per_pop) * 100,
    
    # Socioeconomic indicators
    income_inequality = white_per_cap / black_per_cap,
    
    # Population density category
    pop_density_cat = case_when(
      pop_dens < 1000 ~ "Low",
      pop_dens < 5000 ~ "Medium",
      TRUE ~ "High"
    )
  )

# Final data summary
summary_stats <- summary(crime_data_clean[c("population", "violent_crimes_per_pop", "non_viol_per_pop")])
cat("\nSummary statistics after cleaning:\n")
print(summary_stats)

# Save the cleaned data to the same desktop location
output_path <- "C:/Users/ASUSCenter/Desktop/crimedata_cleaned.csv"
write.csv(crime_data_clean, output_path, row.names = FALSE)

# Report cleaning summary
cat("\nData cleaning complete!\n")
cat("Original rows:", nrow(crime_data), "\n")
cat("Cleaned rows:", nrow(crime_data_clean), "\n")
cat("Number of records with outliers:", sum(crime_data_clean$has_outliers), "\n")
cat("Cleaned dataset saved to:", output_path, "\n")


###First Plot###

# First, check if column names have been cleaned to snake_case
if("violent_crimes_per_pop" %in% names(crime_data_clean)) {
  # If column names are in snake_case format
  high_crime_communities <- crime_data_clean %>%
    arrange(desc(violent_crimes_per_pop)) %>%
    head(10) %>%
    select(community_name, violent_crimes_per_pop)
} else {
  # If column names are in the original format (camelCase or PascalCase)
  # First, check what column names are available
  print(names(crime_data_clean))
  
  # Then create a new data frame with the appropriate columns
  # Assuming original column names are ViolentCrimesPerPop and communityName
  high_crime_communities <- crime_data_clean %>%
    arrange(desc(ViolentCrimesPerPop)) %>%
    head(10) %>%
    select(communityName, ViolentCrimesPerPop)
  
  # Rename columns to match the rest of the code
  names(high_crime_communities) <- c("community_name", "violent_crimes_per_pop")
}

# First, modify the community names to remove "city" suffix
high_crime_communities <- high_crime_communities %>%
  mutate(community_name = gsub("city$", "", community_name, ignore.case = TRUE))

# Create the plot with the updated labels
high_crime_plot <- ggplot(high_crime_communities, 
                          aes(x = reorder(community_name, violent_crimes_per_pop), 
                              y = violent_crimes_per_pop)) +
  geom_bar(stat = "identity", fill = "firebrick", width = 0.7) +
  coord_flip() +  # Flip coordinates for horizontal bars
  theme_minimal() +
  labs(title = "Top 10 Communities with Highest Violent Crime Rates",
       x = "Community",
       y = "Violent Crimes per Population (2018)") +  # Updated y-axis label
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", color = "black"),
    panel.grid.major.y = element_blank(),   # Remove horizontal grid lines
    panel.background = element_rect(fill = "white"),  # White background
    plot.background = element_rect(fill = "white", color = NA),  # White plot background
    axis.text = element_text(color = "black"),  # Black axis text
    axis.title = element_text(color = "black")  # Black axis titles
  )

# Save the plot
ggsave("PS6a_Karbalaei.png", high_crime_plot, width = 10, height = 6, dpi = 300, bg = "white")
print("High crime communities plot saved as PS6a_Karbalaei.png")

###Second Plot###

# Get 5 communities with highest violent crime rates
high_crime_communities <- crime_data_clean %>%
  arrange(desc(violent_crimes_per_pop)) %>%
  head(5) %>%
  mutate(crime_level = "High Crime",
         community_name = gsub("city$", "", community_name, ignore.case = TRUE))

# Get 5 communities with lowest violent crime rates (excluding those with rate = 0)
low_crime_communities <- crime_data_clean %>%
  filter(violent_crimes_per_pop > 0) %>%  # Exclude communities with no crime reported
  arrange(violent_crimes_per_pop) %>%
  head(5) %>%
  mutate(crime_level = "Low Crime",
         community_name = gsub("city$", "", community_name, ignore.case = TRUE))

# Combine the datasets
comparison_data <- bind_rows(high_crime_communities, low_crime_communities) %>%
  select(community_name, crime_level, violent_crimes_per_pop, income_inequality)

# Create the plot
inequality_plot <- ggplot(comparison_data, 
                          aes(x = reorder(community_name, -violent_crimes_per_pop), 
                              y = income_inequality, 
                              fill = crime_level)) +
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_manual(values = c("High Crime" = "firebrick", "Low Crime" = "steelblue")) +
  theme_minimal() +
  labs(title = "Income Inequality: High vs. Low Crime Communities",
       subtitle = "Comparison of White-to-Black Per Capita Income Ratio",
       x = "Community",
       y = "Income Inequality Ratio (White/Black Per Capita Income)",
       fill = "Crime Level") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(color = "black"),
    legend.position = "top"
  )

# Save the plot
ggsave("PS6b_Karbalaei.png", inequality_plot, width = 10, height = 6, dpi = 300, bg = "white")
print("Income inequality plot saved as PS6b_Karbalaei.png")

###Third Plot###

# Aggregate data at state level for ranking
state_violence <- crime_data_clean %>%
  group_by(state) %>%
  summarize(
    avg_violent_crime = mean(violent_crimes_per_pop, na.rm = TRUE),
    community_count = n()
  ) %>%
  arrange(desc(avg_violent_crime)) %>%
  filter(community_count >= 3)  # Only include states with at least 3 communities

# Get top 15 states by average violent crime rate
top_states <- state_violence$state[1:15]

# Filter for communities in the top 15 states
state_communities <- crime_data_clean %>%
  filter(state %in% top_states) %>%
  select(state, community_name, violent_crimes_per_pop)

# Create the plot
state_violence_plot <- ggplot(state_communities, 
                              aes(x = reorder(state, violent_crimes_per_pop, FUN = mean), 
                                  y = violent_crimes_per_pop)) +
  geom_jitter(width = 0.25, height = 0, color = "darkblue", alpha = 0.7) +  # Add jittered points
  geom_boxplot(alpha = 0.2, fill = "firebrick", outlier.shape = NA) +  # Add transparent boxplots
  theme_minimal() +
  labs(title = "States Ranked by Violent Crime Rates",
       subtitle = "Each point represents a community within the state",
       x = "State (Ranked by Average Violent Crime Rate)",
       y = "Violent Crime Rate (per Population)") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "white"),
    plot.background = element_rect(fill = "white", color = NA),
    axis.text = element_text(color = "black"),
    axis.text.x = element_text(angle = 45, hjust = 1),
    axis.title = element_text(color = "black")
  )

# Save the plot
ggsave("PS6c_Karbalaei.png", state_violence_plot, width = 10, height = 6, dpi = 300, bg = "white")
print("State violent crime ranking plot saved as PS6c_Karbalaei.png")

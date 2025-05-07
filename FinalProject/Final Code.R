#I chose 2000-2002 from the whole sample 2000 to 2016 so what I am doing is on these three years
# You may see working with 2003 and 2004, because I did not realize the release format was changed since 2003
#Sometimes the file paths change because I worked with three different PCs, I apologize if it makes it messy.
# install packages:
install.packages(c("readr", "dplyr"))
path <- "C:/Users/karb0008/OneDrive - University of Oklahoma/native_american_births_2000_2004_with_counties_cleaned.csv"
file.exists(path)
library(readr)
df <- read_csv(path)
# 3) drop columns AK:BZ → that’s cols 37 through 78
df_trimmed <- df %>% 
  select(1:36)   # keep only the first 36 columns

# 4) write back out
out_path <- "C:/Users/karb0008/OneDrive - University of Oklahoma/native_american_births_2000_2004_trimmed.csv"
write_csv(df_trimmed, out_path)

message("Wrote trimmed file to:\n  ", out_path)
###Create FIPS
library(readr)
library(dplyr)
library(stringr)

# 1) read in your trimmed CSV
in_path <- "C:/Users/karb0008/OneDrive - University of Oklahoma/native_american_births_2000_2004_trimmed.csv"
df <- read_csv(in_path)

# 2) build FIPS: 2‐digit state + 3‐digit county, except when county = 999
df2 <- df %>%
  mutate(
    # make sure both are character, zero‐pad to fixed width
    state_fips  = str_pad(as.character(birthplace), width = 2, pad = "0"),
    county_fips = str_pad(as.character(countybirth), width = 3, pad = "0"),
    
    # only glue them when county != "999", else NA
    FIPS = if_else(county_fips != "999",
                   paste0(state_fips, county_fips),
                   NA_character_
    )
  )

# 3) inspect
df2 %>% 
  select(birthplace, countybirth, state_fips, county_fips, FIPS) %>%
  slice(1:10) %>% 
  print()

# 4) write back out if you like
out_path <- "C:/Users/karb0008/OneDrive - University of Oklahoma/native_american_births_2000_2004_with_FIPS.csv"
write_csv(df2, out_path)
message("Wrote ", nrow(df2), " rows with FIPS to:\n  ", out_path)


#Find unique values of FIPS
# 1) pull out the unique FIPS codes (dropping the NA’s)
unique_fips <- df2 %>% 
  distinct(FIPS) %>% 
  filter(!is.na(FIPS)) %>% 
  arrange(FIPS) %>% 
  pull(FIPS)

# 2) how many are there?
length(unique_fips)

unique_fips

##Let's do the merge
# I have to merge the birth dataset with IHS budget dataset and the identifier is county FIPS 
library(readr)
library(dplyr)

# 1) read in your two files
births <- read_csv(
  "C:/Users/karb0008/OneDrive - University of Oklahoma/native_american_births_2000_2004_with_FIPS.csv",
  col_types = cols(.default = "c")  # most of your vars are character anyway
)

index <- read_csv(
  "C:/Users/karb0008/OneDrive - University of Oklahoma/ihs_budget_index.csv",
  col_types = cols(.default = "c")
)

# 2) peek at how many unique FIPS in each
length(unique(births$FIPS))  # 209
length(unique(index$FIPS))   # 204

# 3) do an inner‐join (keeps only FIPS in common)
merged <- inner_join(
  births,
  index,
  by = "FIPS"
)

# 4) check that you ended up with exactly the mutual FIPS
length(unique(merged$FIPS))  # should be <= 204

# FIPS in births but not in index
births_only <- setdiff(unique(births$FIPS), unique(index$FIPS))
length(births_only)  # how many birth‐only FIPS
births_only         # list them

# FIPS in index but not in births
index_only <- setdiff(unique(index$FIPS), unique(births$FIPS))
length(index_only)
index_only

#Lets pad the four digit fips code in index file 
# identify the 4‑digit ones
index_only <- setdiff(unique(index$FIPS), unique(births$FIPS))

# pad them
index <- index %>%
  mutate(
    FIPS = if_else(nchar(FIPS) == 4, paste0("0", FIPS), FIPS)
  )

# now re‑check
index_only_fixed <- setdiff(unique(index$FIPS), unique(births$FIPS))
length(index_only_fixed)  # should be zero or very small

# and finally merge
births_plus_index <- left_join(births, index, by = "FIPS")

# how many rows & columns?
dim(merged)
# how many unique FIPS made it through?
length(unique(merged$FIPS))
# see the first few records
head(merged)

# point to your OneDrive path
out_path <- "C:/Users/karb0008/OneDrive - University of Oklahoma/native_plus_budget_index.csv"

# make sure R sees it
file.exists(dirname(out_path))
# → should return TRUE

# write it out
write_csv(merged, out_path)

message("Saved merged file to:\n", out_path)

names(births)
# … look for “AgeAtDeath” in that list …

# summarize using the real column name
births %>% 
  summarize(
    n_with_death   = sum(!is.na(AgeAtDeath)),
    pct_with_death = mean(!is.na(AgeAtDeath)) * 100
  )
install.packages("janitor")
library(janitor)
# 1) read it in
path <- "C:/Users/karb0008/OneDrive - University of Oklahoma/native_plus_budget_index.csv"
df2  <- read_csv(path, col_types = cols(.default = "c"))

# 2) clean up names (optional but often helps eliminate stray spaces, uppercase, etc.)
df2 <- df2 %>% clean_names()

# 3) now take a look at what R thinks your names are
names(df2)
#––> e.g. you might see "birth_year", or "birthyear", or "birth_year_1", etc.

# convert to numbers and factor FIPS
mutate(
  year               = as.integer(year),
  prenatal_1         = as.numeric(prenatal_1),
  birth_weight_grams = as.numeric(birth_weight_grams),
  gestation_weeks    = as.numeric(gestation_weeks),
  mother_age         = as.numeric(mother_age),
  # pick the correct IHS budget index for that birth‐year:
  budget_index = case_when(
    year == 2000 ~ as.numeric(IHS_Budget_Index2000),
    year == 2001 ~ as.numeric(IHS_Budget_Index2001),
    year == 2002 ~ as.numeric(IHS_Budget_Index2002),
    year == 2003 ~ as.numeric(IHS_Budget_Index2003),
    year == 2004 ~ as.numeric(IHS_Budget_Index2004),
    TRUE         ~ NA_real_
  ),
  # make the infant‐death flag
  infant_death = if_else(!is.na(as.numeric(AgeAtDeath)), 1L, 0L),
  FIPS         = factor(FIPS)
) %>%
  # drop the raw year‐specific budget columns if you like
  select(-starts_with("IHS_Budget_Index"))
mod <- glm(
  infant_death ~ 
    budget_index    +
    prenatal_1      +
    birth_weight_grams +
    gestation_weeks +
    mother_age      +
    factor(year)    +  # year FE
    FIPS,               # county FE
  data   = df,
  family = binomial(link="logit")
)

library(broom)
tidy(mod, conf.int=TRUE, exponentiate=TRUE)


#######
#REgress
# install.packages(c("readr", "dplyr", "broom"))
library(readr)
library(dplyr)
install.packages("broom")
library(broom)
library(stringr)

in_path <- "C:/Users/ASUSCenter/OneDrive - University of Oklahoma/native_plus_budget_index.csv"

births <- read_csv(in_path, col_types = cols(.default = "c"))

births <- births %>%
  mutate(
    # zero-pad the state_code to width 2, countybirth to width 3
    birthplace = str_pad(birthplace,  2, pad = "0"),
    countybirth = str_pad(countybirth, 3, pad = "0"),
    FIPS = paste0(birthplace, countybirth)
  )

# overwrite the same file
write_csv(births, in_path)
message("✅ FIPS rebuilt from state_code + countybirth and saved to:\n", in_path)

in_path <- "C:/Users/ASUScenter/OneDrive - University of Oklahoma/native_plus_budget_index.csv"

births <- read_csv(in_path, col_types = cols(.default = "c"))

births <- births %>%
  # here I’m assuming that
  #   - the 2-digit state code lives in `birthplace`
  #   - the 3-digit county code lives in `countybirth`
  # adjust those names if yours are different!
  mutate(
    state_code = str_pad(birthplace, 2, pad = "0"),
    county_code = str_pad(countybirth, 3, pad = "0"),
    FIPS        = paste0(state_code, county_code)
  )

# now check:
names(births)
# …and…
head(births$FIPS)
# or
glimpse(births)
######
#####
#####
library(dplyr)
library(stringr)
library(readr)

# 1) point at your existing CSV on OneDrive
in_path <- "C:/Users/ASUScenter/OneDrive - University of Oklahoma/native_plus_budget_index.csv"

# 2) read it in, all as character so we don't lose leading zeros
births <- read_csv(in_path, col_types = cols(.default = "c"))

# 3) now mutate: zero‐pad the two pieces and paste them together
births <- births %>%
  mutate(
    # make sure these names match exactly what you see in names(births)
    state_code  = str_pad(birthplace,   2, pad = "0"),  
    county_code = str_pad(countybirth,  3, pad = "0"),
    FIPS        = paste0(state_code, county_code)
  )

# 4) sanity‐check: you should see FIPS in the names and its first few values
print(names(births))     
# …should include "FIPS"
head(births$FIPS, 10)    
# …should look like "10003", "10097", …

# 5) (optional) assert that you really did get 5 characters everywhere
stopifnot(all(nchar(births$FIPS) == 5))

# 6) overwrite the same file (so you don’t end up pointing at the wrong one)
write_csv(births, in_path)
message("✅ Added FIPS and overwrote: ", in_path)

##Some more cleaning to have the dataset with counties and budget
library(readr)
library(dplyr)
library(stringr)

# 1) Read the file
df <- read_csv(
  "C:/Users/yegan/OneDrive - University of Oklahoma/native_plus_budget_index.csv",
  col_types = cols(.default = "c")
)

df2_clean <- read_csv(
  "C:/Users/yegan/OneDrive - University of Oklahoma/native_plus_budget_index.csv",
  col_types = cols(.default = "c")
) %>%
  # 1) collapse your 5 budget columns into one `budget_index`
  mutate(
    birth_year    = as.integer(birth_year),
    budget_index  = case_when(
      birth_year == 2000 ~ as.numeric(IHS_Budget_Index2000),
      birth_year == 2001 ~ as.numeric(IHS_Budget_Index2001),
      birth_year == 2002 ~ as.numeric(IHS_Budget_Index2002),
      birth_year == 2003 ~ as.numeric(IHS_Budget_Index2003),
      birth_year == 2004 ~ as.numeric(IHS_Budget_Index2004),
      TRUE               ~ NA_real_
    ),
    # 2) build your other continuous predictors
    infant_death    = if_else(!is.na(AgeAtDeath), 1L, 0L),
    prenatal_visits = as.integer(prenatal_1),
    birthweight     = as.integer(birthweight),
    gestation       = as.integer(Gestation),
    motherage       = as.integer(motherage),
    
    # 3) categorical recodes
    year            = factor(birth_year, levels = 2000:2004),
    FIPS_factor     = factor(FIPS), # Renamed from FIPS...66
    
    place_delivery  = factor(
      placeofdelivey,
      levels = c("1","2","3","4","5","9"),
      labels = c("Hospital",
                 "Birthing center",
                 "Doctor's office",
                 "Residence",
                 "Other",
                 "Unknown")
    ),
    
    mother_marital  = factor(
      mothermarit,
      levels = c("1","2","9"),
      labels = c("Married","Unmarried","Unknown")
    ),
    
    mother_education = recode(
      mothered,
      "00" = "No formal education",
      "01" = "Elem 1–8",
      "09" = "HS 1 yr",
      "10" = "HS 2 yrs",
      "11" = "HS 3 yrs",
      "12" = "HS 4 yrs",
      "13" = "College 1 yr",
      "14" = "College 2 yrs",
      "15" = "College 3 yrs",
      "16" = "College 4 yrs",
      "17" = "College 5+ yrs",
      "99" = NA_character_
    )
  )

# After the main data frame creation, convert mother_education to a factor with the reference level
df2_clean$mother_education <- relevel(factor(df2_clean$mother_education), ref = "No formal education")

# performing a fixed effects regression analysis to examine the relationship between IHS funding and infant survival rates

# Load necessary libraries
library(dplyr)
install.packages("fixest")
library(fixest) # For fixed effects regression with clustering
install.packages("haven")
library(haven) # For reading/writing Stata files
library(readr) # For reading CSV files
install.packages("margins")
library(margins) # For marginal effects

# Set working directory
setwd("C:/Users/yegan/OneDrive - University of Oklahoma")

# 1) Import data
data <- read_csv("native_plus_budget_index_with_ct.csv")

# 2) Prepare key variables
# 2a) Convert birth_year to numeric
data$birth_year <- as.numeric(data$birth_year)

# 2b) Create infant survival indicator
data$infant_survival <- ifelse(data$ageatdeath == "NA", 1, 0)

# 2c) Create county fixed-effect ID
data$fips_id <- as.factor(data$fips)

# 2d) Convert continuous controls to numeric
data$motherage <- as.numeric(data$motherage)
data$prenatal_1 <- as.numeric(data$prenatal_1)
data$ihs_times_ratio <- as.numeric(data$ihs_times_ratio)

# 2e) Convert categorical controls to factors
data$place_del_id <- as.factor(data$placeofdelivey)
data$mothered_id <- as.factor(data$mothered)

# 3) Run regression with fixed effects and clustered standard errors
model <- feols(infant_survival ~ ihs_times_ratio + 
                 motherage + 
                 prenatal_1 + 
                 place_del_id + 
                 mothered_id | 
                 birth_year + fips_id, 
               data = data,
               cluster = "fips_id")

# Display regression results
summary(model)

# 4) Calculate and display marginal effects
mfx <- margins(model)
summary(mfx)

# 5) Save results to file
# Install if needed: install.packages("texreg")
library(texreg)
texreg(model, file = "figures/lpm_infant_death.tex", 
       stars = c(0.01, 0.05, 0.10),
       caption = "LPM w/ FEs",
       label = "tab:lpm")

# Save processed dataset, I saved it in dta to check it agin in STATA
write_dta(data, "processed_infant_data.dta")


###Let's try simpler model

# Load necessary libraries
library(dplyr)
library(readr)

# Import data - be specific about column types
setwd("C:/Users/yegan/OneDrive - University of Oklahoma")
data <- read_csv("native_plus_budget_index_with_ct.csv", 
                 col_types = cols(.default = "c")) # Import all as character first

# Check dimensions to make sure you have all rows
print(paste("Number of rows:", nrow(data)))

# Examine ageatdeath values
print(table(data$ageatdeath, useNA = "always"))

# Create infant survival indicator properly
data$infant_survival <- ifelse(data$ageatdeath == "NA", 1, 0)

# Verify we now have variation
print(table(data$infant_survival))

# Convert other variables to appropriate types
data$birth_year <- as.numeric(data$birth_year)
data$motherage <- as.numeric(data$motherage)
data$prenatal_1 <- as.numeric(data$prenatal_1)
data$ihs_times_ratio <- as.numeric(data$ihs_times_ratio)

# Try a simple model first, just checking ihs ratio!
simple_model <- lm(infant_survival ~ ihs_times_ratio, data = data)
summary(simple_model)

###Let's import the data srom STATA
# Load required libraries
library(haven)
library(fixest)
library(margins)
library(dplyr)

# Import the processed Stata dataset
data <- read_dta("C:/Users/yegan/OneDrive - University of Oklahoma/processed_infant_data.dta")

# Check the data to make sure everything is imported correctly
dim(data)
summary(data$infant_survival)
summary(data$ihs_times_ratio)

# Run the regression model with fixed effects
model <- feols(infant_survival ~ ihs_times_ratio + 
                 motherage + 
                 prenatal_1 + 
                 place_del_id + 
                 mothered_id | 
                 birth_year + fips_id, 
               data = data,
               cluster = "fips_id")

# Display regression results
summary(model)

###ٌWith more control variables
# Prepare the mothermarit and cigar variables first:
# Recode mothermarit: drop 9 (unknown) or create dummies
data <- data %>% 
  mutate(mothermarit = ifelse(mothermarit == 9, NA, mothermarit))
data <- data %>%
  mutate(log_ihs_ratio = log(ihs_times_ratio))


# Clean cigar variable: set 98 and 99 to NA (unknown/too many cigarettes)
data <- data %>%
  mutate(cigar = ifelse(cigar >= 98, NA, cigar))

data$birthweight <- as.numeric(data$birthweight)
data$gestation <- as.numeric(data$gestation)
data$cigar <- as.numeric(data$cigar)

# Now run the updated fixed effects model
model <- feols(infant_survival ~ log_ihs_ratio + 
                 motherage + 
                 prenatal_1 + 
                 place_del_id + 
                 mothered_id + 
                 birthweight +
                 gestation +
                 mothermarit +
                 cigar |
                 birth_year + fips_id, 
               data = data,
               cluster = "fips_id")

summary(model)
#To fix education
library(haven)
attr(data$mothered_id, "labels")
data$mothered_id <- as.character(data$mothered_id)
data$mothered_id[data$mothered_id == "99"] <- NA  # Set "Not stated" to NA

data$mothered_id <- factor(data$mothered_id,
                           levels = sprintf("%02d", 0:17),  # ensures "00", "01", ..., "17"
                           labels = c(
                             "No formal education",
                             "Elem 1", "Elem 2", "Elem 3", "Elem 4", "Elem 5", "Elem 6", "Elem 7", "Elem 8",
                             "HS 1 yr", "HS 2 yrs", "HS 3 yrs", "HS 4 yrs",
                             "College 1 yr", "College 2 yrs", "College 3 yrs", "College 4 yrs", "College 5+ yrs"
                           )
)
data$mothered_id <- relevel(data$mothered_id, ref = "No formal education")
model <- feols(infant_survival ~ ihs_times_ratio + 
                 motherage + 
                 prenatal_1 + 
                 place_del_id + 
                 mothered_id + 
                 birthweight +
                 gestation +
                 mothermarit +
                 cigar |
                 birth_year + fips_id, 
               data = data,
               cluster = "fips_id")

summary(model)




# 1) Import the data

library(haven)

# The path is already provided in code
data <- read_dta("C:/Users/yegan/OneDrive - University of Oklahoma/processed_infant_data.dta")

# Get the column names
names(data)

# For more detailed information about the variables
str(data)

# Alternative way to see column names with more information
library(dplyr)
glimpse(data)


###Get the fips values

# Load necessary libraries
library(haven)
library(dplyr)

# Import the data
data <- read_dta("C:/Users/yegan/OneDrive - University of Oklahoma/processed_infant_data.dta")

# View unique FIPS codes
unique_fips <- unique(data$fips)
print(unique_fips)

# Count of each FIPS code
fips_counts <- data %>%
  count(fips, sort = TRUE)
print(fips_counts)

# Summary statistics of FIPS codes
summary(data$fips)

# Structure of FIPS variable
str(data$fips)

# First few values of FIPS
head(data$fips, 20)


###See whether we can get ihs facilities counties 
# Install required packages
install.packages(c("readxl", "httr", "jsonlite", "dplyr"))

# Load libraries
library(readxl)
library(httr)
library(jsonlite)
library(dplyr)

# Read the IHS facilities data
ihs_data <- read_excel("ihs_facilities.xlsx", skip = 1)

# Define a function to get county information
get_county_info <- function(lat, lon) {
  # Create the API URL
  url <- paste0("https://geo.fcc.gov/api/census/area?lat=", lat, "&lon=", lon, "&format=json")
  
  # Make the API request
  response <- tryCatch({
    GET(url)
  }, error = function(e) {
    # Return NULL if the request fails
    return(NULL)
  })
  
  # If the request was successful, parse the JSON
  if (!is.null(response) && status_code(response) == 200) {
    results <- fromJSON(content(response, "text", encoding = "UTF-8"))
    
    # Check if we have county information
    if (length(results$results) > 0) {
      return(data.frame(
        county_name = results$results$county_name[1],
        state_name = results$results$state_name[1],
        county_fips = results$results$county_fips[1],
        stringsAsFactors = FALSE
      ))
    }
  }
  
  # Return empty data if we couldn't get county information
  return(data.frame(
    county_name = NA,
    state_name = NA,
    county_fips = NA,
    stringsAsFactors = FALSE
  ))
}

# Fix column names for clarity
names(ihs_data)[names(ihs_data) == "__EMPTY_3"] <- "facility_name"
names(ihs_data)[names(ihs_data) == "__EMPTY_7"] <- "state"
names(ihs_data)[names(ihs_data) == "__EMPTY_6"] <- "city"
names(ihs_data)[names(ihs_data) == "__EMPTY_24"] <- "latitude"
names(ihs_data)[names(ihs_data) == "__EMPTY_25"] <- "longitude"

# Process facilities in batches to avoid overwhelming the API
# Start with a small batch for testing
test_facilities <- ihs_data[1:5, ]

# Get county information for the test facilities
county_info <- lapply(1:nrow(test_facilities), function(i) {
  facility <- test_facilities[i, ]
  county_data <- get_county_info(facility$latitude, facility$longitude)
  # Add a small delay to avoid rate limiting
  Sys.sleep(0.5)
  return(county_data)
})

# Combine the results
county_df <- bind_rows(county_info)

# Add the county information to the test facilities data
test_results <- bind_cols(test_facilities, county_df)

# View the results
head(test_results[, c("facility_name", "city", "state", "county_name", "state_name", "county_fips")])

# To process all facilities, remove the subsetting:
# county_info <- lapply(1:nrow(ihs_data), function(i) { ... })


### Map to show the counties, I did not include it on my paper.

# Install required packages if not already installed
install.packages(c("tigris", "sf", "dplyr", "ggplot2"))

# Load libraries
library(tigris)
library(sf)
library(dplyr)
library(ggplot2)
options(tigris_use_cache = TRUE)

# Load county shapefiles
counties_sf <- counties(cb = TRUE, resolution = "5m", year = 2021)

# Your FIPS list
selected_fips <- c(
  "10003", "11001", "12009", "12011", "12031", "12033", "12057", "12071", "12095", "12103", "12105",
  "12117", "12127", "12099", "12101", "12115", "13121", "13067", "13089", "13135", "15003", "17097",
  "17197", "17201", "17031", "17163", "17043", "17089", "18089", "18003", "18097", "19153", "20091",
  "20173", "21111", "22071", "22051", "22033", "24510", "24031", "24005", "24033", "24003", "25025",
  "25027", "25023", "25005", "25009", "25021", "25013", "25017", "26049", "26065", "26081", "26125",
  "26161", "26099", "26163", "27037", "27053", "27123", "28049", "29095", "29510", "29189", "31055",
  "32003", "32031", "33011", "34003", "34017", "34021", "34013", "34025", "34029", "34027", "34007",
  "34031", "34005", "34023", "34039", "35001", "36029", "36059", "36065", "36087", "36119", "36001",
  "36055", "36103", "36067", "36071", "36027", "36085", "36081", "36061", "36047", "36005", "37051",
  "37067", "37081", "37119", "37183", "39017", "39061", "39093", "39095", "39153", "39035", "39049",
  "39099", "39113", "39151", "40143", "40109", "41051", "41039", "41067", "41005", "42101", "42091",
  "42045", "42029", "42003", "42133", "42079", "42017", "42077", "42049", "42071", "42011", "42129",
  "44007", "45019", "45045", "45079", "47037", "47093", "47157", "47065", "48121", "48201", "48085",
  "48439", "48355", "48029", "48453", "48141", "48113", "48215", "48061", "49035", "49049", "51059",
  "51710", "51810", "53033", "53061", "53063", "53053", "55079", "55025", "55133"
)

# Mark counties
counties_sf <- counties_sf %>%
  filter(!STATEFP %in% c("02", "15", "72")) %>%  # Exclude Alaska, Hawaii, Puerto Rico
  mutate(highlight = ifelse(GEOID %in% selected_fips, "Selected", "Other"))

# Create the map with larger dimensions
p <- ggplot(counties_sf) +
  geom_sf(aes(fill = highlight), color = "gray40", size = 0.05) +
  scale_fill_manual(values = c("Selected" = "#e41a1c", "Other" = "#f0f0f0")) +
  theme_void(base_size = 24) +  # Increased base font size
  labs(title = "U.S. Counties Highlighted by FIPS Codes") +
  theme(
    plot.title = element_text(size = 36, hjust = 0.5, face = "bold"),  # Larger title
    legend.position = "none",
    plot.margin = margin(20, 20, 20, 20)  # Add margins around the plot
  )

# For interactive viewing, adjust the device size
# For Windows
if (.Platform$OS.type == "windows") {
  windows(width = 24, height = 14)  # Much larger window size
  print(p)
}

# Alternatively, save as PDF for vector graphics (infinitely scalable)
ggsave("big_county_map.pdf",
       plot = p,
       width = 24, 
       height = 14)
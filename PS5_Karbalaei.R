# PS5_Karbalaei.R - Web Scraping Wikipedia Economics Page

# Load required libraries
library(rvest)
library(dplyr)
library(readr)
library(stringr)

# URL of the Wikipedia Economics page
url <- "https://en.wikipedia.org/wiki/Economics"

# Read the HTML content
webpage <- read_html(url)

# extract the content about heterodox econ
cat("Fetching information about heterodox economic theories...\n")

# extract paragraphs that mention heterodox economics
heterodox_paragraphs <- webpage %>%
  html_elements("p") %>%
  html_text(trim = TRUE) %>%
  .[grepl("heterodox|alternative economic|non-mainstream", ., ignore.case = TRUE)]

# Print how many paragraphs found
cat("Found", length(heterodox_paragraphs), "paragraphs mentioning heterodox economics\n")

# Extract mentions of heterodox schools using link patterns
# Links to economic schools/theories are typically formatted as internal Wikipedia links
heterodox_schools_links <- webpage %>%
  html_elements("a[href*='/wiki/']") %>%
  html_attr("title") %>%
  .[grepl("economics|economic theory|school", ., ignore.case = TRUE)] %>%
  .[!grepl("wikipedia|category|help|portal", ., ignore.case = TRUE)] %>%
  unique()
# First, find the section containing heterodox economics
heterodox_section <- webpage %>%
  html_elements(".mw-parser-output") %>%
  html_text() %>%
  .[grepl("heterodox economic theories", ., ignore.case = TRUE)]
# Common heterodox schools like what we saw on wikipedia
known_heterodox_schools <- c(
  "Marxian economics", 
  "Austrian School",
  "Post-Keynesian economics", 
  "Ecological economics",
  "Feminist economics",
  "Constitutional economics",
  "Institutional economics", 
  "Evolutionary economics", 
  "Dependency theory",
  "Structuralist economics",
  "World systems theory",
  "Econophysics",
  "Econodynamics",
  "Biophysical economics"
)

# Create a data frame to store all identified heterodox schools
heterodox_schools <- data.frame(
  school_name = character(),
  description = character(),
  stringsAsFactors = FALSE
)

# Function to extract content based on a school name
extract_school_description <- function(webpage, school_name) {
  # Try to find elements that mention the school
  school_elements <- webpage %>%
    html_elements(paste0("p:contains('", school_name, "'), li:contains('", school_name, "')")) %>%
    html_text(trim = TRUE)
  
  if (length(school_elements) > 0) {
    # Return the first element that contains a meaningful description
    for (elem in school_elements) {
      if (nchar(elem) > 50) {  # Arbitrary minimum length for a meaningful description
        return(elem)
      }
    }
  }
  return(NA)  # Return NA if no good description found
}

# Populate the data frame with known heterodox schools
for (school in known_heterodox_schools) {
  description <- extract_school_description(webpage, school)
#rbind:combine the matrices by rows
heterodox_schools <- rbind(heterodox_schools, 
                             data.frame(school_name = school,
                                        description = description,
                                        stringsAsFactors = FALSE))
}

# Clean up descriptions - remove excessive whitespace and newlines
heterodox_schools$description <- str_trim(heterodox_schools$description)
#gsub is used for substitution and str_trim is used to remove whitespace from the beginning and end of a string.
heterodox_schools$description <- gsub("\\s+", " ", heterodox_schools$description)

# If a school has no description, provide a basic one
heterodox_schools$description[is.na(heterodox_schools$description)] <- 
  "A heterodox school of economic thought (description not extracted)"

# Save the data to a CSV file
write_csv(heterodox_schools, "heterodox_economics_schools.csv")

# Print summary of what we extracted
cat("\nData extraction summary:\n")
cat("Heterodox economics schools identified:", nrow(heterodox_schools), "\n")

# Show sample of the heterodox schools
cat("\nSample of heterodox economics schools:\n")
print(head(heterodox_schools, 10))

# Extract any tables related to economic schools/theories
economics_tables <- webpage %>% 
  html_elements("table.wikitable") %>%
  .[grepl("economic|theory|school", html_text(.), ignore.case = TRUE)]

if (length(economics_tables) > 0) {
  cat("\nFound", length(economics_tables), "tables related to economic theories\n")
  
  # Process each table
  for (i in seq_along(economics_tables)) {
    table_data <- economics_tables[[i]] %>% html_table(header = TRUE)
    table_name <- paste0("economics_table_", i, ".csv")
    write_csv(table_data, table_name)
    cat("Saved table", i, "to", table_name, "\n")
  }
}

cat("\nAnalysis complete! Data saved to heterodox_economics_schools.csv\n")





# PS5_Karbalaei.R - Using Financial Data API

# Install required packages if needed
# install.packages(c("quantmod", "ggplot2", "dplyr", "tidyr", "plotly"))

install.packages("quantmod")
install.packages(c("ggplot2", "dplyr", "tidyr", "plotly"))

# PS5_LastName.R - Simplified Stock Analysis using Yahoo Finance data

# Install required packages
install.packages(c("quantmod", "ggplot2", "dplyr", "tidyr"))

# Load libraries
library(quantmod)
library(ggplot2)
library(dplyr)
library(tidyr)

# function to safely get stock data
safe_get_symbols <- function(symbol, from_date, to_date) {
  # Try to download the stock data it understands we mean yahoo finance because of quantmod package
  tryCatch({
    stock_data <- getSymbols(symbol, src = "yahoo", 
                             from = from_date, 
                             to = to_date, 
                             auto.assign = FALSE)
    return(stock_data)
  }, error = function(e) {
    cat("Error fetching data for", symbol, ":", e$message, "\n")
    return(NULL)
  })
}

# Set timeframe
from_date <- Sys.Date() - 365  # One year ago
to_date <- Sys.Date()          # Today

# Define stock symbols - focus on Sprouts and major retail competitors
symbols <- c("SFM", "KR", "WMT", "TGT", "COST", "^GSPC")

# Step 1: Get each stock's data separately with diagnostics
cat("Fetching stock data...\n")
stock_data_list <- list()

for (symbol in symbols) {
  cat("Attempting to fetch data for", symbol, "...\n")
  stock_data <- safe_get_symbols(symbol, from_date, to_date)
  
  if (!is.null(stock_data)) {
    cat("Successfully retrieved", nrow(stock_data), "days of data for", symbol, "\n")
    stock_data_list[[symbol]] <- stock_data
  }
}

# Check what data we retrieved
cat("\nStock data retrieved for:", paste(names(stock_data_list), collapse=", "), "\n")

# If no data was retrieved, create sample data for demonstration
if (length(stock_data_list) == 0) {
  cat("No stock data could be retrieved. Creating sample data for demonstration.\n")
  
  # Create sample dates for the past year
  dates <- seq(from = as.Date(from_date), to = as.Date(to_date), by = "day")
  dates <- dates[weekdays(dates) %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")]
  
  # Create a data frame with the Date column
  all_stocks <- data.frame(Date = dates)
  
  # Add sample data for each stock
  set.seed(123)  # For reproducibility
  for (symbol in symbols) {
    # Create random walk price series
    base_price <- switch(symbol,
                         "SFM" = 25,
                         "KR" = 35,
                         "WMT" = 150,
                         "TGT" = 130,
                         "COST" = 550,
                         "^GSPC" = 4000)
    
    # Generate random price movements
    changes <- rnorm(length(dates), mean = 0.0003 * base_price, sd = 0.01 * base_price)
    prices <- base_price + cumsum(changes)
    
    # Add to data frame
    all_stocks[[symbol]] <- prices
  }
  
  # Normalize prices to 100 at the start
  all_stocks_normalized <- all_stocks %>%
    mutate(across(-Date, ~ . / first(.) * 100))
} else {

  # Prepare data frame for combined stock prices
  all_stocks <- data.frame(Date = index(stock_data_list[[1]]))
  
  # Extract adjusted closing prices for each stock
  for (symbol in names(stock_data_list)) {
    price_col_name <- paste0(symbol, ".Adjusted")
    prices <- as.numeric(stock_data_list[[symbol]][, 6])  # 6th column is adjusted prices
    
    # Add to data frame, handling any length discrepancies
    if (length(prices) == nrow(all_stocks)) {
      all_stocks[[symbol]] <- prices
    } else {
      cat("Warning: Length mismatch for", symbol, "- expected", nrow(all_stocks), "but got", length(prices), "\n")
      # Pad or trim as needed
      if (length(prices) > nrow(all_stocks)) {
        all_stocks[[symbol]] <- prices[1:nrow(all_stocks)]
      } else {
        temp_prices <- rep(NA, nrow(all_stocks))
        temp_prices[1:length(prices)] <- prices
        all_stocks[[symbol]] <- temp_prices
      }
    }
  }
  
  # Convert Date column to actual Date type if it's not already
  if (!inherits(all_stocks$Date, "Date")) {
    all_stocks$Date <- as.Date(all_stocks$Date)
  }
  
  # Normalize prices to 100 at the start for comparison
  all_stocks_normalized <- all_stocks %>%
    mutate(across(-Date, ~ (. / first(na.omit(.))) * 100))
}

# Verify we have data before continuing, ncol: number od columns
if (ncol(all_stocks) <= 1) {
  stop("No stock data available for analysis.")
}

# Display dimensions of our datasets
cat("\nDimensions of all_stocks:", dim(all_stocks)[1], "rows by", dim(all_stocks)[2], "columns\n")
cat("Column names:", paste(names(all_stocks), collapse=", "), "\n\n")

# Convert to long format for plotting
all_stocks_long <- all_stocks_normalized %>%
  pivot_longer(-Date, names_to = "Stock", values_to = "RelativeValue")

# A simpler approach using base R plotting instead of ggplot2 I had some issues with it
# Make sure the Date column is properly formatted
all_stocks$Date <- as.Date(all_stocks$Date)

# Get the stock symbols (excluding the Date column)
stock_symbols <- names(all_stocks)[-1]

# Create a color palette
colors <- rainbow(length(stock_symbols))

# Create the plot
png("stock_performance_comparison.png", width = 10, height = 6, units = "in", res = 300)

# Set up the plot
plot(all_stocks$Date, all_stocks_normalized[[2]], 
     type = "l", 
     col = colors[1],
     ylim = c(min(all_stocks_normalized[,-1], na.rm = TRUE), 
              max(all_stocks_normalized[,-1], na.rm = TRUE)),
     xlab = "Date", 
     ylab = "Relative Value",
     main = "Relative Stock Performance",
     sub = "Normalized to 100 at start date")

# Add lines for each other stock
for (i in 2:length(stock_symbols)) {
  lines(all_stocks$Date, all_stocks_normalized[[i+1]], col = colors[i])
}

# Add a legend, legend is a function creates a legend or key on your plot that explains what the different colors, symbols, or line types represent.
legend("bottomright", 
       legend = stock_symbols, 
       col = colors, 
       lty = 1,
       cex = 0.8)

# Close the device
dev.off()

cat("Plot saved using base R graphics\n")

# Calculate statistics for each stock
stock_stats <- data.frame(
  Symbol = names(all_stocks)[-1],  # Exclude Date column
  stringsAsFactors = FALSE
)

# Calculate performance metrics
for (i in seq_along(stock_stats$Symbol)) {
  symbol <- stock_stats$Symbol[i]
  prices <- all_stocks[[symbol]]
  
  # Handle possible NA values
  valid_prices <- na.omit(prices)
  
  stock_stats$StartPrice[i] <- valid_prices[1]
  stock_stats$EndPrice[i] <- tail(valid_prices, 1)
  stock_stats$PctChange[i] <- ((stock_stats$EndPrice[i] / stock_stats$StartPrice[i]) - 1) * 100
  stock_stats$MinPrice[i] <- min(valid_prices)
  stock_stats$MaxPrice[i] <- max(valid_prices)
  
  # Volatility calculation
  returns <- diff(valid_prices) / valid_prices[-length(valid_prices)]
  stock_stats$Volatility[i] <- sd(returns, na.rm = TRUE) * 100
}

# Save statistics to CSV
write.csv(stock_stats, "stock_statistics.csv", row.names = FALSE)

# Print summary statistics
cat("\nStock Performance Summary:\n")
print(stock_stats)

# If SFM data is available, do monthly analysis
if ("SFM" %in% names(all_stocks)) {
  # Extract SFM data
  sfm_data <- data.frame(
    Date = all_stocks$Date,
    Price = all_stocks$SFM
  )
  
  # Create monthly summary
  sfm_monthly <- sfm_data %>%
    mutate(Month = format(Date, "%Y-%m")) %>%
    group_by(Month) %>%
    summarize(
      StartPrice = first(Price),
      EndPrice = last(Price),
      HighPrice = max(Price),
      LowPrice = min(Price),
      MonthlyReturn = (EndPrice / StartPrice - 1) * 100
    ) %>%
    arrange(Month)
  
  # Save to CSV
  write.csv(sfm_monthly, "sfm_monthly_performance.csv", row.names = FALSE)
  
  # Print monthly summary
  cat("\nSFM Monthly Performance:\n")
  print(sfm_monthly)
}

# Final message
cat("\nAnalysis complete! Files saved:\n")
cat("1. stock_performance_comparison.png - Comparative performance chart\n")
cat("2. stock_statistics.csv - Key statistics for each stock\n")
if ("SFM" %in% names(all_stocks)) {
  cat("3. sfm_monthly_performance.csv - Monthly performance breakdown for SFM\n")
}

plot(stock_plot)

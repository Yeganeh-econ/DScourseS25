# Step 1: Convert fixed-width files to CSV format
# This makes the data much easier to work with

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(readr)

# Define a function to convert fixed-width to CSV
convert_fixed_to_csv <- function(input_file, output_file, col_positions) {
  # Read raw lines with error handling
  cat("Reading file:", input_file, "\n")
  tryCatch({
    lines <- readLines(input_file)
    cat("Successfully read", length(lines), "lines from file\n")
    
    # Check if any lines are long enough
    line_lengths <- nchar(lines)
    if (max(line_lengths) < 213) {
      cat("WARNING: Maximum line length is", max(line_lengths), "characters, but AGED column requires 213 characters\n")
      cat("First few lines of the file:\n")
      for (i in 1:min(5, length(lines))) {
        cat(i, ":", lines[i], "\n")
      }
    }
    
    # Create data frame by extracting fields with safety checks
    cat("Extracting fields from fixed-width format...\n")
    df <- data.frame(
      MRACE = ifelse(line_lengths >= 37, substr(lines, 36, 37), NA),
      NPREVIST = ifelse(line_lengths >= 55, substr(lines, 54, 55), NA),
      FRACE = ifelse(line_lengths >= 66, substr(lines, 65, 66), NA),
      MEDRISK = ifelse(line_lengths >= 117, substr(lines, 100, 117), NA),
      AGED = ifelse(line_lengths >= 213, substr(lines, 211, 213), NA),
      stringsAsFactors = FALSE
    )
    
    # Convert columns to appropriate types with warnings suppressed
    df <- df %>%
      mutate(
        MRACE = suppressWarnings(as.numeric(MRACE)),
        NPREVIST = suppressWarnings(as.numeric(NPREVIST)),
        FRACE = suppressWarnings(as.numeric(FRACE)),
        AGED_numeric = suppressWarnings(as.numeric(AGED)),
        infant_death = ifelse(!is.na(AGED_numeric) & AGED_numeric <= 364, 1, 0)
      )
    
    # Display summary of infant deaths found
    num_infant_deaths <- sum(df$infant_death, na.rm = TRUE)
    cat("Found", num_infant_deaths, "infant deaths out of", nrow(df), "records\n")
    
    # Write CSV file
    cat("Writing CSV file:", output_file, "\n")
    write.csv(df, output_file, row.names = FALSE)
    
    return(df)
  }, error = function(e) {
    cat("ERROR reading file:", e$message, "\n")
    cat("Checking if file exists:", file.exists(input_file), "\n")
    if (file.exists(input_file)) {
      cat("File exists but could not be read properly.\n")
      cat("Trying to read first few bytes to diagnose issue...\n")
      con <- file(input_file, "rb")
      raw_data <- readBin(con, "raw", 100)
      close(con)
      cat("First few bytes:", raw_data, "\n")
    }
    # Return empty data frame
    return(data.frame())
  })
}

# Step 2: Set the directory path and list files
file_path <- "C:/Users/yegan/OneDrive - University of Oklahoma"

# Change working directory to the specified path
setwd(file_path)
cat("Changed working directory to:", getwd(), "\n")

# List files in the directory
all_files <- list.files()
cat("Files in directory:", paste(all_files[1:min(10, length(all_files))], collapse=", "), 
    if(length(all_files) > 10) "... and more" else "", "\n")

# Find files matching the pattern
pattern_files <- list.files(pattern = "total200[0-2]")
cat("Found", length(pattern_files), "files matching the pattern 'total200[0-2]'\n")

# Define file paths directly if needed
if (length(pattern_files) == 0) {
  cat("No files found with pattern. Please enter the exact file names:\n")
  
  # List some potential data files to help user identify them
  potential_data_files <- grep("^total|^data|^.txt$|^.dat$", all_files, value = TRUE)
  if (length(potential_data_files) > 0) {
    cat("Potential data files found:\n")
    print(potential_data_files)
  }
  
  # Ask if user wants to use all files in the directory
  cat("\nWould you like to specify file names manually? [y/n]: ")
  answer <- readline()
  
  if (tolower(answer) == "y") {
    cat("Enter file names (comma-separated):\n")
    manual_files <- readline()
    files <- trimws(unlist(strsplit(manual_files, ",")))
  } else {
    # Ask user if they want to continue using a sample file for testing
    cat("Would you like to create a sample file for testing? [y/n]: ")
    test_answer <- readline()
    
    if (tolower(test_answer) == "y") {
      # Create a sample file for testing
      sample_file <- "sample_data.txt"
      sample_lines <- c(
        "   1089 R95 139                                                                                             111",
        "   0002 P010075                                                                                              ",
        "   0053 A49022                                                                                               ",
        "   3455 P456789                                                                                             222"
      )
      writeLines(sample_lines, sample_file)
      cat("Created sample file:", sample_file, "\n")
      files <- sample_file
    } else {
      stop("No files to process. Please check file names and working directory.")
    }
  }
} else {
  files <- pattern_files
}

# Create CSV file names
csv_files <- paste0(files, ".csv")
cat("Will convert these files to CSV:", paste(csv_files, collapse=", "), "\n")

# Convert all files to CSV format
all_data <- data.frame()
for (i in 1:length(files)) {
  cat("\nProcessing file", i, "of", length(files), ":", files[i], "\n")
  tryCatch({
    df <- convert_fixed_to_csv(files[i], csv_files[i], NULL)
    all_data <- rbind(all_data, df)
    cat("Successfully processed file:", files[i], "\n")
  }, error = function(e) {
    cat("Error processing file", files[i], ":", e$message, "\n")
    cat("Skipping this file and continuing with others.\n")
  })
}

# Step 3: Analyze the combined data
cat("\nAnalysis of combined data\n")

# Define race label mappings
mrace_labels <- c(
  "1" = "White", "2" = "Black", "3" = "American Indian",
  "4" = "Chinese", "5" = "Japanese", "6" = "Hawaiian",
  "7" = "Filipino", "18" = "Asian Indian", "28" = "Korean",
  "38" = "Samoan", "48" = "Vietnamese", "58" = "Guamanian",
  "68" = "Other Asian/Pacific", "78" = "Other Combined"
)

frace_labels <- c(
  "1" = "White", "2" = "Black", "3" = "American Indian",
  "4" = "Chinese", "5" = "Japanese", "6" = "Hawaiian",
  "7" = "Filipino", "8" = "Other Asian/Pacific", "58" = "Guamanian",
  "99" = "Unknown"
)

# Map race codes to labels
all_data <- all_data %>%
  mutate(
    MRACE_str = as.character(MRACE),
    FRACE_str = as.character(FRACE),
    MRACE_LABEL = mrace_labels[MRACE_str],
    FRACE_LABEL = frace_labels[FRACE_str]
  )

# Analyze: Average prenatal visits by mother's race
prenatal_by_race <- all_data %>%
  filter(!is.na(NPREVIST) & NPREVIST < 99) %>%
  group_by(MRACE_LABEL) %>%
  summarise(
    avg_prenatal_visits = mean(NPREVIST, na.rm = TRUE),
    n = n()
  ) %>%
  filter(!is.na(MRACE_LABEL)) %>%
  arrange(desc(avg_prenatal_visits))

# Print results
cat("\nAverage Prenatal Visits by Mother's Race:\n")
print(prenatal_by_race)

# Plot average prenatal visits by mother's race
p1 <- ggplot(prenatal_by_race, aes(x = reorder(MRACE_LABEL, avg_prenatal_visits), y = avg_prenatal_visits)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Average Prenatal Visits by Mother's Race",
    x = "Mother's Race",
    y = "Average Number of Visits"
  ) +
  theme_minimal()

# Analyze: Infant death rates by mother's race
infant_death_rate_by_mrace <- all_data %>%
  # Group by mother's race
  group_by(MRACE_LABEL) %>%
  summarise(
    # Count total number of infant deaths
    infant_deaths = sum(infant_death, na.rm = TRUE),
    # Count total number of births (all records in your dataset)
    total_births = n(),
    # Calculate death rate as deaths per 1,000 births
    death_rate = (infant_deaths / total_births) * 1000,
    # Also include the raw proportion for reference
    death_proportion = infant_deaths / total_births
  ) %>%
  filter(!is.na(MRACE_LABEL)) %>%
  arrange(desc(death_rate))

# Print results
cat("\nInfant Death Rate by Mother's Race (per 1,000 births):\n")
print(infant_death_rate_by_mrace)

# Plot infant death rate by mother's race
p2 <- ggplot(infant_death_rate_by_mrace, aes(x = reorder(MRACE_LABEL, death_rate), y = death_rate)) +
  geom_col(fill = "red") +
  coord_flip() +
  labs(
    title = "Infant Death Rate by Mother's Race",
    x = "Mother's Race",
    y = "Deaths per 1,000 Births"
  ) +
  theme_minimal()

# Save the plots
pdf("prenatal_visits_by_race.pdf", width = 10, height = 6)
print(p1)
dev.off()

pdf("infant_death_rate_by_race.pdf", width = 10, height = 6)
print(p2)
dev.off()

# Write the combined data to a CSV file for further analysis
write.csv(all_data, "combined_data.csv", row.names = FALSE)

cat("\nAnalysis complete. Results saved to CSV files and PDF plots.\n")
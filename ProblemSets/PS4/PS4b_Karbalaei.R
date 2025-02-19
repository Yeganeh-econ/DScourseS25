# Load required packages
library(sparklyr)
library(tidyverse)

# Set up connection to Spark
sc <- spark_connect(master = "local")

# Create a tibble with iris data
df1 <- as_tibble(iris)

# Copy tibble to Spark
df <- copy_to(sc, df1)

# Check class of both dataframes
class(df1)
class(df)

# Check column names

# Select operation - first 6 rows of Sepal_Length and Species
df %>% 
  select(Sepal_Length, Species) %>% 
  head %>% 
  print

# Filter operation - Sepal_Length > 5.5
df %>% 
  filter(Sepal_Length > 5.5) %>% 
  head %>% 
  print

# Combine select and filter
df %>%
  select(Sepal_Length, Species) %>%
  filter(Sepal_Length > 5.5) %>%
  head %>%
  print

# Group by operation with mean and count
df2 <- df %>%
  group_by(Species) %>%
  summarize(
    mean = mean(Sepal_Length),
    count = n()
  ) %>%
  head %>%
  print

# Try to sort by Species (Note: this might give an error as mentioned in the problem set)
df2 %>%
  arrange(Species) %>%
  head %>%
  print

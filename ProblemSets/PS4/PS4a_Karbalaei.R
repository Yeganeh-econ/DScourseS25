download.file("https://www.vizgr.org/historical-events/search.php?format=json&begin_date=00000101&end_date=20240209&lang=en", "dates.json")
if(file.exists("dates.json")) {
  print("File downloaded successfully")
} else {
  print("File download failed")
}
library(jsonlite)
library(tidyverse)

json_text <- readLines("dates.json", warn = FALSE)
mylist <- fromJSON(paste(json_text, collapse = ""))

# Convert list to data frame
mydf <- bind_rows(mylist$result[-1])

# Check class of objects
print(class(mydf))
print(class(mydf$date))

# Print first few rows
print(head(mydf))

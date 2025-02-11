#!/bin/sh

# Download the file
wget http://spatialkeydocs.s3.amazonaws.com/FL_insurance_sample.csv.zip

# List contents
ls

# Unzip the file
unzip FL_insurance_sample.csv.zip

# Remove unnecessary files
rm -rf __MACOSX
rm -f FL_insurance_sample.csv.zip

# Check file size
ls -al --block-size=MB FL_insurance_sample.csv

# Look at first 5 lines
head -5 FL_insurance_sample.csv

# Count lines
wc -l FL_insurance_sample.csv

# Fix EOL conversion
dos2unix -c mac FL_insurance_sample.csv

# Verify file is readable
head -5 FL_insurance_sample.csv

# Verify line count
wc -l FL_insurance_sample.csv


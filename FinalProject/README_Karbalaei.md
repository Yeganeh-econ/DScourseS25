# IHS Funding and Native American Infant Mortality Analysis

This repository contains data processing and analysis code for examining the relationship between Indian Health Service (IHS) funding and infant mortality rates among American Indian and Alaska Native (AIAN) populations.

## Project Overview

This research investigates whether variations in IHS funding levels correlate with Native American infant survival disparities. The analysis uses CDC Birth-Infant Death Data from 2000-2002 combined with IHS budget allocation data at the county level.

## Data Sources

1. **CDC Period/Cohort Linked Birth-Infant Death Data** (2000-2002)
   - Contains detailed birth and infant death records for AIAN populations
   - Includes maternal characteristics, birth outcomes, and geographic identifiers

2. **IHS Budget Index Data**
   - Annual IHS funding allocations by county
   - Derived index values for 2000-2002

## Prerequisites

The following R packages are required:

```r
install.packages(c(
  "readr",       # CSV file handling
  "dplyr",       # Data manipulation
  "stringr",     # String processing
  "janitor",     # Data cleaning
  "broom",       # Model output formatting
  "fixest",      # Fixed effects regression
  "haven",       # Stata file import/export
  "margins",     # Marginal effects calculation
  "texreg",      # LaTeX regression table output
  "ggplot2",     # Visualization
  "sf",          # Spatial data handling
  "tigris"       # US geographic data
))
```

## Workflow

The full analysis consists of the following steps:

### 1. Data Preparation

Run these scripts in sequence:

```r
# Step 1: Trim the dataset to essential columns
source("scripts/1_trim_dataset.R")

# Step 2: Create FIPS county codes
source("scripts/2_create_fips.R")

# Step 3: Merge birth data with IHS budget data
source("scripts/3_merge_datasets.R")
```

### 2. Data Analysis

```r
# Step 4: Clean and prepare variables for regression
source("scripts/4_clean_variables.R")

# Step 5: Run main regression models
source("scripts/5_regression_models.R")
```

### 3. Visualization

```r
# Step 6: Generate county map visualization
source("scripts/6_county_map.R")
```

## Key Variables

- `infant_survival`: Binary indicator of infant survival (1) or death (0)
- `ihs_times_ratio` / `budget_index`: Measure of IHS funding allocation for each county based on Native American population
- `prenatal_1`: Number of prenatal visits
- `birthweight`: Birth weight in grams
- `gestation`: Gestational age in weeks
- `motherage`: Age of mother
- `FIPS`: County FIPS code (5-digit identifier)

## Regression Models

Several regression models are included:

1. **Simple linear model**: Basic relationship between IHS funding and infant survival
2. **Fixed-effects model**: 
   - Controls for county and year fixed effects
   - Includes maternal characteristics (age, education, marital status)
   - Includes birth characteristics (prenatal visits, birthweight, gestation)
   - Uses clustered standard errors at the county level

## Outputs

The analysis generates:

1. **LaTeX Tables**: Regression results formatted for academic papers (`figures/lpm_infant_death.tex`)
2. **Stata Dataset**: Processed data saved as .dta file for alternate analysis
3. **County Map**: Visualization of counties included in the analysis

## File Structure

```
.
├── data/
│   ├── raw/                              # Original data files
│   │   ├── native_american_births_2000_2004_with_counties_cleaned.csv
│   │   └── ihs_budget_index.csv
│   └── processed/                        # Generated data files
│       ├── native_american_births_2000_2004_trimmed.csv
│       ├── native_american_births_2000_2004_with_FIPS.csv
│       ├── native_plus_budget_index.csv
│       └── processed_infant_data.dta
├── figures/                              # Output visualizations
│   ├── lpm_infant_death.tex
│   └── big_county_map.pdf
├── scripts/                              # Analysis code
│   ├── 1_trim_dataset.R
│   ├── 2_create_fips.R
│   ├── 3_merge_datasets.R
│   ├── 4_clean_variables.R
│   ├── 5_regression_models.R
│   └── 6_county_map.R
└── README.md                             # This file
```

## Data Path Configuration

Before running scripts, update file paths in each R script to match your local environment. The current scripts reference multiple path patterns:

```r
# Examples of different path patterns used in scripts
"C:/Users/karb0008/OneDrive - University of Oklahoma/..." 
"C:/Users/ASUScenter/OneDrive - University of Oklahoma/..."
"C:/Users/yegan/OneDrive - University of Oklahoma/..."
```


## Replicating the Analysis

1. Clone this repository to your local machine
2. Install required R packages (see Prerequisites section)
3. Adjust file paths in scripts to match your environment
4. Run scripts in sequential order (1-6)
5. Check the generated outputs in the figures/ directory

## Results

Key findings:
- IHS budget index shows a positive and statistically significant association with infant survival
- Birth weight and gestation are strong predictors of infant survival
- Surprisingly, some variables (like prenatal visits) show counterintuitive relationships that may reflect selection effects

## Citation

If you use this code or analysis in your research, please cite as:

```
Karbalaei, Y. (2025). The Relationship Between Indian Health Service Funding and Native American Infant Mortality Rates. University of Oklahoma.
```

## Contact

For questions or feedback, contact: yeganeh.karbalaei-1@ou.edu
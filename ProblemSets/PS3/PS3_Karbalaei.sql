-- (a) Read in the Florida insurance data CSV file
.mode csv
.import FL_insurance_sample.csv insurance_data

-- (b) Print first 10 rows
SELECT *
FROM insurance_data
LIMIT 10;

-- (c) List unique counties
SELECT DISTINCT county,
COUNT(*) as count
FROM insurance_data
GROUP BY county
ORDER BY county;

-- (d) Compute average property appreciation
SELECT AVG(tiv_2012 - tiv_2011) as avg_appreciation
FROM insurance_data;

-- (e) Create frequency table of construction types
SELECT construction,
COUNT(*) as count,
ROUND(CAST(COUNT(*) AS FLOAT) / (SELECT COUNT(*) FROM insurance_data) * 100, 2) as percentage
FROM insurance_data
GROUP BY construction
ORDER BY count DESC;


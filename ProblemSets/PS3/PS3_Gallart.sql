-- read in csv
.mode csv
.separator ","
.import FL_insurance_sample.csv FL_insur

--print out the first 10 lines
SELECT * 
FROM FL_insur
LIMIT 10;

-- list unique counties
SELECT DISTINCT county
FROM FL_insur;

-- average property appreciation from 2011 to 2012
SELECT AVG (tiv_2012 - tiv_2011) AS difference
FROM FL_insur;

-- frequency table of the construction
WITH cte AS(SELECT construction, COUNT(*) as frequency
FROM FL_insur
GROUP BY construction
)
SELECT construction, frequency, (frequency*1.0)/(SELECT SUM(frequency) FROM cte) AS fraction
FROM cte;

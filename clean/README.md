Clean data
================

Clean Moosilauke and BC data.

## Moosilauke data
Depends on [read-moose.R](http://github.com/ghandi9000/ecodatascripts/read/read-moose.R) to read raw data from master file.

* Checks for columns: "DBH", "HT", "EHT", "BV"
 * "EHT" is estimated height
* Replaces "HTTCR" and "ebv" with "HT" and "BV"
* Creates BA columns for each year (Basal Area)
* Creates BAGROWTH columns (annualized basal area growth)
* Creates PRIORDBH/PRIORBA/PRIORHT/PRIORBV (dbh/ba/ht/bv from prior time period)
* Converts column names to lowercase
* Removes unused columns
* Creates [moose-wide.csv](http://github.com/ghandi9000/data/moose/moose-wide.csv)


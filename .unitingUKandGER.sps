*Encoding: UTF-8.
*PRODUCING ADDITIONAL COUNTRY/WEIGHT VARIABLES
*FOR STANDARD AND SPECIAL EUROBAROMETER DATASETS
*SEE ALSO: eb_patch_UK_DE_weights.pdf
*GESIS-DAS, EUROBAROMETER DATA SERVICE (2013-07-05)
*NOTE: The variable names in the example below are based on survey 77.1 (ZA5597). For older Eurobarometer surveys the syntax must be adjusted to conform to the supplied V### variable names.

compute W3A=V8.
if (V11 eq 1) W3A=0.
compute W3A=W3A+V12.
formats W3A (F8.5).

compute W4A=V8.
if (V9 eq 1) W4A=0.
compute W4A=W4A+V10.
formats W4A (F8.5).

compute CNTR_DE=V6.
formats CNTR_DE (F2.0).
recode CNTR_DE (14=4).

compute CNTR_GB=V6.
formats CNTR_GB (F2.0).
recode CNTR_GB (10=9).

compute W3A4A=w3a.
if (V9 eq 1) W3A4A=0.
compute W3A4A=W3A4A+V10.
formats W3A4A (F8.5).

compute NATION=V6.
formats NATION (F2.0).
recode NATION (14=4) (10=9).

execute.

Variable labels
  W3A "WEIGHT RESULT FROM TARGET - CNTR_DE"
  CNTR_DE "NATION (GERMANY EAST+WEST)"
  W4A "WEIGHT RESULT FROM TARGET - CNTR_GB"
  CNTR_GB "NATION (UNITED KINGDOM)"
  W3A4A "WEIGHT RESULT FROM TARGET - NATION"
  NATION "NATION (UNITED KINGDOM AND GERMANY UNITED)".

value labels
   CNTR_DE
 1 "FR - France"
 2 "BE - Belgium"
 3 "NL - The Netherlands"
 4 "DE - GERMANY"
 5 "IT - Italy"
 6 "LU - Luxembourg"
 7 "DK - Denmark"
 8 "IE - Ireland"
 9 "GB-GBN - Great Britain"
 10 "GB-NIR Northern Ireland"
 11 "GR - Greece"
 12 "ES -Spain"
 13 "PT - Portugal"
 14 "-"
 15 "-"
 16 "FI - Finland"
 17 "SE - Sweden"
 18 "AT - Austria"
 19 "CY - Cyprus (Republic)"
 20 "CZ - Czech Republic"
 21 "EE - Estonia"
 22 "HU - Hungary"
 23 "LV - Latvia"
 24 "LT - Lithuania"
 25 "MT - Malta"
 26 "PL - Poland"
 27 "SK - Slovakia"
 28 "SI - Slovenia"
 29 "BG - Bulgaria"
 30 "RO - Romania"
 31 "TR - Turkey"
 32 "HR - Croatia"
 33 "CY-TCC - Cyprus TCC"
 34 "MK - Macedonia/FYROM"
 35 "ME - Montenegro"
 36 "RS - Serbia"
 41 "NO - Norway"
 42 "CH - Switzerland"
 43 "IS - Iceland"
 44 "LI - Liechtenstein".

value labels
   CNTR_GB
 1 "FR - France"
 2 "BE - Belgium"
 3 "NL - The Netherlands"
 4 "DE-W - Germany West"
 5 "IT - Italy"
 6 "LU - Luxembourg"
 7 "DK - Denmark"
 8 "IE - Ireland"
 9 "GB - UNITED KINGDOM"
 10 "-"
 11 "GR - Greece"
 12 "ES -Spain"
 13 "PT - Portugal"
 14 "DE-E - Germany East"
 15 "-"
 16 "FI - Finland"
 17 "SE - Sweden"
 18 "AT - Austria"
 19 "CY - Cyprus (Republic)"
 20 "CZ - Czech Republic"
 21 "EE - Estonia"
 22 "HU - Hungary"
 23 "LV - Latvia"
 24 "LT - Lithuania"
 25 "MT - Malta"
 26 "PL - Poland"
 27 "SK - Slovakia"
 28 "SI - Slovenia"
 29 "BG - Bulgaria"
 30 "RO - Romania"
 31 "TR - Turkey"
 32 "HR - Croatia"
 33 "CY-TCC - Cyprus TCC"
 34 "MK - Macedonia/FYROM"
 35 "ME - Montenegro"
 36 "RS - Serbia"
 41 "NO - Norway"
 42 "CH - Switzerland"
 43 "IS - Iceland"
 44 "LI - Liechtenstein".

value labels
   NATION
 1 "FR - France"
 2 "BE - Belgium"
 3 "NL - The Netherlands"
 4 "DE - GERMANY"
 5 "IT - Italy"
 6 "LU - Luxembourg"
 7 "DK - Denmark"
 8 "IE - Ireland"
 9 "GB - UNITED KINGDOM"
 10 "-"
 11 "GR - Greece"
 12 "ES -Spain"
 13 "PT - Portugal"
 14 "-"
 15 "-"
 16 "FI - Finland"
 17 "SE - Sweden"
 18 "AT - Austria"
 19 "CY - Cyprus (Republic)"
 20 "CZ - Czech Republic"
 21 "EE - Estonia"
 22 "HU - Hungary"
 23 "LV - Latvia"
 24 "LT - Lithuania"
 25 "MT - Malta"
 26 "PL - Poland"
 27 "SK - Slovakia"
 28 "SI - Slovenia"
 29 "BG - Bulgaria"
 30 "RO - Romania"
 31 "TR - Turkey"
 32 "HR - Croatia"
 33 "CY-TCC - Cyprus TCC"
 34 "MK - Macedonia/FYROM"
 35 "ME - Montenegro"
 36 "RS - Serbia"
 41 "NO - Norway"
 42 "CH - Switzerland"
 43 "IS - Iceland"
 44 "LI - Liechtenstein".

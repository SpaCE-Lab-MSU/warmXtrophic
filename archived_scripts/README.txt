Nina Lany
June 2019

This directory contains the scripts for cleaning, harmonizing, and prepping all data from the wzrmXtropic project for analysis.

--The organizing logic is that scripts that begin with:

1_ clean and prep percent cover data, including the ‘greenup’ files that were stored separately from the percent cover files in 2015-2018 even though the data are collected in the same way. It is possible that not all taxa have a percent cover reported in the greenup files, which are indicated with a “1” in the greenup column in the L2 event table.

2_ clean and prep reproductive phenology data.

3_ clean and prep HOBO data

4_clean and prep percent herbivory data

5_ clean and prep foliar chemistry data

6_ clean and prep PAR and  other plot-level data

7_ make L3 data.

--functions.R contains functions for data checks and plotting. Source it at the beginning of a script to access the functions. 
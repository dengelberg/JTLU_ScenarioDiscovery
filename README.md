# JTLU_ScenarioDiscovery

Code and Data for Scenario Discovery paper in Journal of Transport and Land Use

This directory contains for R files and code for the scenario discovery experiment in the Engelberg (2024) JTLU publication. SILO output files are contained within each folder. Code is run in the following order

1. SiloResultsProcessor.R is a special function for processing raw results
2. fypAnalysis.R performs scenario discovery with full 100 lhs sample
3. comparingAlternatives.R is used to compare lhs to alternative samples

Raw data files from SILO is available on google drive https://drive.google.com/drive/folders/1sqoT_Grl9lcVE124mP5VbhcErX1qRzUp?usp=sharing. The files are numerous and difficult to upload to Github directly. SILO produces 8 outputs for every run and I save the parameter file, so full results include 4,500 files.

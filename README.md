# Blood Metabolite Profiling Detects Clinically Impactful Gut Dysbiosis Code and data for the manuscript: 

## Overview 
This repository contains analysis code for predicting gut microbiome composition and diversity from plasma metabolite profiles in three cohorts: 
acute myeloid leukemia (AML) patients, medical ICU patients with sepsis, and healthy donors.

## Data Availability

Shotgun metagenomics (NCBI SRA) [PRJNA1420396](https://www.ncbi.nlm.nih.gov/bioproject/PRJNA1420396)<br>
Targeted metabolomics AML [MSV000100516](https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?accession=MSV000100516)<br>
Targeted metabolomics MICU [MSV000100781](https://massive.ucsd.edu/ProteoSAFe/dataset.jsp?accession=MSV000100781)<br>
Processed data files used in the analysis are included in the `data/` folder.<br>
[![DOI](https://zenodo.org/badge/1151566234.svg)](https://doi.org/10.5281/zenodo.18716430)

## Usage 
Open `blood-metabolite-microbiome.Rproj` in RStudio. 
Scripts in `R_scripts/` are numbered in order of execution. 

## Requirements  
Analysis was performed using R 4.5.1, RStudio 2026.01.0+392 (2026.01.0+392), on Mac OS Tahoe 26.2.
Full session info and required packages list available in `sessionInfo.txt`.

## Citation 
If you use this code or data, please cite: 
``` [Citation pending publication] ``` 

## License 
MIT License 

## Contact
Ramanujam Ramaswamy
ramanujam.r03@gmail.com

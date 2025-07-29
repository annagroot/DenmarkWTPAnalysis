# Eliciting a Willingness-to-Pay Threshold in Denmark: Analysis of Published Reimbursement Decisions

This repository contains the data and R code underlying the analysis of willingness-to-pay (WTP) thresholds in Denmark, focusing on published reimbursement decisions.  
**This is an R Project** designed for reproducibility and accompanies a scientific publication.

## Overview

This project estimates and critically examines WTP thresholds for health interventions in Denmark, using published reimbursement decisions as data. All analysis is performed in R, and the workflow is fully reproducible.

## Repository Structure

- **DenmarkWTPAnalysis.Rproj**  
  The R Project file for use in RStudio.

- **/data/**
  - `data.RData` – Core dataset.
  - `0. data_prep.R` – Script for cleaning and preparing the dataset.
  - `data_ready.RData` – Prepared, analysis-ready data.

- **1. functions.R**  
  Custom R functions for the analysis (data cleaning, statistics, plotting).

- **2. analysis.R**  
  Main script to reproduce the analysis and tables/figures in the manuscript.

## Getting Started

1. **Open the R Project**

   - Download or clone this repository.
   - Open `DenmarkWTPAnalysis.Rproj` in RStudio.

2. **Install R dependencies:**  
   The analysis requires several R packages. Install required packages using:
   ```r
   install.packages(c("dplyr", "ggplot2", "pROC", "scales")) # List all required packages
   ```

3. **Run the analysis:**  
   Open `2. analysis.R` in RStudio or your preferred IDE, set the working directory to the repository root, and run the code.

## Data

All data used in this project are derived from publicly available assessments by the Danish Medicines Council:  
[https://medicinraadet.dk/](https://medicinraadet.dk/)

## Citation

If you use this code or data in your research, please cite the associated publication:

> [placeholder]

## Contact

For questions, suggestions, or contributions, please contact [Anna Grootendorst](mailto:anna.grootendorst@gmail.com) or open an issue in this repository.

---

**Note:**
This repository is intended for scientific and academic purposes. For commercial or policy-related inquiries, please contact the author directly.

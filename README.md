# Eliciting a Willingness-to-Pay Threshold in Denmark: Analysis of Published Reimbursement Decisions

This repository contains the data and R code underlying the analysis of willingness-to-pay (WTP) thresholds in Denmark, focusing on published reimbursement decisions.

## Overview

This project aims to estimate and critically examine WTP thresholds for health interventions in Denmark, utilizing published reimbursement decisions as a data source. The analysis supports transparency and reproducibility for scientific communication and is designed to accompany a scientific publication.

## Repository Structure

- **/data/**  
  Contains the core dataset used for the analyses, alongside codings for primary, secondary, and sensitivity analyses.  
  - `data.RData`
  - `0. data_prep.R`
  - `data_ready.RData`

- **1. functions.R**  
  Custom R functions to facilitate and streamline the analysis process (e.g., data cleaning, statistical summaries, plotting).

- **2. analysis.R**  
  The main script to reproduce the core analyses reported in the manuscript. This script loads the data, applies pre-processing, executes the statistical analysis, and generates outputs (tables, figures).

## How to Reproduce the Analysis

1. **Clone the repository:**
   ```sh
   git clone https://github.com/annagroot/DenmarkWTPAnalysis.git
   cd DenmarkWTPAnalysis
   ```

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

# CPS Case Outcomes Analysis (England & Wales)

?m1e9l6e5k?

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)
[![Made with R](https://img.shields.io/badge/Made%20with-R-1f425f.svg)](https://www.r-project.org/)

This repository presents a comprehensive data science analysis on criminal case outcomes from the Crown Prosecution Service (CPS), focusing on patterns in convictions and unsuccessful prosecutions using open government data.

## 🗂 Project Structure

├── cps_case_outcomes_analysis.R # Main R script for data processing, analysis, and modeling

├── cps_case_outcomes_analysis_report.pdf # Final academic report (PDF format)

├── install_packages.R # Script to install required R libraries

├── README.md # Project overview and documentation

├── LICENSE # MIT License

└── .gitattributes # Git metadata and encoding rules

## 📊 Project Summary

The analysis applies a full data science pipeline in **R**, including:
- Data integration and cleaning of monthly datasets (2014–2015)
- Feature engineering and handling of missing values
- Descriptive statistics and visualization
- Hypothesis testing (ANOVA, Kruskal-Wallis)
- Predictive modelling (OLS, Ridge, Lasso)
- Clustering (K-Means, Hierarchical, DBSCAN)
- Classification models (Random Forest, Logistic Regression, XGBoost)

## 📊 Key Results Summary

- 📈 **Lasso Regression** explained 90.5% of variance in drug convictions.
- 🔍 **DBSCAN** achieved the best clustering quality with a silhouette score of 0.886.
- 📉 **Logistic Regression** outperformed others in predicting homicide conviction with Macro F1 = 0.625.
- 📍 Regional disparities are significant, especially in London and high-population areas.

## 📏 Evaluation Metrics

- **Regression**  
  - Lasso Regression: R² = 0.905, RMSE = 47.9  
  - OLS Regression: R² = 0.845, RMSE = 44.3  

- **Clustering**  
  - DBSCAN: Silhouette Score = 0.886  
  - K-Means: Silhouette Score = 0.85  
  - Hierarchical: Silhouette Score = 0.35  

- **Classification** (Homicide Conviction Prediction)  
  - Logistic Regression: Macro F1 Score = 0.625  
  - Random Forest: Macro F1 Score = 0.620  
  - XGBoost: Macro F1 Score = 0.620  

---

## 📈 Key Highlights

- Significant regional disparity in homicide and drug offence convictions
- DBSCAN performed best among clustering methods (Silhouette Score: 0.886)
- Lasso regression achieved the highest R² (0.905) in drug offence prediction
- Macro F1-scores around 0.62 in homicide classification tasks
- Extensive use of time-series and region-based analysis

  ---

## 📁 Dataset

- Source: [data.gov.uk – CPS Outcomes by Principal Offence Category](https://www.data.gov.uk/dataset/89d0aef9-e2f9-4d1a-b779-5a33707c5f2c/crown-prosecution-service-case-outcomes-by-principal-offence-category-data)
- License: Open Government Licence v3.0 (OGL)
- Period: January 2014 – December 2015

Contains public sector information licensed under the Open Government Licence v3.0.
## 🧪 Experimental Setup

All experiments were conducted on:
- **Device:** Acer Aspire A315-44P  
- **Processor:** AMD Ryzen 5 5500U (12 threads, ~2.1GHz)  
- **RAM:** 16 GB  
- **OS:** Windows 11 Pro (64-bit)

## 📚 R Libraries Used

The project relies on several R packages for data manipulation, visualization, statistical analysis, and machine learning, including:

- `tidyverse`, `dplyr`, `ggplot2`, `reshape2`, `caret`, `broom`, `GGally`, `ggpubr`
- `glmnet`, `cluster`, `factoextra`, `dbscan`, `randomForest`, `xgboost`, `fmsb`

To install all required packages in one step, you can run:

```r
source("install_packages.R")
```

## ▶️ How to Run

1. Clone the repository

```bash
git clone https://github.com/onurtuncay/CPS-Case-Outcomes-Analysis.git
```

2. Open the R script in RStudio

3. Install required packages:

```r
source("install_packages.R")
```

4. Run the main script (after changing dataset directory):

```r
source("cps_case_outcomes_analysis.R")
```

## 📄 Report

📥 The full academic report is available as a downloadable PDF in this repository.  
It includes detailed methodology, data preparation steps, statistical analysis, model evaluation, and references in an academic format.

👉 [Download the PDF report](./cps_case_outcomes_analysis_report.pdf)

---

## 📝 License

This project is licensed under the MIT License. The dataset is used under the Open Government License.

## 👤 Author

**Onur Tuncay**  
MSc Data Science – University of Gloucestershire  
Senior Data Scientist | LinkedIn: [onur-tuncay](https://linkedin.com/in/onur-tuncay)



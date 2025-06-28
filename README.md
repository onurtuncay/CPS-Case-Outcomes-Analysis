# CPS Case Outcomes Analysis (England & Wales)

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
  

## 📁 Dataset

- Source: [data.gov.uk – CPS Outcomes by Principal Offence Category](https://www.gov.uk/government/statistics)
- License: Open Government License (OGL)
- Period: January 2014 – December 2015

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


## 📈 Key Highlights

- Significant regional disparity in homicide and drug offence convictions
- DBSCAN performed best among clustering methods (Silhouette Score: 0.886)
- Lasso regression achieved the highest R² (0.905) in drug offence prediction
- Macro F1-scores around 0.62 in homicide classification tasks
- Extensive use of time-series and region-based analysis

## 📄 Report

📥 The full academic report is available in this repository as a PDF for offline access and easy review.

## 📝 License

This project is licensed under the MIT License. The dataset is used under the Open Government License.

## 👤 Author

**Onur Tuncay**  
MSc Data Science – University of Gloucestershire  
Senior Data Scientist | LinkedIn: [onur-tuncay](https://linkedin.com/in/onur-tuncay)

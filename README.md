# CPS Case Outcomes Analysis (England & Wales)

This repository presents a comprehensive data science analysis on criminal case outcomes from the Crown Prosecution Service (CPS), focusing on patterns in convictions and unsuccessful prosecutions using open government data.

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

Libraries include `dplyr`, `ggplot2`, `tidyr`, `lubridate`, `car`, `cluster`, `caret`, `xgboost`, and others for statistical analysis and machine learning.

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

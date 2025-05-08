## Data Analysis and Machine Learning Modeling for Cybersecurity Event Classification

This repository contains my work for a data analysis and machine learning assignment. The dataset used in this analysis simulates cybersecurity event data from a healthcare organization, FauxCura, with the goal of identifying malicious events from a large number of logged network data.

## Background
FauxCura Health is concerned about undetected cybersecurity breaches within their systems. They used Splunk, a Security Information and Event Management (SIEM) platform, to collect vast amounts of log data. This data is then used for anomaly detection and breach detection. The dataset provided for this task contains historical logs of network activity, including both normal and malicious events.

How the given dataset looked like:

[![image.png](https://i.postimg.cc/cL1Cp1Ps/image.png)](https://postimg.cc/HVKT5mpK)

## Objectives
The primary objective of this project is to clean and prepare the data for machine learning and then use supervised learning models to classify malicious network events. The project involved:

- Cleaning and preparing the dataset for analysis
- Developing machine learning models for classification of malicious vs. normal events
- Evaluating model performance using metrics like accuracy, precision, recall, F1-score, and AUC

## Data Preparation and Cleaning
### Steps Involved:
1. **Mislabeled Categories**: Merged inconsistent categories in the `NetworkEventType` and `AlertCategory` features to improve consistency.
2. **Extreme Values**: Handled extreme outlier values in `NetworkAccessFrequency` and `ResponseTime` features by replacing them with `NA` values.
3. **Omitting Missing Values**: Removed rows with missing values (NA) in critical features to prevent issues during model training.
4. **Feature Engineering**: Combined categories within the `NetworkInteractionType` feature to create an "Others" category, reducing model complexity.

The final cleaned dataset consists of 494,685 rows with 13 features, ready for training machine learning models.

## Machine Learning Algorithms
### Models Evaluated:
1. **Logistic Lasso Regression**: Applied to both balanced and unbalanced datasets with hyperparameter tuning.
2. **Bagging Tree (Random Forest)**: Evaluated on both balanced and unbalanced datasets, with grid search for optimal hyperparameters.

Each model was trained and evaluated using 10-fold cross-validation, and performance was measured using metrics such as accuracy, precision, recall, F1-score, and AUC.

### Results:
- **Logistic Lasso Regression** (Balanced Data):
  - Accuracy: 88.04%
  - Recall: 88.91%
  - Precision: 23.63%
  - F1 Score: 37.33%
  - AUC: 92.83%

- **Bagging Tree** (Balanced Data):
  - Accuracy: 94.05%
  - Recall: 30.58%
  - Precision: 27.89%
  - F1 Score: 29.17%
  - AUC: 91.37%

## Conclusion and Recommendations
Based on the evaluation metrics, the **Logistic Lasso Regression** model trained on balanced data is recommended due to its higher performance in detecting malicious events (high recall and AUC). The unbalanced models, while achieving high accuracy, failed to detect malicious events effectively, as they were biased towards the majority class.

## Files in the Repository:
- **R Code**: The R script for data preparation, cleaning, and machine learning modeling is included in this repository.

The raw data and detailed report of this assignment is excluded due to privacy concerns.

## How to Run the Code:
1. Clone the repository to your local machine.
2. Install the necessary R packages by running `install.packages()` for packages like `caret`, `dplyr`, `ggplot2`, etc.
3. Run the R script to reproduce the data cleaning, model training, and evaluation steps.

## License:
This project is for educational purposes only and is not licensed for commercial use.

## Acknowledgments:
The data used in this assignment is fictional and was provided for the purpose of this project.

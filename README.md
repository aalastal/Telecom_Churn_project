# Telecom Customer Churn Project
This project completes part of the final assessment in HarvardX's Professional Certificate in Data Science. 

The data we will use is part of Kaggle competition https://www.kaggle.com/radmirzosimov/telecom-users-dataset  Dataset includes churn data from the telecom Operator. The dataset has 22 variables and 5,987 observations.
The main object is to predect the customers with a high probability of churn. We will analysis the dataset and focuses on the behavior of telecom customers who are more likely to leave the platform, and then we will use several techniques of Machine Learning to build a model focused on maximizing the true predictions of customers that will stay with the company. 
We will use various technical performance measures to compare the models with each other on the basis of : accuracy, sensitifity, specificity, MCC, ROC curve, AUC ...

The machine-learning models which constructed to  predicts whether customer is at risk of churn are: Logistic Regression, k-Nearest Neighbours, Decision Tree and Random Forest.

## Graded Files

### `Telecom_code.R`
This is the main R scrips which loads and visualises the data and constructs the models. this script requires that the file `telecom_users.csv` is in the working (project) directory. The data is included in this repo and relative file paths (in line with the repo) are used to load the data.

### `Telecom_Report.rmd`
This is the final report in .Rmd form. The files which are loaded are created in `Telecom_code.R`. Because the script takes a while to run, all of the R objects and images required to run this report are included in the folder `save_files`. This folder must be present in the working (project) directory. Again, relative file paths in line with this repo are used to load the files. This report is also available through [this RPubs link](https://https://github.com/aalastal/Telecom_Churn_project).

### `Telecom_Report.pdf`
This is the final report in PDF format.


## Extra Files

# NHANES_Activity_Phenotypes_Analysis
R code and analytical workflow for the study: "Beyond Aggregate Volume: Accelerometer-Derived Activity Phenotypes Reveal a Decoupling of Lean Mass and Function: A Cross-Sectional Study".
```markdown
# Analysis Code for: Beyond Aggregate Volume: Accelerometer-Derived Activity Phenotypes Reveal a Decoupling of Lean Mass and Function

This repository contains the R source code and analytical workflow for the study titled **"Beyond Aggregate Volume: Accelerometer-Derived Activity Phenotypes Reveal a Decoupling of Lean Mass and Function"**.

This study utilizes data from the **National Health and Nutrition Examination Survey (NHANES) 2011-2014** to identify accelerometer-derived physical activity phenotypes using unsupervised machine learning (K-Means clustering) and examines their associations with appendicular lean mass index (ALMI) and grip strength.

## 📂 Repository Structure

* `NHANES_Activity_Phenotypes_Analysis.R`: The main R script containing the complete analytical pipeline (Data cleaning, K-Means clustering, Regression models).
* `data/`: (Optional) Folder containing the processed datasets. *Note: If you cannot share the data directly due to size, please describe how to download it from the CDC website below.*
* `figures/`: Generated plots (e.g., Cluster visualizations, Forest plots).

## 🛠 Prerequisites & Dependencies

To run this code, you need **R** and **RStudio** installed. The analysis relies on the following R packages. Please ensure they are installed before running the script:

```r
install.packages(c("tidyverse", "survey", "factoextra", "cluster", "gridExtra"))

```

## 🚀 Usage Instructions (Important!)

To ensure the script runs correctly, you must strictly follow the directory setup steps below.

### Step 1: Prepare Your Data

Ensure your NHANES data files (e.g., `.XPT` or `.csv` files) are stored in a specific folder on your local machine. Let's call this folder `data`.

### Step 2: Set Working Directory (CRITICAL)

**Before running any code**, you must set your RStudio working directory to the location where your data is stored.

1. Open `NHANES_Activity_Phenotypes_Analysis.R` in RStudio.
2. Go to the top menu: **Session** -> **Set Working Directory** -> **Choose Directory...**
3. Select the folder where your data files are located.

Alternatively, you can manually run the following command in the R console (replace the path with your actual folder path):

```r
# Example:
setwd("C:/Users/YourName/Documents/Research/NHANES_Project/data")

```

**Note:** The script assumes that all data files are located in the current working directory.

### Step 3: Run the Analysis

Once the working directory is set, you can execute the script line-by-line or source the entire file.

## 📊 Analysis Workflow

The script is organized into the following sections:

1. **Data Import & Merging:** Combining Demographics, Examination (DXA, Grip Test), and Accelerometry data from NHANES 2011-2014 cycles.
2. **Feature Engineering:** Calculating Gini Index, Average Sedentary Bout Length, and extracting fPCA features.
3. **Clustering:** Unsupervised K-Means clustering to derive "High-Volume/Consolidated" (HVC) and "Low-Volume/Fragmented" (LVF) phenotypes.
4. **Statistical Modeling:** Survey-weighted linear regression (using the `survey` package) to assess associations with ALMI and Grip Strength.
5. **Model Comparison:** AIC comparison between Phenotype-based models and Traditional Volume-based models.

## 📝 Citation

If you use this code or methodology in your research, please cite our paper:

> [Insert Authors]. Beyond Aggregate Volume: Accelerometer-Derived Activity Phenotypes Reveal a Decoupling of Lean Mass and Function: A Cross-Sectional Study. *[Journal Name]*. [Year].

## 📜 License

This project is licensed under the MIT License - see the LICENSE file for details.

---

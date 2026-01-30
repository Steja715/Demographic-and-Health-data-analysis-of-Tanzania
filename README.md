#  Determinants of Childhood Malnutrition in Tanzania (DHS 2022)
![R](https://img.shields.io/badge/Language-R-blue) ![DHS](https://img.shields.io/badge/Data-Demographic_Health_Survey-orange) ![Status](https://img.shields.io/badge/Analysis-Survey_Weighted_GLM-green)

>  Using data from the **2022 Tanzania Demographic and Health Survey (TDHS)**, this study investigates the socio-economic drivers of childhood stunting, underweight, and wasting. The analysis applies **survey-weighted logistic regression** to identify vulnerable populations among children under five.

---

## 📊 1. Key Findings: The Wealth Gradient
*The prevalence of malnutrition is starkly stratified by household wealth.*

<p align="center">
  <img src="Rplot04.png" alt="Prevalence by Wealth" width="85%">
  <br>
  <em>Figure 1: Prevalence of Stunting (Green), Wasting (Red), and Underweight (Blue) by Wealth Index.</em>
</p>

### 🧐 Epidemiological Insight
> **The Poverty Penalty:**
> * **Stunting:** Children from the **Poorest** households have a **37.3%** prevalence of stunting, compared to only **15.2%** in the Richest households.
> * **Gradient Effect:** There is a clear dose-response relationship; as wealth increases, malnutrition outcomes linearly decrease.

---

## 🌍 2. Demographic Disparities (Urban vs. Rural)
*Comparing maternal education levels across residential settings.*

<p align="center">
  <img src="Rplot01.png" alt="Maternal Education" width="85%">
  <br>
  <em>Figure 2: Distribution of Maternal Education by Residence (Urban vs. Rural).</em>
</p>

> **Observation:**
> * **Educational Gap:** In rural areas (Teal), **25.9%** of mothers have no formal education, compared to only **8.3%** in urban areas.
> * **Impact:** Maternal education is a known protective factor against child malnutrition; this disparity likely contributes to the higher stunting rates observed in rural Tanzania (31.65%) vs. Urban (21.55%).

---

## 📈 3. Multivariate Analysis (Adjusted Odds Ratios)
*Identifying significant risk factors while controlling for confounders.*

<p align="center">
  <img src="Rplot10.png" alt="Regression Table" width="95%">
  <br>
  <em>Table 1: Multivariable Logistic Regression Models for Stunting, Underweight, and Wasting (Adjusted Odds Ratios).</em>
</p>

### 📝 Clinical Interpretation
The weighted logistic regression model reveals three critical determinants:
1.  **Male Vulnerability:** Male children have significantly higher odds of all three malnutrition outcomes compared to females (**aOR for Stunting: 1.52**, 95% CI: 1.28–1.80).
2.  **Socio-economic Status:** Being in the "Poorest" wealth quintile triples the odds of stunting (**aOR: 3.26**) compared to the "Richest" reference group.
3.  **Protective Care:** Mothers who attended **At least 4 Antenatal Care (ANC)** visits had significantly lower odds of having a stunted child (**aOR: 0.82**, p=0.027).

---

## 🛠️ Methodology
This analysis followed a rigorous data science pipeline using **R (v4.x)**. The workflow was divided into three stages: Data Wrangling, Statistical Modeling, and Visualization.

### 🔹 1. Data Cleaning & Variable Modification
Raw DHS data was processed using the `tidyverse` and `haven` packages to create analysis-ready variables:
* **Recoding:** Categorical variables were harmonized (e.g., Maternal Education `v106` re-grouped into *No Education, Primary, Secondary, Higher*).
* **Variable Transformation:**
    * **Stunting/Wasting:** Converted continuous Z-scores (`hw70`, `hw72`) into binary outcome variables (Stunted = HAZ < -2 SD).
    * **Wealth Index:** Preserved the original 5-level quintile structure (`v190`) for socio-economic gradient analysis.
* **Missing Data:** Handled implicitly by excluding cases with missing anthropometric data or "Flagged" Z-scores.

### 🔹 2. Statistical Analysis
All analyses accounted for the **Complex Survey Design** (Stratification, Clustering, and Sampling Weights) using the `survey` package.

#### **A. Descriptive Statistics**
* **Weighted Proportions:** Estimated the national prevalence of malnutrition outcomes.
* **Bivariate Analysis:** Chi-square tests (Rao-Scott corrected) were used to test associations between socio-demographic factors and malnutrition.

#### **B. Regression Modeling**
Two types of models were fitted to assess determinants:
1.  **Survey-Weighted Logistic Regression (`svyglm`):** Used for binary outcomes (e.g., Stunting Yes/No) to calculate **Adjusted Odds Ratios (aOR)**.
2.  **Survey-Weighted Linear Regression:** Used for continuous outcomes (e.g., Height-for-Age Z-scores) to assess the linear relationship between maternal BMI and child growth.



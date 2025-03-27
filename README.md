# 📊 An Introduction to Regression Models in R

### Developed by: Mohammad Hassan Darabi  
🎓 Bachelor's Degree in Data Analytics  
🏛️ University of Campania Luigi Vanvitelli, Italy  
🗓️ Academic Year: 2022-2023

---

## 🚀 Project Overview

This project presents a hands-on introduction to linear regression techniques using the R programming language. It is designed to demonstrate simple and multiple linear regression concepts, diagnostic methods, variable transformations, and assumption checking. The analysis is grounded in real-world datasets to give practical context to regression modeling theory.

---

## 📂 Datasets Used

### 1. **Income1.csv**  
A synthetic dataset containing information about **Education** and **Income**. Used to explore simple regression models, model interpretation, and diagnostic plots.

### 2. **cars** (built-in R dataset)  
Analyzed to examine regression between **speed** and **stopping distance**.

### 3. **marketing** (from `datarium` package)  
Contains advertising spending across three channels (**facebook**, **newspaper**, **youtube**) and resulting **sales**. Used for multiple regression modeling, variable interaction, and assumption testing.

---

## 🌐 Project Structure

### Part 1: Simple Linear Regression
- Fit a basic linear model: `lm(Income ~ Education)`
- Visualize regression lines and residuals
- Interpretation of fitted values, coefficients, and predictions
- Exercise using the `cars` dataset
- Compare models with and without intercept
- Understand model performance using R² and diagnostic plots

### Part 2: Multiple Linear Regression
- Load and explore the `marketing` dataset
- Analyze relationships between predictors and the outcome
- Build additive and interaction models:
  - `sales ~ newspaper`
  - `sales ~ newspaper * facebook`
  - `sales ~ youtube + facebook + newspaper`
- Plot residuals and leverage diagnostics

### Part 3: Assumption Testing
- **Linearity:** Checked via `crPlots()`
- **Normality of residuals:**
  - Visual (QQ plot)
  - Statistical tests: Shapiro-Wilk, Anderson-Darling, Jarque-Bera, Lilliefors
- **Homoscedasticity:**
  - Breusch-Pagan test (`lmtest::bptest()`)
- **Independence of residuals:**
  - ACF plots
  - Runs test (`lawstat::runs.test()`)
  - Durbin-Watson test (`lmtest::dwtest()`)

---

## 📈 Key Learnings
- Understand how to build, interpret, and evaluate simple and multiple regression models in R
- Perform and visualize residual diagnostics
- Apply transformations and model variations
- Check and interpret model assumptions
- Gain insight into practical regression analysis on real datasets

---

## 🔧 Packages Used
- `base`, `car`, `datarium`, `pastecs`, `ggplot2`, `ggpubr`, `dplyr`, `lmtest`, `tseries`, `nortest`, `lawstat`

Install packages using:
```r
install.packages(c("car", "datarium", "pastecs", "ggplot2", "ggpubr", "dplyr", "lmtest", "tseries", "nortest", "lawstat"))
```

---

## 📄 How to Run
1. Clone this repository.
2. Open R or RStudio.
3. Ensure all required packages are installed.
4. Run each script sectionally or as part of a full regression walkthrough.
5. Follow inline comments and visuals to understand each stage.

---

## 🌟 Acknowledgments
This project was completed as part of the "Statistical Learning" course under the supervision of academic mentors at the University of Campania Luigi Vanvitelli.

---

## 🙌 License
This project is open-source and licensed under the MIT License.


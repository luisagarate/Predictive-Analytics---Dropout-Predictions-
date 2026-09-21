# Predictive-Analytics---Dropout-Predictions-

## Why This Project?

Universities should care about their dropout rates because:

- Decline in rankings and reputation
- Loss of tuition revenue
- Negative outcomes for students

Predictive models can identify at-risk students early so universities can provide support and reduce dropout rates.

---

## Question and Hypothesis

### Research Question

To what extent can demographic, socioeconomic, macroeconomic, and academic factors predict student dropout in higher education?

### Hypothesis

Students facing unfavorable demographic, socioeconomic, and macroeconomic conditions are more likely to drop out.

---

## Dataset Overview

The data comes from the **Polytechnic Institute of Portalegre** and covers the years **2008–2019**. It was obtained from the **UCI Machine Learning Repository**.

The dataset contains:

- **4,424 undergraduate students**
- **36 variables**
- Demographic factors
- Socioeconomic and macroeconomic factors
- Academic factors

---

## Dataset Cleaning

The following preprocessing steps were completed:

- Removed the **“enrolled”** category
- Converted the target into a binary classification problem
- Removed variables that directly influenced the dropout rate, including:
  - Grades
  - Units taken
  - Prerequisites
  - Special needs

---

## Descriptive Statistics

## Indirect Variable Logistic Regression

### Model Fit

- **Deviance reduction:** 14.8%
- **Benchmark:** Stronger fit than categorical models

| Model Category | Deviance Reduction |
|---|---:|
| Demographic | 9.1% |
| Socioeconomic | 5.9% |
| Academic | 4.7% |
| Macroeconomic | 0.1% |

### Significant Variables

| Variable | Coefficient | P-value | Category |
|---|---:|---:|---|
| Application Mode | -0.015 | <0.001 | Academic |
| Daytime/Evening Attendance | -0.388 | 0.048 | Academic |
| Debtor | -1.677 | <0.001 | Socioeconomic |
| Gender | -0.893 | <0.001 | Demographic |
| Age at Enrollment | -0.063 | <0.001 | Demographic |
| Unemployment Rate | 0.049 | 0.028 | Macroeconomic |
| GDP | 0.054 | 0.039 | Macroeconomic |

### Other Semi-Significant Factors

Using group-based regression at a **0.05 alpha level**, another semi-significant factor was:

- Mother’s qualification

The logistic regression identifies socioeconomic and demographic factors as the strongest predictors of dropout in terms of indirect causes.

---

## Predictors of Student Dropout

The decision tree suggests that demographic and socioeconomic factors are the strongest predictors of student dropout, particularly:

- Age at enrollment
- Gender
- Debtor status

---

## Prediction Models

The performance of the logistic regression model and decision tree was compared using recall and AUC.

| Model | Recall | AUC |
|---|---:|---:|
| Logistic Regression | 0.86 | 0.75 |
| Decision Tree | 0.77 | 0.69 |

The logistic regression model performed better than the decision tree based on both recall and AUC.

### High-Risk Student Identification

The model:

1. Calculates a dropout probability for each student.
2. Flags students with a dropout probability greater than **70%** as high risk.

The logistic regression model identifies **75 currently enrolled students** at high risk of dropping out, demonstrating how predictive analytics can help universities intervene early and improve student retention.

---

## Limitations

- The dataset may not be representative of students in other countries.
- Other external factors may also affect dropout rates, including:
  - Mental health
  - Motivation
  - Emergencies
- Early academic performance may influence student outcomes.
- Indirect variables may not be as strong as direct variables.

---

## Conclusion and Recommendations

### Expand Financial Support

- Scholarships
- Monthly payment options

### Provide Early Academic Support

- Early academic advising
- Tutoring

### Develop Mentorship and Engagement Programs

- Alumni outreach
- Programs for and by people of different backgrounds

### Use Predictive Analytics

- Identify high-risk students
- Intervene before students drop out
- Continue accounting for indirect variables

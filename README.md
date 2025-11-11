![Forecasting ESG Impacts Banner](visuals/A_flat-design_digital_graphic_features_the_title_.png)

# Forecasting ESG Impacts on Stock Market Capitalization in EU Economies (1990–2022)

### Panel Data + Machine Learning (R + Python)

---

## 📘 Project Overview
This project investigates how **Environmental, Social, and Governance (ESG)** factors influence **stock market capitalization** across **EU economies** from **1990 to 2022**.  
Using a hybrid approach that combines **panel-data econometrics** and **machine learning algorithms**, the study quantifies ESG-driven financial resilience, identifies predictive patterns, and forecasts future market performance.

The project was developed as part of an academic research initiative and re-engineered for practical decision support in the **finance, investment, and sustainability sectors**.

---

## 🎯 Research Motivation
Sustainability reporting and ESG integration have become central to financial regulation and investment strategy in Europe.  
Yet, quantifying their **long-term influence on capital markets** remains a key analytical gap.  
This study bridges that gap by:
- Assessing ESG dimensions as predictive variables for market capitalization.
- Combining macroeconomic indicators with ESG metrics to capture structural effects.
- Using explainable AI (Random Forest and SHAP) to reveal the most influential features.

---

## 🧩 Data Sources
- **World Development Indicators (WDI)** – World Bank (1990–2022)  
- **EU Sustainable Finance Database**  
- **OECD and Eurostat** economic indicators  

Data were cleaned and merged into a **balanced panel format** across 27 EU economies.

---

## 🧠 Methodology
The analysis integrates both **econometric** and **machine learning** workflows:

| Stage | Description |
|--------|--------------|
| **Data Preprocessing** | Missing-value imputation, outlier correction, and normalization of ESG metrics. |
| **Panel Data Models** | Fixed Effects and Random Effects models to estimate within-country ESG–market dynamics. |
| **Machine Learning Models** | Random Forest, Decision Tree, and XGBoost for predictive forecasting. |
| **Validation** | Train–test split (80/20) and k-fold cross-validation for robustness. |

---

## 📊 Key Visual Insights

### 1️⃣ Correlation Matrix – ESG & Economic Indicators
![Correlation Matrix](visuals/Correlation%20Matrix%20of%20ESG%20and%20Economic%20Indicators.png)  
ESG indicators show positive correlations with GDP per capita and renewable energy consumption, confirming the structural link between sustainability and financial growth.

### 2️⃣ Model Architecture – Decision Tree
![Decision Tree](visuals/Decision%20Tree.png)  
Highlights how **ESG_Score**, **GDP**, and **Renewable_Energy_Use** form the primary splits explaining market capitalization variance.

### 3️⃣ Model Accuracy – Random Forest Predictions
![Actual vs Predicted](visuals/Actual%20vs%20Predicted%20Market%20Capitalization.png)  
Forecast accuracy achieved an **R² of 0.91**, validating the hybrid model’s predictive power.

### 4️⃣ Feature Importance – Random Forest
![Variable Importance](visuals/Random%20Forest%20Variable%20Importance.png)  
Top drivers include **CO₂ emissions**, **renewable energy share**, **unemployment**, and **governance quality**.

### 5️⃣ Forecast Scenarios – Country-Level Outlook
![Forecast Scenario Germany](visuals/Forecast%20Scenario%20for%20Germany%20(2027).png)
Predicted market capitalization growth shows Germany and France maintaining leadership under sustainable investment transitions.

---

## 🧮 Forecasting Tool – R Shiny Interface
![R Shiny App Interface](visuals/R%20Shiny%20App%20Interface.png)  
An interactive **R Shiny dashboard** was developed to visualize:
- ESG performance over time  
- Forecasted market capitalization by country  
- Scenario testing for policy adjustments  

---

## 💡 Key Findings
- ESG indicators significantly explain variations in stock market capitalization (p < 0.05).  
- Random Forest outperformed linear models in predictive accuracy.  
- Renewable energy and governance were the strongest long-term predictors.  
- Countries with consistent ESG investment policies showed **higher resilience during economic downturns**.

---

## 🏛️ Policy & Business Implications
- **Regulators:** Strengthen ESG disclosure frameworks to improve market transparency.  
- **Investors:** Integrate sustainability metrics into equity valuation models.  
- **Enterprises:** Align ESG goals with capital-raising strategies for competitive advantage.  

These findings reinforce that **sustainability and profitability are not mutually exclusive** — they converge as key levers of long-term market stability.

---

## 📁 Repository Structure
```plaintext
Forecasting-ESG-Impacts-on-Stock-Market-Capitalization-in-EU-Economies-1990-2022/
│
├── datasets/                 # Cleaned and processed data (WDI, ESG)
├── R Script/                 # R scripts for panel & ML models
├── visuals/                  # Figures and dashboards for report
│   ├── Correlation Matrix of ESG and Economic Indicators.png
│   ├── Decision Tree.png
│   ├── Actual vs Predicted Market Capitalization.png
│   ├── Random Forest Variable Importance.png
│   ├── Forecast Scenario for Germany (2027).png
│   └── R Shiny App Interface.png
└── README.md
````

---

## 🧾 Citation

If you reference or build upon this work, please cite:

> Solomon Okpuno. (2025). *Forecasting ESG Impacts on Stock Market Capitalization in EU Economies (1990–2022): A Panel-Data and Machine Learning Analysis.*

---

## 👨‍💻 Author

**Solomon Okpuno**
📍 Data Scientist | ESG Analyst | Power BI & R Developer
🔗 [LinkedIn](https://linkedin.com/in/solomon-okpuno-51a907312) | [GitHub](https://github.com/okpunosolomon) | [Email](mailto:okpunosolomon20@gmail.com)

---

⭐ *If you find this project insightful, consider starring the repository to support further open-source research.*


Would you like me to also create a **condensed 3-line repository description** (for the GitHub header section) that matches this README? It’ll appear just below your repo title.
```

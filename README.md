# 🛂Time-Series-Forecasting-of-Daily-Passenger-Flow

# 📌Overview

This repository contains a time series forecasting project for daily passenger flow at five major cross-border ports in the Greater Bay Area:

Hong Kong International Airport (HKIA)

Hong Kong–Zhuhai–Macao Bridge (HZMB)

Lo Wu (LoWu)

Lok Ma Chau Spur Line (LMC)

Shenzhen Bay (SZBay)

The project applies multiple forecasting models to historical passenger data to predict short-term and long-term flows, helping evaluate model accuracy and produce actionable forecasts.


# ⚙️Requirements

R >= 4.0

Packages: tidyverse, lubridate, forecast, Metrics, purrr, readxl

Install required packages in R:

install.packages(c("tidyverse", "lubridate", "forecast", "Metrics", "purrr", "readxl"))

# ▶️Usage

Clone the repository

git clone https://github.com/yourusername/passenger-flow-forecast.git
cd passenger-flow-forecast


Place your Excel files

Training files in data_training/

Testing/validation files in data_testing/

Run the forecasting script

source("Time_Series_Forecasting.R")


Outputs generated in outputs/

Short-term evaluation plots (8-day forecasts vs actuals)

MAPE evaluation table for model comparison

Long-term (30-day) forecasts per port

Combined forecast plots

# 📄Output Files
File/Folder	Description
outputs/plots/	PNG plots of short-term evaluation and long-term forecasts
outputs/forecast_results.csv	30-day forecasts for all ports
outputs/evaluation_summary.csv	MAPE values for each model per port

# 📝License

MIT License

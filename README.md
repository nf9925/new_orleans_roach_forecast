Forecasting Cockroach Search Interest in New Orleans 🪳

New Orleans may have four seasons: winter, spring, roach, and fall. After a month of cockroach battles in a new apartment, I tested that idea with data.

This project forecasts monthly Google search interest for "cockroach" in the New Orleans metropolitan area, using time series models in R. It also tests whether hot or rainy months bring more searches than usual for their time of year.


<img width="4800" height="1800" alt="forecast_to_2029_monthly" src="https://github.com/user-attachments/assets/b19aa5de-d0ca-4cd3-9791-958cf0573345" />


Why it matters

Cockroaches are a public health concern, not only a nuisance. Their allergens are a known asthma trigger, they can carry bacteria such as Salmonella and E. coli onto food, and infestations bring stress, lost sleep, and pesticide exposure. In the 2015 American Housing Survey, New Orleans had the highest share of households reporting cockroaches among 25 large metropolitan areas. Predictable timing means prevention and outreach can start before each summer peak.

Key findings
Roach season is real and punctual. Search interest peaks every July and reaches its low every December. A typical July brings more than twice the searches of a typical December.
A measurement change matters. Google changed its data collection in January 2022, and the series jumps at that point. The models include a step variable for the change instead of treating it as a cockroach explosion.
Models beat the benchmark. ETS and regression with ARIMA errors reduced typical forecast errors by about 24% compared with a "same as last year" benchmark, across 15 rolling cross validation windows.
Weather barely matters. A summer 3°F warmer than normal shifts searches by about 3%, and the data cannot rule out zero. Heavy rain showed no association.
The forecast. July peaks between 76 and 84 on Google's 0 to 100 scale through 2029, with wide uncertainty.
Important caveat

Search interest measures public attention to cockroaches, not the number of infestations. People who live with cockroaches for years may search less, and newcomers may search more.

Methods
Monthly Google Trends data, January 2016 to September 2026, for the term "cockroach" (New Orleans metro, US-LA-622)
One month reported as 0 (December 2024) treated as a sampling artifact and imputed with a seasonal ARIMA model
Exploration with seasonal plots, STL decomposition, and a log transformation
Temperature anomalies built from daily weather, using training months only to avoid data leakage
Rainfall tested with heavy rain days before any modeling
Four models compared on a 24 month test period and with time series cross-validation: seasonal naive, ETS, and two regressions with ARIMA errors
Forecast to August 2029 with 80% prediction intervals, plus a temperature sensitivity check

Tools

R, RStudio, tsibble, fable, feasts, ggtime, ggplot2, Quarto

Data sources
Search data: Google Trends (trends.google.com), retrieved October 7, 2026
Weather data: Open-Meteo.com, historical weather archive (CC BY 4.0)
Housing statistic: U.S. Census Bureau, American Housing Survey, 2015
Full report

A full PDF report with code, plots, and every modeling decision is coming soon.

**📄Master's Thesis Time Series: Evaluating the predictive ability of BigVAR models, an application to energy consumption and CO₂ emissions in the United States**

This master's thesis analyzes the predictive power of high-dimensional BigVAR models applied to multivariate time series of energy consumption and related carbon dioxide emissions in the United States (1973–2025). The primary objective was to overcome the overparameterization limitations of VAR models through the use of hierarchical regularizations (HVAR) and Lasso penalizations, which were extended to exogenous variables (VARX-L). The applied preprocessing methodology extracts the stationary cyclic component of the time series using TRAMO-SEATS and a Hodrick–Prescott with Jumps (HPJ) filter. The results obtained through rolling validation and window expander show that the penalized models reduce forecasting errors by 70–85% compared to unstructured benchmarks. Impulse response functions (IRFs) highlight the direct link between fossil fuels and carbon dioxide emissions and the sensitivity of renewable sources to the macroeconomic drivers of exogenous variables. The effectiveness of the analyses conducted was confirmed by a bootstrap forecasting procedure on the best-fit model obtained, validating the BigVAR procedure as an effective tool for observing the US energy landscape. ​

[Time Series Analysis (Italian version)](https://github.com/AuroraMusitelli/Masters---Thesis/blob/main/tesi_TimeSeries.pdf)

📎 Tags: Multivariate Time Series | BigVAR Models | Tramo-Seats | Hodrick-Prescott Filter | Bootstrap Forecasting




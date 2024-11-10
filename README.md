🔴**Note: This code and the below information refer to the pre-print available at https://doi.org/10.1101/2024.01.09.574944. The paper is now published in Theoretical Ecology, and is available at https://doi.org/10.1007/s12080-024-00593-5. For the updated code that accompanies the published article, see the latest release: https://github.com/pentathis/DEV-Efficacy/releases/tag/final-code.** 🔴

# DEV-Efficacy
Code accompanying "Efficacy of dynamic eigenvalue in anticipating and distinguishing tipping points"

Notes on the files in this repository:
- Always open `DEV_Analysis.Rproj` first before running the code from any file.
- The files `univariate_DEV.R` and `multivariate_DEV.R` contain functions that evaluate univariate and multivariate DEV, respectively. These functions are imported and used by other files that generate the DEV results from time series.
- The files `discrete_model_functions.R`, `continuous_model_functions.R`, `non-smooth_model_functions.R ` and `red_noise_functions.R` contain functions that generate time series from the considered discrete-time models, continuous-time models, non-smooth (piecewise smooth) discrete models (all with additive white noise) and discrete models with multiplicative white noise, respectively. These functions are imported and used by other files that generate the DEV results from time series.
- The file `sensitivity_heatmap.R` contains functions to generate a heatmap for sensitivity analysis. These functions are used by the file `/plots/sensitivity_analysis/discrete_sensitivity_plots.R`.
- The folder `data_generation` contains the code to generate the data used for various plots. These data are saved in the `data` folder. The data have already been saved, and re-running the files in the `data_generation` folder would only overwrite them.
- The folder `plots` contains files to generate various plots.
  - `/plots/avg_DEV/avg_DEV_discrete_plots.R`: Fig. 1
  - `/plots/avg_DEV/avg_DEV_continuous_plots.R`: Figs. 3, S1, S2
  - `/plots/avg_DEV/avg_DEV_non-smooth_plots.R`: Figs. 4, S3
  - `/plots/rate_analysis/rate_anal_plots.R`: Fig. 2
  - `/plots/red_noise/red_noise_plots.R`: Fig. 5
  - `/plots/sensitivity_analysis/discrete_sensitivity_plots.R`: Fig. 6, S4, S5

All the simulations were performed in `R (version 4.3.2)`. S-maps were implemented using `rEDM (version 1.14.0)`.

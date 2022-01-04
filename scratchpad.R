# file for playing with data and testing ideas.
rm(list=ls())
library(tidyverse)

KELVIN = 273.15

T0_calc <- function(coeffs, N) {
  T0_approx = coeffs[1] + coeffs[2]*N + coeffs[3]*N^2 + coeffs[4]*N^3
  T0_approx = round(T0_approx, 2)
  return (T0_approx)
}

B_calc <- function(coeffs, N) {
  B_approx = coeffs[1] + coeffs[2]*N + coeffs[3]*N^2  + coeffs[4]*N^3
  B_approx = round(B_approx, 2)
  return (B_approx)
}

viscosity_calc <- function(T0, B, Temperature) {
  log_visc = B*(1/Temperature - 1/T0)
  # The log in the de Guzman eqn is base 10.
  return(10^(log_visc))
}

ethane_tbl = as_tibble(read.csv("data/ethane-temp-viscosity.csv"))

N_exp = c(0.79, 1.91, 3.13, 4.07, 4.91, 5.91, 7.04, 7.97, 9.05, 9.99, 
          11.04, 11.99, 12.99, 13.98, 15.02, 16.00, 17.01, 18.01, 19.00, 19.98)

T0 = c(57.60, 95.57, 133.41, 160.20, 182.48, 207.09, 232.53, 251.71, 272.12, 288.37, 
       305.01, 318.78, 332.10, 344.21, 355.92, 366.11, 375.90, 385.00, 393.54, 401.67)

B_exp = c(114.14, 156.60, 222.67, 265.84, 313.66, 362.79, 436.73, 473.70, 525.56, 558.61,
          605.50, 631.63, 664.10, 689.85, 718.51, 738.30, 757.88, 777.40, 793.62, 811.29)

df_raw = data.frame(N_exp, T0, B_exp)

N = N_exp
N2 = N^2
N3 = N * N2

# fit a cubic model to the data
fit_cubic_T0 = lm(T0 ~ N_exp + I(N_exp^2) + I(N_exp^3), data = df_raw)
summary(fit_cubic_T0)

fit_cubic_B = lm(B_exp ~ N_exp + I(N_exp^2) + I(N_exp^3), data = df_raw)
summary(fit_cubic_B)

T_coeffs = coef(fit_cubic_T0)
B_coeffs = coef(fit_cubic_B)

# generate the constants for ethane 
T0_ethane = T0_calc(T_coeffs, 2)
B_ethane = B_calc(B_coeffs,2)

# Add a new column with temps in Kelvin for use in calculations
ethane_tbl = ethane_tbl %>% 
  mutate(TempK = ethane_tbl$Temp + KELVIN)

# Compute viscosity
ethane_tbl = ethane_tbl %>% 
  mutate(CalcTest = viscosity_calc(T0_ethane, B_ethane, ethane_tbl$TempK))
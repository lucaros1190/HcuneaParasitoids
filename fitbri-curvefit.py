
# fitbri.py Python script to fit data with a line
# Created by Luca Rossini on 23 February 2021
# E-mail luca.rossini@unitus.it
# Last update 24 Febryary 2021

import pandas as pd
from math import *
from scipy import stats
from scipy.optimize import curve_fit
from scipy.stats.distributions import chi2
import numpy as np
import matplotlib.pyplot as plt

# Read the data to fit and plot

Data = pd.read_csv("data.csv", sep=";", header=None)


# Set the header of the dataset

Data.columns = ["x", "y", "err_x", "err_y"]

x = Data['x']
y = Data['y']
err_x = Data['err_x']
err_y = Data['err_y']

# Fit data with a Briere function
    # Definition of the Briere function
    
def brifun(x, a, T_L, T_M, m):
    return a * x * (x - T_L) * ((T_M - x) ** (1/m))

    # Fit with curve_fit: bounds are the limitations on the parameters
    
popt, pcov = curve_fit(brifun, x, y, bounds=([0.000001, -10., 27., 0.5], [0.05, 14, 36, 4.]), p0=(0.03, 3, 31, 1.3), method='dogbox')

    # Best fit values
    
a = popt[0]
T_L = popt[1]
T_M = popt[2]
m = popt[3]

    # Perameters error

perr = np.sqrt(np.diag(pcov))

err_a = perr[0]
err_T_L = perr[1]
err_T_M = perr[2]
err_m = perr[3]

# Ask how many sigma do you want to include in the confidence band

print('\n How many sigma do you want to include in the confidence band? (2 sigma is 95%): \n')

num_sigma = float(input())

# Upper ad lower confidence bands

    # Create the linespace to plot the best fit curve
x_fun = np.linspace(0, 40, 5000)

    # Calculations
fitted_fun = brifun(x, *popt)
fitted_plot = brifun(x_fun, *popt)

a_up = a + num_sigma * err_a
a_low = a - num_sigma * err_a
T_L_up = T_L + num_sigma * err_T_L
T_L_low = T_L - num_sigma * err_T_L
T_M_up = T_M + num_sigma * err_T_M
T_M_low = T_M - num_sigma * err_T_M
m_up = m + num_sigma * err_m
m_low = m - num_sigma * err_m

upper_fit = brifun(x_fun, a_up, T_L_up, T_M_up, m_up)
lower_fit = brifun(x_fun, a_low, T_L_low, T_M_low, m_low)

# Calculating R-squared

resid = y - fitted_fun
ss_res = np.sum(resid**2)
ss_tot = np.sum((y - np.mean(y))**2)
r_squared = 1 - (ss_res / ss_tot)


# Number of degrees of freedom (NDF)

ndf = len(x) - 4

# Calculate the chi-squared (with error below the fraction)

chi_sq = 0
for i in range(len(x)):
    chi_sq = pow((y[i] - fitted_fun[i]), 2)/err_y[i]

# Calculate the P-value from chi-square

Pvalue = 1 - chi2.sf(chi_sq, ndf)

# Calculate AIC and BIC

AIC = 2 * 4 - 2 * np.log(ss_res/len(x))
BIC = 4 * np.log(len(x)) - 2 * np.log(ss_res/len(x))

# Optimal temperature  - Maximum of the function

def optemp(y):
    a, T_L, T_M, m = y
    T_opt = (2 * m * T_M + T_L * (m + 1) + sqrt(4 * m * m * T_M * T_M + T_L * T_L * (m + 1) * (m + 1) - 4 * m * T_M * T_L))  / (4 * m + 2)
    return T_opt
    
def err_optemp(y, ey):
    a, T_L, T_M, m = y
    ea, eT_L, eT_M, em = ey
    
    A = pow(eT_L, 2) * pow( (m + 1)/(4*m + 2) + (1/(4*m + 2)) * ((T_L * pow((m + 1), 2) - 2*m * T_M)/(sqrt(4 * m * m * T_M * T_M + T_L * T_L * pow(m+1, 2) - 4*m * T_M * T_L))) ,2);
                
    B = pow(eT_L, 2) * pow(m/(2*m +1) + (1/(4*m + 2) * (4*m*m * T_M - 2*m * T_L)/(sqrt(4*m*m * T_M*T_M + T_L*T_L * pow((m+1), 2) - 4*m * T_L * T_M))) ,2);
                
    C = pow(em, 2) * pow(2*T_M * ((1/(4*m + 2)) - ((4*m)/(pow((4*m + 2) ,2))) + (T_L/((pow((4*m + 2) ,2)))) * ((4*T_M*T_M * (m - 2*m*m) - T_L*T_L * (m+1) * (4*m + 3) + T_M * T_M * (8*m - 2) )/( (pow((4*m + 2) ,2)) * sqrt(4*m*m * T_M*T_M + T_L*T_L * pow((m + 1), 2) - 4*m*T_L * T_M) )) ) ,2);
    
    eT_opt = sqrt(A + B + C)
    
    return eT_opt


# Print the results

print('\n Briere fit (a * x * (x - T_L) * pow((T_M - x), m)) results: \n')

    # Define the parameters' name
parname = (' a = ', ' T low = ', ' T max = ', ' m = ')

for i in range(len(popt)):
    print(parname[i] + str(round(popt[i], 7)) + ' +/- ' + str(round(perr[i],7)))

print(' Optimal temperature = ' + str(optemp(popt)) + ' +/- ' + str(err_optemp(popt, perr)))

print(' R-squared = ', round(r_squared, 5))
print(' Chi-squared = ', round(chi_sq, 5))
print(' P-value = ', round(Pvalue, 7))
print(' Number of degrees of freedom (NDF) =', ndf)
print(' Akaike Information Criterion (AIC):', round(AIC, 5))
print(' Bayesian Information Criterion (BIC)', round(BIC, 5))
print('\n')

print(' Covariance matrix: \n')
    # Define the row names to print
rowname = (' ', 'a    ', 'T low', 'T max', 'm    ')
print(rowname[0] + '     \t' + rowname[1] + '     \t' + rowname[2] + '     \t' + rowname[3] + '     \t' + rowname[4])

for i in range(len(pcov)):
    print(rowname[i+1] + ' ' + str(pcov[i]))

print(' ')

# Plot the data

plt.figure(1)

plt.scatter(x, y, color="C0", alpha=0.5, label=f'Experimental data')
plt.errorbar(x, y, xerr=err_x, yerr=err_y, fmt='o')
plt.plot(x_fun, fitted_plot, color="C1", alpha=0.5, label=f'Best fit function')
plt.fill_between(x_fun, upper_fit, lower_fit, color='b', alpha=0.1)
plt.xlabel('Temperature (°C)')
plt.ylabel('Development rate (1/day)')
plt.legend()

plt.show()

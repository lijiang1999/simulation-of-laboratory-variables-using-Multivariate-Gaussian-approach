## Simulation of laboratory variables using Multivariate Gaussian approach
Simulation of quantitative laboratory variables using multivariate Gaussian approach given mean, standard deviation, and correlation coefficient.
## Outline:

1. load the 'real world' data and calculate mean and sd
2. calculate correlation matrix for all selected laboratory variables
3. create a covariance matrix and simulate data based on mean, sd, and the covariance matrix
4. put the mask of missing from the realworld dataframe on simulated dataframe
5. comparing the distribution and correlation of real world and simulated data with the same mask

## Files included
1. R code deposit at "05022022_simulation_gnesis.R"
2. simulated data with mask on deposit at "dffinal_repeat_select_widformat_t0_masked_test_1st_version2.RData" in version 2. 
3. simulated data without mask on deposit at "dffinal_repeat_select_widformat_t0.RData" in version 2.
4. The correponding .csv file are also available with the same prefix.

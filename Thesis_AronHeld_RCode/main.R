source("Thesis_AronHeld_RCode/functions.R")

##### MAIN
#########################
### LOAD DATA
path_directory = paste(getwd(),"/data/",sep="")

installpackages()

DATA <- load_data(path_directory)

# Apply ADF function to return test results (price data)
# adf_results <- tests_for_adf(DATA,5)
# adf_results 

# Apply ADF function to return test results (log data)
adf_results_log <- tests_for_adf(log(DATA),5)
adf_results_log 

# Return descriptive summary w.r.t. its created function 
summary_data <-descriptive_summary(DATA)
summary_data

# Return descriptive summary w.r.t. its created function (on log data)
# summary_data_log <-descriptive_summary(log(DATA))
# summary_data_log

# Plot set of cryptos together (log data - ln data)
plot_all_series_in_one(DATA)

# Apply function to return SADF test results with respective CV (10%, 5%, 1%) 
  # plus plot BADF in a series with CV (5%) 
    # This function may take some while to load! (ca. 0.5-1 hour)
object_sadf <- unitroot_sadf_gsadf(DATA,1,1)

# Apply function to test on Granger causality (simultaniously) for data set and automatic lag size 
# granger_tests <- granger_causality_tests_simultaniously(DATA,5)
# granger_tests

# Apply function to test on Granger causality (bivariate) for data set and given lags
biv_granger_test_lag1_log <- granger_causality_bivariate(DATA,1,1)
biv_granger_test_lag2_log <- granger_causality_bivariate(DATA,2,1)
biv_granger_test_lag3_log <- granger_causality_bivariate(DATA,3,1)
biv_granger_test_lag4_log <- granger_causality_bivariate(DATA,4,1)
biv_granger_test_lag5_log <- granger_causality_bivariate(DATA,5,1)

biv_granger_test_lag1_log
biv_granger_test_lag2_log
biv_granger_test_lag3_log
biv_granger_test_lag4_log
biv_granger_test_lag5_log




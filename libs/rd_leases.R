library(dplyr)
library(tidyr)
library(purrr)
#-------------------------------------------------------------------------------------------------
rd.leases.adjustments.f <- function(financial_data, cost_debt=0.04){
  EBITDA_margin <- if (financial_data$total_revenue_ltm_us_dmm_historical_rate > 0){
    financial_data$ebitda_ltm_us_dmm_historical_rate / 
      financial_data$total_revenue_ltm_us_dmm_historical_rate
  } else {
    NA
  }
  
  operating_margin <- if(financial_data$total_revenue_ltm_us_dmm_historical_rate > 0){
    adjusted_ebit / #adjusted_ebit to be coded in later
      inancial_data$total_revenue_ltm_us_dmm_historical_rate 
  } else {
    NA
  }
}
#-------------------------------------------------------------------------------------------------
# rm(rd.leases.adjustments.f)

library(dplyr)
library(tidyr)
library(purrr)
#-------------------------------------------------------------------------------------------------
rd.leases.adjustments.f <- function(financial_data, cost_debt=0.04){
  
  columns = c(
    "exchange_ticker",
    "EBITDA_margin",
    "operating_margin", 
    "effective_tax_rate",
    "roic",
    "rnd_adjusted_roe",
    "growth_in_revenue_last10yrs",
    "growth_in_ebitda_last10yrs",
    "debt_ratio_book",
    "debt_ratio_market",
    "enterprise_value",
    "invested_capital_incl_capitlized_rnd",
    "adjusted_net_income",
    "adjusted_ebit",
    "total_debt_incl_leases",
    "pe",
    "non_cash_pe",
    "rnd_adjusted_pe",
    "ev_sales",
    "ev_ebit",
    "ev_invested_capital",
    "ev_ebitda",
    "capitalized_rnd",
    "ev_ebitda_rnd",
    "turnover_ratio",
    "only_taxable_income",
    "missing_taxable_income",
    "only_positive_taxes",
    "only_positive_net_income",
    "only_positive_market_cap",
    "capitalized_leases"
  )
  company_name <- financial_data$company_name
  
  exchange_ticker <- financial_data$exchange_ticker
  
  EBITDA_margin <- if (financial_data$total_revenue_ltm_us_dmm_historical_rate > 0){
    financial_data$ebitda_ltm_us_dmm_historical_rate / 
      financial_data$total_revenue_ltm_us_dmm_historical_rate
  } else {
    NA
  }
  
  adjusted_ebit <- financial_data$ebit_ltm_us_dmm_historical_rate +
    financial_data$r_d_expense_ltm_us_dmm_historical_rate - 
    (
      financial_data$r_d_expense_ltm_1_us_dmm_historical_rate + 
        financial_data$r_d_expense_ltm_2_us_dmm_historical_rate +
        financial_data$r_d_expense_ltm_3_us_dmm_historical_rate +
        financial_data$r_d_expense_ltm_4_us_dmm_historical_rate +
        financial_data$r_d_expense_ltm_5_us_dmm_historical_rate
    ) / 5
  
  operating_margin <- if(financial_data$total_revenue_ltm_us_dmm_historical_rate > 0){
    adjusted_ebit / 
      financial_data$total_revenue_ltm_us_dmm_historical_rate 
  } else {
    NA
  }
  
  effective_tax_rate <-financial_data$effective_tax_rate_ltm_percent / 100
  
  capitalized_rnd <- financial_data$r_d_expense_ltm_us_dmm_historical_rate +
    financial_data$r_d_expense_ltm_1_us_dmm_historical_rate * 0.8 + 
    financial_data$r_d_expense_ltm_2_us_dmm_historical_rate * 0.6 +
    financial_data$r_d_expense_ltm_3_us_dmm_historical_rate * 0.4 +
    financial_data$r_d_expense_ltm_4_us_dmm_historical_rate * 0.2 
  
  invested_capital_incl_capitlized_rnd <- financial_data$total_debt_latest_quarter_us_dmm_historical_rate + 
    financial_data$total_equity_latest_quarter_us_dmm_historical_rate - 
    financial_data$cash_and_equivalents_latest_quarter_us_dmm_historical_rate +
    capitalized_rnd
  
  roic <- if(invested_capital_incl_capitlized_rnd > 0){ 
    adjusted_ebit * (1 - effective_tax_rate) / invested_capital_incl_capitlized_rnd
  } else {
    NA
  }
  
  adjusted_net_income <- financial_data$net_income_ltm_us_dmm_historical_rate +
    financial_data$r_d_expense_ltm_us_dmm_historical_rate - 
    (
      financial_data$r_d_expense_ltm_1_us_dmm_historical_rate + 
        financial_data$r_d_expense_ltm_2_us_dmm_historical_rate +
        financial_data$r_d_expense_ltm_3_us_dmm_historical_rate +
        financial_data$r_d_expense_ltm_4_us_dmm_historical_rate +
        financial_data$r_d_expense_ltm_5_us_dmm_historical_rate
    ) / 5
  
  
  rnd_adjusted_roe <- if(financial_data$total_equity_latest_quarter_us_dmm_historical_rate > 0){
    adjusted_net_income / financial_data$total_equity_latest_quarter_us_dmm_historical_rate
  } else {
    NA
  }
    
  growth_in_revenue_last10yrs <- if(financial_data$total_revenues_10_yr_cagr_percent_ltm_percent == 0){
    NA
  } else {
    financial_data$total_revenues_10_yr_cagr_percent_ltm_percent / 100
  }
    
  growth_in_ebitda_last10yrs <- if(financial_data$ebitda_10_yr_cagr_percent_ltm_percent == 0){
    NA
  } else {
    financial_data$ebitda_10_yr_cagr_percent_ltm_percent / 100
  }
  
  capitalized_leases <- financial_data$operating_lease_commitment_due_1_latest_annual_us_dmm_historical_rate/(1+cost_debt) +
    financial_data$operating_lease_commitment_due_2_latest_annual_us_dmm_historical_rate/(1+cost_debt)**2 +
    financial_data$operating_lease_commitment_due_3_latest_annual_us_dmm_historical_rate/(1+cost_debt)**3 +
    financial_data$operating_lease_commitment_due_4_latest_annual_us_dmm_historical_rate/(1+cost_debt)**4 +
    financial_data$operating_lease_commitment_due_5_latest_annual_us_dmm_historical_rate/(1+cost_debt)**5 +
    ((financial_data$operating_lease_commitment_due_after_5_yrs_latest_annual_us_dmm_historical_rate/5)*(1-(1+cost_debt)**5)/cost_debt)/(1+cost_debt)**5


  debt_ratio_book <- if(financial_data$total_equity_latest_quarter_us_dmm_historical_rate > 0){
    (capitalized_leases + financial_data$total_debt_latest_quarter_us_dmm_historical_rate) / 
      (financial_data$total_equity_latest_quarter_us_dmm_historical_rate + 
         capitalized_leases +
         financial_data$total_debt_latest_quarter_us_dmm_historical_rate)
  } else {
    NA
  }
  
  debt_ratio_market <- if(financial_data$market_capitalization_latest_us_dmm_historical_rate > 0){
    (capitalized_leases + financial_data$total_debt_latest_quarter_us_dmm_historical_rate)/
      (financial_data$market_capitalization_latest_us_dmm_historical_rate + 
         capitalized_leases +
         financial_data$total_debt_latest_quarter_us_dmm_historical_rate)
  }
  
  enterprise_value <- financial_data$market_capitalization_latest_us_dmm_historical_rate +
    financial_data$total_debt_latest_quarter_us_dmm_historical_rate + 
    capitalized_leases -
    financial_data$cash_and_equivalents_latest_quarter_us_dmm_historical_rate
    
  

  


  
  total_debt_incl_leases <- financial_data$total_debt_latest_quarter_us_dmm_historical_rate +
    capitalized_leases
  
  pe <- if(financial_data$net_income_ltm_us_dmm_historical_rate > 0){
    financial_data$market_capitalization_latest_us_dmm_historical_rate /
      financial_data$net_income_ltm_us_dmm_historical_rate
  } else {
    NA
  }
  
  non_cash_pe <- if(financial_data$net_income_ltm_us_dmm_historical_rate > 0){
    (financial_data$market_capitalization_latest_us_dmm_historical_rate - 
       financial_data$cash_and_equivalents_latest_quarter_us_dmm_historical_rate) /
      (financial_data$net_income_ltm_us_dmm_historical_rate - 
         financial_data$interest_and_invest_income_ltm_us_dmm_historical_rate)
  } else {
    NA
  }
  
  rnd_adjusted_pe <- if(adjusted_net_income > 0){
    financial_data$market_capitalization_latest_us_dmm_historical_rate /
      adjusted_net_income
  } else {
    NA
  }
  
  ev_sales <- if(financial_data$total_revenue_ltm_us_dmm_historical_rate == 0){
    NA
  } else {
    enterprise_value / financial_data$total_revenue_ltm_us_dmm_historical_rate
  }
  
  ev_ebit <- if(adjusted_ebit > 0){
    enterprise_value / adjusted_ebit
  } else {
    NA
  }
  
  ev_invested_capital <- if(invested_capital_incl_capitlized_rnd > 0){
    enterprise_value / invested_capital_incl_capitlized_rnd
  } else {
    NA
  }
  
  ev_ebitda <- if(financial_data$ebitda_ltm_us_dmm_historical_rate > 0){
    enterprise_value / financial_data$ebitda_ltm_us_dmm_historical_rate
  } else {
    NA
  }
  

    
    
  ev_ebitda_rnd <- if(financial_data$ebitda_ltm_us_dmm_historical_rate > 0){
    enterprise_value / (financial_data$ebitda_ltm_us_dmm_historical_rate + 
                          financial_data$r_d_expense_ltm_us_dmm_historical_rate)
  } else {
    NA
  }
  
  turnover_ratio <- if(financial_data$daily_value_traded_latest_us_dmm_historical_rate > 0){
    if(financial_data$market_capitalization_latest_us_dmm_historical_rate > 0){
      if(financial_data$daily_value_traded_latest_us_dmm_historical_rate * 250/
         financial_data$market_capitalization_latest_us_dmm_historical_rate > 10){
        NA
      } else {
        financial_data$daily_value_traded_latest_us_dmm_historical_rate * 250/
          financial_data$market_capitalization_latest_us_dmm_historical_rate
      }
    } else {
      NA
    }
  } else {
    NA
  }
  
  only_taxable_income <- if(financial_data$ebt_incl_unusual_items_ltm_us_dmm_historical_rate > 0){
    financial_data$ebt_incl_unusual_items_ltm_us_dmm_historical_rate
  } else {
    NA
  }
  
  missing_taxable_income <- if(financial_data$ebt_incl_unusual_items_ltm_us_dmm_historical_rate == 0){
    0
  } else {
    1
  }
  
  only_positive_taxes <- if(financial_data$ebt_incl_unusual_items_ltm_us_dmm_historical_rate > 0){
    financial_data$income_tax_expense_ltm_us_dmm_historical_rate
  } else {
    NA
  }
  
  only_positive_net_income <- if(financial_data$net_income_ltm_us_dmm_historical_rate > 0){
    financial_data$net_income_ltm_us_dmm_historical_rate
  } else {
    NA
  }
  
  only_positive_market_cap <- if(financial_data$net_income_ltm_us_dmm_historical_rate > 0){
    financial_data$market_capitalization_latest_us_dmm_historical_rate
  } else {
    NA
  }
  
  output <- as.data.frame(list(
    company_name,
    exchange_ticker,
    EBITDA_margin,
    operating_margin, 
    effective_tax_rate,
    roic,
    rnd_adjusted_roe,
    growth_in_revenue_last10yrs,
    growth_in_ebitda_last10yrs,
    debt_ratio_book,
    debt_ratio_market,
    enterprise_value,
    invested_capital_incl_capitlized_rnd,
    adjusted_net_income,
    adjusted_ebit,
    total_debt_incl_leases,
    pe,
    non_cash_pe,
    rnd_adjusted_pe,
    ev_sales,
    ev_ebit,
    ev_invested_capital,
    ev_ebitda,
    capitalized_rnd,
    ev_ebitda_rnd,
    turnover_ratio,
    only_taxable_income,
    missing_taxable_income,
    only_positive_taxes,
    only_positive_net_income,
    only_positive_market_cap,
    capitalized_leases
  ), col.names = columns)
  
  return(output)
  
}
#-------------------------------------------------------------------------------------------------
# rm(rd.leases.adjustments.f)

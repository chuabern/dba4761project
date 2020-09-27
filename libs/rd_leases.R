library(dplyr)
library(tidyr)
library(purrr)
#-------------------------------------------------------------------------------------------------
rd.leases.adjustments.f <- function(financial_data, cost_debt=0.04){
  ### Average R&D
  rnd_avg=data.frame(row.names=1:nrow(data))
  for (i in 1:5){
    df <- data.frame(data[,paste0('r_d_expense_ltm_',i,'_us_dmm_historical_rate')])
    rnd_avg <- cbind(rnd_avg,df)}
  rnd_avg <- rnd_avg %>% mutate(RnD_average=rowMeans(rnd_avg)) %>% select(RnD_average)
  rnd_avg <- as.vector(rnd_avg[[1]])
  
  ### Function for capitalized_rd 
  capitalized_rd <- function(n){
    x <- data.frame(y=n$r_d_expense_ltm_us_dmm_historical_rate) 
    for (i in 1:4){
      df <- data.frame(y=n[,paste0('r_d_expense_ltm_',i,'_us_dmm_historical_rate')])
      x <- cbind(x,df)}
    x <- data.frame(t(t(x)*seq(1,0.2,length=5)))
    x <- x %>% mutate(capital_rd=rowSums(x)) %>% select(capital_rd)
    x <- as.vector(x[[1]])}
  
  ### Function for capitalized_leases
  capital <- function(financial_data){
    x=data.frame(row.names=1:nrow(financial_data))
    for (i in 1:5){
      df <- data.frame(y=(financial_data[,paste0('operating_lease_commitment_due_',i,'_latest_annual_us_dmm_historical_rate')]/
                            ((1+cost_debt)^i)))
      x=cbind(x,df)}
    x[,6] <-((financial_data$operating_lease_commitment_due_after_5_yrs_latest_annual_us_dmm_historical_rate/5)*(1-1.04^-5)/
               cost_debt)/((1+cost_debt)^5)
    x <- x %>% mutate(capitalized_leases=rowSums(x)) %>% select(capitalized_leases)
    x <- as.vector(x[[1]])}
  
  #Create the required table
  processed <- financial_data %>%
    mutate(across(everything(), ~replace_na(.x, 0))) %>%
    mutate(
      
      ebitda_margin = ifelse(total_revenue_ltm_us_dmm_historical_rate > 0, 
                             ebitda_ltm_us_dmm_historical_rate /total_revenue_ltm_us_dmm_historical_rate,NA),
      
      adjusted_ebit = ebit_ltm_us_dmm_historical_rate + r_d_expense_ltm_us_dmm_historical_rate - rnd_avg,
      
      operating_margin = ifelse(total_revenue_ltm_us_dmm_historical_rate > 0,
                                adjusted_ebit / total_revenue_ltm_us_dmm_historical_rate,NA),
      
      effective_tax_rate = effective_tax_rate_ltm_percent / 100,
      
      capitalized_rd = capitalized_rd(financial_data),
      
      adjusted_invested_capital = total_debt_latest_quarter_us_dmm_historical_rate +
        total_equity_latest_quarter_us_dmm_historical_rate -
        cash_and_equivalents_latest_quarter_us_dmm_historical_rate +
        capitalized_rd,
      
      roic = ifelse(adjusted_invested_capital > 0,
                    adjusted_ebit * (1 - effective_tax_rate) / adjusted_invested_capital,NA),
      
      adjusted_net_income = net_income_ltm_us_dmm_historical_rate + r_d_expense_ltm_us_dmm_historical_rate - rnd_avg,
      
      adjusted_roe = ifelse(total_equity_latest_quarter_us_dmm_historical_rate > 0,
                            adjusted_net_income / total_equity_latest_quarter_us_dmm_historical_rate,NA),
      
      revenue_growth = ifelse(total_revenues_10_yr_cagr_percent_ltm_percent == 0,NA,
                              total_revenues_10_yr_cagr_percent_ltm_percent / 100),
      
      ebitda_growth = ifelse(ebitda_10_yr_cagr_percent_ltm_percent == 0,NA,
                             ebitda_10_yr_cagr_percent_ltm_percent / 100),
      
      capitalized_leases = capital(financial_data),
      
      book_value_debt = ifelse(total_equity_latest_quarter_us_dmm_historical_rate > 0,
                               (capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate) /
                                 (total_equity_latest_quarter_us_dmm_historical_rate + capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate),NA),
      
      market_value_debt = ifelse(market_capitalization_latest_us_dmm_historical_rate > 0,
                                 (capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate) /
                                   (market_capitalization_latest_us_dmm_historical_rate + capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate),NA),
      
      enterprise_value = market_capitalization_latest_us_dmm_historical_rate +
        total_debt_latest_quarter_us_dmm_historical_rate +
        capitalized_leases - cash_and_equivalents_latest_quarter_us_dmm_historical_rate,
      
      adjusted_total_debt = total_debt_latest_quarter_us_dmm_historical_rate + capitalized_leases,
      
      pe = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                  market_capitalization_latest_us_dmm_historical_rate / net_income_ltm_us_dmm_historical_rate,NA),
      
      non_cash_pe = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                           (market_capitalization_latest_us_dmm_historical_rate - cash_and_equivalents_latest_quarter_us_dmm_historical_rate) /(net_income_ltm_us_dmm_historical_rate - interest_and_invest_income_ltm_us_dmm_historical_rate),NA),
      
      adjusted_pe = ifelse(adjusted_net_income > 0 ,
                           market_capitalization_latest_us_dmm_historical_rate / adjusted_net_income,NA),
      
      ev_sales = ifelse(total_revenue_ltm_us_dmm_historical_rate == 0,NA,
                        enterprise_value/ total_revenue_ltm_us_dmm_historical_rate),
      
      ev_ebit = ifelse(adjusted_ebit > 0,enterprise_value/adjusted_ebit,NA),
      
      ev_invested_capital = ifelse(adjusted_invested_capital > 0,enterprise_value/adjusted_invested_capital,NA),
      
      ev_ebitda = ifelse(ebitda_ltm_us_dmm_historical_rate > 0,enterprise_value/ebitda_ltm_us_dmm_historical_rate,NA),
      
      ev_adjusted_ebitda = ifelse(ebitda_ltm_us_dmm_historical_rate > 0,
                                  enterprise_value/(ebitda_ltm_us_dmm_historical_rate +r_d_expense_ltm_us_dmm_historical_rate),
                                  NA),
      
      turnover_ratio = ifelse(daily_value_traded_latest_us_dmm_historical_rate > 0,
                              ifelse(market_capitalization_latest_us_dmm_historical_rate > 0 ,
                                     ifelse(daily_value_traded_latest_us_dmm_historical_rate * 250 /
                                              market_capitalization_latest_us_dmm_historical_rate > 10,NA,
                                            daily_value_traded_latest_us_dmm_historical_rate * 250 /
                                              market_capitalization_latest_us_dmm_historical_rate),NA),NA),
      
      only_taxable_income = ifelse(ebt_incl_unusual_items_ltm_us_dmm_historical_rate > 0,
                                   ebt_incl_unusual_items_ltm_us_dmm_historical_rate,NA),
      
      missing_taxable_income = ifelse(ebt_incl_unusual_items_ltm_us_dmm_historical_rate == 0,0, 1),
      
      only_taxes = ifelse(ebt_incl_unusual_items_ltm_us_dmm_historical_rate > 0,
                          income_tax_expense_ltm_us_dmm_historical_rate,NA),
      
      only_net_income = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                               net_income_ltm_us_dmm_historical_rate,NA),
      
      only_market_cap = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                               market_capitalization_latest_us_dmm_historical_rate,NA)
    ) %>%
    select("company_name", "exchange_ticker", "ebitda_margin", "operating_margin", "effective_tax_rate",
           "roic", "adjusted_roe", "revenue_growth", "ebitda_growth", "book_value_debt", "market_value_debt",
           "enterprise_value", "adjusted_invested_capital", "adjusted_net_income", "adjusted_ebit",
           "adjusted_total_debt", "pe", "non_cash_pe", "adjusted_pe", "ev_sales", "ev_ebit",
           "ev_invested_capital", "ev_ebitda", "capitalized_rd", "ev_adjusted_ebitda", "turnover_ratio",
           "only_taxable_income", "missing_taxable_income", "only_taxes", "only_net_income", "only_market_cap",
           "capitalized_leases"  )
  
  return(processed)

  }
#-------------------------------------------------------------------------------------------------
# rm(rd.leases.adjustments.f)

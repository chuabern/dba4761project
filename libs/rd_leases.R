library(dplyr)
library(tidyr)
library(purrr)
#-------------------------------------------------------------------------------------------------
rd.leases.adjustments.f <- function(financial_data, cost_debt=0.04){
    # print(names(financial_data))
    processed <- financial_data %>%
        mutate(across(everything(), ~replace_na(.x, 0))) %>%
        mutate( ebitda_margin = ifelse(total_revenue_ltm_us_dmm_historical_rate > 0,
                                        ebitda_ltm_us_dmm_historical_rate / total_revenue_ltm_us_dmm_historical_rate,
                                        NA),
                adjusted_ebit = ebit_ltm_us_dmm_historical_rate +
                                r_d_expense_ltm_us_dmm_historical_rate -
                                    (r_d_expense_ltm_1_us_dmm_historical_rate +
                                    r_d_expense_ltm_2_us_dmm_historical_rate +
                                    r_d_expense_ltm_3_us_dmm_historical_rate +
                                    r_d_expense_ltm_4_us_dmm_historical_rate +
                                    r_d_expense_ltm_5_us_dmm_historical_rate)/5,
                operating_margin = ifelse(total_revenue_ltm_us_dmm_historical_rate > 0,
                                        adjusted_ebit / total_revenue_ltm_us_dmm_historical_rate,
                                        NA),
                effective_tax_rate = effective_tax_rate_ltm_percent / 100,
                capitalized_rd = r_d_expense_ltm_us_dmm_historical_rate +
                                0.8 * r_d_expense_ltm_1_us_dmm_historical_rate +
                                0.6 * r_d_expense_ltm_2_us_dmm_historical_rate +
                                0.4 * r_d_expense_ltm_3_us_dmm_historical_rate +
                                0.2 * r_d_expense_ltm_4_us_dmm_historical_rate,
                adjusted_invested_capital = total_debt_latest_quarter_us_dmm_historical_rate +
                                            total_equity_latest_quarter_us_dmm_historical_rate -
                                            cash_and_equivalents_latest_quarter_us_dmm_historical_rate +
                                            capitalized_rd,
                roic = ifelse(adjusted_invested_capital > 0,
                            adjusted_ebit * (1 - effective_tax_rate) / adjusted_invested_capital,
                            NA),
                adjusted_net_income = net_income_ltm_us_dmm_historical_rate +
                                    r_d_expense_ltm_us_dmm_historical_rate -
                                    (r_d_expense_ltm_1_us_dmm_historical_rate +
                                        r_d_expense_ltm_2_us_dmm_historical_rate +
                                        r_d_expense_ltm_3_us_dmm_historical_rate +
                                        r_d_expense_ltm_4_us_dmm_historical_rate +
                                        r_d_expense_ltm_5_us_dmm_historical_rate)/5,
                adjusted_roe = ifelse(total_equity_latest_quarter_us_dmm_historical_rate > 0,
                                    adjusted_net_income / total_equity_latest_quarter_us_dmm_historical_rate,
                                    NA),
                revenue_growth = ifelse(total_revenues_10_yr_cagr_percent_ltm_percent == 0,
                                        NA,
                                        total_revenues_10_yr_cagr_percent_ltm_percent / 100),
                ebitda_growth = ifelse(ebitda_10_yr_cagr_percent_ltm_percent == 0,
                                        NA,
                                        ebitda_10_yr_cagr_percent_ltm_percent / 100),
                capitalized_leases = operating_lease_commitment_due_1_latest_annual_us_dmm_historical_rate / (1 + cost_debt) +
                                    operating_lease_commitment_due_2_latest_annual_us_dmm_historical_rate / (1 + cost_debt) ** 2 +
                                    operating_lease_commitment_due_3_latest_annual_us_dmm_historical_rate / (1 + cost_debt) ** 3 +
                                    operating_lease_commitment_due_4_latest_annual_us_dmm_historical_rate / (1 + cost_debt) ** 4 +
                                    operating_lease_commitment_due_5_latest_annual_us_dmm_historical_rate / (1 + cost_debt) ** 5 +
                                    ((operating_lease_commitment_due_after_5_yrs_latest_annual_us_dmm_historical_rate / 5) * (1 - (1 + cost_debt)**-5)/cost_debt) / (1 + cost_debt)**5,
                book_value_debt = ifelse(total_equity_latest_quarter_us_dmm_historical_rate > 0,
                                        (capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate) /
                                        (total_equity_latest_quarter_us_dmm_historical_rate + capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate),
                                        NA),
                market_value_debt = ifelse(market_capitalization_latest_us_dmm_historical_rate > 0,
                                            (capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate) /
                                            (market_capitalization_latest_us_dmm_historical_rate + capitalized_leases + total_debt_latest_quarter_us_dmm_historical_rate),
                                            NA),
                enterprise_value = market_capitalization_latest_us_dmm_historical_rate +
                                    total_debt_latest_quarter_us_dmm_historical_rate +
                                    capitalized_leases - cash_and_equivalents_latest_quarter_us_dmm_historical_rate,
                adjusted_total_debt = total_debt_latest_quarter_us_dmm_historical_rate + capitalized_leases,
                pe = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                            market_capitalization_latest_us_dmm_historical_rate / net_income_ltm_us_dmm_historical_rate,
                            NA),
                non_cash_pe = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                                    (market_capitalization_latest_us_dmm_historical_rate - cash_and_equivalents_latest_quarter_us_dmm_historical_rate) /
                                    (net_income_ltm_us_dmm_historical_rate - interest_and_invest_income_ltm_us_dmm_historical_rate),
                                    NA),
                adjusted_pe = ifelse(adjusted_net_income > 0 ,
                                    market_capitalization_latest_us_dmm_historical_rate / adjusted_net_income,
                                    NA),
                ev_sales = ifelse(total_revenue_ltm_us_dmm_historical_rate == 0,
                                NA,
                                enterprise_value/ total_revenue_ltm_us_dmm_historical_rate),
                ev_ebit = ifelse(adjusted_ebit > 0,
                                enterprise_value/adjusted_ebit,
                                NA),
                ev_invested_capital = ifelse(adjusted_invested_capital > 0,
                                            enterprise_value/adjusted_invested_capital,
                                            NA),
                ev_ebitda = ifelse(ebitda_ltm_us_dmm_historical_rate > 0,
                                    enterprise_value/ebitda_ltm_us_dmm_historical_rate,
                                    NA),
                ev_adjusted_ebitda = ifelse(ebitda_ltm_us_dmm_historical_rate > 0,
                                            enterprise_value/(ebitda_ltm_us_dmm_historical_rate +
                                                            r_d_expense_ltm_us_dmm_historical_rate),
                                            NA),
                turnover_ratio = ifelse(daily_value_traded_latest_us_dmm_historical_rate > 0,
                                        ifelse(market_capitalization_latest_us_dmm_historical_rate > 0 ,
                                            ifelse(daily_value_traded_latest_us_dmm_historical_rate * 250 /
                                                    market_capitalization_latest_us_dmm_historical_rate > 10,
                                                    NA,
                                                daily_value_traded_latest_us_dmm_historical_rate * 250 /
                                                market_capitalization_latest_us_dmm_historical_rate),
                                                NA),
                                        NA),
                only_taxable_income = ifelse(ebt_incl_unusual_items_ltm_us_dmm_historical_rate > 0,
                                            ebt_incl_unusual_items_ltm_us_dmm_historical_rate,
                                            NA),
                missing_taxable_income = ifelse(ebt_incl_unusual_items_ltm_us_dmm_historical_rate == 0,
                                                0, 1),
                only_taxes = ifelse(ebt_incl_unusual_items_ltm_us_dmm_historical_rate > 0,
                                    income_tax_expense_ltm_us_dmm_historical_rate,
                                    NA),
                only_net_income = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                                        net_income_ltm_us_dmm_historical_rate,
                                        NA),
                only_market_cap = ifelse(net_income_ltm_us_dmm_historical_rate > 0,
                                        market_capitalization_latest_us_dmm_historical_rate,
                                        NA)
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

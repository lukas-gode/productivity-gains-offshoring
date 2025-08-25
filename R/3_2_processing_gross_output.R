

get_df_gross_output <- function(df_gross_output_raw,
                                df_exchange_rates,
                                df_deflators,
                                df_sectors_icio_to_agg,
                                df_sectors_agg){
  
  # Deflation
  df_go_deflated <- df_gross_output_raw %>%
    left_join(
      df_exchange_rates %>% 
        rename(USD_to_NAC = value)
    ) %>%
    left_join(
      df_deflators %>% rename(deflator = value)
    ) %>%
    mutate(
      # go: gross output at current prices (CP), in national currency
      go_cp = go_USD * USD_to_NAC,  
      # go_kp2020: gross output deflated at constant 2020 prices (KP), in national currency
      go_kp2020 = go_cp / deflator * 100, 
      .keep = "unused"
    ) 
  
  # Aggregation
  ## Tidy data for calculating volume change using Laspeyre indexes
  df_go_tidy <- df_sectors_icio_to_agg %>% 
    left_join(
      df_go_deflated,
      join_by(code_icio == sector_code)
    ) %>% 
    transmute(
      country_code,
      sector_code = code_icio, 
      sector_code_agg = code_agg,
      year,
      go_cp,
      go_kp = go_kp2020
    ) %>% 
    arrange(country_code, sector_code, year)
  
  ## Gross output at current prices of the "aggregated" sectors
  df_go_cp_agg <- df_go_tidy %>% 
    group_by(country_code, sector_code_agg, year) %>% 
    summarize(go_cp_agg = sum(go_cp))
  
  df_go_preprocessed <- df_go_tidy %>% 
    left_join(
      df_go_cp_agg,
      join_by(country_code, sector_code_agg, year)
    ) %>% 
    group_by(country_code, sector_code_agg, sector_code) %>% 
    transmute(
      year,
      lag_go_cp = lag(go_cp),
      lag_go_cp_agg = lag(go_cp_agg),
      go_kp,
      lag_go_kp = lag(go_kp)
    )
  
  ## Volume change rate of the "aggregated" sectors using Laspeyre indexes
  df_volume_change <- df_go_preprocessed %>% 
    group_by(country_code, sector_code_agg, year) %>%
    summarize(
      volume_change = sum(lag_go_cp / lag_go_cp_agg * go_kp / lag_go_kp)
    ) %>% 
    mutate(
      volume_change = ifelse(year == min(year), 1, volume_change)
    )
  
  ## Unchaining
  df_go_aggregated <- df_volume_change %>% 
    left_join(df_go_cp_agg) %>% 
    group_by(country_code, sector_code_agg) %>% 
    mutate(
      # unchained volume growth index with first year (1995) as the reference year
      cum_volume_change = cumprod(volume_change),
      # unchained volume growth index with 2020 as the reference year
      # (2020 is the year at which go_cp_agg = go_kp_agg)
      cum_volume_change_ref2020 = cum_volume_change / cum_volume_change[year == 2020],
      # output at the aggregated level, in constant 2020 prices 
      go_kp_agg = go_cp_agg[year == 2020] * cum_volume_change_ref2020
    ) %>% 
    transmute(
      country_code, 
      sector_code_agg, 
      year, 
      go_cp = go_cp_agg, 
      go_kp2020 = go_kp_agg
    )
  
  # Smoothing 
  df_go_smoothed <- df_go_aggregated %>% 
    group_by(country_code, sector_code_agg) %>% 
    mutate(
      across(starts_with("go"), 
             ~ global_ksmooth(year, .)
      )
    ) %>% 
    ungroup()
  
  # Formatting
  df_gross_output <- df_go_smoothed %>% 
    pivot_longer(
      cols = starts_with("go_"),
      names_prefix = "go_",
      names_to = "price_base",
      values_to = "value"
    ) %>% 
    left_join(
      df_sectors_agg,
      join_by(sector_code_agg)
    ) %>% 
    transmute(
      sector_code = sector_code_agg,
      sector_name = sector_name_agg,
      sector_category,
      price_base,
      year,
      value,
      unit = "MEUR"
    ) %>% 
    arrange(sector_code, price_base, year)
  
}

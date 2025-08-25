

get_df_ida <- function(df_labour, 
                       df_gross_output,
                       df_sectors_agg){
  
  # Computation
  ## Data wrangling
  df_gross_output_wide <- df_gross_output %>% 
    filter(price_base == "kp2020") %>% 
    transmute(
      sector_code,
      year,
      go_kp2020 = value
    )
  
  df_labour_wide <- df_labour %>% 
    select(sector_code, localisation, year, value) %>% 
    pivot_wider(
      names_from = "localisation",
      values_from = "value",
      names_prefix = "labour_"
    )
    
  ## Decomposition of pD into three drivers of changes: pE, alpha, beta
  df_ida_drivers <- df_gross_output_wide %>% 
    left_join(
      df_labour_wide,
      join_by(sector_code, year)
      ) %>% 
    group_by(sector_code) %>% 
    mutate(
      pD = go_kp2020 / labour_direct,
      pE = go_kp2020 / (labour_direct + 
                       labour_indirect_domestic + 
                       labour_indirect_foreign_TiM),
      alpha = (labour_direct + labour_indirect_domestic + 
               labour_indirect_foreign_TiM) / 
              (labour_direct + labour_indirect_domestic),
      beta =  (labour_direct + labour_indirect_domestic) / labour_direct
      ) %>% 
    # Rescale all drivers according to pD = 100 in 1995,
    transmute(
      year,
      pD = pD / first(pD) * 100,
      pE = pE / first(pD) * 100,
      alpha = alpha / first(pD) * 100,
      beta = beta / first(pD) * 100
    )
  
  ## Preprocessing
  df_ida_preprocessed <- df_ida_drivers %>% 
    group_by(sector_code) %>% 
    arrange(sector_code, year) %>% 
    transmute(
      year,
      delta_pD = ifelse(
        year == min(year), 
        0, 
        pD - lag(pD)
        ),
      ratio_pD = ifelse(
        year == min(year), 
        1, 
        pD / lag(pD)
        ),
      ratio_pE = ifelse(
        year == min(year), 
        1, 
        pE / lag (pE)
        ),
      ratio_alpha = ifelse(
        year == min(year), 
        1, 
        alpha / lag(alpha)
        ),
      ratio_beta = ifelse(
        year == min(year), 
        1, 
        beta / lag(beta)
        )
      )
  
  ## Computing year-by-year effects of each driver
  df_ida_by_year <- df_ida_preprocessed %>% 
    group_by(sector_code) %>%
    arrange(sector_code, year) %>% 
    transmute(
      year,
      delta_pD,
      contribution_pE = ifelse(
        year == min(year), 
        0, 
        delta_pD * log(ratio_pE) / log(ratio_pD)
        ),
      contribution_alpha = ifelse(
        year == min(year), 
        0, 
        delta_pD * log(ratio_alpha) / log(ratio_pD)
        ),
      contribution_beta = ifelse(
        year == min(year), 
        0, 
        delta_pD * log(ratio_beta) / log(ratio_pD)
        )
    )
  
  ## Computing the cumulative effect over time of each driver 
  df_ida_cumulative <- df_ida_by_year %>% 
    group_by(sector_code) %>%
    arrange(sector_code, year) %>% 
    transmute(
      year,
      delta_pD = cumsum(delta_pD),
      contribution_pE = cumsum(contribution_pE),
      contribution_alpha = cumsum(contribution_alpha),
      contribution_beta = cumsum(contribution_beta)
    )

  # Formatting
  df_ida <- df_ida_cumulative %>% 
    left_join(
      df_sectors_agg,
      join_by(sector_code == sector_code_agg)
      ) %>% 
    group_by(sector_code, sector_name_agg) %>% 
    pivot_longer(
      cols = c("delta_pD", 
               starts_with("contribution_")
               ), 
      names_to = "indicator"
      ) %>%
    ungroup() %>% 
    transmute(
      sector_code,
      sector_name = sector_name_agg,
      indicator,
      year,
      value,
      unit = "%"
      ) %>% 
    arrange(indicator, sector_code, year)
  
}

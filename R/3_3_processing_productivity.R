

get_df_labour_productivity <- function(df_labour,
                                       df_gross_output,
                                       df_sectors_agg){

  # Computation
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
  
  df_labour_productivity <- df_gross_output_wide %>% 
    left_join(df_labour_wide) %>% 
    mutate(
      pD = go_kp2020 / labour_direct,
      pE = go_kp2020 / ( labour_direct + 
                         labour_indirect_domestic + 
                         labour_indirect_foreign_TiM
                         )
    ) %>%
    # Formatting
    pivot_longer(
      cols = c("pD", "pE"),
      names_to = "indicator",
      values_to = "value"
      ) %>% 
    left_join(
      df_sectors_agg,
      join_by(sector_code == sector_code_agg)
      ) %>% 
    transmute(
      sector_code,
      sector_name = sector_name_agg,
      sector_category,
      indicator,
      year,
      value,
      unit = "MEUR_per_1000people",
      price_base = "kp2020"
      ) %>% 
    arrange(sector_code, indicator)
  
}
  
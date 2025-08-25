

get_df_ratio_intermediate_imp <- function(df_ratio_intermediate_imp_raw,
                                          df_sectors_agg){
  
  ## Smoothing
  df_ratio_intermediate_imp_smoothed <- df_ratio_intermediate_imp_raw %>% 
    group_by(sector_code_agg) %>% 
    mutate(
      value = global_ksmooth(year, value)
    ) %>% 
    ungroup()
  
  ## Formatting
  df_ratio_intermediate_imp <- df_ratio_intermediate_imp_smoothed %>% 
    left_join(
      df_sectors_agg,
      join_by(sector_code_agg)
    ) %>% 
    transmute(
      sector_code = sector_code_agg,
      sector_name = sector_name_agg,
      sector_category,
      year,
      value = value * 100,
      unit = "%") %>% 
    arrange(sector_code, year) 

}
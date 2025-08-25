


get_df_labour <- function(df_labour_raw,
                          df_sectors_agg){
  
  # Smoothing
  df_labour_smoothed <- df_labour_raw %>% 
    group_by(country_code, sector_code_agg) %>% 
    mutate(
      across(starts_with("labour"), 
             ~ global_ksmooth(year, .)
      )
    ) %>% 
    ungroup()
  
  # Formatting 
  df_labour <- df_labour_smoothed %>% 
    pivot_longer(
      cols = starts_with("labour"), 
      names_prefix = "labour_", 
      names_to = "localisation", 
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
      localisation,
      year,
      value,
      unit = "1000people"
    ) %>% 
    arrange(sector_code, localisation, year)

}


get_df_labour_offshoring <- function(df_labour){
  
  # Computation
  df_labour_offshoring <- df_labour %>%
    pivot_wider(
      names_from = "localisation",
      values_from = "value"
    ) %>% 
    mutate(
      Loff_TiM_countries = indirect_foreign_TiM / (
        direct + 
          indirect_domestic + 
          indirect_foreign_TiM 
      ),
      Loff_world = (indirect_foreign_TiM  + 
                      indirect_foreign_ROW
      ) / (
        direct + 
          indirect_domestic + 
          indirect_foreign_TiM + 
          indirect_foreign_ROW
      ),
      .keep = "unused"
    ) %>%
    # Formatting
    pivot_longer(
      cols = starts_with("Loff_"),
      names_prefix = "Loff_",
      names_to = "geo_coverage",
      values_to = "value"
    ) %>%  
    transmute(
      sector_code,
      sector_name,
      sector_category,
      geo_coverage,
      year,
      value = value * 100,
      unit = "%") %>% 
    arrange(sector_code, geo_coverage, year)
  
}

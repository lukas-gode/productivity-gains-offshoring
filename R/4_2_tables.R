

get_df_table1 <- function(df_labour_offshoring,
                          df_sectors_agg){

  df_delta_Loff_TiM = df_labour_offshoring %>% 
    filter(
      geo_coverage == "TiM_countries",
      year %in% c(1995, 2007, 2020)
      ) %>% 
    pivot_wider(
      names_from = year, 
      values_from = value, 
      names_prefix = "Loff_"
      ) %>%  
    transmute(
      sector_name,
      delta_Loff_TiM_9507 = Loff_2007 - Loff_1995,
      delta_Loff_TiM_0720 = Loff_2020 - Loff_2007,
      delta_Loff_TiM_9520 = Loff_2020 - Loff_1995
      ) 
    
  df_mean_Loff = df_labour_offshoring %>% 
    pivot_wider(
      names_from = geo_coverage, 
      values_from = value, 
      names_prefix = "Loff_"
      ) %>%
    group_by(sector_name) %>% 
    summarize(
      across(starts_with("Loff"), ~ mean(.), 
             .names = "mean_{.col}"
             )
      )

  df_table1 = df_sectors_agg %>%
    left_join(df_mean_Loff,
              join_by(sector_name_agg == sector_name)) %>% 
    left_join(df_delta_Loff_TiM,
              join_by(sector_name_agg == sector_name)) %>% 
    transmute(
      sector_category,
      sector_name = sector_name_agg,
      mean_Loff_world = round(mean_Loff_world, 1),
      mean_Loff_TiM = round(mean_Loff_TiM_countries, 1),
      delta_Loff_TiM_9507 = round(delta_Loff_TiM_9507, 1),
      delta_Loff_TiM_0720 = round(delta_Loff_TiM_0720, 1),
      delta_Loff_TiM_9520 = round(delta_Loff_TiM_9520, 1)
      )
  
}


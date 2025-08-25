

get_df_si_fig1 <- function(df_deflators, 
                           df_sectors_icio_to_agg,
                           df_sectors_agg){
  
  df_si_fig1 <- df_sectors_agg %>% 
    left_join(
      df_sectors_icio_to_agg,
      join_by(sector_name_agg == name_agg)
    ) %>%
    left_join(
      df_deflators,
      join_by(code_icio == sector_code)
    ) %>% 
    transmute(
      sector_name = sector_name_agg,
      sector_code_icio = code_icio,
      year,
      value
    ) %>% 
    arrange(sector_name, sector_code_icio, year)

}


get_df_si_fig2 <- function(df_gross_output){
  
  df_si_fig2 <- df_gross_output %>% 
    transmute(
      sector_code,
      sector_name,
      price_base,
      year,
      value = round(value * 1e-3, 1),
      unit = "billion €"
    )

}


get_df_si_fig3 <- function(df_ratio_intermediate_imp){
  
  df_si_fig3 <- df_ratio_intermediate_imp 
  
}


get_df_si_fig4_y <- function(df_H_gloria_tidy, df_H,
                             year_y,
                             df_countries_tim){
  
  df_countries_tim_available <- df_countries_tim %>% 
    filter(data_availability == TRUE) %>% 
    select(country_code)
  
  df_H_gloria = df_H_gloria_tidy %>%
    filter(
      country_code %in% df_countries_tim_available$country_code
    ) %>%
    transmute(
      country_code, 
      sector_code, 
      year = year_y, 
      value_gloria = value
    )
  
  df_H_icio = df_H %>% 
    filter(
      country_code %in% df_countries_tim_available$country_code
    ) %>%
    transmute(
      country_code,
      sector_code,
      year = year_y, 
      value_icio = value
    )
  
  df_fig4_si_y = left_join(df_H_gloria, df_H_icio) %>%
    transmute(
      country_code,
      sector_code,
      year,
      ratio_gloria_over_icio = ifelse(
        value_icio != 0, 
        round(value_gloria / value_icio, 3),
        NA
      )
    ) %>% 
    filter(!is.na(ratio_gloria_over_icio))

}


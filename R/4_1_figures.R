

get_df_fig1 <- function(df_labour){
  
  df_fig1 <- df_labour %>%
    filter(
      sector_code == "NATIONAL",
      localisation != "indirect_domestic"
    ) %>% 
    transmute(
      localisation,
      year,
      indicator = "embodied_labour_inputs",
      value = round(value * 1e-3, 2),
      unit = "Mpeople"
    )
  
}


get_df_fig2 <- function(df_labour){
  
  df_fig2 <- df_labour %>%
    filter(sector_code != "NATIONAL") %>% 
    transmute(
      sector_code,
      sector_name,
      localisation,
      year,
      indicator = "embodied_labour_inputs",
      value = round(value * 1e-3, 2),
      unit = "Mpeople"
    )
  
}


get_df_fig3 <- function(df_labour_productivity){
  
  df_fig3 = df_labour_productivity %>%
    pivot_wider(
      names_from = "indicator",
      values_from = "value"
    ) %>% 
    group_by(sector_code, sector_name) %>%
    mutate(
      pD_base100 = pD / pD[which.min(year)]*100,
      pE_same_base_as_pD = pE / pD[which.min(year)]*100,
      pE_base100 = pE / pE[which.min(year)]*100
    ) %>%
    pivot_longer(
      cols = c(
        "pD_base100", 
        "pE_base100", 
        "pE_same_base_as_pD"
      ), 
      names_to = "indicator",
    ) %>% 
    transmute(
      sector_code,
      sector_name,
      indicator, 
      year,
      value = round(value, 1),
      unit = "Index"
    ) %>% 
    arrange(indicator, sector_code, year)
  
}


get_df_fig4 <- function(df_ida){
  
  df_fig4 <- df_ida %>% 
    transmute(
      sector_name,
      indicator,
      year,
      value = round(value, 1),
      unit
    )
  
}


get_df_fig5 <- function(df_labour_productivity, 
                        df_labour_offshoring){
  
  df_mean_Loff <- df_labour_offshoring %>% 
    filter(
      sector_code != "NATIONAL",
      geo_coverage == "TiM_countries"
    ) %>% 
    group_by(sector_category, sector_code, sector_name) %>% 
    summarize(
      mean_Loff = mean(value)
    ) %>% 
    transmute(
      sector_category,
      sector_code, 
      sector_name,
      indicator = "mean_Loff",
      value = round(mean_Loff, 2),
      unit = "%"
    )
  
  df_cagr_pD <- df_labour_productivity %>% 
    filter(indicator == "pD") %>% 
    group_by(sector_category, sector_code, sector_name) %>% 
    summarize(
      instantaneous_growth_rate_pD = (coef(lm(log(value) ~ year))[2])
    ) %>% 
    ungroup() %>% 
    mutate(
      cagr_pD = exp(instantaneous_growth_rate_pD)- 1
    ) %>% 
    transmute(
      sector_code,
      sector_name,
      sector_category,
      indicator = "cagr_pD",
      value = round(cagr_pD * 100, 2),
      unit = "%"
    )
  
  df_fig5 <- df_mean_Loff %>% 
    rbind(df_cagr_pD) %>%
    arrange(indicator, sector_category, sector_code)
  
}

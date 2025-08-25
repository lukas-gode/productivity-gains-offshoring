

get_df_tim <- function(file_tim, 
                       list_years,
                       df_sectors_icio){
  
  df_tim <- read_ods(file_tim, sheet = "data") %>%
    transmute(
      country_code = `COU`, 
      sector_code_2 = `IND`,
      year = `TIME`, 
      value = `Value`
    ) %>% 
    filter(
      sector_code_2 %in% df_sectors_icio$sector_code_2,
      year %in% list_years
    ) %>% 
    left_join(
      df_sectors_icio,
      join_by(sector_code_2)
    ) %>% 
    select(
      country_code, 
      sector_code, 
      year, 
      value
    )
  
}


get_df_h <- function(df_tim, 
                     year_y, 
                     df_rows_icio){
  
  df_tim_y <- df_tim %>% 
    filter(year == year_y)
  
  df_h <- df_rows_icio %>%
    left_join(df_tim_y) %>% 
    transmute(country_code,
              sector_code,
              value = ifelse(!is.na(value), value, 0)
    )
  
}


get_df_countries_tim <- function(df_tim, 
                                 list_years,
                                 df_rows_icio){
  
  df_countries_tim <- expand.grid(
    country_code = unique(df_rows_icio$country_code),
    sector_code = unique(df_rows_icio$sector_code),
    year = list_years
  ) %>%
    left_join(
      df_tim, 
      by = c("country_code", "sector_code", "year")
    ) %>%
    # Count the number of sectors with NA values for each country / year
    group_by(country_code, year) %>%
    summarize(
      n_sectors_NA = sum(is.na(value)), 
      .groups = "drop"
    ) %>% 
    # Create a boolean "data_availability" = TRUE if the country is covered by TiM
    group_by(country_code) %>%
    summarize(
      data_availability = ifelse(sum(n_sectors_NA) == 0, "TRUE", "FALSE")
    )
  
}


get_df_deflators <- function(file_deflators,
                             country_c0, 
                             list_years,
                             df_sectors_icio){
  
  df_deflators_raw <- read_ods(
    file_deflators, 
    sheet = "data"
  ) %>% 
    # Retain only deflators ("D")
    filter(PRICE_BASE == "D",
           REF_AREA == country_c0,
           TIME_PERIOD %in% list_years
    ) %>% 
    transmute(
      year = TIME_PERIOD, 
      country_code = REF_AREA, 
      sector_code = ACTIVITY,
      value = OBS_VALUE
    ) %>% 
    arrange(sector_code, year)
  
  df_deflators <- crossing(
    country_code = country_c0,
    sector_code = df_sectors_icio$sector_code,
    year = list_years
  ) %>%
    left_join(df_deflators_raw) 
  
  # Defining the OPI of the three "Mining and quarrying" OECD-ICIO sub-sectors 
  # as the OPI of the aggregate "Mining and quarrying"  STAN sector
  df_deflators$value[df_deflators$sector_code %in% c("B05_06", "B07_08", "B09")] <-
    df_deflators_raw$value[df_deflators_raw$sector_code == "B"]
  
  
  # Defining the deflators of the "Paper products and printing" OECD-ICIO sector 
  # as a weighted average of the deflators from the two STAN sub-sectors
  df_deflators_C17_C18 <- df_deflators_raw %>%
    filter(sector_code %in% c("C17", "C18")) %>%
    pivot_wider(
      names_from = sector_code, 
      # OPI: output price index
      names_prefix = "OPI_", 
      values_from = value
    )
  
  df_X_cp_C17_C18 <- read_ods(file_deflators, sheet = "data") %>%
    filter(PRICE_BASE == "V") %>%
    transmute(
      year = TIME_PERIOD, 
      country_code = REF_AREA, 
      sector_code = ACTIVITY,
      X_cp = OBS_VALUE
    ) %>%
    filter(
      year %in% list_years, 
      country_code == country_c0,
      sector_code %in% c("C17","C18")
    ) %>%
    pivot_wider(
      names_from = sector_code, 
      names_prefix = "X_cp_", 
      values_from = X_cp
    )
  
  df_deflators_C17C18 <- left_join(
    df_deflators_C17_C18, 
    df_X_cp_C17_C18
  ) %>%
    mutate(
      OPI_C17C18 = ( OPI_C17 * X_cp_C17 / (X_cp_C17 + X_cp_C18) ) + 
        ( OPI_C18 * X_cp_C18 / (X_cp_C17 + X_cp_C18) )
    ) %>% 
    arrange(year)
  
  df_deflators$value[df_deflators$sector_code == "C17_18"] <- df_deflators_C17C18$OPI_C17C18
  
  df_deflators <- df_deflators %>%
    transmute(
      country_code, 
      sector_code,
      year, 
      value
    )
  
}


get_df_exchange_rates <- function(file_exchange_rates,
                                  country_c0, 
                                  list_years){
  
  df_exchange_rates <- read_ods(file_exchange_rates, sheet = "data") %>%
    filter(Measure == "National currency per US dollar") %>%
    transmute(
      country_code = `LOCATION`,
      year = `Year`,
      value = `Value`
    ) %>% 
    filter(
      country_code == country_c0,
      year %in% list_years
    )
  
}


get_df_population_XAS <- function(file_population, 
                                  list_years){
  
  ### Countries/areas considered: Taiwan, Timor Leste, Maldives 
  ### and countries from the Pacific Islands with a population > 10 000
  list_iso3c_XAS <- c("TWN", "MDV", "TLS", "FJI", "NCL", "SLB", "VUT", 
                      "GUM", "KIR", "MHL", "FSM", "NRU", "MNP", "PLW", 
                      "ASM", "COK", "PYF", "WSM", "TON", "TUV", "WLF")
  
  df_population_XAS <- read_ods(
    file_population, 
    sheet = "data"
  ) %>% 
    transmute(
      name = Location,
      iso3c = ISO3_code,
      year = Time,
      population = PopTotal
    ) %>% 
    filter(
      iso3c %in% list_iso3c_XAS,
      year %in% list_years
    )
  
}


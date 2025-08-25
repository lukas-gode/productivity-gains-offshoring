

get_df_rows_gloria <- function(path_gloria){
  
  df_gloria_country_labels = read_excel(
    paste0(path_gloria, "/GLORIA_ReadMe_059a.xlsx"), 
    sheet = "Regions"
  ) %>%
    transmute(
      region_id = `Lfd_Nr`, 
      country_code = `Region_acronyms`, 
      country_name = `Region_names`
    )
  
  df_gloria_sector_labels = read_excel(
    paste0(path_gloria, "/GLORIA_ReadMe_059a.xlsx"), 
    sheet = "Sectors"
  ) %>%
    transmute(
      sector_id = `Lfd_Nr`, 
      sector_name = `Sector_names`
    )
  
  df_rows_gloria = expand.grid(
    sector_id = df_gloria_sector_labels$sector_id,
    region_id = df_gloria_country_labels$region_id
  ) %>% 
    mutate(
      row_id = as.character(seq(1, 19680))
    ) %>% 
    left_join(df_gloria_country_labels) %>%
    left_join(df_gloria_sector_labels) %>%
    transmute(
      row_id, 
      country_code, 
      country_name, 
      sector_name
    )
  
}


get_df_sectors_gloria_to_icio <- function(file_gloria_to_icio){
  
  df_sectors_gloria_to_icio = read_ods(
    path = file_gloria_to_icio,
    sheet = "data_sectors", 
    col_names = T
  ) %>%
    rename(sector_name_gloria = 1) %>% 
    slice(3:n()) %>%
    pivot_longer(
      cols = !sector_name_gloria, 
      names_to = "sector_code", 
      values_to = "sector_weight"
    ) %>% 
    filter(sector_weight != 0)
  
}


get_df_countries_gloria_to_icio <- function(file_gloria_to_icio){
  
  df_countries_gloria_to_icio = read_ods(
    path = file_gloria_to_icio,
    sheet = "data_regions", 
    col_names = T
  ) %>% 
    rename(country_code_gloria = 1) %>% 
    select(-code_icio) %>% 
    tail(-2) %>%
    pivot_longer(
      cols = !country_code_gloria, 
      names_to = "country_code", 
      values_to = "country_weight"
    ) %>%
    filter(country_weight != 0) %>%
    mutate(
      country_weight = na_if(country_weight, "variable")
    ) %>%
    mutate(
      country_weight = as.numeric(country_weight)
    )
  
}


get_df_h_gloria_raw <- function(path_gloria, 
                                year_y,
                                df_rows_gloria){
  
  # Select only columns with data by sectors
  # (i.e., remove columns with data by product)
  # (GLORIA is provided as MR-SUT tables, see documentation)
  n_regions <- 164
  n_sectors <- 120
  
  cols_sectors <- c()
  for(r in seq(1, n_regions)){
    cols_sectors = c(
      cols_sectors,
      seq(2 * (r - 1) * n_sectors + 1,
          2 * (r - 1) * n_sectors + n_sectors)
    ) }
  
  rows_emp <- read_excel(
    path = paste0(path_gloria, "/GLORIA_ReadMe_059a.xlsx"),
    sheet = "Satellites"
  ) %>% 
    filter(Sat_head_indicator == "Employment") %>%
    pull(Lfd_Nr)
  
  file_mat_emp_gloria <- list.files(
    path = paste0(
      path_gloria, 
      "/GLORIA_SatelliteAccounts_059_", 
      year_y
    ),
    pattern = paste0(
      "120secMother_AllCountries_002_TQ-Results_",
      year_y, 
      "_059_Markup001"
    ),
    full.names = TRUE
  )[1]
  
  mat_emp_gloria <- fread(
    file = file_mat_emp_gloria,
    select = cols_sectors,
    header = F,
    skip = min(rows_emp) - 1,
    nrows = length(rows_emp)
  )
  
  df_h_gloria_raw <- df_rows_gloria %>%
    cbind(value = colSums(mat_emp_gloria))
  
}


get_df_h_gloria_tidy <- function(df_h_gloria_raw,
                                 list_icio,
                                 year_y, 
                                 df_population_XAS,
                                 df_sectors_gloria_to_icio,
                                 df_countries_gloria_to_icio,
                                 df_rows_icio){
  
  mat_X <- list_icio$mat_X
  
  df_h_gloria_raw <- df_h_gloria_raw %>%
    transmute(
      country_code_gloria = country_code, 
      sector_name_gloria = sector_name, 
      value = value
    ) 
  
  # Match regions_gloria and regions_icio
  df_h_gloria_intermediate = df_h_gloria_raw %>% 
    left_join(
      df_countries_gloria_to_icio, 
      relationship = "many-to-many",
      join_by(country_code_gloria)
    ) 
  
  # Disaggregate labour inputs from the GLORIA region "Rest of Asia-Pacific" (XAS) 
  # Between the ICIO regions Taiwan (TWN) and Rest of the World (ROW). 
  # Weights: population share
  
  pop_ratio_TWN_to_XAS = df_population_XAS %>% 
    group_by(year) %>% 
    summarize(
      value = population[iso3c == "TWN"] / sum(population)
    ) %>% 
    filter(year == year_y) %>% 
    select(value) %>% 
    as.numeric()
  
  df_h_gloria_intermediate <- df_h_gloria_intermediate %>%
    mutate(
      country_weight = case_when(
        country_code_gloria == "XAS" & country_code == "TWN" ~ pop_ratio_TWN_to_XAS,
        country_code_gloria == "XAS" & country_code == "ROW" ~ (1 - pop_ratio_TWN_to_XAS),
        TRUE ~ country_weight
      )
    )
  
  df_h_gloria_intermediate = df_h_gloria_intermediate %>%
    mutate(country_weight = as.numeric(country_weight)) %>%
    group_by(country_code, sector_name_gloria) %>%
    summarize(
      value = sum(value*country_weight), 
      .groups = 'drop'
    )
  
  # Match sectors_gloria and sectors_icio
  df_h_gloria_intermediate = df_h_gloria_intermediate %>% 
    left_join(
      df_sectors_gloria_to_icio,
      relationship = "many-to-many"
    )
  
  # Distribute labour inputs from the Gloria sector "Other services" between 
  # ICIO sectors S and T (S+T = "Other services). Weights: gross output shares
  df_go_ratio_S_to_ST = df_rows_icio %>% 
    mutate(value = mat_X) %>%
    filter(sector_code %in% c("S", "T")) %>%
    group_by(country_code) %>%
    summarize(
      ratio_S_to_ST = value[sector_code == "S"] / sum(value)
    )
  
  df_h_gloria_intermediate = df_h_gloria_intermediate %>% 
    left_join(df_go_ratio_S_to_ST) %>%
    mutate(
      sector_weight = ifelse(
        !(sector_code %in% c("S", "T")), 
        sector_weight, 
        ifelse(
          sector_code == "S", 
          ratio_S_to_ST, 
          1 - ratio_S_to_ST
        )
      )
    )
  
  df_h_gloria_tidy = df_h_gloria_intermediate %>%
    group_by(country_code, sector_code) %>%
    mutate(
      sector_weight = as.numeric(sector_weight)
    ) %>% 
    summarize(
      value = sum(value*sector_weight),
      .groups = "drop"
    ) %>% 
    mutate(
      value = round(value * 1e-3, 2)
    )
  
}


get_mat_h_adjusted <- function(df_H, 
                               df_h_gloria_tidy, 
                               year_y, 
                               df_rows_icio, 
                               df_countries_tim){
  
  df_h_gloria = df_h_gloria_tidy %>%
    rename(value_gloria_base = value)
  
  df_h_icio = df_H %>%
    rename(value_icio = value)
  
  mat_h_adjusted = df_h_icio %>%
    left_join(df_h_gloria) %>%
    left_join(df_countries_tim) %>% 
    transmute(
      value = ifelse(
        data_availability == T, 
        value_icio, 
        value_gloria_base
      )
    ) %>% 
    as.matrix()
  
}
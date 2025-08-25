

get_df_sectors_icio_to_agg <- function(file_icio_to_agg){
  
  df_sectors_icio_to_agg <- read_ods(
    file_icio_to_agg,
    sheet = "data"
  )
  
}


get_df_rows_icio <- function(path_icio, 
                             df_sectors_icio_to_agg){
  
  df_sectors_icio_to_agg <- df_sectors_icio_to_agg %>% 
    transmute(
      sector_code = code_icio,
      sector_code_agg = code_agg
    )
  
  df_rows_icio <- read_excel(
    paste0(path_icio, "ReadMe_ICIO_small.xlsx"),
    sheet = "RowItems",
    range = "C4:E3469"
  ) %>% 
    transmute(
      row_id = `Sector code`,
      country_code = `Country`,
      sector_code = `Industry/Final demand`
    ) %>% 
    left_join(
      df_sectors_icio_to_agg,
      join_by(sector_code)
    )
  
}


get_df_sectors_agg <- function(df_sectors_icio_to_agg){
  
  df_sectors_agg <- data.frame(
    code_agg = c("AGRI", "MINQ", "FOOD", "TRAN", "ELMA", 
                 "CLOT", "CHEM", "META", "WOOD", "MANO", 
                 "UTIL", "CONS", "MOBI", "RETA", "INCO", 
                 "FIRE", "HRES", "AERE", "HEPA", "OSER")
  ) %>% 
    left_join(
      df_sectors_icio_to_agg,
      join_by(code_agg)
    ) %>% 
    transmute(
      sector_category = name_category,
      sector_code_agg = code_agg,
      sector_name_agg = name_agg
    ) %>% 
    unique() %>% 
    rbind(
      data.frame(
        sector_category = "National level", 
        sector_name_agg = "National level", 
        sector_code_agg = "NATIONAL")
    )
  
}


get_df_sectors_icio <- function(path_icio){
  
  df_sectors_icio <- read_excel(
    paste0(path_icio, "ReadMe_ICIO_small.xlsx"), 
    sheet = "Area_Activities",
    range = "H3:J48"
  ) %>% 
    transmute(
      sector_code_2 = `Code`,
      sector_code = `new .Stat code`,
      sector_name_icio = `Industry`
    )
  
}


get_df_countries_icio <- function(path_icio){
  
  df_countries_icio <- read_excel(
    paste0(path_icio, "ReadMe_ICIO_small.xlsx"),
    sheet = "Area_Activities",
    range = "C3:E80"
  ) %>% 
    transmute(
      country_code = `Code`,
      country_name = `countries`
    )
  
}


get_list_icio <- function(path_icio, year_y){
  
  df_icio <- fread(paste0(path_icio, year_y, ".SML.csv")) %>% 
    rename("row_id" = 1)
  
  # Matrix of inter-industry flows (Z)
  mat_Z <- df_icio[1:3465,2:3466] %>% 
    mapply(FUN = as.numeric) %>% 
    as.matrix()
  
  # Vector of gross outputs (x)
  mat_X <- df_icio[1:3465,3544] %>%
    mapply(FUN = as.numeric) %>% 
    as.matrix()
  
  list_icio = list(
    mat_Z = mat_Z,
    mat_X = mat_X
  )
  
}

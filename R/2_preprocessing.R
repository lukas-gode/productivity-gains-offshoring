

get_df_E_prime <- function(list_icio,
                           mat_h_adjusted, 
                           year_y, 
                           sector_s0, 
                           country_c0, 
                           df_rows_icio){
  
  mat_X <- list_icio$mat_X
  mat_Z <- list_icio$mat_Z
  
  np <- import("numpy")
  
  if(sector_s0 == "NATIONAL"){ 
    rows_CS = which(df_rows_icio$country_code == country_c0) 
  } else{
    rows_CS = which(df_rows_icio$country_code == country_c0 & 
                      df_rows_icio$sector_code_agg == sector_s0)
  }
  
  # Z'
  Z_CS_row <- colSums(matrix(mat_Z[rows_CS,], 
                             ncol = ncol(mat_Z)
  )
  )
  
  Z_prime_row <- rbind(Z_CS_row, mat_Z[-rows_CS,])
  
  Z_CS_col <- rowSums(matrix(Z_prime_row[,rows_CS], 
                             nrow = nrow(Z_prime_row)
  )
  )
  
  Z_prime <- cbind(Z_CS_col,
                   Z_prime_row[,-rows_CS]
  )   
  
  # x'
  X_CS = sum(mat_X[rows_CS,])
  X_prime = matrix(c(X_CS, mat_X[-rows_CS,]), 
                   ncol = 1
  ) 
  
  # h'
  H_CS <- sum(mat_h_adjusted[rows_CS,])
  H_prime <- matrix(c(H_CS, mat_h_adjusted[-rows_CS, ]), 
                    ncol = 1
  )
  
  # A'
  A_prime = t(t(Z_prime) * as.vector(1/X_prime))
  A_prime[is.nan(A_prime)] = 0
  
  # (I-A')^-1
  I_minus_A_prime <- np$array(diag(nrow = dim(A_prime)[1]) - A_prime)
  
  # L'
  L_prime <- np$linalg$inv(I_minus_A_prime)
  
  # Vector of direct labour inputs coefficients (h_c')
  Hc_prime <- matrix(H_prime / X_prime, 
                     nrow = 1
  )
  
  # L'*
  L_prime_star = sweep(L_prime, 2, diag(L_prime), "/")
  
  # E' - Only the columns corresponding to sector S0 in country C0 are retrieved
  E_prime <- as.vector(Hc_prime) * as.vector(L_prime_star[,1]) * X_prime[1]
  E_prime[is.nan(E_prime)] <- 0
  
  df_rows_icio <- df_rows_icio %>% 
    transmute(country_code, 
              sector_code
    ) %>% 
    slice(-rows_CS)
  
  df_E_rows <- data.frame(
    country_code = country_c0, 
    sector_code = sector_s0
  ) %>%
    rbind(df_rows_icio)
  
  df_E_prime <- cbind(df_E_rows,
                      data.frame(value = E_prime)
  )
  
}


get_df_labour_raw <- function(df_E_prime, 
                              year_y, 
                              sector_s0, 
                              country_c0,
                              df_countries_tim){
  
  df_countries_tim_available <- df_countries_tim %>% 
    filter(data_availability == TRUE)
  
  # Direct labour inputs
  labour_direct_C0S0 <- df_E_prime %>%
    filter(
      country_code == country_c0,
      sector_code == sector_s0
    ) %>% 
    summarize(value = sum(value)) %>% 
    as.numeric()
  
  # Indirect domestic labour inputs
  labour_indirect_domestic_C0S0 <- ifelse(
    sector_s0 == "NATIONAL",
    0,
    df_E_prime %>%
      filter(
        country_code == country_c0,
        sector_code != sector_s0
      ) %>% 
      summarize(value = sum(value)
      ) %>% 
      as.numeric()
  )
  
  # Indirect foreign labour provided in countries covered by the TiM dataset 
  labour_indirect_foreign_TiM_C0S0 <- df_E_prime %>%
    filter(
      country_code != country_c0, 
      country_code %in% df_countries_tim_available$country_code
    ) %>% 
    summarize(value = sum(value)) %>%
    as.numeric()
  
  # Indirect foreign labour provided in countries missing in TiM
  labour_indirect_foreign_ROW_C0S0 <- df_E_prime %>%
    filter(
      country_code != country_c0, 
      !(country_code %in% df_countries_tim_available$country_code)
    ) %>%
    summarize(value = sum(value)) %>% 
    as.numeric()
  
  df_labour_raw <- data.frame(
    country_code = country_c0, 
    sector_code_agg = sector_s0, 
    year = year_y,
    labour_direct = labour_direct_C0S0,
    labour_indirect_domestic = labour_indirect_domestic_C0S0,
    labour_indirect_foreign_TiM = labour_indirect_foreign_TiM_C0S0,
    labour_indirect_foreign_ROW = labour_indirect_foreign_ROW_C0S0
  )
  
}


get_df_gross_output_raw <- function(list_icio,
                                    year_y,
                                    country_c0,
                                    df_rows_icio){
  
  mat_X <- list_icio$mat_X
  
  df_gross_output_raw = df_rows_icio %>%
    mutate(go_USD = as.vector(mat_X)) %>% 
    filter(country_code == country_c0) %>%
    transmute(
      country_code, 
      sector_code, 
      year = year_y,
      # go_USD: gross output at current prices (in US dollars)
      go_USD
    )
  
}


get_df_ratio_intermediate_imp_raw <- function(list_icio, 
                                              year_y, 
                                              country_c0,
                                              df_rows_icio, 
                                              df_sectors_icio_to_agg){
  
  mat_Z <- list_icio$mat_Z
  mat_X <- list_icio$mat_X
  
  # Aggregate intermediate outputs from all countries except c0
  mat_Z_RoW <- df_rows_icio %>% 
    select(country_code, sector_code) %>% 
    cbind(mat_Z) %>%
    filter(country_code != country_c0) %>% 
    group_by() %>% 
    summarize(
      across(-c(country_code, sector_code), 
             sum
      )
    )
  
  # Aggregate value of intermediate imports in C0, by production sector
  df_intermediate_imp_c0 <- df_rows_icio %>% 
    select(country_code, sector_code, sector_code_agg) %>%
    cbind(imp = t(mat_Z_RoW)) %>%
    filter(country_code == country_c0) %>% 
    group_by(country_code, sector_code_agg) %>% 
    summarize(imp = sum(imp))
  
  # Gross output in C0, by production sector
  df_gross_output_c0 <- df_rows_icio %>% 
    select(country_code, sector_code, sector_code_agg) %>% 
    cbind(go = as.vector(mat_X)) %>% 
    filter(country_code == country_c0) %>% 
    group_by(country_code, sector_code_agg) %>% 
    summarize(go = sum(go))
  
  df_ratio_intermediate_imp_raw <- df_intermediate_imp_c0 %>% 
    left_join(df_gross_output_c0) %>% 
    transmute(
      country_code,
      sector_code_agg,
      year = year_y,
      value = imp / go
    )

}


# Description -----------------------------------------------------------------------

# This script contains the pipeline used to generate the results presented in 
# the paper: "Labour productivity gains or offshoring? 
# Implications for post-growth proposals on the future of work". 

# Instructions on how to reproduce the results using the pipelines 
# can be found in the README file attached.

# Install packages ------------------------------------------------------------------

## You can install the packages required to execute the pipeline 
## by un-commenting  and executing the following lines:
# 
# if(!require("pacman")){install.packages("pacman")}
# pacman::p_load(# Targets pipeline
#                targets, tarchetypes,
#                # Data import, export, wrangling
#                here, readxl, readODS, tidyverse, data.table, zoo,
#                # Python interface
#                reticulate
#                )

# Pipeline --------------------------------------------------------------------------

# Load necessary packages
if(!require("pacman")){install.packages("pacman")}
pacman::p_load(here, targets, tarchetypes, ggplot2)

# Set the "targets" pipeline
tar_option_set(
  format = "qs",
  packages = c(
    # Data import, export, wrangling
    "here", "readxl", "readODS", "tidyverse", "data.table", "zoo",
    # Python interface
    "reticulate"
    )
  )

# Load functions used by the pipelines
tar_source("R")

# Create output directories
output_dirs <- c(
  here("outputs"),
  here("outputs", "main"),
  here("outputs", "si")
  )

lapply(output_dirs, function(dir) if(!dir.exists(dir)) dir.create(dir))

# Main parameters
parameters <- tar_plan(
  
  # Paths to the MRIO datasets - must be manually set
  path_gloria = normalizePath("../MRIO_dataset/Gloria_059a/raw/"),
  path_icio = "../MRIO_dataset/ICIO_2022_small/", 
  # Country and years of the analysis
  country_c0 = "DEU",
  list_years = seq(1995, 2020)
  
)

# Trace local files
files <- tar_plan(
  
  ## Concordance tables
  tar_file(file_icio_to_agg,
           here("data", "internal", "concordance", "icio_to_agg.ods")
           ),
  tar_file(file_gloria_to_icio,
           here("data", "internal", "concordance", "gloria_to_icio.ods")
           ),
  
  ## TiM dataset
  tar_file(file_tim,
           here("data", "external", "OECD_TiM.ods")
           ),
  
  ## Deflators
  tar_file(file_deflators,
           here("data", "external", "OECD_deflators.ods")
           ),
  
  ## EXchange rates
  tar_file(file_exchange_rates,
           here("data", "external", "OECD_exchange_rates.ods")
           ),
  
  ## Population
  tar_file(file_population,
           here("data", "external", "UN_population.ods")
           )

)

# Wrangling non-MRIO data
wrangling_other <- tar_plan(

  
  # Labour inputs from TiM and related -----------------------------
  
  ## TiM dataset
  tar_target(df_tim,
             get_df_tim(
               file_tim, 
               list_years, 
               df_sectors_icio
               )
             ),
  
  ## Vector of direct labour inputs with ICIO classification (h)
  tar_target(df_h_y, 
             get_df_h(
               df_tim, 
               list_years, 
               df_rows_icio
               ), 
             pattern = map(list_years)
             ),
  
  ## ICIO countries with available labour inputs data in TiM
  tar_target(df_countries_tim,
             get_df_countries_tim(
               df_tim, 
               list_years, 
               df_rows_icio
               )
             ),
  
  # Deflators ------------------------------------------------------
  
  tar_target(df_deflators,
             get_df_deflators(
               file_deflators, 
               country_c0, 
               list_years,
               df_sectors_icio
               )
             ),
  
  # Other ----------------------------------------------------------
  
  ## Exchange rates (from national currency to USD)
  tar_target(df_exchange_rates,
             get_df_exchange_rates(
               file_exchange_rates,
               country_c0, 
               list_years
               )
             ),
  
  ## # Population in the "Rest of Asia" (XAS) GLORIA region
  tar_target(df_population_XAS,
             get_df_population_XAS(
               file_population, 
               list_years
               )
             )
  
  )


# Wrangling OECD-ICIO data
wrangling_icio <- tar_plan(
  
  # Concordances ---------------------------------------------------
  # ICIO elementary sectors to aggregated sectors
  tar_target(df_sectors_icio_to_agg,
             get_df_sectors_icio_to_agg(
               file_icio_to_agg
               )
             ), 
  
  ## Rows of the ICIO tables: row_id, country_code, sector_code + agg_sector_code
  tar_target(df_rows_icio, 
             get_df_rows_icio(
               path_icio, 
               df_sectors_icio_to_agg
               )
             ),
  
  # Labels ---------------------------------------------------------
  ## Aggregated sectors: category, name, code
  tar_target(df_sectors_agg,
             get_df_sectors_agg(
               df_sectors_icio_to_agg
               )
             ),

  
  ## Elementary sectors of the ICIO dataset
  tar_target(df_sectors_icio,
             get_df_sectors_icio(
               path_icio
               )
             ),
  
  ## Countries of the ICIO dataset
  tar_target(df_countries_icio,
             get_df_countries_icio(
               path_icio)
             ),
  
  # OECD-ICIO dataset ----------------------------------------------
  ## Lists containing two elements: mat_Z, mat_X
  tar_target(list_icio_y, 
             get_list_icio(
               path_icio, 
               list_years
               ), 
             pattern = map(list_years)
             )
  
)

# Wrangling GLORIA data
wrangling_gloria <- tar_plan(
  
  # Concordance GLORIA sectors to ICIO sectors
  tar_target(df_sectors_gloria_to_icio,
             get_df_sectors_gloria_to_icio(
               file_gloria_to_icio
               )
             ),
  
  # Concordance GLORIA regions to ICIO countries
  tar_target(df_countries_gloria_to_icio,
             get_df_countries_gloria_to_icio(
               file_gloria_to_icio
               )
             ),
  
  ## Rows of the GLORIA tables: row_id, country_code, sector_name
  tar_target(df_rows_gloria,
             get_df_rows_gloria(
               path_gloria
               )
             ),
  
  # Vector of direct labour inputs (GLORIA)
  tar_target(df_h_gloria_raw_y, 
             get_df_h_gloria_raw(
               path_gloria, 
               list_years,
               df_rows_gloria
               ),
             pattern = map(list_years)
             ),
  
  # Vector of direct labour inputs (GLORIA, with ICIO classification) 
  tar_target(df_h_gloria_tidy_y, 
             get_df_h_gloria_tidy(
               df_h_gloria_raw_y,
               list_icio_y,
               list_years,  
               df_population_XAS,
               df_sectors_gloria_to_icio,
               df_countries_gloria_to_icio,
               df_rows_icio
               ), 
             pattern = map(
               df_h_gloria_raw_y, 
               list_icio_y,
               list_years
               )
             ),
  
  # Vector of direct labour inputs (ICIO complemented with data from GLORIA) 
  tar_target(mat_h_adjusted_y,
             get_mat_h_adjusted(
               df_h_y, 
               df_h_gloria_tidy_y, 
               list_years,
               df_rows_icio, 
               df_countries_tim
               ),
             pattern = map(
               df_h_y, 
               df_h_gloria_tidy_y, 
               list_years
               )
             )
  
)

# Preprocessing MRIO objects and results
preprocessing <- tar_plan(
  
  # List of the "aggregated" sectors
  list_sectors = as.character(df_sectors_agg$sector_code_agg),
  
  # Matrix of total flow labour requirements (E')
  tar_target(df_E_prime_y_S,
             get_df_E_prime(
               list_icio_y,
               mat_h_adjusted_y, 
               list_years, 
               list_sectors, 
               country_c0, 
               df_rows_icio
               ),
             pattern = cross(
               map(
                 list_icio_y,
                 mat_h_adjusted_y, 
                 list_years
                 ),
               list_sectors
               )
             ),
  
  # Embodied labour inputs: direct, indirect domestic, indirect foreign
  tar_target(df_labour_raw_y_S,
             get_df_labour_raw(
               df_E_prime_y_S, 
               list_years, 
               list_sectors, 
               country_c0, 
               df_countries_tim
             ),
             pattern = map(
               df_E_prime_y_S, 
               cross(
                 list_years, 
                 list_sectors
                 )
               )
             ),
  
  # Gross outputs by ICIO sector in USD at current prices 
  tar_target(df_gross_output_raw_y,
             get_df_gross_output_raw(
               list_icio_y, 
               list_years,
               country_c0, 
               df_rows_icio
               ),
             pattern = map(
               list_icio_y,
               list_years
               )
             ),
  
  # Value ratio of intermediate imports to gross outputs (Supplementary Info)
  tar_target(df_ratio_intermediate_imp_raw_y,
             get_df_ratio_intermediate_imp_raw(
               list_icio_y,
               list_years,
               country_c0,
               df_rows_icio, 
               df_sectors_icio_to_agg
               ),
             pattern = map(
               list_icio_y,
               list_years
               )
             ),
  
  # Get time-series
  tar_target(df_labour_raw, df_labour_raw_y_S),
  tar_target(df_gross_output_raw, df_gross_output_raw_y),
  tar_target(df_ratio_intermediate_imp_raw, df_ratio_intermediate_imp_raw_y )
  
)

# Processing results 
processing <- tar_plan(
  
  # Embodied labour inputs
  tar_target(df_labour,
             get_df_labour(
               df_labour_raw,
               df_sectors_agg
               )
             ),
  
  ##  Labour offshoring
  tar_target(df_labour_offshoring,
             get_df_labour_offshoring(
               df_labour
               )
             ),
  
  # Gross output
  tar_target(df_gross_output,
             get_df_gross_output(
               df_gross_output_raw, 
               df_exchange_rates, 
               df_deflators,
               df_sectors_icio_to_agg,
               df_sectors_agg
               )
             ),
  
  # Labour productivity (direct and embodied)
  tar_target(df_labour_productivity,
             get_df_labour_productivity(
               df_labour, 
               df_gross_output,
               df_sectors_agg
             )
             ),
  
  # Index Decomposition analysis
  tar_target(df_ida,
             get_df_ida(
               df_labour, 
               df_gross_output,
               df_sectors_agg
               )
             ),
  
  # Value ratio of intermediate imports to gross outputs (for Supplementary Info)
  tar_target(df_ratio_intermediate_imp,
             get_df_ratio_intermediate_imp(
               df_ratio_intermediate_imp_raw,
               df_sectors_agg
               )
             )
)

# Prepare data for figures for main paper
figures <- tar_plan(
  
  df_fig1 = get_df_fig1(df_labour),
  df_fig2 = get_df_fig2(df_labour),
  df_fig3 = get_df_fig3(df_labour_productivity),
  df_fig4 = get_df_fig4(df_ida),
  df_fig5 = get_df_fig5(df_labour_productivity, 
                        df_labour_offshoring)
  
)

# Prepare tables for main paper
tables <- tar_plan(
  
  df_table1 = get_df_table1(df_labour_offshoring, 
                            df_sectors_agg)
  
)

# Save data for figures and tables presented in the article
outputs <- tar_plan(
  
  ods_data_tables = write_ods(
    list("Table 1" = df_table1),
    path = here("outputs", "main", "tables.ods")
    ),
  
  ods_data_fig = write_ods(
    list(
      "Figure 1" = df_fig1,
      "Figure 2" = df_fig2,
      "Figure 3" = df_fig3,
      "Figure 4" = df_fig4,
      "Figure 5" = df_fig5
      ),
    path = here("outputs", "main", "figures.ods")
    )
  
)

# Prepare data for figures presented in the Supplementary Information
si_figures <- tar_plan(
  
  tar_target(df_si_fig1,
             get_df_si_fig1(
               df_deflators, 
               df_sectors_icio_to_agg, 
               df_sectors_agg
               )
             ),
  
  tar_target(df_si_fig2,
             get_df_si_fig2(
               df_gross_output
               )
             ),
  
  tar_target(df_si_fig3,
             get_df_si_fig3(
               df_ratio_intermediate_imp
               )
             ),
  
  tar_target(df_si_fig4_y,
             get_df_si_fig4_y(df_h_gloria_tidy_y, 
                              df_h_y, 
                              list_years,
                              df_countries_tim
                              ),
             pattern = map(df_h_gloria_tidy_y, 
                           df_h_y, 
                           list_years
                           )
             ),
  
  tar_target(df_si_fig4, df_si_fig4_y),
  
)

# Save figures and tables for Supplementary Info
si_outputs <- tar_plan(
  
  ods_si_fig = write_ods(
    list(
      "Figure 1" = df_si_fig1,
      "Figure 2" = df_si_fig2,
      "Figure 3" = df_si_fig3,
      "Figure 4" = df_si_fig4
      ),
    path = here("outputs", "si", "figures.ods")
    )
  
)

list(
  
  # Parameters
  parameters,
  # Trace local files
  files,
  #--------------------
  # 1. Import and wrangle local files 
  wrangling_other,
  wrangling_icio,
  wrangling_gloria,
  # 2. Preprocessing MRIO objects and results
  preprocessing,
  # 3. Process results
  processing,
  # 4. Produce data for the figures and tables presented in the article
  figures,
  tables,
  outputs,
  # 5. Produce data fpr the figures presented in the Supplementary Information
  si_figures,
  si_outputs
  
)

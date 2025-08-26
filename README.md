**Research project repository**

## Labour productivity gains or offshoring?<br>Implications for post-growth proposals on the future of work

Lukas Godé<sup>a</sup>, Simon Mair<sup>b</sup>, Erik Gómez-Baggethun<sup>a,c</sup>

<small><sup>a</sup> *Department of International Development and International Studies, Norwegian University of Life Sciences, Ås, Norway*</small>   
<small><sup>b</sup> *Department of Environment and Geography, University of York, York, United Kingdom*</small>   
<small><sup>c</sup> *Norwegian Institute for Nature Research, Oslo, Norway*</small>   

### General description of the repository

This repository contains the source code used to produce the results presented in the research article:   
*"Labour productivity gains or offshoring? Implications for post-growth proposals on the future of work"*

The file `_targets.R` contains the pipeline to generate the results.

The folder `./data/` contains the raw data used in the pipeline, including external (`./external/`) and own data (`./internal/`).

The folder `./R/` contains the .R files with the functions called by the pipeline.

The .R files are organized as following:

1. Import and wrangle local files: 
  - `1_1_wrangling_other.R`
  - `1_2_wrangling_icio.R`
  - `1_3_wranglig_gloria.R`
2. Preprocess Multi-Regional Input-Output data:
  - `2_preprocessing.R`
3. Process the main data for the analysis:
  - `3_1_processing_labour.R`
  - `3_2_processing_gross_outputs.R`
  - `3_3_processing_productivity.R`
  - `3_4_processing_ida.R`
  - `3_5_processing_si.R`
4. Generate data for the figures and tables of the article: 
  - `4_1_figures.R`
  - `4_2_tables.R`
5. Generate data for the figures and tables of the Supplementary Information: 
  - `5_1_si_figures.R`
  - `5_2_si_tables.R`

Global functions are defined in a separate file (`global_functions.R`).

### Reproduction of the results

#### Software requirements

You need [R](https://cran.r-project.org/) to execute the source code.

We recommend the use of the [RStudio Integrated Development Environment](https://posit.co/products/open-source/rstudio/).

#### Information on external data

External data is stored as .ods files in the following folder: `./data/external/`  
Each file contains two sheets: "data" and "metadata" (description of the data, including data source).

⚠ The **ICIO dataset** and the **satellite accounts from the GLORIA dataset** used in the analysis are not stored on this repository.  
They must be downloaded separately and placed in an appropriate folder before to execute the script (see "Instructions" below).

**OECD Inter-Country Input-Output (ICIO) MRIO dataset**  

- Source: OECD
- Release: 2022, "small version"
- Link: https://oe.cd/icio
- Files downloaded: 
    - `ICIO-1995-2000-small.zip`
    - `ICIO-2001-2005-small.zip`
    - `ICIO-2006-2010-small.zip`
    - `ICIO-2011-2015-small.zip`
    - `ICIO-2016-2020-small.zip`
    - `ReadMe_ICIO_small.xlsx`

**Satellite accounts from the GLORIA dataset**  

- Source: 
   - Lenzen et al., 2022. *Implementing the material footprint to measure progress towards Sustainable Development Goals 8 and 12*. Nat Sustain 5, 157–166.
   - Lenzen et al., 2017. *The Global MRIO Lab – Charting the World Economy*. Econ. Systems Research 29 (2): 158–86.
- Release: 059a
- Link: [IELab.info](https://ielab.info/) (creation of a free account is required)
- Files downloaded:
   - `GLORIA_SatelliteAccounts_059_1995.zip`
   - ... (all years from 1995 to 2020)
   - `GLORIA_SatelliteAccounts_059_2020.zip`
   - `GLORIA_ReadMe_059a.xlsx`
   
#### Instructions

1.  Download this repository and extract it in a working folder
2.  Download the ICIO dataset and the satellite accounts of the GLORIA dataset (see "Information on external data" above)
3.  For the ICIO dataset: after extraction, store all the **files** containing ICIO tables (`year.SML.csv`) together with the `ReadMe_ICIO_small.xlsx` file in a relevant folder, for example: `./data/external/icio/`
4.  For the GLORIA satellite accounts: after extraction, store all the **folders** containing GLORIA satellite accounts (`/GLORIA_SatelliteAccounts_057_year/`) together with `GLORIA_ReadMe_057.xlsx` in a relevant folder, for example: `./data/external/gloria/`
5.  Open the main `.Rproj` file, then the `_targets.R` file and:
      - Section *"Install packages"*: follow instructions to install potentially missing packages
      - Section *"Main parameters"*: update `path_icio` and `path_gloria` according to where you have stored the ICIO dataset and the GLORIA satellite accounts
6.  Execute the following commands from the console:
      - Load the targets library: `library(targets)` 
      - Run the pipeline: `tar_make()`

#### Information on own execution

We executed the script on a computer with the following specifications:

- **Processor:** Intel Core i7-1165G7
- **RAM:** 16GB
- **Operating System:** Windows 11 Enterprise (version 23H2), 64-bit

Software versions used: 

- **R**: 4.5.1
- **RStudio**: 2025.05.1, Build 513

Time of execution: \~*42 minutes*
#### HEADER -------

## Code that generates table with coverage of selected sample

#####################

## Setup ------
rm(list = ls())

if(Sys.info()[["user"]] =="JARDANG"){
  folder <- "X:/Documents/JARDANG" 
}

raw_data <- paste0(folder, "/carbon_policy_networks/data/raw")

int_data <- paste0(folder, "/carbon_policy_networks/data/intermediate")

proc_data <- paste0(folder, "/carbon_policy_networks/data/processed")

output <- paste0(folder, "/carbon_policy_networks/output")

code <- paste0(folder, "/carbon_policy_networks/code")

# Libraries ----

library(tidyverse)
library(dplyr)
library(readxl)

# Import data ----

  load(paste0(proc_data,"/firm_year_balance_sheet_selected_sample.RData"))

  load(paste0(proc_data,"/df_belgian_national_agg.RData"))
  
  load(paste0(proc_data,"/firm_year_belgian_euets.RData"))

# Get agg. numbers from firms in sample ----
  
  total_selected_sample <- firm_year_balance_sheet_selected_sample %>% 
    group_by(year) %>% 
    summarise(
      total_turnover = sum(turnover, na.rm = TRUE),
      total_value_added = sum(value_added, na.rm = TRUE),
      total_wage_bill = sum(wage_bill, na.rm = T),
      total_sales = sum(total_sales, na.rm = T),
      total_network_sales = sum(network_sales, na.rm = T),
      total_imports = sum(imports, na.rm = T),
      total_exports = sum(exports, na.rm = T),
      unique_vat_ano_count = n_distinct(vat_ano)
    )
  
  # number of firms in EUETS
  firm_year_belgian_euets %>% filter(year == 2012) %>%
    mutate(count = n_distinct(vat)) %>% select(count)
  # 281 for both 2012, 2022

# Build table -----
  
  data_for_table <- bind_cols(df_belgian_national_agg, total_selected_sample %>%
                                filter(year %in% c(2002,2012,2022)) %>% select(-year))
  
  data_for_table <- data_for_table %>% 
    mutate(
      across(10:16, ~ round(as.numeric(.x) / 10^9))
    )
  
  # Create LaTeX content
  latex_content <- paste0(
    "\\begin{table}[h]\n",
    "\\centering\n",
    "\\scalebox{0.6}{ \n",
    "\\begin{tabular}{lccccccccccccc}\n",
    "\\toprule\n",
    " & \\multicolumn{5}{c}{Aggregate statistics} & \\multicolumn{7}{c}{Selected sample} \\\\\n",
    "\\cmidrule(lr){2-6} \\cmidrule(lr){7-13}\n",
    " &  &  &  & \\multicolumn{2}{c}{Emissions} &  &  & \\multicolumn{2}{c}{Sales} &  &  & \\\\\n",
    "\\cmidrule(lr){5-6} \\cmidrule(lr){9-10}\n",
    "Year & GDP & Output & Wage bill & Total & EUETS sectors & Count & V.A. & Total & Netw. & Wage bill & Emissions & \\# EUETS firms\\\\\n",
    "\\midrule\\midrule\n",  # <--- Backslash escaped
    paste0("2002 & ", gdp_2002, " & ", output_2002, " & ", wage_bill_2002, " & ", total_emissions_2002, " & ", euets_sectors_emissions_2002, " & ", count_firms_sample_2002, " & ", count_firms_sample_2002, " & B16 & B17 & B18 & C1-3 & C2-3\\\\"), # Data row 1
    "\\midrule\n", # <--- Backslash escaped
    "2012 & A13 & A14 & A15 & A16 & B19 & B20 & B21 & B22 & B23 & B24 & C1-4 & C2-4\\\\\n",
    "\\midrule\n", # <--- Backslash escaped
    "2022 & A17 & A18 & A19 & A20 & B25 & B26 & B27 & B28 & B29 & B30 & C1-5 & C2-5\\\\\n",
    "\\bottomrule\n",
    "\\end{tabular}\n",
    "}\n",
    "\\caption{A table with 12 columns and five rows}\n",
    "\\label{tab:mytable}\n",
    "\\end{table}"
  )
  
  # Specify the file path
  file_path <- paste0(output,"/table_coverage_selected_sample.tex")
  
  # Write LaTeX content to a .tex file
  writeLines(latex_content, file_path)


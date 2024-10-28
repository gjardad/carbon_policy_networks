#### HEADER -------

## This code creates the Leontief inverse matrix for each year

#####################

# Setup ------
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
library(dplyr) # even though dplyr is included in tidyverse, still need to load it separately
library(Matrix)

# Import data ------

load(paste0(proc_data, "/b2b_sample.RData"))
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
load(paste0(proc_data, "/firm_year_labor_costs.RData"))
load(paste0(proc_data, "/euets_prices_year.RData"))

library(haven)
df_b2b <- read_dta(paste0(raw_data,"/NBB/B2B_ANO.dta"))

# Create emission costs ----

df_price_year$year <- as.numeric(df_price_year$year)

emissions_costs <- firm_year_belgian_euets %>% 
  select(vat, year, emissions, allowance_shortage) %>% 
  left_join(df_price_year %>% select(year, nominal_price),
            by = "year") %>% 
  mutate(emissions = emissions*nominal_price,
         allowance_shortage = allowance_shortage*nominal_price) %>% 
  select(-nominal_price) %>% 
  filter(year <= 2022)

  # make allowance_shortage = 0 if negative
  # this is an assumption! I don't know if firm collects revenue from selling its allowances
  # when allowances > emissions

  emissions_costs <- emissions_costs %>% 
    mutate(allowance_shortage = ifelse(allowance_shortage < 0 , 0, allowance_shortage))

# Create Leontief inverse -------
  
leontief_list <- list()
i <- 0

for(y in 2005:2022){
  
  # subset the relevant year
  df <- df_b2b %>%
    filter(year == y)
  
  # make sure buyers and suppliers cover the same set
  firms <- unique(c(df$vat_j_ano, df$vat_i_ano))  # Ensures firms are aligned
  n_firms <- length(firms)
  firm_index_j <- match(df$vat_j_ano, firms)
  firm_index_i <- match(df$vat_i_ano, firms)
  
  # create a square sparse matrix A (firms x firms) where rows are buyers
  A <- sparseMatrix(i = firm_index_i,
                    j = firm_index_j,
                    x = df$corr_sales_ij,
                    dims = c(length(firms), length(firms)))
  
  # emissions "sector"
    emissions_y <- emissions_costs %>% 
      filter(year == y) %>% 
      select(vat, emissions)
  
    # map the firms in emissions_y to the appropriate rows in A
    buyer_indices <- match(emissions_y$vat, firms)  # Find the row indices of the buyers in A
    valid_indices <- !is.na(buyer_indices)
    buyer_indices <- buyer_indices[valid_indices]
    
    emissions_valid <- emissions_y$emissions[valid_indices]
    
    emissions_column <- sparseMatrix(
      i = buyer_indices,  # Rows corresponding to the buyers
      j = rep(1, length(buyer_indices)),  # All in the new last column
      x = emissions_valid,  # The actual emissions values
      dims = c(n_firms, 1)  # Adding only one new column
    )
  
  # labor "sector"
  
    labor_y <- firm_year_labor_costs %>% 
      filter(year == y) %>% 
      select(vat_ano, labor)
    
    # impute sector-level average labor costs to with NA labor
    
    # map the firms in labor_y to the appropriate rows in A
    buyer_indices <- match(labor_y$vat_ano, firms)  # Find the row indices of the buyers in A
    valid_indices <- !is.na(buyer_indices)
    buyer_indices <- buyer_indices[valid_indices]
    
    labor_valid <- labor_y$labor[valid_indices]
    
    labor_column <- sparseMatrix(
      i = buyer_indices,  # Rows corresponding to the buyers
      j = rep(1, length(buyer_indices)),  # All in the new last column
      x = labor_valid,  # The actual emissions values
      dims = c(n_firms, 1)  # Adding only one new column
    )
  
  # add emissions sector as supplier column
  A_extended <- cbind(A, emissions_column, labor_column)
  
  # add emissions sector as buyer to make sure A is square
  firms_extended <- c(firms, "emissions", "labor")  # Ensures firms are aligned
  n_firms_extended <- length(firms_extended)
  A_extended <- rbind(A_extended, sparseMatrix(i = integer(0), j = integer(0), dims = c(2, n_firms_extended)))
  
  # normalize it so that it sums to 1
  row_sums <- rowSums(A_extended)
  
  row_sums[row_sums == 0] <- 1  # Replace zero row sums with 1 to avoid division by zero
  
  A_normalized <- A_extended
  A_normalized@x <- A_normalized@x / row_sums[A_normalized@i + 1]  # Element-wise division of each row
  # normalizes the non-zero entries in the sparse matrix by dividing them by the corresponding row sums.
  # The @x slot contains the non-zero values of the sparse matrix, and @i gives the row indices (zero-based, hence the +1).
  
  # now get rid of labor sector
  A_normalized <- A_normalized[1:(nrow(A_normalized) - 1), 1:(ncol(A_normalized) - 1)]
  
  # select the last column of A_normalized (this is vector v)
  v <- A_normalized[, ncol(A_normalized)]
  
  # Neumann series calculation (iterative)
    result <- v  # Initialize the result with v (A^0 v = v)
    current_power <- v  # This will store the intermediate A^n v
    
    # Loop to sum A^n v up to A^50 v
    for (n in 1:2) {
      current_power <- A_normalized %*% current_power  # Compute A^n v (iteratively applying A)
      result <- result + current_power  # Sum A^n v to the result
    }
  
  leontief_list[[i]] <- result

}

# Save it -------------------
save(leontief_list, file = "leontief_inverse.RData")

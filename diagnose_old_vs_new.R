###############################################################################
# diagnose_old_vs_new.R
#
# PURPOSE:
#   (1) Replicate your "old" LOFO findings inside the *new* infrastructure
#       (fixest + consistent LOFO + same metrics).
#   (2) Attribute differences vs the current pipeline to:
#         - FE level (nace5d vs nace2d)
#         - importer definition (old strict CN8 list vs broad ch27)
#         - smearing bug (re-using benchmark smear for excl model)
#         - sector threshold (>=2 vs >=3)
#
# OUTPUT:
#   Saves:
#     output/loocv/results/diagnose_old_vs_new.csv
#     output/loocv/results/diagnose_old_vs_new.rds
###############################################################################

# --- working directory ---
if (tolower(Sys.info()[["user"]]) == "jardang") {
  setwd("X:/Documents/JARDANG/carbon_policy_networks/code/loocv")
} else stop("Unknown user; setwd manually.")

source("00_config.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(fixest)
  library(stringr)
})

# ----------------------------
# 0) Load datasets
# ----------------------------
load(paste0(proc_data, "/firm_year_belgian_euets.RData"))
load(paste0(proc_data, "/annual_accounts_selected_sample_key_variables.RData"))
load(paste0(proc_data, "/b2b_selected_sample.RData"))
load(paste0(proc_data, "/firm_cncode_year_ef_weighted_qty.RData"))  # ef_weighted_fuel_qty
load(paste0(proc_data, "/siec_to_ipcc.RData"))                      # not needed here but ok
load(paste0(proc_data, "/cn8digit_codes_for_fossil_fuels.RData"))

# revenue variable name: prefer turnover_VAT, else fallback if present
aa <- df_annual_accounts_selected_sample_key_variables
aa_id <- intersect(names(aa), c("vat_ano", "vat"))
stopifnot(length(aa_id) == 1)
aa_id <- aa_id[[1]]

rev_var <- intersect(names(aa), c("turnover_VAT", "revenue", "turnover"))
if (length(rev_var) == 0) stop("Could not find a revenue variable in annual accounts key variables.")
rev_var <- rev_var[[1]]

# EUETS id variable name
euets_id <- intersect(names(firm_year_belgian_euets), c("vat_ano", "vat"))
stopifnot(length(euets_id) == 1)
euets_id <- euets_id[[1]]

# ----------------------------
# 1) Build firm -> NACE mapping (nace5d + nace2d)
# ----------------------------
firm_sector <- aa %>%
  transmute(
    firm_id = .data[[aa_id]],
    nace5d = as.character(nace5d),
    nace2d = substr(gsub("\\D", "", as.character(nace5d)), 1, 2),
    revenue = as.numeric(.data[[rev_var]])
  ) %>%
  distinct()

euets_outcomes <- firm_year_belgian_euets %>%
  transmute(
    firm_id = .data[[euets_id]],
    year = year,
    emissions = as.numeric(emissions)
  ) %>%
  left_join(firm_sector, by = "firm_id")

# ----------------------------
# 2) Build importer sets (NEW broad vs OLD strict CN8 list)
# ----------------------------

# Helper: extract CN8 list robustly from cn8digit_codes_for_fossil_fuels
stopifnot(exists("cn8digit_codes_for_fossil_fuels"))

cn8_list <- cn8digit_codes_for_fossil_fuels %>%
  pull(cn_code) %>%
  as.character() %>%
  trimws() %>%
  unique()

# Sanity checks
stopifnot(all(nchar(cn8_list) == 8))

ef <- ef_weighted_fuel_qty %>%
  transmute(
    vat_ano = as.character(vat_ano),
    year = as.numeric(year),
    cncode = as.character(cncode),
    imports_value = as.numeric(imports_value),
    is_euets = as.integer(is_euets)
  )

# NEW broad definition: any CN starting 27 excluding 2716
ef_new <- ef %>%
  filter(substr(cncode, 1, 2) == "27", substr(cncode, 1, 4) != "2716")

new_importers <- ef_new %>% distinct(vat_ano) %>% pull(vat_ano)

# OLD strict definition: CN in the ex-ante list
ef_old <- ef %>%
  filter(cncode %in% cn8_list)

old_importers <- ef_old %>% distinct(vat_ano) %>% pull(vat_ano)

# firm-year own imports totals + EUETS status for importers
importer_year_totals <- ef %>%
  group_by(vat_ano, year) %>%
  summarise(
    own_imports_value = sum(imports_value, na.rm = TRUE),
    is_euets = max(is_euets, na.rm = TRUE),
    .groups = "drop"
  )

# ----------------------------
# 3) Build fuel proxies from B2B + own imports
#    Benchmark: purchases from importers + own imports if importer
#    Excl EUETS importers: purchases from importers with is_euets==0 + own imports if importer & non-euets
# ----------------------------

b2b <- df_b2b_selected_sample %>%
  transmute(
    supplier_id = as.character(vat_i_ano),
    buyer_id = as.character(vat_j_ano),
    year = as.numeric(year),
    value = as.numeric(corr_sales_ij)
  )

purchases_from_importers <- function(importer_ids) {
  b2b %>%
    filter(supplier_id %in% importer_ids) %>%
    group_by(buyer_id, year) %>%
    summarise(purch = sum(value, na.rm = TRUE), .groups = "drop")
}

build_proxy_pair <- function(importer_ids, label) {
  # Purchases from importers
  purch <- purchases_from_importers(importer_ids)
  
  # Own imports (if importer)
  own <- importer_year_totals %>%
    filter(vat_ano %in% importer_ids) %>%
    transmute(buyer_id = vat_ano, year, own_imports = own_imports_value, own_is_euets = is_euets)
  
  # Benchmark
  bench <- purch %>%
    left_join(own, by = c("buyer_id", "year")) %>%
    mutate(own_imports = ifelse(is.na(own_imports), 0, own_imports),
           fuel_proxy = purch + own_imports) %>%
    select(buyer_id, year, fuel_proxy)
  
  # Excl EUETS importers: restrict importers to those with is_euets==0 (ever, by firm-year max)
  non_euets_importers <- importer_year_totals %>%
    filter(vat_ano %in% importer_ids, is_euets == 0) %>%
    distinct(vat_ano) %>% pull(vat_ano)
  
  purch_ne <- purchases_from_importers(non_euets_importers)
  
  own_ne <- importer_year_totals %>%
    filter(vat_ano %in% non_euets_importers) %>%
    transmute(buyer_id = vat_ano, year, own_imports = own_imports_value)
  
  excl <- purch_ne %>%
    left_join(own_ne, by = c("buyer_id", "year")) %>%
    mutate(own_imports = ifelse(is.na(own_imports), 0, own_imports),
           fuel_proxy = purch + own_imports) %>%
    select(buyer_id, year, fuel_proxy)
  
  list(
    bench = bench %>% mutate(proxy_def = "benchmark", importer_def = label),
    excl  = excl  %>% mutate(proxy_def = "exclude_euets_importers", importer_def = label)
  )
}

proxy_new <- build_proxy_pair(new_importers, "NEW_broad_ch27")
proxy_old <- build_proxy_pair(old_importers, "OLD_strict_cn8list")

# ----------------------------
# 4) Utilities: sample filters + LOFO runner + metrics
# ----------------------------

summarise_perf <- function(df_pred) {
  # Same definitions you used earlier
  err <- df_pred$pred - df_pred$emissions
  sst <- sum((df_pred$emissions - mean(df_pred$emissions, na.rm = TRUE))^2, na.rm = TRUE)
  sse <- sum(err^2, na.rm = TRUE)
  
  tibble(
    nRMSE = sqrt(mean(err^2, na.rm = TRUE)) / sd(df_pred$emissions, na.rm = TRUE),
    MAPD = mean(abs(err / df_pred$emissions), na.rm = TRUE),
    R2_LOO = 1 - sse / sst,
    Rho_Spearman = cor(df_pred$emissions, df_pred$pred, method = "spearman", use = "complete.obs"),
    n_obs = nrow(df_pred),
    n_firms = n_distinct(df_pred$firm_id)
  )
}

apply_sector_threshold <- function(df, sector_var = c("nace5d", "nace2d"), min_firms = 2) {
  sector_var <- match.arg(sector_var)
  df1 <- df %>%
    mutate(sector = as.character(.data[[sector_var]])) %>%
    filter(!is.na(sector))
  
  keep <- df1 %>%
    group_by(sector) %>%
    summarise(n_firms = n_distinct(firm_id), .groups = "drop") %>%
    filter(n_firms >= min_firms) %>%
    pull(sector)
  
  df1 %>% filter(sector %in% keep)
}

# LOFO runner:
# - uses fixest
# - optionally includes log(revenue)
# - optionally includes sector FE
# - optionally emulates the "smear bug": for excl model, reuse smear from benchmark model (within fold)
run_lofo <- function(df,
                     include_sector_fe = TRUE,
                     include_revenue = FALSE,
                     smear_bug = FALSE,
                     sector_var = c("nace5d", "nace2d")) {
  
  sector_var <- match.arg(sector_var)
  
  # Base filters consistent with your old script
  df0 <- df %>%
    filter(emissions > 0,
           !is.na(fuel_proxy),
           fuel_proxy > 0) %>%
    mutate(
      year_factor = factor(year),
      sector_factor = factor(.data[[sector_var]])
    )
  
  if (include_revenue) {
    df0 <- df0 %>%
      filter(!is.na(revenue), revenue > 0)
  }
  
  firms <- unique(df0$firm_id)
  out <- vector("list", length(firms))
  
  for (k in seq_along(firms)) {
    f <- firms[[k]]
    train <- df0 %>% filter(firm_id != f)
    test  <- df0 %>% filter(firm_id == f)
    
    # Build formula
    rhs <- "log(fuel_proxy)"
    if (include_revenue) rhs <- paste0(rhs, " + log(revenue)")
    
    if (include_sector_fe) {
      fml <- as.formula(paste0("log(emissions) ~ ", rhs, " | year_factor + sector_factor"))
    } else {
      fml <- as.formula(paste0("log(emissions) ~ ", rhs, " | year_factor"))
    }
    
    fit <- feols(fml, data = train, warn = FALSE)
    
    smear <- mean(exp(resid(fit)), na.rm = TRUE)
    
    # Smear bug emulation handled outside (we pass the correct smear unless asked)
    test$pred <- exp(predict(fit, newdata = test)) * smear
    
    out[[k]] <- test %>%
      transmute(firm_id, year, emissions, pred)
  }
  
  preds <- bind_rows(out)
  list(preds = preds, perf = summarise_perf(preds))
}

# Smearing bug emulation for the *pair* of models benchmark vs excl:
# - run benchmark fold model first, keep its smear, then use that smear for excl
run_lofo_pair_with_bug_option <- function(df_bench, df_excl,
                                          include_sector_fe = TRUE,
                                          include_revenue = FALSE,
                                          smear_bug = FALSE,
                                          sector_var = c("nace5d", "nace2d")) {
  sector_var <- match.arg(sector_var)
  
  # Harmonize base filters so both models evaluated on their own available sample
  prep <- function(df) {
    d <- df %>%
      filter(emissions > 0,
             !is.na(fuel_proxy),
             fuel_proxy > 0) %>%
      mutate(year_factor = factor(year),
             sector_factor = factor(.data[[sector_var]]))
    if (include_revenue) d <- d %>% filter(!is.na(revenue), revenue > 0)
    d
  }
  
  bench0 <- prep(df_bench)
  excl0  <- prep(df_excl)
  
  # IMPORTANT: to compare apples-to-apples, use intersection of firm-years
  keys <- inner_join(
    bench0 %>% transmute(firm_id, year),
    excl0  %>% transmute(firm_id, year),
    by = c("firm_id", "year")
  )
  
  bench0 <- bench0 %>% semi_join(keys, by = c("firm_id", "year"))
  excl0  <- excl0  %>% semi_join(keys, by = c("firm_id", "year"))
  
  firms <- sort(unique(keys$firm_id))
  
  out_bench <- vector("list", length(firms))
  out_excl  <- vector("list", length(firms))
  
  for (k in seq_along(firms)) {
    f <- firms[[k]]
    
    train_b <- bench0 %>% filter(firm_id != f)
    test_b  <- bench0 %>% filter(firm_id == f)
    
    train_e <- excl0 %>% filter(firm_id != f)
    test_e  <- excl0 %>% filter(firm_id == f)
    
    rhs <- "log(fuel_proxy)"
    if (include_revenue) rhs <- paste0(rhs, " + log(revenue)")
    
    if (include_sector_fe) {
      fml <- as.formula(paste0("log(emissions) ~ ", rhs, " | year_factor + sector_factor"))
    } else {
      fml <- as.formula(paste0("log(emissions) ~ ", rhs, " | year_factor"))
    }
    
    fit_b <- feols(fml, data = train_b, warn = FALSE)
    smear_b <- mean(exp(resid(fit_b)), na.rm = TRUE)
    test_b$pred <- exp(predict(fit_b, newdata = test_b)) * smear_b
    
    fit_e <- feols(fml, data = train_e, warn = FALSE)
    smear_e <- mean(exp(resid(fit_e)), na.rm = TRUE)
    
    # BUG: reuse smear from benchmark model
    smear_used <- if (smear_bug) smear_b else smear_e
    test_e$pred <- exp(predict(fit_e, newdata = test_e)) * smear_used
    
    out_bench[[k]] <- test_b %>% transmute(firm_id, year, emissions, pred)
    out_excl[[k]]  <- test_e %>% transmute(firm_id, year, emissions, pred)
  }
  
  preds_b <- bind_rows(out_bench)
  preds_e <- bind_rows(out_excl)
  
  list(
    bench = list(preds = preds_b, perf = summarise_perf(preds_b)),
    excl  = list(preds = preds_e, perf = summarise_perf(preds_e))
  )
}

# ----------------------------
# 5) Assemble datasets for evaluation
# ----------------------------
make_eval_df <- function(proxy_tbl) {
  euets_outcomes %>%
    inner_join(proxy_tbl %>% select(buyer_id, year, fuel_proxy), by = c("firm_id" = "buyer_id", "year")) %>%
    select(firm_id, year, emissions, nace5d, nace2d, revenue, fuel_proxy)
}

eval_new_bench <- make_eval_df(proxy_new$bench)
eval_new_excl  <- make_eval_df(proxy_new$excl)
eval_old_bench <- make_eval_df(proxy_old$bench)
eval_old_excl  <- make_eval_df(proxy_old$excl)

# ----------------------------
# 6) Run decomposition grid
# ----------------------------
grid <- expand_grid(
  importer_def = c("OLD_strict_cn8list", "NEW_broad_ch27"),
  sector_var = c("nace5d", "nace2d"),
  min_firms = c(2, 3),
  include_revenue = c(FALSE, TRUE),
  smear_bug = c(FALSE, TRUE)
)

run_case <- function(importer_def, sector_var, min_firms, include_revenue, smear_bug) {
  
  if (importer_def == "OLD_strict_cn8list") {
    df_b <- eval_old_bench
    df_e <- eval_old_excl
  } else {
    df_b <- eval_new_bench
    df_e <- eval_new_excl
  }
  
  # apply sector threshold *before* LOFO
  df_b <- apply_sector_threshold(df_b, sector_var = sector_var, min_firms = min_firms)
  df_e <- apply_sector_threshold(df_e, sector_var = sector_var, min_firms = min_firms)
  
  # run paired LOFO so the smear bug is implemented correctly
  out <- run_lofo_pair_with_bug_option(
    df_bench = df_b,
    df_excl = df_e,
    include_sector_fe = TRUE,        # your old spec had year+sector FE
    include_revenue = include_revenue,
    smear_bug = smear_bug,
    sector_var = sector_var
  )
  
  tibble(
    importer_def = importer_def,
    sector_var = sector_var,
    min_firms = min_firms,
    include_revenue = include_revenue,
    smear_bug = smear_bug,
    model = c("benchmark", "exclude_euets_importers"),
    nRMSE = c(out$bench$perf$nRMSE, out$excl$perf$nRMSE),
    MAPD  = c(out$bench$perf$MAPD,  out$excl$perf$MAPD),
    R2_LOO = c(out$bench$perf$R2_LOO, out$excl$perf$R2_LOO),
    Rho_Spearman = c(out$bench$perf$Rho_Spearman, out$excl$perf$Rho_Spearman),
    n_obs = c(out$bench$perf$n_obs, out$excl$perf$n_obs),
    n_firms = c(out$bench$perf$n_firms, out$excl$perf$n_firms)
  )
}

diagnose_tbl <- purrr::pmap_dfr(
  grid,
  ~ run_case(..1, ..2, ..3, ..4, ..5)
)

# Save
saveRDS(diagnose_tbl, file.path(RESULTS_DIR, "diagnose_old_vs_new.rds"))
write_csv(diagnose_tbl, file.path(RESULTS_DIR, "diagnose_old_vs_new.csv"))

# Print a useful view
cat("\n=== Top 20 by nRMSE (benchmark model) ===\n")
print(
  diagnose_tbl %>%
    filter(model == "benchmark") %>%
    arrange(nRMSE) %>%
    slice(1:20)
)

cat("\n=== Top 20 by nRMSE (exclude EUETS importers model) ===\n")
print(
  diagnose_tbl %>%
    filter(model == "exclude_euets_importers") %>%
    arrange(nRMSE) %>%
    slice(1:20)
)

cat("\nSaved:\n")
cat(" - ", file.path(RESULTS_DIR, "diagnose_old_vs_new.rds"), "\n", sep = "")
cat(" - ", file.path(RESULTS_DIR, "diagnose_old_vs_new.csv"), "\n", sep = "")

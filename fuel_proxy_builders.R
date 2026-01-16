###############################################################################
# fuel_proxy_builders.R
#
# PURPOSE:
#   Builds a fuel consumption proxy for any combination of your 5 proxy
#   modifications, using cached auxiliary tables in /output/loocv/cache.
#
# OUTPUT:
#   Returns a tibble: buyer_id, year, fuel_proxy
###############################################################################

source(file.path(LOOCV_CODE, "00_config.R"))
source(file.path(FUN_DIR, "progress_utils.R"))

load_aux <- function() {
  list(
    aa_nace = readRDS(file.path(CACHE_DIR, "aux_aa_nace.rds")),
    likelihood_tbl = readRDS(file.path(CACHE_DIR, "aux_likelihood_tbl.rds")),
    firm_year_fuel_totals = readRDS(file.path(CACHE_DIR, "aux_firm_year_fuel_totals.rds")),
    firm_year_siec = readRDS(file.path(CACHE_DIR, "aux_firm_year_siec.rds")),
    siec_all_used = readRDS(file.path(CACHE_DIR, "aux_siec_all_used.rds")),
    buyer_sector_siec = readRDS(file.path(CACHE_DIR, "aux_buyer_sector_siec.rds")),
    b2b = readRDS(file.path(CACHE_DIR, "aux_b2b_minimal.rds"))
  )
}

build_fuel_proxy <- function(mods, aux) {
  
  mods <- modifyList(list(
    use_siec_all = FALSE,
    supplier_non_euets = FALSE,
    buyer_sector_siec = FALSE,
    supplier_nace_filter = "none",
    emissions_weighted = FALSE
  ), mods)
  
  suppliers <- aux$firm_year_fuel_totals %>%
    transmute(
      supplier_id = firm_id,
      year,
      imports_value_total,
      co2_kg_total,
      intensity_kg_per_eur,
      is_euets
    )
  
  if (isTRUE(mods$use_siec_all)) {
    suppliers_siec_ok <- aux$firm_year_siec %>%
      inner_join(aux$siec_all_used, by = "siec_code") %>%
      distinct(firm_id, year) %>%
      transmute(supplier_id = firm_id, year, ok = TRUE)
    
    suppliers <- suppliers %>%
      left_join(suppliers_siec_ok, by = c("supplier_id","year")) %>%
      mutate(ok = if_else(is.na(ok), FALSE, ok)) %>%
      filter(ok) %>%
      select(-ok)
  }
  
  if (isTRUE(mods$supplier_non_euets)) {
    suppliers <- suppliers %>% filter(is_euets == 0)
  }
  
  if (!identical(mods$supplier_nace_filter, "none")) {
    sup_nace <- aux$aa_nace %>%
      transmute(supplier_id = firm_id, nace4d) %>%
      left_join(aux$likelihood_tbl, by = "nace4d")
    
    suppliers <- suppliers %>% left_join(sup_nace, by = "supplier_id")
    
    if (identical(mods$supplier_nace_filter, "high")) {
      suppliers <- suppliers %>% filter(likelihood == "high")
    } else if (identical(mods$supplier_nace_filter, "high_medium")) {
      suppliers <- suppliers %>% filter(likelihood %in% c("high","medium"))
    } else stop("Unknown supplier_nace_filter")
    
    suppliers <- suppliers %>% select(-nace4d, -likelihood)
  }
  
  b2b_edges <- aux$b2b %>%
    inner_join(suppliers, by = c("supplier_id","year"))
  
  if (isTRUE(mods$buyer_sector_siec)) {
    
    buyers_nace2d <- aux$aa_nace %>%
      transmute(buyer_id = firm_id, nace2d) %>%
      distinct()
    
    buyer_year_allowed <- b2b_edges %>%
      distinct(buyer_id, year) %>%
      left_join(buyers_nace2d, by = "buyer_id") %>%
      left_join(aux$buyer_sector_siec, by = "nace2d") %>%
      filter(!is.na(siec_code)) %>%
      select(buyer_id, year, siec_code) %>%
      distinct()
    
    supplier_year_siec <- aux$firm_year_siec %>%
      transmute(supplier_id = firm_id, year, siec_code) %>%
      distinct()
    
    eligible_pairs <- buyer_year_allowed %>%
      inner_join(supplier_year_siec, by = c("year","siec_code")) %>%
      distinct(buyer_id, supplier_id, year)
    
    b2b_edges <- b2b_edges %>%
      inner_join(eligible_pairs, by = c("buyer_id","supplier_id","year"))
  }
  
  if (isTRUE(mods$emissions_weighted)) {
    b2b_edges <- b2b_edges %>% mutate(contrib = sales_ij * intensity_kg_per_eur)
  } else {
    b2b_edges <- b2b_edges %>% mutate(contrib = sales_ij)
  }
  
  purchases_component <- b2b_edges %>%
    group_by(buyer_id, year) %>%
    summarise(purchases = sum(contrib, na.rm = TRUE), .groups = "drop")
  
  buyer_own <- aux$firm_year_fuel_totals %>%
    transmute(
      buyer_id = firm_id,
      year,
      own_imports_value = imports_value_total,
      own_co2_kg = co2_kg_total
    )
  
  proxy <- purchases_component %>%
    full_join(buyer_own, by = c("buyer_id","year")) %>%
    mutate(
      purchases = coalesce(purchases, 0),
      own_imports_value = coalesce(own_imports_value, 0),
      own_co2_kg = coalesce(own_co2_kg, 0),
      fuel_proxy = if (isTRUE(mods$emissions_weighted)) purchases + own_co2_kg else purchases + own_imports_value
    ) %>%
    select(buyer_id, year, fuel_proxy)
  
  proxy
}

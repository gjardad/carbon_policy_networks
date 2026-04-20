# Data Cleaning Assumptions and Decisions

This document records every non-trivial assumption, interpolation, or judgment call made during data construction. Each entry is dated and references the script that implements it.

---

## PPI Deflator Construction

**Script:** `analysis/phase0_build_deflator.R`

### Sources

| Source | Level | Base year | Period with data | Market |
|--------|-------|-----------|-----------------|--------|
| Eurostat `sts_inppd_a` | NACE 2-digit | 2010=100 | 1981-2017 | Domestic |
| Eurostat `sts_inppd_a` | NACE 2-digit | 2015=100 | 2001-2023 (varies) | Domestic |
| Eurostat `sts_inppd_a` | NACE 2-digit | 2021=100 | 2001-2025 (varies) | Domestic |
| Statbel output price index | NACE 4-digit | 2010=100 | 2010-2024 | Domestic |

Raw files on disk:
- `NBB_data/raw/Eurostat/sts_inppd_a__custom_21089145_linear.csv`
- `NBB_data/raw/Statbel/TABEL_WEBSITE_AANGEVERS_EN.xlsx`

### Assumption 1: Chain-linking Eurostat base years

Eurostat provides three overlapping index series for each NACE 2-digit sector (base years 2010, 2015, 2021). We chain-link them into a single continuous series:

1. **Anchor:** 2010-base series (longest history, 1981-2017).
2. **Extension 2018+:** Chain-link the 2015-base series at the latest overlapping year with the 2010-base (typically 2017). That is: `ppi_chain(t) = ppi_b2015(t) / ppi_b2015(overlap_year) * ppi_chain(overlap_year)`.
3. **Extension beyond 2015-base:** If the 2015-base doesn't cover later years, chain-link the 2021-base at the latest overlapping year with the current chain.

This assumes that the different base-year series are consistent (i.e., they differ only in the normalization, not in methodology). Eurostat documents confirm that re-basing is a rescaling exercise, though minor methodological revisions can occur at base-year changes.

### Assumption 2: Chain-linking Statbel 4-digit to Eurostat 2-digit

The final deflator uses Statbel NACE 4-digit indices from 2010 onward and Eurostat NACE 2-digit for 2005-2009. These are chain-linked at 2010:

```
deflator(t) = Eurostat_2d(t)                                          if t < 2010
deflator(t) = Statbel_4d(t) / Statbel_4d(2010) * Eurostat_2d(2010)   if t >= 2010
```

All values are re-indexed to 2005 = 100.

**Implication for 2005-2009:** All firms within the same NACE 2-digit sector get the same deflator. From 2010 onward, firms in different NACE 4-digit subsectors within the same 2-digit get different deflators.

### Assumption 3: NACE 18 (Printing) and NACE 30 (Other transport equipment)

Statbel has no 4-digit PPI data for these two sectors. At the Eurostat 2-digit level, the individual series (C18, C30) have data only through 2009. We use the broader Eurostat aggregates instead:

- **NACE 18:** uses the C16-C18 aggregate (Manufacture of wood, paper, printing)
- **NACE 30:** uses the C29-C30 aggregate (Motor vehicles + other transport)

These aggregates have complete 2010-base data through 2017 and 2021-base data from 2021 onward, but **no data for 2018-2020** in any base year.

### Assumption 4: Linear interpolation for 2018-2020 gap (NACE 18, 30)

For NACE 18 and 30, the chain-linked series has a gap in 2018-2020 (between the end of the 2010-base at 2017 and the start of the 2021-base at 2021). We fill this gap by **linear interpolation** between the 2017 and 2021 values.

This affects a very small number of firms (printing and other transport equipment are minor sectors for EU ETS purposes).

### Assumption 5: NACE 12 (Tobacco) — no data

Eurostat reports no PPI values for Belgian tobacco manufacturing (NACE 12) in any base year across our sample period. Firms in NACE 12, if any, will have no deflator and will be dropped from the analysis.

### Assumption 6: Domestic market PPI

We use the **domestic market** PPI throughout (not the total/global market index), because turnover in Belgian Annual Accounts reflects producer-level invoiced sales excluding VAT, which corresponds to domestic output prices.

### Fallback logic for firm-level matching

When deflating firm revenue:
1. Match on NACE 4-digit first (Statbel-based deflator, 139 sectors).
2. If no 4-digit match, fall back to NACE 2-digit (Eurostat-based, 26 sectors).
3. If no match at either level, the firm is dropped.

---

## EU ETS Firm Data

**Script:** `legacy/firm_year_belgium_euets.R`

### Source

- EUTL Belgium: `NBB_data/raw/NBB/EUTL_Belgium.dta`
- Linked to NBB firm identifiers via BvD ID and company registration number.

### Known issues

- `firm_id` and `bvd_id` are not 1:1. Some firm_ids have leading zeros stripped. Deduplication is done on `(vat, year, bvd_id)`.
- The set of firms in `firm_year_emissions` (EUTL) with Belgian country_id is a subset of `df_belgium_euets`.

---

*Last updated: 2026-04-20*

# Data Sources

## Producer Price Indices (Deflators)

### Statbel — Output Price Index (Industry), Domestic Market

Belgian producer price indices at NACE 4-digit level, used to deflate firm-level turnover to obtain real output.

**Source page:** https://statbel.fgov.be/en/themes/indicators/prices/output-price-index-industry

**Download links:**

| File | NACE level | Base year | Period | Link |
|------|-----------|-----------|--------|------|
| Detailed, 2010=100 | 4-digit | 2010 | 2010-2024 | [TABEL_WEBSITE_AANGEVERS_EN.xlsx](https://statbel.fgov.be/sites/default/files/files/documents/Conjunctuur/4.10%20Afzetprijzen/TABEL_WEBSITE_AANGEVERS_EN.xlsx) |
| Detailed, 2021=100 | 4-digit | 2021 | ~2015-2026 | [Statbel_TABEL_WEBSITE_AANGEVERS_2021_EN.xlsx](https://statbel.fgov.be/sites/default/files/files/documents/Conjunctuur/4.10%20Afzetprijzen/Statbel_TABEL_WEBSITE_AANGEVERS_2021_EN.xlsx) |
| Aggregated, 2010=100 | 2-digit | 2010 | 2010-2024 | [PRESS_OPI08_WEB_EN.xlsx](https://statbel.fgov.be/sites/default/files/files/documents/Conjunctuur/4.10%20Afzetprijzen/PRESS_OPI08_WEB_EN.xlsx) |
| Aggregated, 2021=100 | 2-digit | 2021 | ~2015-2026 | [Statbel_PRESS_OPI_WEB_2021_EN.xlsx](https://statbel.fgov.be/sites/default/files/files/documents/Conjunctuur/4.10%20Afzetprijzen/Statbel_PRESS_OPI_WEB_2021_EN.xlsx) |
| Conversion coefficients | — | — | — | [OPI_coeff_conv_EN.xlsx](https://statbel.fgov.be/sites/default/files/files/documents/Conjunctuur/4.10%20Afzetprijzen/OPI_coeff_conv_EN.xlsx) |

**Usage notes:**
- Use the **domestic market** column to deflate firm turnover (turnover in Belgian Annual Accounts is a producer-level concept, not retail).
- The 2010=100 detailed file covers 2010-2024. For 2005-2009, fall back to Eurostat NACE 2-digit PPI or use the conversion coefficients to chain-link.
- Monthly data; average to annual for our purposes.
- Comparable to the NBER-CES deflators used by Shapiro & Walker (2018), which are also industry-specific PPIs at the 4-6 digit level.

### Eurostat — Producer Prices in Industry (sts_inpp_a)

Annual PPI by NACE 2-digit division, available for Belgium from ~2000 onward. Use as fallback for years not covered by Statbel 4-digit data.

**Source:** https://ec.europa.eu/eurostat/databrowser/view/STS_INPP_A/default/table?lang=en

**Programmatic access:** R package `eurostat`, dataset code `sts_inpp_a`.

---

## EU Emissions Trading System

### EUTL — European Union Transaction Log

Installation-level verified emissions and free allowance allocations for all EU ETS regulated installations.

**Source:** https://ec.europa.eu/clima/ets/

**Pre-processed version:** https://www.euets.info/

**Contents:** Installation-year level data on verified emissions (tCO2), free allowances allocated, main activity, operator, country, NACE industry.

**Belgian EUTL data** at NBB: `NBB_data/raw/NBB/EUTL_Belgium.dta` (linked to firm identifiers via BvD ID and company registration numbers).

### EUA Prices

Daily EU Emission Allowance prices.

**Source used in legacy code:** `NBB_data/raw/icap_price.csv` (from ICAP).

**Key milestones:**
- 2005-2007 (Phase I): prices rose to ~30 EUR then crashed to 0
- 2008-2012 (Phase II): fluctuated 8-30 EUR
- 2013-2016 (Phase III, pre-MSR): ~5-8 EUR
- 2017-2020 (post-MSR reform): rose from ~5 to ~30 EUR
- 2021-2023: reached 60-100 EUR

### Carbon Leakage List

Sectors deemed at risk of carbon leakage, receiving higher free allowance allocations.

**Source:** European Commission decisions pursuant to Directive 2003/87/EC. Available at https://climate.ec.europa.eu/eu-action/eu-emissions-trading-system-eu-ets/free-allocation/carbon-leakage_en

---

## Belgian Firm-Level Data (NBB, on RMD)

### Annual Accounts

Firm-year balance sheet and income statement data for the Belgian private sector.

**Location on RMD:** `NBB_data/raw/NBB/Annual_Accounts_MASTER_ANO.dta`

**Key variables:** `vat_ano` (anonymized VAT identifier), `year`, `turnover_VAT` (revenue), `v_0022_27` (capital), `v_0001003` (FTE employment), `v_0001023` (wage bill), `v_0009800` (value added), `nace5d` (NACE 5-digit sector code).

### B2B Transactions

Firm-pair-year domestic input-output linkages from VAT declarations.

**Location on RMD:** Various files in `NBB_data/`

### Processed Files

| File | Contents |
|------|----------|
| `NBB_data/processed/firm_year_belgian_euets.RData` | ETS firm-year panel with emissions, allowances, financials, NACE codes |
| `NBB_data/processed/sector_year_emissions_all_gases.RData` | Eurostat sector-year emissions for Belgium |
| `NBB_data/processed/firm_year_emissions.RData` | EUTL emissions at firm-year level |

---

## Sector-Level Emissions (Non-ETS)

### Eurostat GHG Emissions by NACE Sector

Sector-year total GHG emissions for Belgium, used to compute non-ETS emissions as the residual (total sector - EUTL).

**Location on RMD:** `NBB_data/raw/Eurostat/ghg_annual_belgium*.xlsx`

### National Inventory Reports (NIR)

Belgian national emissions inventories with breakdowns by IPCC sector and CRF category.

Used in `legacy/emissions_by_sector_year_from_nir.R` and related scripts.

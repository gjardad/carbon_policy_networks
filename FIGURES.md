# Figures: Carbon Pricing Policy Incompleteness

This document describes the data sources and cleaning procedures for each descriptive figure in the project. All figures make the point that carbon pricing policies (CPPs) as implemented in practice are incomplete: they target certain sectors, and certain firms within each sector.

---

## Panel A: Sector targeting across ETS instruments

**Claim:** Policymakers selectively target certain economic activities when designing carbon pricing instruments. Some activities (e.g., power generation, cement, iron and steel) are nearly universally covered, while others (e.g., agriculture, waste, road transport) are rarely included.

**Data source:** ICAP ETS Map factsheets, scraped from `https://icapcarbonaction.com/en/compare/{id}` (April 2026). Each factsheet contains a "Sectors and thresholds" section describing which specific industrial activities are covered by the ETS.

**Cleaning procedure:**

1. Identified all valid factsheet IDs (43-138) by checking HTTP status codes for `https://icapcarbonaction.com/system/files/ets_pdfs/icap-etsmap-factsheet-{id}.pdf`.
2. For each valid ID, fetched the compare page and extracted:
   - ETS name and status (In force / Under development / Under consideration)
   - Free-text description of covered sectors and thresholds
3. Coded each ETS against a standardized list of activities: power/heat generation, oil refining, iron and steel, cement, glass, lime, ceramics/bricks, pulp/paper/cardboard, chemicals/petrochemicals, aluminum/non-ferrous metals, ammonia, food/beverages, other industry, aviation, maritime/shipping, buildings/heating fuels, road transport, waste incineration.
4. Coding is based on explicit mentions in the "Sectors and thresholds" text. "Yes" = activity explicitly listed or implied by a general threshold that would cover it. "No" = not mentioned or explicitly excluded. "Unclear" = ambiguous.
5. Filtered to ETSs with status "In force" only.
6. "Unclear" values (where the factsheet text was ambiguous) are treated as not confirmed — only "Yes" counts toward coverage.
7. The "Other industry" category is dropped from the figure. Many ETSs (especially Canadian provincial OBPS systems) describe coverage generically as "power and industry" without enumerating specific industrial activities. This residual category inflates the bar without conveying which activities are actually targeted.
8. Note: This dataset covers only ETSs, not carbon taxes. Carbon taxes are harder to characterize at the activity level because they often apply via fuel taxation rather than installation-level regulation.

**Raw data:** `NBB_data/raw/icap_ets_sector_coverage.csv`

**Script:** `analysis/panel_a_sector_targeting.R`

---

## Panel B: Distribution of jurisdictional GHG coverage across CPPs

**Claim:** Most carbon pricing instruments cover well under 100% of their jurisdiction's GHG emissions. The median instrument covers only about 36% of jurisdictional emissions.

**Data source:** Annex B, *State and Trends of Carbon Pricing 2025*, World Bank (pp. 69-72). The table summarizes key metrics for all 80 implemented carbon taxes and ETSs as of April 1, 2025.

**Cleaning procedure:**

1. Manually transcribed the Annex B table from the PDF (`NBB_data/raw/State and Trends of Carbon Pricing 2025.pdf`, pages 69-72) into a CSV file.
2. Columns retained: instrument name, type (Carbon tax / ETS), start date, jurisdiction, region, economy income group, main price rate (USD), share of jurisdiction's GHG emissions covered (%), government revenue from direct carbon pricing (2024).
3. For the histogram, dropped instruments with missing coverage data (5 instruments: Saitama ETS, Mexico ETS, State of Mexico carbon tax, UK Carbon Price Support, and a few others where the column was blank in the PDF).
4. Price and revenue fields left blank where the PDF showed no value ("Price and revenue values are blank where data is not available or where revenue has not been collected" — note on p. 72).

**Raw data:** `data/raw/annex_b_carbon_pricing_2025.csv`

**Script:** `analysis/descriptive_figures_cpp_incompleteness.R`

---

## Panel C: Within-sector EU ETS coverage across 4 EU countries

**Claim:** Even within sectors targeted by a carbon pricing instrument, coverage is incomplete — some firms are covered while others in the same sector are not (due to size thresholds, activity-specific exemptions, or reporting boundaries).

**Data source:** Article 14 Annex XII of national inventory reports (NIRs), submitted under the EU Governance Regulation. These annexes report, for each IPCC/CRF sector, the ratio of verified EU ETS emissions to total GHG inventory emissions.

**Countries and vintages:**
- Belgium (2025 submission): `BE_Art14_AnnexXII_2025.xlsx`
- France (2024 submission): `FR_Art14_AnnexXII_2024.xlsx`
- Bulgaria (2026 submission): `BG_Art14_AnnexXII_2026.xlsx`
- Czechia (2026 submission): `CZ_Art14_AnnexXII_2026.xlsx`

**Why only 4 countries:** Only 4 of 27 EU member states publish this annex on EIONET CDR. The remaining 23 countries only upload CRT data tables and NIR documents without the governance regulation annexes.

**Cleaning procedure:**

1. All four files share the same 5-column structure:
   - Column A: IPCC/CRF sector label (e.g., `1.A.1.a Public electricity and heat production`)
   - Column B: GHG inventory emissions [kt CO2eq]
   - Column C: Verified EU ETS emissions [kt CO2eq]
   - Column D: Ratio (verified / inventory), i.e., the ETS coverage share
   - Column E: Comment
2. Metadata rows (country name, reporting year, header descriptions) occupy approximately rows 1-10. Data rows are identified by sector labels starting with a digit (CRF codes like `1.A.1.a`) or "Iron" (for the combined iron and steel row).
3. **Ratio scale normalization:** Belgium reports ratios as percentages (0-100); France, Bulgaria, and Czechia report as fractions (0-1). Detection rule: if max(ratio) <= 2, multiply by 100.
4. **Exclusions:**
   - Sectors with coverage = 0%: these are sectors entirely outside the ETS scope, not evidence of within-sector incompleteness.
   - Sectors with coverage > 100%: these are reporting artefacts where the reporting authority cannot cleanly separate emissions from fuel combustion vs. industrial processes across CRF categories (26 observations dropped).
   - Aggregate rows (e.g., "1.A Fuel combustion activities, total", "1.A.2 Manufacturing industries and construction") are dropped to avoid double-counting with their sub-sector rows.
5. The "Aggregate" vertical line on the figure is computed as: sum(verified ETS emissions) / sum(inventory emissions) across all retained sector-country pairs, giving an emissions-weighted average coverage rate.

**Raw data:** `NBB_data/raw/NIR/` (4 Excel files listed above)

**Script:** `analysis/panel_c_within_sector_coverage.R`

---

## Data download details

### EIONET CDR (Annex XII files)

Files downloaded from the Central Data Repository of the European Environment Information and Observation Network:

| Country | URL path | Envelope |
|---------|----------|----------|
| Belgium | `cdr.eionet.europa.eu/be/eu/govreg/inventory/envz9p5nw/` | 2025 - 15th March Submission |
| France | `cdr.eionet.europa.eu/fr/eu/govreg/inventory/envzfrxvg/` | FR - GHG Inventory - 15 March 2024 |
| Bulgaria | `cdr.eionet.europa.eu/bg/eu/govreg/inventory/envabcow/` | (March 2026 submission) |
| Czechia | `cdr.eionet.europa.eu/cz/eu/govreg/inventory/envabp7qq/` | (March 2026 submission) |

Navigation path: EIONET > CDR > Country > European Union (EU) obligations > EU Governance Regulation > GHG inventories > [version].

### World Bank Carbon Pricing Dashboard

Sector coverage data (Yes/No/In principle) per instrument scraped from the JavaScript data object embedded in `https://carbonpricingdashboard.worldbank.org/compliance/instrument-detail` (April 2026). The `compliance_detail_table_instruments` array contains 136 instruments with 30 fields each, including 10 binary sector coverage indicators.

**Raw data:** `NBB_data/raw/carbon_pricing_dashboard_sectors.csv`

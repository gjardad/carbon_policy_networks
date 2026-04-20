# Martinsson, Sajtos, Stromberg & Thomann (2024) — "The Effect of Carbon Pricing on Firm Emissions: Evidence from the Swedish CO2 Tax"

**Published in:** *Review of Financial Studies*, 37(6), 1848-1886.

---

## 1. Research question
What is the elasticity of firm-level CO2 emissions to carbon pricing? How does it vary across firms with different abatement costs, mobility, and financial constraints? What fraction of Sweden's manufacturing emission reductions can be attributed to carbon pricing?

## 2. Audience
Financial economists (note: published in RFS), environmental economists, climate policy researchers. Bridges the finance-climate nexus literature with the environmental regulation literature.

## 3. Method
- **Panel regression** estimating carbon pricing elasticities using firm and year fixed effects.
- Specification: Δln(E/Y) = α + Σβ_s · Δln(1 - C_{i,t-s}) + μ_i + μ_t + ε, where E/Y is emission intensity (CO2/PPI-deflated sales) and C is the marginal emissions cost share of sales.
- Allows up to 3 lags to capture delayed investment responses.
- **DiD analysis** around specific tax regime changes (1991 introduction, 1993 revision, 1997 change) using exemption status as treatment/control.
- Identification from cross-sectional variation in marginal tax rates due to caps, exemptions, and EU ETS phase-in.
- **Decomposition** of aggregate emissions into scale, composition, and technique effects (Grossman & Krueger 1993; Levinson 2009).
- **Back-of-envelope calibration** to quantify aggregate impact.

## 4. Data
- **Country:** Sweden
- **Period:** 1990-2015 (26 years — longest firm-level CO2 panel to date as of publication)
- **Unit of observation:** Firm-year
- **Sample:** ~4,000 manufacturing firms, covering 85-90% of Swedish manufacturing CO2 emissions.
- **Key data sources:**
  - **SEPA** (Swedish Environmental Protection Agency) — plant- and firm-level CO2 emissions.
  - **UC** (Upplysningscentralen, 1990-1997) and **Bisnode Serrano** (1998-2015) — firm registry data (accounting variables, sector codes).
  - **EUTL** — EU ETS registry for identifying regulated installations.
  - **Statistics Sweden investment survey** — for constructing mobility measures.
  - **Statistics Sweden environmental expenditure survey** (2002-2015) — for PACE measures.
  - Swedish PPI at 4-digit NACE level for deflating sales.
- Carbon taxes calculated from actual CO2 heating emissions × applicable tax schedule (including exemptions).
- For EU ETS period: marginal tax = weighted sum of carbon tax rate + emission allowance prices.

## 5. Statistical methods
- OLS with firm and year FE (main specification).
- Industry × year FE as robustness.
- Clustering at firm level.
- Subgroup analysis by: emission decile, PACE (pollution abatement costs), asset mobility, EU carbon leakage list, listing status, size, dividend payout, age, and financial crisis periods.

## 6. Findings

### Carbon pricing elasticities (Table 4):
| Sample | Elasticity (sum of 3 lags) |
|--------|---------------------------|
| All firms | **2.08** |
| Deciles 1-4 (low emitters) | **6.72** |
| Deciles 5-8 | **2.73** |
| Deciles 9-10 (top emitters) | **1.30** |

### Heterogeneity (Table 6 — PACE × Mobility):
| Group | Elasticity | Share of emissions |
|-------|-----------|-------------------|
| Low PACE, Low mobility | **2.78** | 4.2% |
| Low PACE, High mobility | **2.93** | 1.3% |
| High PACE, Low mobility | **1.72** | 90.2% |
| High PACE, High mobility | **2.45** | 4.4% |

### EU carbon leakage list (Table 7):
| Group | Elasticity |
|-------|-----------|
| Not on leakage list | **2.59** |
| On leakage list (trade only) | **3.35** |
| On leakage list (high emissions) | **1.54** |

### Financial constraints (Table 8):
- **Less constrained** (public, large, high dividend, mature): elasticities 2.1-2.9
- **More constrained** (private, small, low dividend, young): elasticities 0.5-0.7 (insignificant)
- Financial constraints matter **only in high-PACE sectors** (where abatement requires significant investment).

### Short-term DiD results (Table 3):
- 1991 introduction: firms without exemptions showed no increase in emission intensity; exempt firms (zero marginal tax) increased emissions by 18%.
- 1997 change: firms facing marginal tax increase reduced emissions ~11%; firms with tax decrease increased ~11%.

### Aggregate effect (Table 9):
- Swedish manufacturing CO2 decreased 31% (1990-2015).
- Decomposition: 3pp scale + 10pp composition + **18pp technique**.
- Calibration: carbon pricing accounts for **~30% higher emissions** absent the tax (2015 base year).
- With uniform high marginal pricing (post-EU ETS), effect could account for nearly all of the emission decline.

## 7. Contributions
- Estimates carbon pricing **elasticities** (not just average treatment effects), enabling cross-scheme comparisons.
- Longest firm-level CO2 panel (26 years), capturing full dynamic response.
- Documents that **tax exemptions for top emitters** drastically reduced policy effectiveness by lowering marginal incentives.
- Shows **financial constraints** impede emission abatement, especially in high-PACE sectors — relevant for policy design combining carbon pricing with green finance.
- Demonstrates that even a unilateral carbon tax can be effective.

## 8. Replication feasibility
- SEPA emissions data: available upon request from Swedish Environmental Protection Agency.
- Firm registry data: UC/Bisnode — commercial databases, may require purchase.
- EU ETS data: EUTL publicly available.
- Sweden-specific tax schedule is well-documented.
- Replication code and data available at Harvard Dataverse: https://doi.org/10.7910/DVN/9NWRW8.

---

## Notes and thoughts for our project

- **Key insight for us:** The finding that tax exemptions for top emitters (who had zero marginal tax despite high average tax) drastically reduced effectiveness is a policy design lesson. It also creates variation we might exploit in network analysis.
- **Financial constraints finding** is novel and connects to our interest in how firm characteristics mediate policy propagation through networks.
- **Emission concentration:** 80-90% of manufacturing CO2 comes from top two deciles of sectors — any network analysis must account for this skewness.
- **Contrast with Colmer et al.:** Both study EU ETS impact on manufacturing firms but from very different angles. Colmer finds 14-16% reduction for French ETS firms; Martinsson estimates elasticity of ~1.3 for Swedish top emitters. The Swedish tax also covers non-ETS firms.
- **PACE data** could be useful proxy for abatement costs in our context.

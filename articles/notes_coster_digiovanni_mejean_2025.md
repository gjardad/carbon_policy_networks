# Coster, di Giovanni & Mejean (2025) — "Firms' Supply Chain Adaptation to Carbon Taxes"

**Version:** November 17, 2025 working paper. Authors at USC, FRB New York/CEPR, Sciences-Po/CEPR.

---

## 1. Research question
How do firms adjust their input sourcing in response to climate policy (the EU ETS), and does this lead to carbon leakage through supply chains? What would be the additional impact of a Carbon Border Adjustment Mechanism (CBAM)?

## 2. Audience
Trade economists, environmental/climate economists, policymakers designing carbon border adjustments. Bridges quantitative trade models with climate policy evaluation.

## 3. Method

### Empirical:
- **Novel product-level dataset** classifying goods as regulated (covered by ETS/CBAM) or unregulated based on actual policy coverage, not emissions content.
- **PPML difference-in-differences:** compare the geographic sourcing of regulated vs. unregulated inputs from ETS vs. non-ETS countries, before and after ETS implementation.
- Regression: y_{fpit} = exp[Σβ_τ · 1(i∉ETS) · 1(p∈regulated) + X'θ + ε]
- Two dependent variables: (i) import share, (ii) import probability (extensive margin).
- Rich FE: firm×product×country, country×year, sector×year.
- Balanced panel in firm×product×country dimension.

### Structural model:
- Extension of **Antras, Fort & Tintelnot (2017)** global sourcing model with:
  - Two input types (regulated, unregulated) in nested CES production.
  - Country- and input-specific carbon taxes.
  - Carbon damages in utility (following Shapiro 2021).
- Firms face fixed costs of sourcing from each country; pay these to access cheaper inputs.
- Combinatorial optimization problem solved with hybrid algorithm (mixed supermodularity/submodularity).
- **Estimation:** sourcing potentials from firm-level import data (country FE regression), technical coefficients from IO tables, elasticities calibrated, fixed costs estimated via SMM.
- **Tax rates** computed using WIOD sector×country emission intensities × Leontief inverse (to capture IO linkages).

## 4. Data
- **Country:** France (firm-level)
- **Period:** 2000-2019 (empirical); 2004 cross-section (model calibration)
- **Unit of observation:** Firm × product × source country × year
- **Sample:** 27,240 firms in regression sample; 62,525 manufacturing firms (18,929 importers) in 2004 calibration data. 44 regulated-intensive manufacturing sectors. 47 source countries.
- **Key data sources:**
  - **French customs data** (DGDDI) — firm-level imports by origin country and product (CN8), 2000-2019.
  - **EU Transaction Log (EUTL)** — ETS sector coverage.
  - **EU CBAM regulation** (2023/956) — product list.
  - **INSEE IO table** (2011, 138 NAF sectors) — for identifying regulated-intensive sectors and core inputs.
  - **INSEE-FICUS** (2004) — firm balance sheets.
  - **WIOD 2016** — sector×country emissions and IO tables for tax rate computation.
  - **INSEE PPI** — for documenting relative price dynamics.
  - **World Bank Doing Business** (Trading Across Borders) — for fixed cost estimation.
  - **Yale EPI Climate Mitigation sub-index** — for fixed cost estimation.
- Regulated products defined by mapping ETS activities and CBAM product list to HS codes (1,464 regulated products).

## 5. Statistical methods

### Empirical:
- PPML estimation with multiple FE specifications.
- de Chaisemartin & D'Haultfoeuille (2020) heterogeneous treatment effects estimator as robustness.
- Leave-one-out country exercise.
- MNE vs. non-MNE subsample analysis.

### Structural:
- SMM with 1,500 Latin Hypercube starting points.
- Gravity-style fixed cost parameterization.
- Free entry condition pins down equilibrium mass of firms.
- Counterfactual: hold nominal expenditure fixed, re-solve sourcing + free entry.

## 6. Findings

### Empirical — Carbon leakage via supply chains:
- **Import share** of regulated products from non-ETS countries increased by **14%** relative to unregulated products by 2019 (relative to 2004).
- In levels: 4.3 pp increase in the share of ETS-regulated products sourced from outside the EU (from 29.7% to 34%).
- **Extensive margin:** probability of importing a regulated product from a non-ETS country increased from 10.5% to 14.0% (3.6 pp).
- Leakage significant from **Phase 3 onward** (2013+), when ETS became binding.
- Driven by **non-ETS-regulated firms** (downstream users of regulated inputs), not by ETS firms themselves.
- Driven by **non-MNE firms**.
- Relative prices of regulated vs. unregulated products rose over the ETS period (Figure 1).

### Structural — ETS-only simulation (€100/ton):
- Model reproduces observed leakage at the intensive margin (coefficient 0.134 vs. 0.119 in data).
- Underestimates extensive margin adjustment (0.049 vs. 0.136 in data).
- **Emissions:** -0.67M tons (-0.42%) relative to no-tax baseline.
  - Regulated inputs: -0.92M tons; unregulated inputs: +0.25M tons (substitution).
  - ETS countries: -2.19M tons; non-ETS countries: +1.83M tons (leakage).
- **Price index:** +0.87%.
- **Welfare:** -0.20% (price effect dominates emission reduction benefit).

### Structural — ETS + CBAM simulation (€100/ton each):
- Leakage **reversed**: firms shift sourcing back to ETS countries.
- **Emissions:** -4.97M tons (-3.07%) — **7× larger** than ETS alone.
- Bulk of import reduction from Russia and China; increased domestic sourcing.
- **Price index:** +1.42%.
- **Welfare:** -0.33% (larger price cost outweighs larger emission benefit).
- With tax rebate: welfare loss approximately halved.

### Heterogeneity across firm productivity:
- No leakage in bottom 50% of productivity distribution (don't import regulated inputs from non-ETS countries).
- Leakage concentrated in **top 1%** of firms (already have wide sourcing networks).
- ETS+CBAM adjustment also concentrated in top 1%.

### Non-linearity:
- Leakage increases non-linearly with tax level due to extensive margin jumps.
- Model best matches data at ~€70/ton.

## 7. Contributions
- First paper to document carbon leakage through **input sourcing** (supply chain channel), distinct from leakage through own production relocation.
- Novel product-level classification of regulated goods based on actual policy coverage.
- Extends Antras et al. (2017) to handle regulated/unregulated inputs + carbon taxes.
- Quantifies that CBAM is 7× more effective than ETS alone for reducing emissions, but at significant price cost.
- Shows leakage is concentrated in the most productive firms with widest sourcing networks.

## 8. Replication feasibility
- French customs data: through CASD.
- EUTL and CBAM product lists: publicly available.
- WIOD: publicly available.
- INSEE IO tables and FICUS: through CASD.
- Model code: not yet publicly available (working paper).
- Authors' product classification (HS → regulated/unregulated) is a key novel contribution.

---

## Notes and thoughts for our project

- **Most directly relevant paper** for our network/supply-chain perspective on carbon policy.
- **Key complementarity:** They study how downstream firms adjust sourcing of regulated inputs; we can study how carbon policy propagates through the full production network (not just direct input sourcing).
- **Their finding that leakage is driven by non-ETS firms** (downstream users) is crucial — it means looking only at ETS-regulated firms misses a major channel.
- **Contrast with Colmer et al.:** Colmer et al. find no leakage for French ETS firms' own imports. Coster et al. find leakage in downstream firms' sourcing of regulated inputs. These are **complementary**, not contradictory.
- **Product-level classification** (ETS/CBAM → HS codes) is a methodological contribution we should build on.
- **IO table approach** for identifying regulated-intensive downstream sectors is directly applicable to our setting with Belgian data.
- **€100/ton tax** is their baseline; CBAM 7× multiplier is a key policy result.
- **Limitation they acknowledge:** partial equilibrium (one country), no GE adjustment of trade/production abroad. Our network approach could potentially address some of this.

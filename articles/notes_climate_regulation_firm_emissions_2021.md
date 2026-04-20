# "Climate Regulation, Firm Emissions, and Green Takeovers" (2021)

**Authors:** Not listed on title page (appears to be De Jonghe, Mulier, and Schepens based on footnote acknowledgments and NBB affiliation). Previously circulated as "Going green by putting a price on pollution: Firm-level evidence from the EU."
**Date:** November 19, 2021. Prepared for the 2020 NBB Colloquium "Climate Change: Economic Impact and Challenges for Central Banks and the Financial System."

**Status:** Fully read (32 pages). This is a preview/working paper version (tables referenced as "HERE" placeholders, but text reports all key coefficients).

---

## 1. Research question
How do polluting firms react to an unexpected tightening of EU climate regulation (carbon price increase)? Do they improve emission efficiency? Do they change their M&A behavior to acquire "green" firms? Is there evidence of carbon leakage through cross-border M&A?

## 2. Audience
Financial economists, climate policy researchers, M&A researchers, central bankers concerned with climate risk.

## 3. Method
- **Difference-in-differences** exploiting the 2017 EU ETS regulatory tightening (Market Stability Reserve reform). Same identification strategy as Mulier et al. (2024) — this is the precursor paper.
- **Treatment variable:** *Allowance Shortage* = firm-level 2013-2016 average of (verified emissions - free allowances) / total assets. *High Exposure* dummy = above median allowance shortage within NACE 2-digit sector.
- **Specification:** Y_{i,t} = β₁Post_t + β₂HighExposure_i + β₃(HighExposure_i × Post_t) + β₄X_{i,t} + ε_{i,t}
- Y_{i,t} = emission efficiency (log operating revenue / emissions), log total emissions, or log operating revenue.
- Pre-period: 2014-2016 (EUA prices ~5 EUR, not binding); Post-period: 2017-2019 (prices rising to 30 EUR).
- Firm FE and year FE (baseline); country-sector-year FE (saturated).
- Also study M&A behavior (probability of acquiring, target characteristics).

## 4. Data
- **Sample:** 20,095 firm-year observations from 3,952 firms in 27 European countries owning 8,820 installations (aggregated to firm-year level).
- **Coverage:** ~70% of total GHG emissions in the EU ETS (Phase III, 2013-2020).
- **M&A:** 1,132 acquisitions announced by 481 firms (806 unique firm-years).
- **Key data sources:**
  - **EUTL** — installation-level verified emissions, free allowances, 14,268 installations in Phase III.
  - **ORBIS** (Bureau van Dijk) — firm-level balance sheet and P&L.
  - **ZEPHYR** (Bureau van Dijk) — M&A deals.
- Free allowance allocation formula: F_i = Benchmark × CLEF × HAL_i (efficiency benchmark × carbon leakage exposure factor × historical activity level).
- ~43% of allowances allocated for free in Phase III on average; 100% for carbon leakage sectors, 0% for power generators.

## 5. Statistical methods
- DiD with firm and year FE.
- Comparison within NACE 2-digit sectors.

## 6. Findings

### 6.1 Emission efficiency (Table 2):
- Average firm: emission efficiency increased ~8% post-2017 vs. 2014-2016 (Post dummy).
- **DiD (High Exposure × Post):** high-exposed firms improved efficiency by **4.9%** (firm + year FE) to **8.7%** (firm + country-sector-year FE) relative to low-exposed firms.
- Event study (Figure 8): no pre-trends in 2014-2015; significant divergence in 2017 (~7%) growing to ~10% in 2018.

### 6.2 Emissions vs. revenue decomposition (Table 3):
- High-exposed firms reduced **emissions by 7.2%** (average) to **11.3%** (by 2019) relative to low-exposed.
- **No significant difference in operating revenue** between high and low exposure groups.
- Efficiency gain is driven by **emission reduction, not output expansion**.

### 6.3 Free allowance allocation matters (Table 4):
- **Carbon leakage list firms** (get maximum free allowances): high-exposed firms do **not** significantly improve efficiency.
- **Non-leakage-list firms** (fewer free allowances): high-exposed firms improve efficiency by **10.7%**.
- **Electricity producers** (zero free allowances): strongest response — **15.7%** improvement.
- Non-electricity, non-leakage firms: **9.8%** improvement.

### 6.4 M&A — Green takeovers (Table 5):
- "M&A greenness" variable: % of green words in deal text (editorial, rationale, comments).
- High-exposed firms shifted toward **greener targets** after 2017: coefficient on triple interaction = **0.350** (p<0.05), representing a **71% increase** relative to the unconditional mean.
- Effect driven by firms **not on the carbon leakage list** (those with fewer free allowances).
- Among non-leakage firms, **electricity producers** show the strongest shift toward green targets.

### 6.5 M&A — Location of targets (Table 7):
- High-exposed firms became **8-12% more likely** to acquire targets **inside** the EU ETS area post-2017.
- Effect significant only for firms **not on the carbon leakage list**.
- **No evidence of carbon leakage via M&A** — firms are not relocating production outside the regulated area.
- High-exposed firms also more likely to acquire **new targets** (rather than increasing stakes in existing ones), driven by non-leakage-list firms.

### 6.6 Robustness (Tables 8-9):
- Alternative efficiency proxies (sales/emissions, assets/emissions, total assets/emissions): all show significant 7.8-12.4% improvement.
- Adding firm-level controls (log total assets, profit margin): unchanged.
- Continuous allowance shortage variable: confirms results.
- Dropping largest sectors (electricity, non-metallic minerals, basic metals): still significant 6.1-7.9% improvement, driven by emission reduction not revenue changes.

### Key policy implications:
1. Carbon pricing works better when EUA prices are sufficiently high and informative.
2. Free allowance allocation design matters — more free allowances = weaker incentives.
3. M&A is an important mechanism for the green transition (green takeovers).
4. No evidence of carbon leakage through FDI/M&A, but trade-based leakage remains an open question.

## 7. Contributions
- Documents M&A as a mechanism for achieving emission efficiency under carbon pricing — "green takeovers."
- Shows that free allowance allocation affects the strength of firms' responses.
- Provides evidence against carbon leakage via cross-border M&A.

## 8. Replication feasibility
- Same data sources as Mulier et al. (2024) — EUTL, ORBIS, ZEPHYR.
- All commercially available or publicly accessible.

---

## Notes and thoughts for our project

- **This appears to be a precursor to Mulier et al. (2024)** — similar authors (Mulier), same identification strategy (2017 ETS tightening), same data sources. The 2024 paper adds the overlapping policies dimension.
- **Green takeovers are a network phenomenon** — polluting firms acquire cleaner ones, restructuring the corporate network. Relevant to our network perspective.
- **No carbon leakage via M&A** complements Colmer et al.'s finding of no leakage via imports for French ETS firms.
- **Need to finish reading** for full results, robustness, and data details.

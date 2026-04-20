# Mulier, Ovaere & Stimpfle (2024) — "Emission Trading and Overlapping Environmental Support: Installation-level Evidence from the EU ETS"

**NBB Working Paper No. 461, October 2024. National Bank of Belgium.**

**Status:** Fully read (pages 1-40, main text complete). Appendix figures not read.

---

## 1. Research question
How do national environmental support policies (renewable energy subsidies, cost compensation for energy-intensive firms, energy efficiency investments, R&D funding) interact with the EU ETS carbon price? Do they amplify or weaken its effectiveness?

## 2. Audience
Climate policy researchers, EU policymakers designing overlapping policy instruments, environmental economists studying policy interactions.

## 3. Method
- **Difference-in-differences** exploiting two sources of variation:
  1. The unexpected 2017 regulatory tightening of the EU ETS (Market Stability Reserve reform), which led EUA prices to rise from ~5-6 EUR to 60-100 EUR.
  2. Cross-sectional variation in installations' **carbon price exposure** — measured by the gap between verified emissions and free allowances (allowance shortage).
- Compare high-exposed (least efficient) installations to low-exposed ones within the same NACE 2-digit sector.
- Pre-period: 2014-2016; Post-period: 2017-2023 (after regulatory tightening became credible, May 12, 2017).
- Novel **production-based emission efficiency** measure at installation level (avoids reliance on firm-level balance sheet data).
- Interaction of EU ETS effect with country-industry-year-level measures of four types of national support.

## 4. Data
- **Coverage:** ~90% of total EU ETS emissions
- **Period:** 2012-2023
- **Unit of observation:** Installation-year
- **Sample:** 53,662 installation-years; 3,033 firms; 4,609 installations; 70 NACE 4-digit industries; 27 countries (excl. UK). Power: 727 firms, 1,283 installations. Manufacturing: 2,356 firms, 3,326 installations.
- **Average installation emissions:** 289k tCO2 (full), 656k (power), 145k (manufacturing).
- **Key data sources:**
  - **EUTL** (EU Transactions Log) — verified emissions and free emission allowances at installation level. Pre-processed version from euets.info.
  - **EU Competition Case Search (CCS)** — 24,000+ state aid cases with objectives, legal basis, aid instrument, duration, annual expenditures.
  - **EU Transparency Award Module (TAM)** — beneficiary-level aid awards linked to CCS cases (mandatory from 2016, covers awards >EUR 500k).
  - **ORBIS** (Bureau van Dijk) — firm-level balance sheet and P&L data.
  - **ZEPHYR** (Bureau van Dijk) — M&A deal information.
  - **Global Energy Monitor (GEM)** — power plant technology, fuel type, age (matched to EUTL for power producer emission intensity).
- **Environmental support policy intensity** = Σ expenditures / Σ emissions (EUR/tCO2) at country-industry-year level.
- Four support policy types: (i) renewable energy support (res), (ii) compensation for energy-intensive undertakings (eiu), (iii) investment aid for energy efficiency (eff), (iv) R&D funding (rnd). Res and eiu dominate (~80% of total environmental support).
- **Carbon price exposure (CPE):** Installation-level emission intensity, constructed from the harmonised free allocation benchmarks (manufacturing) or GEM fuel type (power). High-exposed = above median CPE within NACE 4-digit × activity.

## 5. Statistical methods
- DiD with installation and year FE.
- Robustness: alternative FE structures (year-industry-activity, year-industry-activity-country).
- Alternative definitions of support intensity.
- Energy price controls.

## 6. Findings

### Baseline DiD (Table 3 — Poisson QML):
| Sample | β₁ (Post × High-exposed) | ATT% |
|--------|--------------------------|------|
| Full | **-0.279*** (0.031) | ~24% reduction |
| Manufacturing | **-0.067*** (0.017) | ~6.5% reduction |
| Power | **-0.449*** (0.051) | ~36% reduction |

- All specifications include installation FE + year-industry-activity FE. Energy price controls included.
- Clear parallel pre-trends (Figure 11 event study): no differential trends before 2017, sharp divergence after.
- Largest reductions in 2020-2022, coinciding with EUA prices reaching 60-100 EUR.

### Overlapping policies — Horse race (Table 4, triple-diff with all four policy types):
- **Manufacturing:** Only **compensation for energy-intensive undertakings (eiu)** has a significant interaction: β₃ = +0.086*** — positive sign means compensation **weakens** the carbon price effect.
- **Power:** Only **renewable energy support (res)** has a significant interaction: β₃ = -0.448*** — negative sign means renewables support **amplifies** the carbon price effect.
- Energy efficiency aid and R&D: no significant interactions in either sample.

### Renewable energy support × ETS for power (Table 5):
- Low renewable support countries: ~28% reduction (1 - e^{-0.324}).
- High renewable support countries: ~55% reduction (1 - e^{-0.324-0.485}) — almost **twice as large**.
- Robust to year-country FE and year-industry-activity-country FE.

### Cost compensation × ETS for manufacturing (from text):
- High-exposed manufacturing installations in countries with generous compensation reduce emissions by **at least 7% less** than those in countries with less generous compensation.
- This is a **negative interaction**: compensation undermines the marginal carbon price signal by effectively subsidizing energy-intensive production.

### Heterogeneity:
- Carbon leakage list matters: firms on the leakage list show weaker responses.
- Temporal dynamics: effect grows over time, largest in 2020-2022 when EUA prices peaked.

## 7. Contributions
- First empirical estimation of how national environmental support policies interact with the EU ETS carbon price at the installation level.
- Novel dataset of 24,000 EU state aid cases measuring national environmental support.
- Shows that cost compensation weakens the carbon price signal — important policy design implication.
- Documents M&A as a mechanism for emission efficiency gains ("green takeovers").

## 8. Replication feasibility
- EUTL data: publicly available.
- ORBIS/ZEPHYR: commercial databases (Bureau van Dijk).
- EU State Aid data: available through EU transparency requirements.
- National support measures constructed by authors — methodology could be replicated.

---

## Notes and thoughts for our project

- **Directly relevant:** NBB working paper using EU ETS data — same institutional context as our Belgian setting.
- **Key finding for us:** Cost compensation for energy-intensive firms weakens the carbon price signal. This is relevant if Belgian firms receive such compensation.
- **M&A channel ("green takeovers")** is a network-relevant mechanism — polluting firms restructure by acquiring cleaner firms within the ETS zone.
- **The 2017 regulatory tightening** is a useful quasi-natural experiment we could also exploit.
- **Need to finish reading:** Data section, full results tables, and robustness checks.

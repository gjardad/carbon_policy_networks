# Colmer, Martin, Muuls & Wagner (2024) — "Does Pricing Carbon Mitigate Climate Change? Firm-Level Evidence from the European Union Emissions Trading System"

**Version:** January 2024. Forthcoming, *Review of Economic Studies*.

---

## 1. Research question
Does the EU ETS — the world's first major cap-and-trade system — actually reduce firm-level CO2 emissions, and if so, through what mechanisms? The paper asks whether emission reductions represent genuine global abatement (not just carbon leakage) and whether they come at a cost to economic performance.

## 2. Audience
Environmental economists, climate policy researchers, industrial organization scholars, and policymakers evaluating the efficacy of market-based climate regulation.

## 3. Method
- **Matched difference-in-differences** (semi-parametric, following Heckman et al. 1997, 1998).
- ETS firms matched 1-to-1 (with replacement) to non-ETS firms on: CO2 emissions, value added, employment, capital, emissions intensity, imports, gas share, electricity share, number of plants, and 2-digit NCE sector. Matching uses year 2000 (pre-announcement) data.
- Nearest-neighbor matching within sectors using Mahalanobis distance.
- Regression on matched sample comparing outcomes across four periods: pre-announcement (1996-1999), announcement (2001-2004), Phase I (2005-2007), Phase II (2008-2012), relative to base year 2000.
- Two-way clustered standard errors (firm and matching group level), following Abadie & Spiess (2022).
- Sensitivity analysis using Rambachan & Roth (2023) for parallel trends robustness.

## 4. Data
- **Country:** France
- **Period:** 1996-2012 (17 years)
- **Unit of observation:** Firm-year
- **Sample:** 4,201 firms in year 2000 (252 ETS firms, 3,949 non-ETS); matched sample: 149 ETS + 149 non-ETS = 298 firms.
- **Key data sources:**
  - **EACEI** (Annual Survey of Industrial Energy Consumption) — detailed fuel use by type, from INSEE. ~12,000 establishments/year, ~90% response rate. Covers 88% of industrial emissions in 2000.
  - **FICUS/FARE** — fiscal census of manufacturing firms (income statements, balance sheets).
  - **French Customs (DGDDI)** — firm-level imports by product (CN8) and origin country.
  - **EUTL** — EU ETS registry for identifying regulated installations.
  - **Antipol** — environmental protection investment survey.
  - **CO2 emissions** constructed bottom-up from fuel consumption × ADEME conversion factors. Correlation with verified EUTL emissions: 0.96.
- Linked via SIREN (firm) and SIRET (plant) identifiers.

## 5. Statistical methods
- Matched DiD with OLS on the matched sample.
- Matching with replacement (preferred for bias reduction); robustness with matching without replacement.
- Caliper restrictions and cardinality matching (Zubizarreta et al. 2014) as robustness.
- Event study coefficients year-by-year (Figure 3).
- Revenue-based TFP estimation using index-number and estimation-based approaches.
- Controls for Great Recession exposure (departement-level unemployment changes, sector-level employment changes).

## 6. Findings

### Main results (Table 2):
| Outcome | Phase I (2005-07) | Phase II (2008-12) |
|---------|-------------------|---------------------|
| log(CO2) | **-0.140** (0.057) | **-0.163** (0.075) |
| log(Value Added) | -0.050 (0.085) | 0.097 (0.079) |
| log(Employment) | -0.002 (0.036) | 0.046 (0.050) |
| log(Capital) | *0.083* (0.046) | *0.105* (0.060) |
| log(CO2/VA) | -0.099 (0.068) | **-0.174** (0.075) |

- EU ETS induced **14% emission reduction in Phase I, 16% in Phase II**, with no detectable negative effects on value added or employment.
- Emissions intensity of value added fell 17.4% in Phase II.
- Capital stock increased (weakly significant), consistent with investment in cleaner technology.

### Mechanism results (Table 3):
- **No evidence of carbon leakage:** no significant changes in total imports, carbon-intensive imports, or electricity purchased share.
- **No fuel switching:** gas share and electricity share unchanged.
- **Technology adoption:** significant increases in integrated pollution-control investments (cleaner production processes) in both phases.
- **TFP:** positive but insignificant effect on measured revenue TFP.

### Aggregate effect:
- EU ETS accounts for 28-47% of the aggregate reduction in French industrial emissions (5.4M tons/year on average, 2005-2012).
- Marginal abatement cost upper bound: $53/ton CO2 ($2017).

### Theoretical framework:
- Standard model predicts emissions reduction + economic contraction.
- Extended model with **technology switching**: firms pay fixed cost to adopt cleaner technology that reduces emissions intensity AND marginal variable costs. This rationalizes the empirical finding of emission reductions without economic contraction.
- Key condition: clean technology must increase TFP enough to offset carbon pricing costs.

## 7. Contributions
- First study to provide compelling evidence that the EU ETS delivered **global** emission reductions (not just local), by ruling out carbon leakage mechanisms.
- Shows market-based regulation can reduce emissions without detectable economic contraction.
- Technology switching model provides a framework for understanding why firms can reduce emissions without negative economic consequences.
- Demonstrates that EU ETS marginal abatement costs compare favorably with non-market-based policies.

## 8. Replication feasibility
- Data accessed through CASD (Centre d'acces securise distant aux donnees) under project E598.
- EUTL data publicly available.
- Firm-level data requires authorization from Comite du Secret Statistique.
- Replication within France feasible with CASD access; replication in other countries requires similar administrative data linkages.

---

## Notes and thoughts for our project

- **Key relevance:** This is the most directly relevant paper for our project — uses French firm-level data, EU ETS setting, matched DiD approach. We should think about how our network perspective complements their firm-level analysis.
- **Limitation they acknowledge:** Cannot study firm exit (balanced panel of stayers). Our network approach could capture reallocation effects.
- **Their innovation channel vs. our network channel:** Colmer et al. focus on within-firm technology switching. We can study how policy propagates through supply chain networks.
- **Data overlap:** They use EACEI, FICUS/FARE, and customs data — same data sources we may need.
- **Their finding of no leakage** contrasts with Coster et al. (2025) who find leakage via input sourcing. The difference: Colmer et al. look at regulated firms' own imports; Coster et al. look at downstream firms' sourcing of regulated inputs.

# Phase 0 Findings: Decomposition of Belgian ETS Emission Changes, 2005-2022

## Version A — ETS firms only

---

## Summary

Belgian ETS emissions fell approximately **46% between 2005 and 2022**. This decline decomposes into:

| Channel | Magnitude (pp) | Share of total |
|---------|---------------|---------------|
| Between-sector reallocation | 20-23 | ~45-50% |
| Within-firm abatement | 23-26 | ~50-55% |
| Within-sector reallocation | ~0 | ~0% |

The dominant forces are (i) a shift in Belgium's manufacturing mix toward less emission-intensive sectors and (ii) individual firms reducing their emission intensity. There is essentially no evidence that market shares shifted from dirty to clean firms within the same sector.

---

## What we are computing

### The decomposition formula

Total emissions in year t can be written as:

```
E_t = Y_t × Σ_i (θ_it × z_it)
```

where `Y_t` is total real output, `θ_it` is firm i's share of total output, and `z_it` is firm i's emission intensity (emissions per unit of real output). Revenue is deflated by a NACE 4-digit domestic PPI (Statbel for 2010+, Eurostat for 2005-2009, chain-linked at 2010; see DATA_CLEANING.md).

Relative to a base period, we construct three counterfactual emission paths:

1. **Scale only:** Hold every firm's output share and emission intensity at base-period levels. Only total output Y_t changes. This tells us what emissions would have been if the economy simply grew or shrank without any change in who produces what or how cleanly they produce.

2. **Scale + Reallocation:** Let output shares change as in the data, but hold each firm's emission intensity at the base level. This adds the effect of market share shifts across firms — if dirtier firms lost share, this line falls below the scale-only line.

3. **Actual:** Both output shares and emission intensities change as in the data.

The gaps between these lines give us:
- **(1) to (2):** reallocation across firms (composition effect)
- **(2) to (3):** within-firm abatement (technique effect)

We further decompose the reallocation into between-sector (NACE 2-digit output shares changing) and within-sector (firm shares shifting within the same sector).

---

## Two approaches and why we need both

### Approach 1: Fixed base year (2005)

**What it does:** For each year t, take all firms that are present in both 2005 and year t. Compare their current output shares and emission intensities to what they were in 2005.

**Simple example:** Suppose sector X has two firms in 2005: Firm A (dirty, 60% market share) and Firm B (clean, 40% share). By 2015, Firm A has 50% share and Firm B has 50% — that's within-sector reallocation. Meanwhile, Firm A also reduced its emission intensity — that's within-firm abatement.

**Advantage:** Cleanly separates within-firm abatement from within-sector reallocation, because we can compare each firm's current intensity to its own 2005 intensity.

**Problem:** The sample shrinks over time. In 2005, 162 firms have non-zero emissions, non-missing revenue, and a NACE code. By 2022, only 100 of these 162 still have clean data. This is not because firms exit the ETS (all 281 Belgian ETS firms are in the EUTL every year) but because their Annual Accounts data becomes unavailable in some years. This attrition could bias the results if the firms that drop out are systematically different.

**Results (key years):**

| Year | Firms | Total change | Scale | Between-sector | Within-sector reallocation | Within-firm abatement |
|------|-------|-------------|-------|----------------|---------------------------|----------------------|
| 2005 | 162 | 0.0 | 0.0 | 0.0 | 0.0 | 0.0 |
| 2008 | 141 | -1.2 | 12.1 | 16.3 | 1.9 | -31.5 |
| 2012 | 127 | -27.2 | 3.3 | 17.1 | 2.0 | -49.6 |
| 2016 | 114 | -26.1 | -3.7 | 1.7 | 7.2 | -31.3 |
| 2019 | 104 | -25.9 | -7.4 | -7.4 | 3.2 | -16.9 |
| 2022 | 100 | -45.8 | -2.7 | -19.2 | 1.7 | -25.6 |

All values in percentage points relative to 2005 = 100.

### Approach 2: Year-on-year chained

**What it does:** For each consecutive pair of years (2005-2006, 2006-2007, ..., 2021-2022), take all firms present in both years and compute the one-year change. Then chain these changes multiplicatively to build a cumulative index from 2005.

**Same example:** Instead of comparing 2015 to 2005 directly, we compare 2006 to 2005 (using all firms in both), then 2007 to 2006 (using all firms in both), etc. Each pair can use a different set of firms. The cumulative change from 2005 to 2015 is the product of all the year-on-year changes.

**Advantage:** Each year-on-year step uses the maximum available sample (~160-190 firms per pair), so there is no attrition problem.

**Problem:** The year-on-year version cannot cleanly separate within-sector reallocation from within-firm abatement. Firm-level output shares fluctuate substantially from year to year (a firm has a good year, then a bad year), creating enormous and offsetting reallocation and technique terms that are meaningless. We can only reliably decompose at the sector level: between-sector reallocation vs. within-sector residual (which bundles abatement and reallocation together).

**Results (key years):**

| Year | Firms (pair) | Total change | Between-sector | Within-sector residual |
|------|-------------|-------------|----------------|----------------------|
| 2005 | 162 | 0.0 | 0.0 | 0.0 |
| 2008 | 145 | -12.4 | 13.3 | -25.6 |
| 2012 | 161 | -32.3 | 10.1 | -42.3 |
| 2016 | 162 | -23.9 | 1.0 | -25.0 |
| 2019 | 156 | -21.3 | -7.0 | -14.3 |
| 2022 | 149 | -46.4 | -22.9 | -23.5 |

All values in percentage points relative to 2005 = 100.

---

## How we arrive at the summary numbers

### Total decline: ~46%

Both approaches agree: -45.8 (fixed-base) and -46.4 (year-on-year). The small difference comes from the year-on-year version capturing firms that enter the clean sample after 2005.

### Between-sector reallocation: 20-23 pp

The fixed-base approach gives -19.2 pp by 2022; the year-on-year gives -22.9 pp. The range reflects the different sample composition. This channel was actually *positive* (wrong direction — shares shifted toward dirtier sectors) through Phase I and Phase II, only turning negative from ~2017 onward. The positive early period likely reflects differential impacts of the 2008-2009 financial crisis across sectors, not carbon policy.

### Within-firm abatement: 23-26 pp

The fixed-base approach directly measures this at -25.6 pp by 2022. The year-on-year approach gives a within-sector residual of -23.5 pp, which bundles abatement and within-sector reallocation. Since the fixed-base version tells us within-sector reallocation is ~0, the year-on-year within-sector residual is essentially all abatement.

### Within-sector reallocation: ~0

The fixed-base approach is the only one that can measure this directly. It shows values between +1.7 and +7.2 pp across all years — small and if anything *positive*, meaning dirtier firms slightly gained share within their sectors. There is no evidence that carbon pricing shifted market shares from dirty to clean firms within the same narrowly defined sector.

---

## Data caveats

### Sample coverage

The Belgian EUTL contains 281 firms in every year (2005-2023). Our analysis uses a subset:

| Filter | Firms lost | Reason |
|--------|-----------|--------|
| Zero emissions in 2005 | 80 | Installations with allowances but no verified emissions that year. 78 of 80 have positive emissions in other years. |
| Missing revenue | 55 | No match to Annual Accounts data (missing Belgian VAT linkage) |
| Missing NACE code | 53 | Almost entirely overlaps with missing revenue — same underlying cause |
| Missing PPI deflator | ~9 | NACE code not covered by Statbel or Eurostat PPI |

After filtering, the sample covers **88.4% of total Belgian ETS emissions** in 2005. The clean sample ranges from 162-194 firms per year, depending on year.

### Panel attrition in fixed-base approach

The fixed-base approach requires firms to be present in both 2005 and year t. The sample shrinks from 162 (2005) to 100 (2022) — a 38% loss. This is **not** because firms exit the ETS (all 281 are in the EUTL every year) but because Annual Accounts matching varies over time. The year-on-year approach mitigates this by using all available firms in each consecutive pair.

### Zero-emission firms

80 firms have zero verified emissions in 2005 and are excluded from the decomposition (log emission intensity is undefined). Most (78/80) have positive emissions in other years — they are real firms that simply didn't emit in the base year (e.g., installations under construction or temporarily shut down). These firms are captured by the year-on-year approach in years when they do emit.

---

## Interpretation

This decomposition is **purely mechanical** — it attributes emission changes to accounting channels but says nothing about what *caused* those changes. The between-sector reallocation could reflect carbon policy, trade shocks, demand shifts, or technology trends. The within-firm abatement could be driven by carbon pricing, energy prices, regulation, or autonomous technical change. Causal attribution requires the identification strategies in Phase 1 and Phase 2.

The finding that within-sector reallocation is essentially zero is consistent with Colmer, Martin, Muuls & Wagner (2024), who find that French ETS firms reduced emissions without any contraction in economic activity. If firms can abate without losing competitiveness, there is no cost differential to drive within-sector reallocation.

---

*Generated by `analysis/phase0_decomposition.R`, Version A (ETS-only). April 2026.*

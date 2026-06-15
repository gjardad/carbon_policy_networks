# Evidence on the Elasticity of Substitution

This note collects the best-identified empirical estimates of the elasticity of
substitution (EoS) relevant for disciplining **σ** in our model — the elasticity that
governs substitution across intermediate inputs (and, in our build, final demand). It
focuses on papers that estimate a well-identified EoS using quasi-experimental or
instrumental-variable methods, rather than calibration or moment-matching.

**Why this matters for us.** In our CES economy, σ controls the *reallocation* (composition)
channel: how strongly a firm's buyers substitute away from it when a carbon tax raises its
price, including substitution toward untargeted competitors (leakage). The sign and size of
our sufficient statistic, and the entire quantitative counterfactual, scale with σ. The
counterfactual σ-sweep needs a defensible central value and range, and the literature below
says the right number depends sharply on **what level** you substitute at and **over what
horizon**.

---

## TL;DR — the one fact that organizes everything

The same object — substitution across intermediate inputs — looks like **strong
complementarity (σ ≈ 0.1)** at short horizons / under transitory shocks, and like **gross
substitutability (σ ≈ 2–2.5)** at long horizons under permanent shocks. The reconciling
mechanism is **convex adjustment costs**: firms can't cheaply re-tool their input mix in the
short run, so measured short-run elasticities understate the long-run technological
substitutability. A permanent carbon price is a *long-run, permanent* relative-price change —
so the conceptually right anchor for our counterfactual is the **long-run** estimate, not the
short-run complementarity that dominates the older macro-networks literature.

**Important refinement for our leakage margin.** The complementarity result holds *even at the
within-product, across-supplier level* in the short run: Fujiy, Ghose & Khanna (2025) estimate
σ = 0.55 across suppliers of the *same HS product*. So switching a targeted supplier for an
untargeted competitor is **hard in the short run**, not easy — the within-sector elasticity is
*not* mechanically higher than the across-category one, contrary to the usual presumption. The
long-run within-supplier elasticity is, as far as I can find, **not yet cleanly estimated by a
quasi-experiment** — an open gap that happens to sit right under our counterfactual.

**Takeaway for our two-nest CES design.** Existing work *as a modeling convention* assumes
within-sector elasticity > across-sector, and we can cite that. But the ordering is **an
assumption, not a measured fact**: the high within-sector numbers (≈10) come from a *different
object* — oligopoly demand elasticities tuned to produce markups (Atkeson–Burstein,
Edmond–Midrigan–Xu), not input substitution — while the one paper that measures both *domestic
input* nests the same way finds them roughly **equal and both low** (FGK: within-product 0.55,
across-industry 0.69). So we keep the two-nest structure, **decline their numbers**, and treat the
within-sector elasticity as the **swept** parameter. This is not pedantry: the ordering is
load-bearing for our **leakage** channel (substitution from a taxed supplier to an untaxed
same-sector competitor runs entirely on the within-sector elasticity), so it is exactly the
parameter our result turns on and must not be smuggled in. See §8.1–§8.2.

| Paper | Object / level | Horizon | Context & identification | Estimate |
|---|---|---|---|---|
| **Atalay (2017)** | Across intermediate inputs from different **industries** (nested CES) | Short-run (annual) | US, 1997–2013 BEA IO tables; IV on relative input prices | **εM < 0.2** (complements) |
| **Peter & Ruane (2025)** | Across **8 broad material categories**, plant level (CES) | **Long-run (7 yr)** | India 1991 trade liberalization; plant-level shift-share IV from tariff cuts | **σ = 2.5**, CI [1.5, 3.4] (substitutes); **< 1** short-run |
| **Fujiy, Ghose & Khanna (2025)** | Across **suppliers of the same product** (HS-4/6/8), firm-to-firm — *our exact leakage margin* | Short-run (months) | India Covid-19 lockdown zones + transport-cost variation; firm-to-firm tax data w/ unit values | **σ = 0.55**, range [0.50, 0.66] (complements); 0.69 across industries |
| **Adão–Carrillo–Costinot–Donaldson–Pomeranz (2022)** | Across domestic **material suppliers**, firm level (nested CES) | Short-run | Ecuador VAT firm-to-firm 2009–2015; shift-share IV from global product shocks | **σ_input ≈ 1** (cannot reject Cobb-Douglas) |
| **Miranda-Pinto (2021)** | Across **industries** + labor-vs-intermediate bundle, by sector group | Short-run | US BEA IO 1997–2014; Atalay-style military-spending IV | manuf **≈1**; services **≈6**; other-goods **ε_M≈0** |
| **Boehm, Flaaen & Pandalai-Nayar (2019)** | Imported vs domestic inputs, firm level | Short-run (months) | 2011 Tōhoku earthquake natural experiment; Japanese MNC affiliates in US | **≈ 0** (near-Leontief) |
| **Barrot & Sauvagnat (2016)** | Across suppliers of a *specific* input (no σ estimate) | Short-run | US Compustat links; natural-disaster supplier shocks | implies **low** (specific inputs) |
| **Carvalho, Nirei, Saito & Tahbaz-Salehi (2021)** | Across suppliers + primary-vs-intermediate, firm-to-firm (nested CES) | Short-run | Japan; 2011 Tōhoku earthquake; TSR network data | inputs **≳1**; primary-vs-interm **<1** |
| **Boehm, Levchenko & Pandalai-Nayar (2023)** | Trade/Armington, across foreign **varieties** of a product | Short *and* long | 39 countries 1995–2018; MFN tariff changes + local projections | SR **≈ 0.76**, LR **≈ 1.75–2.25** |

---

## 0. Source-verified check of the three anchors cited in the paper

The quantitative section anchors the σ-sweep on three numbers — Atalay **0.1** (low), Baqaee–Farhi
**0.5** (middle), Peter–Ruane **≈4.7** (high). Each was re-checked **directly against the source PDF**
(June 2026). Two of the three are wrong as cited, and all three are defined at *different* levels and
*different* horizons — so the grid is not three comparable measurements of one object.

| Anchor (paper) | Paper cites | **Source-verified value** | Level / object | Horizon | Verdict |
|---|---|---|---|---|---|
| **Atalay (2017)**, *AEJ:Macro* | σ = 0.1 (low) | **ε_M = 0.1** is his *adopted benchmark*; IV point estimates are ≈ −0.1, 90% CI right endpoint ≈ 0.2 (p.14, 17) | Across intermediate inputs from **different upstream industries** — BEA industries (~30); **across-sector**. (His VA-vs-intermediate ε_Q = 1; consumption ε_D = 1.) | **Short-run** — annual variation, 1997–2013 BEA Annual IO tables | ✅ **Correct.** 0.1 is the right number, across-sector, short-run. |
| **Baqaee–Farhi (2019)**, *Econometrica* | σ = 0.5, "across intermediates" | Benchmark triple **(σ_C, θ, ε) = (0.9, 0.5, 0.001)**. The **across-intermediate** elasticity is **ε = 0.001** (near-Leontief, "matches Atalay"). The **0.5 is θ**, the **value-added-vs-intermediate-bundle** nest — a *different object*. | ε defined across **88 KLEMS industries** (across-sector); θ is the VA-vs-materials margin | **Short/medium** — annual & quadrennial (~4-yr business cycle) | ❌ **Misattributed.** Their across-intermediate value is ~0, essentially Atalay; 0.5 is a different nest. 0.5 is *not* an independent BF anchor for our σ. |
| **Peter–Ruane (2025)**, NBER w31233 | σ ≈ 4.7 (high) | Headline **σ = 2.5**, SE 0.48, **95% CI [1.5, 3.4]** (current Oct-2025 version). Older draft had **4.3** [2.4, 6.1]. **4.7 appears nowhere** in either version. | Across **8 broad material categories**, **plant level**, CES — broader than individual suppliers, narrower than across-sector | **Long-run (~7 yr)**, permanent shock (India 1991 liberalization). Short-run / transitory variants all **< 1**. | ❌ **Wrong number.** Replace 4.7 → **2.5 (CI 1.5–3.4)**. |

**Resolution adopted (June 2026).** The σ-grid changes from `{0.1, 0.5, 1, 4.7}` to **`{0.1, 0.55, 1, 3.4}`**,
mapped explicitly onto the two nests:
1. **Low anchor 0.1 — Atalay (across-sector, short-run) → σ_B.** Unchanged value; horizon corrected to
   *short-run* (the paper had mislabeled it long-run).
2. **Middle anchor 0.5 → 0.55, re-cited to Fujiy–Ghose–Khanna.** BF's "0.5" was the value-added-vs-intermediate
   nest, not across-intermediate substitution (which they set ≈0). 0.55 is FGK's estimate for substitution
   *across suppliers of the same product* — the exact σ_W object, short-run. (BF dropped as an anchor; still
   cited in the model section.)
3. **High anchor 4.7 → 3.4.** The top of Peter–Ruane's 95% CI (headline 2.5, CI [1.5, 3.4]) for long-run
   plant-level material substitution, which coincides with Huneeus's (2018) estimated across-supplier
   elasticity — the elastic, long-run reading of σ_W.
4. **Level/horizon mismatch (honest gap, retained).** The anchors still measure *different objects*: 0.1 is
   across-**sector** short-run; 0.55 is across-**supplier** short-run; 3.4 is the long-run upper reading. The
   exact object the leakage channel turns on — long-run across-supplier-within-sector — has no clean direct
   estimate, which is why σ_W is swept (0.55 → 3.4) rather than pinned.

**Downstream:** the grid change requires re-running the counterfactuals on RMD (`cf_common.R` updated to
`{0.1, 0.55, 1, 3.4}`); results tables (`cf_ets_decomp`, `cf_schemes_compare`) and any inline numbers at the
old σ=4.7 / σ=0.5 rows are stale until re-run.

---

## 1. Atalay (2017), "How Important Are Sectoral Shocks?" — *AEJ: Macroeconomics*

- **What level of substitution.** The elasticity of substitution (he calls it **εM**) *among
  the intermediate inputs produced by different upstream industries* — i.e., how easily a
  downstream industry trades off, say, steel against plastics. Sectors are **BEA industries**
  (industry-level, not firm- or supplier-level). He also estimates the elasticity between the
  value-added bundle and the intermediate bundle, and the consumption-side elasticity. It is a
  **nested-CES** production structure (extension of Foerster, Sarte & Watson 2011), so εM is a
  genuine CES parameter.
- **Short- vs long-run.** Estimated from **annual** comovement of expenditure shares and prices;
  the subsequent literature (incl. Peter & Ruane) reads these as **short-run** elasticities.
- **Context & identification.** US, **1997–2013 BEA Annual Input-Output Tables** (industry output
  series back to 1960 for the GE step). Identification is **IV/structural**, not a single natural
  experiment: in the data, an industry's expenditure share on a particular input is both volatile
  and *positively* correlated with that input's relative price. Under Cobb-Douglas (σ=1) shares
  would be constant; under substitutes (σ>1) shares would fall when the price rises. The observed
  pattern — shares rising with price — only fits **complementarity (σ<1)**. He instruments
  relative prices to break simultaneity. (This is the least "quasi-experimental" of the set —
  well-identified via IV inside a structural model, but not a clean natural experiment.)
- **Findings.** Point estimates of **εM are consistently below 0.2 and always significantly below
  1**: different intermediate inputs are **strong complements**. Plugging these into the GE model,
  **≈ 83%** of the variance of aggregate output growth comes from industry-specific shocks; impose
  unitary elasticities instead and that share collapses to **21%**. The low EoS is what makes
  sectoral shocks matter — complementarity prevents the network from "averaging away" idiosyncratic
  disturbances.

**For us:** Atalay is the canonical source for the "intermediates are complements, σ ≈ 0.1"
calibration used throughout the production-network macro literature (Baqaee–Farhi and others).
It is the right anchor for a **short-run** carbon-price response, and a conservative (dampening)
benchmark, but likely understates substitutability for a *permanent* policy.

---

## 2. Peter & Ruane (2025), "Intermediate Input Substitutability"

This is the most directly relevant paper, because it estimates exactly our σ, isolates the
**long-run** value with a clean natural experiment, and explains *why* it differs from Atalay.

- **What level of substitution.** Elasticity of substitution **between 8 broad categories of
  material inputs** (Metals; Rubber & Plastics; Wood & Paper Products; etc.) **at the plant
  level**, in a standard **CES** production function. They additionally estimate (i) KLEMS-style
  elasticities between energy/materials/services, (ii) between a capital-labor (value-added)
  bundle and the intermediate bundle, (iii) materials-vs-labor and materials-vs-energy, and
  (iv) an industry-level elasticity. The headline is the **across-material-category** plant-level
  elasticity.
- **Short- vs long-run — the central contribution.** They estimate a **long-run (7-year)**
  elasticity by exploiting a *permanent* relative-price change. They show substitutability
  **rises with the horizon and with the permanence of the shock**:
  - **Long run, permanent shock: σ = 2.5** (95% CI **[1.5, 3.4]**) — significantly **above**
    Cobb-Douglas → materials are **gross substitutes**.
  - **Short horizon (early-liberalization subset, same permanent shock): σ < 1**.
  - **Transitory shocks (global commodity-price fluctuations, 1-year horizon): σ < 1**.
  - The KLEMS, value-added-vs-intermediate elasticities are also estimated **below 1**;
    materials-vs-labor and materials-vs-energy are **≈ Cobb-Douglas**.
  - Reconciliation: **sizeable convex adjustment costs** — firms re-tool slowly, so short-run
    estimates (Atalay 2017; Barrot & Sauvagnat 2016; Boehm et al. 2019; Carvalho et al. 2020)
    understate the technological long-run EoS.
- **Context & identification.** **India's 1991 trade liberalization** as a natural policy
  experiment. Data: plant-level **Indian Annual Survey of Industries (ASI)**, **1989 and 1996**.
  The reform was large, unexpected, and quasi-random across products (initial tariffs varied
  widely but converged to ≈ 30%), with little scope for lobbying (Topalova 2010). They build a
  **plant-level shift-share (Bartik) instrument**: plants used detailed sub-inputs with varying
  intensities, so each of the 8 categories saw different tariff-driven price changes. Identifying
  assumption is **exogeneity of the shifts** (the quasi-random tariff cuts); **balance tests**
  confirm tariff changes are uncorrelated with industry characteristics or pre-trends. OLS is
  below the IV estimate, as expected from simultaneity/attenuation. Survives robustness on timing,
  trends, concurrent reforms (delicensing, FDI), and quality upgrading.
- **Aggregate findings.** Using the **long-run** elasticities in a multi-sector model of India:
  misallocation of intermediate inputs reduces aggregate output by **28% — 3.5× larger** than
  under a standard complementary-input (σ<1) calibration; sectoral subsidies cause losses **up to
  5× as high**, especially when targeted at intermediate inputs. (Intuition: higher σ means input
  *wedges/distortions* — exactly what a targeted carbon tax is — distort the input mix far more,
  so the composition channel is much more powerful than the complements view implies.)

**For us:** this is the load-bearing reference. (a) The carbon tax is a *permanent* relative-price
wedge on targeted firms, so the **long-run σ ≈ 2.5** is the conceptually correct central value, with
**σ < 1** as a short-run / transitory bound — this defines the natural bracket for the σ-sweep. (b)
Their headline result is literally that **input distortions are far more consequential when σ > 1** —
which is precisely our reallocation/leakage channel. (c) A level caveat: their σ is
*across broad categories*, whereas our leakage runs *across individual suppliers within a category*.
The natural presumption is that the supplier-switching margin is more elastic — but Fujiy–Ghose–Khanna
(§3) show that, **in the short run, it is not** (σ ≈ 0.55 across same-product suppliers). So we cannot
simply mark 2.5 up for the firm-to-firm margin; the long-run within-supplier elasticity is unknown.

---

## 3. Fujiy, Ghose & Khanna (2025), "Production Networks and Firm-level Elasticities of Substitution"

**This is the paper that directly answers "is there evidence on substitution *across suppliers
within a sector*?" — and it is the single most relevant estimate for our leakage channel.**

- **What level of substitution.** The elasticity of substitution **across suppliers of the *same
  product*** — i.e., a buyer reallocating a given HS product's purchases from one firm to another
  firm selling the same product. This is the firm-to-firm, *within-product / within-sector*
  margin, estimated at the **HS-4, HS-6, and HS-8** product level (results stable across all
  three). They also report a more aggregate **across-industry** elasticity for comparison. "One
  of the first causal estimates of elasticities of substitution across suppliers within the same
  product."
- **Short- vs long-run.** **Short-run** (response over the months of the Covid shock). They
  stress σ is *not a fixed primitive* but an empirical average that rises with the time horizon
  and the level of aggregation (citing Houthakker 1955; Bachmann et al. 2024; Lagos 2006).
- **Context & identification.** **India, 2020 Covid-19 lockdowns**, using **real-time
  administrative firm-to-firm transaction tax data for an Indian state**, with **unit values and
  HS-product classifications** at the buyer–supplier level. Identification exploits India's
  **lockdown mosaic**: ~600 districts were sorted into **Green / Orange / Red** zones (mild /
  medium / severe restrictions), suddenly and unexpectedly, so the supply shock to a supplier's
  district is plausibly exogenous; they add **transport-cost variation** from restrictions in
  districts goods must transit. Saturated with high-dimensional fixed effects (buyer×product×time,
  seller×product×time) to strip out demand shocks, and they construct **counterfactual prices**
  from non-realized transactions to handle the extensive margin (firms that *stop* trading).
- **Findings.** Suppliers of the same product are **highly complementary: σ = 0.55**, robust
  across specifications to a **[0.50, 0.66]** range; similar at HS-6 and HS-8. The across-industry
  elasticity is **0.69** — also complementary, consistent with Atalay (2017) and Boehm et al.
  (2019). Mechanisms behind the low value: **input/contract specificity** (more differentiated
  inputs → more complementary), **inventories**, **institutions**, and **time horizon**. These
  firm-level complementarities **amplify** propagation of negative shocks; in counterfactuals,
  keeping connected firms operating mitigates output declines non-linearly.

**For us:** this is the closest object to our σ — substitution toward an untargeted competitor
*is* the within-product, across-supplier margin. The short-run answer is **complementarity
(0.55)**, so the reallocation/leakage channel is **muted on impact** and the amplification
(complementarity) force dominates in the short run. Two cautions cut in opposite directions:
(i) it is a *short-run, crisis-period* estimate, and the authors themselves argue σ rises with
horizon — so a *permanent* carbon price plausibly sits well above 0.55; (ii) but it directly
falsifies the lazy assumption that within-sector supplier-switching is "easy" (high σ). Bottom
line: **0.55 is the right short-run/impact value for the leakage margin; the long-run value for
this exact object is unmeasured**, and we should treat that as a modeling assumption to sweep,
not a number we can cite.

---

## 4. Boehm, Flaaen & Pandalai-Nayar (2019), "Input Linkages and the Transmission of Shocks" — *REStat*

- **What level of substitution.** Elasticity between **imported and domestically-sourced inputs**
  at the **firm level**, within an Armington-style firm production function.
- **Short- vs long-run.** Squarely **short-run** (the months following a disaster).
- **Context & identification.** A clean **natural experiment**: the **2011 Tōhoku earthquake and
  tsunami**. They use **US-based affiliates of Japanese multinationals**, whose Japanese parents'
  input supply was disrupted exogenously by the disaster. This isolates a supply shock to one
  input source holding demand fixed.
- **Findings.** Output of affected affiliates falls **roughly one-for-one** with the decline in
  imported inputs from Japan — i.e., the short-run EoS between imported and domestic inputs is
  **close to zero (near-Leontief)**. Firms could not quickly replace disrupted inputs with
  domestic substitutes. Strong micro evidence for **short-run complementarity**.

**For us:** independent natural-experiment confirmation of the *short-run* end of the range
(σ → 0). Consistent with Atalay and with Peter & Ruane's transitory-shock estimates; reinforces
that any short-horizon counterfactual should use low σ, while a permanent policy should not.

---

## 5. Boehm, Levchenko & Pandalai-Nayar (2023), "The Long and Short (Run) of Trade Elasticities" — *AER*

Not an intermediate-input EoS per se, but the cleanest quasi-experimental statement of the
**horizon dependence** that we care about, on the trade/Armington margin.

- **What level of substitution.** The **trade (Armington) elasticity** — substitution across
  **foreign source-country varieties** of a given product. (Related but distinct from our
  cross-supplier σ; same mathematical role.)
- **Short- vs long-run.** Estimates the **full impulse response**, separating short- and long-run.
- **Context & identification.** Product-level tariffs and trade flows for **~39 countries,
  1995–2018**. Identification exploits **Most-Favored-Nation (MFN) tariff changes**, which create
  plausibly exogenous tariff variation for trading partners, estimated via **local projections**
  against a control group.
- **Findings.** The elasticity is **≈ 0.76 in the first year** and rises to **≈ 1.75–2.25 in the
  long run** (several years out) — **the long-run elasticity is more than double the short-run**.
  Same qualitative pattern as Peter & Ruane: substitution builds up over time as adjustment costs
  are paid down.

**For us:** a second, independent, quasi-experimental demonstration that the relevant elasticity
roughly **doubles** from short to long run, landing near **≈ 2** in the long run — strikingly
close to Peter & Ruane's 2.5. Two different margins (domestic materials vs. import varieties), two
different natural experiments (Indian liberalization vs. global tariff changes), same long-run
answer of ~2.

---

## 6. The full census of domestic-margin estimates

Peter & Ruane's own literature footnote enumerates the **short-run** domestic estimates and
contrasts them with their **long-run** number. Organized that way, the field is:

> *"Short-run elasticities between material inputs or material suppliers have been estimated at the
> firm-level (Adão, Carrillo, Costinot, Donaldson & Pomeranz, 2022; Barrot & Sauvagnat, 2016;
> Carvalho et al., 2020; Fujiy, Ghose & Khanna, 2023) and industry-level (Atalay, 2017;
> Miranda-Pinto, 2021)."*

### 6.1 Short-run, firm-level

- **Adão, Carrillo, Costinot, Donaldson & Pomeranz (2022), "Imports, Exports, and Earnings
  Inequality" (*QJE* 137(3))** — NBER w28263. **Object:** σ between domestic **material suppliers**
  within a buying firm, inside a nested CES. **Data/ID:** Ecuadorian administrative **VAT
  firm-to-firm** data, 2009–2015; **shift-share IVs** built from product-level (HS-6) global
  export-value and import-unit-value shocks (rest-of-world), interacted with firms' initial
  exposure shares (small-economy exclusion restriction). **Estimate:** they impose Cobb-Douglas and
  *test* it — cannot reject **σ_input ≈ 1** (test coefficient 0.06, s.e. 0.26, on ~1.48M
  transactions). (For reference their separately-estimated factor elasticity is 2.09 and
  firm-products-in-consumption 1.96 — both > 1; the *supplier* one is the unit-elasticity result
  P&R cite.) **Short-run, ≈ Cobb-Douglas.**
- **Fujiy, Ghose & Khanna (2023/25)** — the cleanest within-product across-supplier estimate,
  **σ = 0.55**; see **§3** for the full treatment.
- **Barrot & Sauvagnat (2016), "Input Specificity and the Propagation of Idiosyncratic Shocks"
  (*QJE* 131(3))** — **Not a structural σ estimate**: a firm-level *reduced-form propagation* study
  on US Compustat supplier–customer links, using **major natural disasters** as exogenous supplier
  shocks. A customer's sales growth falls **~2 pp** when a supplier is hit; equity falls **~1%**,
  and the effect is **~2× larger** when the disrupted input is *specific* (Rauch-differentiated /
  R&D- / patent-intensive). Aggregate multiplier ≈ **$1 supplier loss → $3.6 customer loss**. Low
  substitutability is the *inference*, not a measured parameter. **Short-run.**
- **Carvalho, Nirei, Saito & Tahbaz-Salehi (2021), "Supply Chain Disruptions" (*QJE* 136(2))** —
  Japan **2011 Tōhoku earthquake**, full firm-to-firm network (TSR data), nested CES. Elasticities
  are **structurally inferred** (not a clean reduced-form σ): intermediate inputs are **weak gross
  substitutes (σ ≳ 1)**, but the **primary-vs-intermediate bundle is complementary (σ < 1)** — that
  complementarity drives amplification. Propagation: **−3.6 pp** (firms with hit suppliers), **−2.9
  pp** (hit customers); aggregate **−0.47 pp GDP** growth, ~4× a naive size-based estimate.
  **Short-run.**

### 6.2 Short-run, industry-level

- **Atalay (2017)** — across-industry **εM < 0.2** (strong complements); see **§1**.
- **Miranda-Pinto (2021), "Production Network Structure, Service Share, and Aggregate Volatility"
  (*Rev. Econ. Dynamics* 39)** — US **BEA IO, 1997–2014**, Atalay-style estimation with **military
  spending as the IV** for relative prices, run *separately by sector group*. **Key result is
  heterogeneity:** manufacturing is **≈ Cobb-Douglas (ε ≈ 1)**; **services are strong substitutes
  (labor-vs-intermediate ε_Q ≈ 6, ε_M > 1)**; other-goods sectors have **complementary
  intermediates (ε_M ≈ 0)**. Service-input substitutability is the paper's whole point (it dampens
  aggregate volatility in service-heavy economies). **Short-run.**

### 6.3 Long-run, domestic — the (nearly) empty cell

- **Peter & Ruane (2025)** is essentially the *only* well-identified **long-run** domestic estimate:
  **σ = 2.5** across material categories (see **§2**). The narrowest object we need — *long-run,
  across individual suppliers within a sector* — has **no** clean quasi-experimental estimate.

### 6.4 Adjacent literatures (different object — for completeness)

- **Boehm & Oberfield (2020, *QJE*), "Misallocation in the Market for Inputs."** Estimates an
  across-supplier σ structurally from **cross-Indian-state contract-enforcement** variation ×
  input relationship-specificity (Rajan–Zingales-style); inputs come out **complementary**.
  Quasi-experimental in flavor but the elasticity is a means to a misallocation result.
- **Structural / calibrated firm-to-firm σ (not natural experiments):** Huneeus (2018, Chile);
  Lim (2018, US); Demir, Fieler, Xu & Yang (2024, *AER*, Turkey — **calibrates σ = 5** from
  Broda–Weinstein); Dhyne–Kikkawa–Mogstad–Tintelnot (2021, in our `articles/`, calibrated).
- **Demand-side, across firms within a sector** (relevant to our *final-demand* σ, not input-input):
  Hottman, Redding & Weinstein (2016, *QJE*) — substitutable, markup elasticities ≈ 4–7;
  Broda & Weinstein (2006, *QJE*) — across import varieties, median ≈ 3 (trade margin).
- **Factor substitution** (not intermediates): Oberfield & Raval (2021, *Econometrica*) —
  capital–labor σ ≈ 0.5 plant-level, ≈ 0.7 aggregate.

**Bottom line.** No, it isn't only three — but the longer list does not move the conclusion. The
*short-run* domestic estimates (Adão ≈1, Barrot–Sauvagnat low, Carvalho ≳1/<1, FGK 0.55, Atalay
<0.2, Miranda-Pinto ≈1 goods / ≈6 services) cluster **at or below Cobb-Douglas, with services the
notable exception**. The *long-run* domestic literature is **a single paper (Peter & Ruane, 2.5)**.
Everything else is structural/calibrated, demand-side, factor, or the trade margin. The cell we
actually need — long-run, across-supplier-within-sector — is still empty.

---

## 7. Recommendation for our σ-sweep

1. **Central value: σ ≈ 2 – 2.5.** The carbon tax is a *permanent* relative-price wedge, so the
   **long-run, permanent-shock** elasticity is the right object. Two independent quasi-experiments
   (Peter & Ruane 2025: 2.5 across materials; Boehm–Levchenko–Pandalai-Nayar 2023: ~2 across import
   varieties) converge there. This also makes the reallocation/leakage channel quantitatively
   *live* rather than muted.
2. **Lower bound: σ ≈ 0.5 – 0.55** (Fujiy–Ghose–Khanna 2025 for the *exact* within-product
   across-supplier margin; corroborated by Atalay 2017, Boehm–Flaaen–Pandalai-Nayar 2019, and
   Peter & Ruane's transitory-shock estimates at the across-category level). This is the
   **short-run / impact** response: on impact, the leakage channel is muted and complementarity
   amplification dominates. Report it precisely — it is the value most of the macro-networks
   literature defaults to.
3. **Sweep the whole range [≈0.5, ≈4]** and report how the sufficient statistic and aggregate
   counterfactual move across it; flag σ = 1 (Cobb-Douglas) as the knife-edge where the composition
   channel's amplification flips sign. Present **short-run (σ≈0.55) and long-run (σ≈2.5)** as two
   labelled scenarios rather than a single point.
4. **Level/horizon caveat to state explicitly — this is the honest gap.** The two clean numbers
   bracket *different* objects: 2.5 is *long-run but across broad categories* (Peter & Ruane);
   0.55 is *across same-product suppliers but short-run* (Fujiy–Ghose–Khanna). The object we
   actually need — **long-run substitution across individual suppliers within a sector** — has
   **no clean quasi-experimental estimate yet**. So σ at our margin is a *modeling assumption we
   sweep*, disciplined at the short-run end by 0.55 and at the long-run end by the category-level
   2–2.5; we should not claim either number *is* our σ. (This gap is itself worth a sentence in the
   quantitative section, and arguably a contribution hook.)
5. **Sectoral heterogeneity (Miranda-Pinto 2021).** σ is not one number across the network:
   **services substitute much more readily than goods** (ε ≈ 6 vs ≈ 1), while some goods sectors
   are near-Leontief. For Belgium — a service-heavy economy where ETS targeting hits
   energy/materials but the network is thick with services — a single σ may misstate the
   reallocation channel. If the σ-sweep results are sensitive, consider a *sector-specific* σ
   (high for services, low for energy-intensive goods) as a robustness cut.

---

## 8. Modeling note: the two-nest CES (across-sector vs. within-sector)

A natural design for our model is a **two-nest CES**: a low elasticity **across sectors** and a
higher elasticity **across suppliers within a sector** (the textbook "Armington-at-the-firm-level"
structure). Our leakage channel — substitution from a targeted supplier toward an *untargeted
competitor in the same sector* — runs entirely through the **within-sector** nest, so this
parameter is where the action is. The catch: **the direct micro evidence does not support
"within > across" in the short run** — if anything it points the other way.

**The one clean within-vs-across comparison.** Fujiy–Ghose–Khanna estimate *both* nests with the
*same* method on the *same* data:

| Nest | FGK estimate |
|---|---|
| Across **suppliers within the same HS product** (our within-sector nest) | **0.55** |
| Across **industries** (our across-sector nest) | **0.69** |

Within-sector is **not higher** — it is slightly *lower*, and both are below 1 ("inputs are highly
complementary *even* at this granular supplier level"). Adão et al. corroborate the within-supplier
nest at **≈ 1** (Cobb-Douglas). So the empirically-disciplined short-run picture is **both nests
low, within ≈ across**.

**Where "within > across" actually comes from.** Not from domestic firm-to-firm estimation, but
from the **trade/Armington analogy**: substitution across *import varieties* of the same good is
high (Broda–Weinstein ≈ 3; Boehm–Levchenko–Pandalai-Nayar long-run ≈ 2), and one presumes domestic
firms selling "the same product" are like source-country varieties. FGK's contribution is to show
that analogy **fails for domestic links** — relationship-specific investment, customization, and
switching costs (Barrot–Sauvagnat) make same-product domestic suppliers complementary in the short
run. Foreign varieties are an abstraction; real supplier relationships are sticky.

**Honest state of the two nests:**

| | Across sectors | Within sector, across suppliers |
|---|---|---|
| Short-run, domestic | ~0.2 goods (Atalay, MP); ~6 services (MP) | **0.55 (FGK), ~1 (Adão)** |
| Long-run, domestic | ~2.5 (Peter & Ruane) | **unknown — no estimate** |
| Trade / foreign varieties | — | ~3 (BW), ~2 long-run (BLP) |

Two takeaways: (i) "within > across" holds only if we lean on the trade analogy and the long run,
neither of which is a domestic firm-to-firm estimate; (ii) the across-sector nest is not uniformly
low — services ≈ 6 (Miranda-Pinto), so "across-sector low" is a *goods* statement.

**Recommendation for the model.** Keep the two-nest structure (it is standard and clean), but do
**not hardwire within > across as a calibrated fact**:

1. **Anchor the across-sector nest low for goods** (Atalay / Miranda-Pinto ≈ 0.2), flagging the
   services exception (≈ 6) as a robustness cut for our service-heavy Belgian network.
2. **Sweep the within-sector nest** over the full defensible range — **0.55 (FGK, short-run
   domestic) up to ~3–5 (trade-variety / long-run)** — and report how the sufficient statistic and
   the leakage result move across it. This is the headline sensitivity, because it *is* the leakage
   elasticity.
3. **Label two scenarios** rather than picking one: a *short-run / sticky* case (within ≈ 0.55,
   leakage muted, complementarity-amplification dominates) and a *long-run / fluid* case (within
   ≈ 2–3, leakage live). Don't bury either.
4. **Don't over-parameterize.** We have one credible within-supplier number (FGK 0.55, short-run)
   and one across-category long-run number (P&R 2.5) — not enough to separately pin *two*
   independent long-run elasticities. A second nest we cannot discipline adds free parameters; only
   keep it if the sufficient statistic genuinely needs the distinction.
5. **The empty cell is a contribution hook.** The parameter the model most needs — the *long-run,
   within-sector, across-supplier* elasticity — is the one the literature has not cleanly
   estimated. Our Belgian B2B panel is the kind of data that could estimate it, and the ETS-induced
   carbon-cost shocks are a candidate source of supplier-specific price variation (a within-sector
   shift-share in the spirit of FGK / Peter & Ruane). Turning this calibration weakness into a
   measurement exercise would be a natural extension.

### 8.1 Precedents for the nesting architecture (inner = within-sector varieties, outer = across-sector)

The "inner across-supplier/variety (high), outer across-sector (low)" structure **is well
precedented — but mostly in the markup/competition and trade lineages, not in the firm-to-firm
production-network lineage closest to our model.**

**Lineage A — markup / competition / trade models that DO use it (inner > outer):**

| Paper | Inner (within-sector) | Outer (across-sector) | Method | Purpose |
|---|---|---|---|---|
| **Atkeson & Burstein (2008, *AER*)** | ρ = **10** | η = **1.01** | calibrated | variable markups / pricing-to-market (oligopoly) |
| **Edmond, Midrigan & Xu (2015, *AER*)** | γ = **10.5** | θ = **1.24** | calibrated | procompetitive gains from trade / markup dispersion |
| **Hottman, Redding & Weinstein (2016, *QJE*)** | σᶠ = **4.3** across firms (σᵘ = 6.9 within firm) | **1** (Cobb-Douglas across groups) | estimated | decomposing firm heterogeneity (barcode demand) |
| **Caliendo & Parro (2015, *ReStud*)** | sectoral trade elast. θʲ, median ≈ **4** (range 0.4–64) | **1** (Cobb-Douglas) | θʲ estimated | trade policy with sectoral I-O linkages |

In all four, inner > outer and the **outer/across-sector tier is at or near Cobb-Douglas (≈1)**.
Caliendo–Parro is the closest to our *input-sourcing* object (inner = across-origin varieties
within a sector; outer = across sectors); the others are demand/competition objects.

**What the Greek letters mean — and why the inner "10" is NOT an input-substitution elasticity.**
In Atkeson–Burstein the inner (within-sector) parameter is **ρ** and the outer (across-sector) is
**η**, with 1 < η < ρ; in Edmond–Midrigan–Xu they are **γ** (inner) and **θ** (outer), with θ < γ.
(Note θ means across-*sector* in EMX but the *trade* elasticity in Caliendo–Parro — the letters are
not standardized.) Crucially, ρ/γ are **demand elasticities across rival firms' differentiated
products within a narrow market**, *not* elasticities between intermediate inputs in a production
function. Each sector has a *finite* number of firms playing **Cournot**, so a firm's *effective*
demand elasticity is a market-share-weighted blend of the two nests:

&nbsp;&nbsp;&nbsp;&nbsp;ε(ω) = 1 / [ ω·(1/η) + (1−ω)·(1/ρ) ],&nbsp;&nbsp; markup = ε/(ε−1)

so a tiny firm (share ω→0) faces the full inner elasticity ρ=10 and charges a markup of just
10/9 ≈ **11%**, while a dominant firm (ω→1) faces an elasticity collapsing toward η≈1 and charges a
huge markup. The inner elasticity is calibrated to **10 precisely to pin small-firm markups near
11% and match the markup/pass-through (pricing-to-market) data** — it is markup machinery. Reading
it as "how easily a producer swaps one input for another" is a category error; that is what makes
the "10" look absurd. **Our model has price = marginal cost and no variable markups, so this entire
apparatus is switched off**, and the inner nest reverts to its plain input-substitution meaning
(the FGK / Atalay / Peter & Ruane object, 0.55–2.5).

**Lineage B — firm-to-firm production-network models (closest to ours) that do NOT use it:**
DKMT (2021), Bernard et al. (2022), Lim (2018), DKKMT (2025) all use a **single flat CES over
supplier varieties**, with no across-sector sub-nest. Where they carry two elasticities, the
**across-supplier sourcing elasticity is set LOW and at or below the demand elasticity** — DKMT
(Belgian B2B) use sourcing ρ = **2** vs final-demand σ = **4** (so sourcing < demand, the opposite
ranking from "supplier varieties are close substitutes"). Huneeus (2018, Chile) is the one paper
that *estimates* a genuine multi-nest CES; he finds elasticities **rise with the level of
aggregation** (across-supplier ≈ 3.4, above the labor/import margins below it).

**Two cautions for our calibration.**
1. **The high inner values (ρ ≈ 10) are oligopoly-markup objects, not transferable to our
   competitive model.** Atkeson–Burstein and Edmond–Midrigan–Xu set the inner elasticity high
   *precisely to generate variable markups via Cournot competition*. Our model has price = marginal
   cost / full pass-through, so those values carry no discipline here — the inner elasticity should
   be set from the **input-substitution estimates** (FGK 0.55 short-run, Peter & Ruane 2.5 long-run),
   an order of magnitude below 10.
2. **The Belgian firm-to-firm precedents set the supplier elasticity low (≈2), not high.** So even
   the structure-agnostic convention in our exact data setting points to a modest across-supplier
   elasticity, reinforcing §8: sweep it, don't hardwire it high.

**Net:** the two-nest architecture is standard and citable (Caliendo–Parro for the input side;
Atkeson–Burstein / Edmond–Midrigan–Xu / Hottman–Redding–Weinstein for the inner>outer ordering),
but the *numerical* inner>>outer gap in those papers is a markup/competition artifact. In a
competitive carbon-pass-through model, discipline both nests from the substitution literature, keep
the across-sector (goods) nest low (~0.2), and treat the inner (within-sector) elasticity as the
swept leakage parameter.

### 8.2 Huneeus (2018), "Production Network Dynamics and the Propagation of Shocks" — the one estimated multi-nest CES

This is the closest published precedent for *estimating* (not calibrating) a multi-nest CES with
distinct across-supplier vs. broader-nest elasticities directly on firm-to-firm data — worth
understanding in full.

- **Question & data.** How do shocks propagate when the firm-to-firm links themselves are costly to
  form and adjust? Chilean tax-authority (SII) data: the **universe of VAT firm-to-firm
  transactions** (~80% of value added; ~18k firms in the cross-section; avg firm ~30 suppliers /
  ~40 buyers) plus customs trade, ~2006–2013 around the Great Recession.
- **Model.** Dynamic GE with an **endogenous, frictional production network**: firms form/destroy
  links subject to a per-period **fixed cost** (Weibull shock) and **stickiness** ν (each period a
  firm re-evaluates only a fraction of its links, making link choice forward-looking). Monopolistic
  competition, *constant* markups (so no AB/EMX markup machinery). Nested CES, deliberately **not**
  imposing equal elasticities across nests:

  | Nest (σ) | Aggregates | Estimate |
  |---|---|---|
  | σ^L | labor vs. intermediate composite | **2.5** |
  | σ^X | domestic vs. foreign intermediates | **2.8** |
  | **σ^P_HH** | **across individual domestic suppliers** | **3.4** |
  | σ^P_HF | across foreign suppliers (imports) | 3.1 |
  | σ^G_H | home final demand across varieties | 3.8 |

- **Estimation/identification.** SMM / indirect inference on 28 parameters; the substitution
  elasticities are identified by **matching shift-share shock-propagation regressions** (how trade
  shocks pass to directly-hit firms and their up/downstream partners), not by production-function
  inversion — a strategy directly transplantable to a Belgian B2B setting.
- **Findings.** (i) **Network propagation is ~91% of the effect** — the direct effect on hit firms,
  holding the network fixed, is only ~9%. (ii) **Link stickiness makes the downturn ~30% deeper**
  than a frictionless-network counterfactual (output depressed ~5 years past the shock). (iii) A
  Hulten/Cobb-Douglas benchmark **underestimates the impact by ~47%**. (iv) Elasticities **rise with
  the level of aggregation**: across-supplier (3.4) > domestic-vs-foreign (2.8) > labor-vs-
  intermediate (2.5).

**Two caveats for us.**
1. **His σ^P is a *flat* nest across *all* domestic suppliers — no within-sector/across-sector
   split.** So his 3.4 is not a "within-sector" number in our sense (it pools suppliers across
   sectors), and his "rises with aggregation" is the labor→import→supplier hierarchy, not a
   within-vs-across-*sector* statement. He's the template for *estimating* nest elasticities on
   firm-to-firm data, but he does not implement our two-nest either.
2. **His across-supplier 3.4 (substitutes) vs. FGK's 0.55 (complements) is the live tension** that
   defines our calibration uncertainty. The gap is method × object × horizon: Huneeus is a
   *structural, model-dependent* number identified by matching propagation regressions (landing in
   the trade range, 3–4); FGK is a *reduced-form causal* short-run estimate across suppliers of
   *literally the same product*. The honest range for the across-supplier elasticity is **0.55 to
   ~3.4 depending on how it is identified** — which is exactly why we sweep it rather than pick.

### 8.3 How to justify inner > outer in the paper (the demand-vs-production split)

The "within-category products are closer substitutes" intuition is really a **demand-side**
statement. Whether the literature backs **inner (within-sector) > outer (across-sector)** depends on
the *object*, and for an *input-sourcing* nest the evidence is split — so the justification should be
framed carefully (and the markup machinery of §8.1 should be left out of it entirely).

**Demand side — clearly supports inner > outer:**
- Broda & Weinstein (2006): substitution across varieties *within* a good ≈ 3 (estimated; not markups).
- Hottman, Redding & Weinstein (2016): within-firm (6.9) > across-firm (4.3) > across-group (1) —
  elasticity *falls* as groupings broaden.
- The Armington / quantitative-trade nesting (Caliendo–Parro lineage): a high within-sector elasticity
  nested in a near-Cobb-Douglas across-sector aggregator is simply the conventional structure.

**Production / input-sourcing side (what our inner nest actually is — a firm reallocating across
*suppliers*) — genuinely split:**
- *Supports inner > outer:* **Huneeus (2018)** is the best on-point cite — the one paper that
  *estimates* a multi-nest CES on firm-to-firm data, finding the across-supplier elasticity (3.4)
  *above* the broader input nests (domestic-vs-foreign 2.8, labor-vs-intermediate 2.5), citing
  Redding & Weinstein (2018) for the pattern that disaggregated varieties are more substitutable.
- *Cuts against it:* the aggregation principle (Houthakker 1955; Bachmann et al. 2024) and the
  cleanest causal estimate, FGK (2023), where within-product (0.55) is *below* across-industry (0.69)
  — elasticity *rises* with aggregation, because specific supplier relationships are sticky
  (Barrot & Sauvagnat).

**Recommended write-up (three moves):**
1. **Justify the two-nest *structure* with no hedging** — standard quantitative-trade / production
   architecture (Caliendo–Parro, Armington). One sentence, uncontroversial.
2. **Justify inner > outer with the intuition, backed by the demand lineage + Huneeus** — name the
   supportive evidence (closer substitutability within narrow categories: Broda–Weinstein,
   Hottman–Redding–Weinstein; and the firm-to-firm estimate Huneeus 2018 with across-supplier above
   broader-input elasticities). A clean, markup-free justification. Do **not** invoke the
   Atkeson–Burstein / Edmond–Midrigan–Xu "10," which is oligopoly-markup machinery (§8.1).
3. **Disclose the FGK tension in one sentence and sweep.** Referees in this field know FGK and
   Houthakker, so asserting inner > outer as settled is exposed. The safe framing: "our baseline sets
   inner > outer, consistent with [demand lineage / Huneeus 2018]; FGK find within-product
   substitution low, so we report results across a range of the inner elasticity." The sweep converts
   a contested assumption into a robustness feature. **Claim the *ordering*, not a *value* — and
   never a high value.**

---

### Sources

- Atalay (2017), *How Important Are Sectoral Shocks?* — [author PDF](https://enghinatalay.github.io/elasticities.pdf) · [AEA](https://www.aeaweb.org/articles?id=10.1257/mac.20160353)
- Peter & Ruane (2025), *Intermediate Input Substitutability* — [author PDF](https://www.cianruane.com/s/IIS_PeterRuane.pdf) · [NBER w31233](https://www.nber.org/papers/w31233)
- Fujiy, Ghose & Khanna (2025), *Production Networks and Firm-level Elasticities of Substitution* — [PDF](https://bcfujiy.github.io/img/papers/CFGK_ElastSubst.pdf) · [CEPR WP027](https://cepr.org/publications/wp027)
- Adão, Carrillo, Costinot, Donaldson & Pomeranz (2022), *Imports, Exports, and Earnings Inequality* (QJE 137(3):1553–1614) — [NBER w28263](https://www.nber.org/papers/w28263)
- Miranda-Pinto (2021), *Production Network Structure, Service Share, and Aggregate Volatility* (Rev. Econ. Dynamics 39:146–173) — [WP PDF](https://economics.uq.edu.au/files/46390/607.pdf)
- Barrot & Sauvagnat (2016), *Input Specificity and the Propagation of Idiosyncratic Shocks in Production Networks* (QJE 131(3):1543–1592)
- Carvalho, Nirei, Saito & Tahbaz-Salehi (2021), *Supply Chain Disruptions: Evidence from the Great East Japan Earthquake* (QJE 136(2):1255–1321) — [PDF](https://vasco-m-carvalho.github.io/pdfs/JapanEQ.pdf)
- Boehm & Oberfield (2020), *Misallocation in the Market for Inputs* (QJE 135(4):2007–2058) — [NBER w24937](https://www.nber.org/papers/w24937)
- *Nesting-architecture precedents (§8.1):* Atkeson & Burstein (2008, AER 98(5)); Edmond, Midrigan & Xu (2015, AER 105(10)) — [PDF](http://www.chrisedmond.net/Edmond%20Midrigan%20Xu%20AER%202015.pdf); Hottman, Redding & Weinstein (2016, QJE 131(3)) — [PDF](https://www.princeton.edu/~reddings/pubpapers/MPF-QJE-2016.pdf); Caliendo & Parro (2015, ReStud 82(1)) — [NBER w18508](https://www.nber.org/papers/w18508)
- *Firm-to-firm flat-CES precedents (§8.1):* Dhyne, Kikkawa, Mogstad & Tintelnot (2021, ReStud, Belgian B2B, ρ=2/σ=4); Bernard, Dhyne, Magerman, Manova & Moxnes (2022, JPE); Lim (2018); Huneeus (2018, Chile, estimated nested CES)
- Boehm, Flaaen & Pandalai-Nayar (2019), *Input Linkages and the Transmission of Shocks* (REStat 101(1):60–75) — [PDF](https://www.aaronflaaen.com/uploads/3/1/2/4/31243277/boehm_flaaen_pandalainayar.pdf)
- Boehm, Levchenko & Pandalai-Nayar (2023), *The Long and Short (Run) of Trade Elasticities* (AER) — [summary](https://causalclaims.trfetzer.com/paper/DP14645.html)
- Oberfield & Raval (2021), *Micro Data and Macro Technology* (Econometrica)
- Barrot & Sauvagnat (2016, QJE); Carvalho, Nirei, Saito & Tahbaz-Salehi (2021, QJE)

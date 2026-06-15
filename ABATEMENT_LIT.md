# Evidence on the Abatement Elasticity

This note collects the quasi-experimental estimates that can **directly justify the value of the
abatement parameter α** we plug into the model. It is the third companion to
`ELASTICITY_SUBSTITUTION.md` (the substitution elasticity σ) and `PASS_THROUGH_LIT.md` (the
pass-through rate ρ). Unlike those notes, the scope here is deliberately narrow: papers whose
**estimand is an elasticity (or semi-elasticity) of emissions / emission intensity with respect to
the carbon price** — a number we can map onto α — *not* papers establishing that carbon pricing
works (mechanism / treatment-effect studies), which are handled separately and demoted in §7.

**Why this matters for us.** Firms hold a costly abatement technology
`e_i = (1−κ_i)^{α_i} \overline{e}_i` with quadratic cost `κ_i²/2`, where `\overline{e}_i` is the
no-policy emission intensity and α_i governs abatement effectiveness. α enters the **abatement
(technique) channel** of the emissions decomposition,
`− Σ_i (τ_i z_i / Z) α_i² \overline{e}_i`, so it must be calibrated. The next section derives
exactly *which* empirical object pins it down — and shows that the answer depends on whether we read
the model locally or globally, which is the substance of the two design questions behind this note.

---

## 1. What the empirical elasticity must discipline: local vs. global

**The first-order result is a derivative at `p_z = 0`.** The firm's abatement FOC (from cost
minimization with cost `κ_i²/2`) is, exactly and globally,

&nbsp;&nbsp;&nbsp;&nbsp;**κ_i = τ_i · p_z · α_i · \overline{e}_i · (1−κ_i)^{α_i−1}**,&nbsp;&nbsp;
equilibrium intensity&nbsp; **e_i(p_z) = (1−κ_i(p_z))^{α_i} \overline{e}_i.**

Differentiating at the no-policy point (`p_z = 0`, `κ_i = 0`) gives the proposition's abatement
semi-elasticity,

&nbsp;&nbsp;&nbsp;&nbsp;**d log e_i / dp_z |_{p_z=0} = − τ_i α_i² \overline{e}_i.**

So yes — this is a **first-order object, the slope of log-intensity in the carbon price evaluated at
`p_z = 0`**. It is exact only for an infinitesimal price, and it is precisely the linearization that
the sufficient-statistic theorem (also a `p_z = 0` expansion) is built on. Read literally it predicts
log-intensity falling *linearly* in the price, `log e_i ≈ log \overline{e}_i − τ_i α_i² \overline{e}_i p_z`.

**The global abatement channel is nonlinear, with diminishing marginal abatement.** For a discrete,
EU-ETS-sized price you must solve the implicit FOC above; the per-euro response is **not constant**.
Two regimes:
- **Small `p_z`:** `κ_i ≈ τ_i p_z α_i \overline{e}_i`, so the semi-elasticity is the constant
  `−τ_i α_i² \overline{e}_i` (the local formula).
- **Large `p_z`:** the FOC forces `1−κ_i ∼ (τ_i p_z α_i \overline{e}_i)^{−1/(α_i−1)}`, hence
  `e_i ∼ p_z^{−α_i/(α_i−1)}` — a **power law**. The semi-elasticity decays like `1/p_z`, but the
  **elasticity converges to a constant:**

  &nbsp;&nbsp;&nbsp;&nbsp;**d log e_i / d log p_z → − α_i / (α_i − 1).**

Because abatement cost is convex and intensity is bounded below at zero, cumulative abatement is
**concave in `p_z`** — the local linear formula **overstates** how much a firm abates at large
prices. In the global emissions decomposition, the abatement channel is then the re-aggregation of
the equilibrium intensities `e_i(p_z)` against `\overline{e}_i`, holding output and the allocation
matrix fixed (the LMDI vs. Laspeyres choice flagged in the solver notes lives here).

**Implication for calibration — and why it picks the target paper.** Two things follow. (i) When
matching a *measured* elasticity at realized EU ETS prices, invert the **global** relation, not the
local slope. (ii) The clean, scale-free object the model offers for an empirical match is the
**large-price intensity elasticity `α/(α−1)`** — and that is exactly the estimand of a
Martinsson-style regression of log emission intensity on the (log) carbon price/cost. The mapping is
direct: an estimated intensity elasticity of **≈ 2 implies α ≈ 2** (`α/(α−1)=2`); the existing
solver value α ≈ 1.73 implies `α/(α−1) ≈ 2.4`. So the parameter-targeting literature we want is,
specifically, **the carbon-price elasticity of firm-level emission intensity.**

---

## 2. TL;DR — it is essentially a one-paper literature

The carbon-price elasticity of **firm-level manufacturing CO₂ emission intensity**, estimated
quasi-experimentally, is — as of now — a **single-paper literature: Martinsson, Sajtos, Strömberg &
Thomann (2024, *RFS*)**, with a long-run estimate **≈ 2** (and, importantly, **≈ 1.3–1.7 for the
largest emitters that carry ~80% of emissions**). Everything else is an *analog* on a different
margin: the **energy-price** elasticity of manufacturing CO₂ (Marin–Vona ≈ −1.15; Dussaux ≈ −0.9;
French firms), an **implied** energy-demand elasticity from a tax notch (Martin–de Preux–Wagner
≈ 1.4; UK), or a **transport-fuel** carbon-tax elasticity (Andersson −1.57; Rivers–Schaufele). The
treatment-effect / mechanism studies (Colmer et al., Dechezleprêtre et al., the entire China-ETS
literature) report a binary "% reduction," **not** an elasticity, and so are demoted to a
magnitude/validation role in §7.

This is the same shape as the σ note's verdict — **the exact cell we need is nearly empty.** The
defensible strategy is therefore: anchor on Martinsson ≈ 2 (⟹ α ≈ 2), use the energy-price CO₂
elasticities (≈ −0.9 to −1.15) as the bracketing robustness set, and state plainly that this is a
one-paper literature — which both makes Martinsson load-bearing and flags the gap as a contribution
hook (our Belgian B2B + EUTL data could estimate it directly).

| Tier | Paper | Estimand (LHS ← RHS) | Setting & ID | Estimate |
|---|---|---|---|---|
| **1 — the object** | **Martinsson, Sajtos, Strömberg & Thomann (2024)** | log **emission intensity** (CO₂/sales) ← log(1−carbon-cost share) | Sweden mfg, ~4,000 firms, 1990–2015; carbon-tax reforms + exemptions | **LR ≈ 2.1** (impact ≈ 1.0); **≈ 1.3–1.7 for biggest emitters** |
| **1b — method parent** | Shapiro & Walker (2018) | log emission intensity ← abatement-cost share (Cobb-Douglas) | US mfg plants 1990–2008; CAA-nonattainment IV in a GE model | pollution elasticity **β ≈ 0.011** (not a carbon-price elasticity) |
| **2 — energy-price analog** | Marin & Vona (2021) | log CO₂ (and log energy) ← log **energy price** | France mfg estabs 1997–2015; shift-share IV | CO₂ **−1.15**; energy **−0.64** |
| 2 | Dussaux (2020) | log CO₂ / energy ← log energy price | ~8,000 France mfg firms 2001–2016; shift-share IV | CO₂ **≈ −0.9**; energy **≈ −0.63** |
| 2 | Martin, de Preux & Wagner (2014) | energy intensity (treatment effect → *implied* elasticity) | UK CCL; plant IV (full vs 20% rate) | energy intensity −18–21%; **implied energy-demand elasticity ≈ 1.4** |
| 2 | von Graevenitz & Rottner (2024); Linn (2008) | electricity / energy intensity ← price | German plants 2009–17; US plants 1967–97 | elec **−0.4 to −0.6**; energy-intensity **−0.17** |
| **3 — transport carbon tax** | Andersson (2019); Rivers & Schaufele (2015) | log fuel demand ← carbon tax | Sweden; British Columbia | **−1.57** (tax) vs −0.51 (price); salience ~3–4× |
| **4 — cross-country** | Rafaty, Dolphin & Pretis (2021) | CO₂ growth ← carbon-price **level** | 39 countries × 5 sectors 1990–2016 | mfg semi-elasticity **−0.16%/$**/tCO₂ |

---

## 3. Martinsson, Sajtos, Strömberg & Thomann (2024), "The Effect of Carbon Pricing on Firm Emissions" — *RFS*

**The one paper whose estimand is exactly our target — the elasticity of firm emission intensity with
respect to the carbon price — and therefore the load-bearing source for the plug-in value of α.**

- **Specification.** Following Shapiro–Walker (2018): regress **log(CO₂/sales)** on **log(1−C)**,
  where C is the marginal emissions-cost share of sales, with firm and year fixed effects (OLS in
  this log form; a separate DiD handles the short run). The coefficient is the **elasticity of
  emission intensity with respect to the marginal carbon cost** — output netted out by construction,
  i.e. a pure technique elasticity.
- **Estimate and SR/LR build-up.** Long-run (3 lags) elasticity **≈ 2.1** (baseline ≈ 2.115,
  p ≈ 0.009; ≈ 2.2 with industry×year FE), building up **≈ 1.0 (1 lag) → 1.6 (2 lags) → 2.1**. The
  long run is roughly **2× the impact** response — abatement accrues as firms re-tool, the same
  horizon logic that governs σ and ρ.
- **The heterogeneity that bites our z-weighted channel.** The elasticity is **noticeably lower for
  the largest emitters** — deciles 9–10, which carry ~80% of emissions, come in around **1.3–1.7**
  across specifications (and financially-constrained/private firms ~0.4, insignificant). Our
  abatement channel weights firms by `τ_i z_i / Z`, so it is precisely the high-z firms whose lower
  elasticity should govern the calibration. A single average α (≈2) **overstates abatement at the
  nodes that matter**; the empirically faithful α is *lower* for high-`\overline{e}_i` firms.
- **Setting & identification.** Sweden, manufacturing, ~4,000 firms, 1990–2015 (85–90% of
  manufacturing CO₂). Variation from the carbon-tax introduction (1991) and reforms (1997, 2008,
  2011, 2015), the EU ETS overlay, and firm-specific exemptions/caps.
- **Caveat to state.** Intensity is normalized by **sales (revenue), not physical output**, so the
  authors caution it can partly reflect pricing/product-mix; read the "≈ 2" as the intensity
  elasticity "in a broad sense." (Numbers verified against the working paper; check the final RFS
  tables before quoting to three digits.)

**For us:** plug in **α ≈ 2** (so the model's large-price intensity elasticity `α/(α−1) ≈ 2`
matches), and run a robustness with **α set lower for the top emission deciles** (intensity
elasticity ≈ 1.3–1.7 ⟹ α ≈ 2.4–4.3 — note the *inverse* mapping means a lower elasticity implies a
*higher* α here, so check the direction against the global solve rather than eyeballing). Either way
this is the single direct anchor; the rest of the note triangulates it.

---

## 4. Shapiro & Walker (2018), "Why Is Pollution from US Manufacturing Declining?" — *AER*

The **methodological parent** — the source of the `log(E/Y)`-on-cost-share specification Martinsson
uses — and a useful cite for the functional form, though **not itself a carbon-price elasticity.**

- **What it estimates.** A structural GE model of US manufacturing pollution (NOₓ, SO₂, PM, CO, VOC —
  not CO₂) in which the key reduced-form parameter is the **pollution elasticity β ≈ 0.011** (the
  Cobb-Douglas pollution share / elasticity of emission intensity w.r.t. the abatement-cost share),
  identified quasi-experimentally with **Clean Air Act nonattainment** as an instrument for
  abatement-cost shares. The headline "implicit pollution tax roughly doubled 1990–2008" is a
  *model-inverted* object, not a regression coefficient.
- **For us:** cite for the specification lineage and for the technique-channel framing (pollution
  fell because *intensity* fell, not output/composition). Do **not** use β ≈ 0.011 as our elasticity —
  it is a different object (cost-share elasticity, air pollutants, structural).

---

## 5. Energy-price analogs (the bracketing robustness set)

The closest *estimated-elasticity* evidence on the right unit (manufacturing firms/plants) but the
wrong price (energy, not carbon). Carbon and energy prices differ, but a carbon tax acts through
energy/fuel cost, so these bracket the carbon-price elasticity from a related margin.

- **Marin & Vona (2021, *EER*).** French manufacturing establishments, shift-share/Bartik IV
  (national source prices × lagged firm fuel mix): elasticity of **CO₂ w.r.t. energy price = −1.15**
  (SE 0.12), of **energy use = −0.64** (SE 0.08). CO₂ more elastic than energy ⇒ fuel-switching adds
  to conservation, so emission *intensity* falls. 1997–2015. The best on-point analog.
- **Dussaux (2020, OECD WP 154).** ~8,000 French manufacturing firms, 2001–2016, shift-share IV:
  **CO₂ elasticity ≈ −0.9, energy ≈ −0.63** — independent corroboration of Marin–Vona on French
  data. (Verify exact table values before quoting.)
- **Martin, de Preux & Wagner (2014, *J. Public Economics*).** UK Climate Change Levy, plant IV
  (full rate vs 20% CCA rate): a **treatment effect** — energy intensity −18 to −21%, electricity
  −22.6%, CO₂ −8.4% (insignificant in IV) — from which they back out an **implied tax-induced
  energy-demand elasticity ≈ 1.4** (electricity ≈ 1.5), which they note sits at the **upper end** of
  the literature (the salience premium). No output/employment/TFP/exit effect. Use the implied ≈ 1.4
  as a tax-induced anchor, not as a directly-estimated elasticity.
- **von Graevenitz & Rottner (2024, ZEW DP 22-038).** German manufacturing plants 2009–2017,
  within-plant FE on regulated network charges: **own-price electricity elasticity −0.4 to −0.6**.
  (This is the real paper behind the sometimes-miscited "Gerster–Riedel" number.)
- **Linn (2008, *Economic Journal*).** US manufacturing plants 1967–1997: energy-price elasticity of
  **energy intensity = −0.17** (LR ≈ −0.27), a long-run vintage/embodied margin — the conservative
  floor.

**For us:** these say the *energy-price* elasticity of manufacturing CO₂ is **≈ −0.9 to −1.15** — a
bit below Martinsson's carbon-price ≈ 2, consistent with the salience premium (a salient carbon tax
elicits a larger response than a generic energy-price move; Andersson/Rivers–Schaufele below). Use
the −0.9 to −1.15 band as the **lower robustness anchor** for the intensity elasticity (⟹ higher α),
and Martinsson ≈ 2 as the central, tax-specific value.

---

## 6. Transport carbon-tax elasticities and cross-country panels (salience and the level response)

- **Andersson (2019, *AEJ: Economic Policy*).** Sweden: **carbon-tax elasticity of gasoline demand
  = −1.57** vs ordinary **price elasticity = −0.51** — the tax response is **~3×** the price
  response. Transport, not manufacturing, but the cleanest statement of **tax salience**: firms and
  households respond more to a permanent, visible carbon tax than to an equivalent price move.
- **Rivers & Schaufele (2015, *JEEM*).** British Columbia gasoline: carbon-tax response **2.9–4.1×**
  the market-price response (published range). Same salience family.
- **Rafaty, Dolphin & Pretis (2021).** The closest pure **carbon-price-level** estimate: a
  cross-country sector panel (39 countries, 5 sectors, 1990–2016) gives a CO₂-growth semi-elasticity
  of **≈ −0.16% per +$1/tCO₂ in manufacturing** (the only significant sector). Tiny per-dollar
  numbers, aggregate margin — a level-response sanity check, not a firm elasticity.
- **Power-sector studies that deliberately do NOT report an elasticity.** Leroutier (2022, *JEEM*)
  and Abrell, Kosch & Rausch (2022, *JEEM*) estimate the UK Carbon Price Support's effect on power
  emissions (−20–26%/yr; −6.2% at ~€18/t respectively), but Abrell et al. **explicitly argue a
  single carbon-price semi-elasticity is not well-defined** for the power sector because the
  price–abatement relation is non-linear (coal/gas switching). A caution against importing a
  power-sector number.

**For us:** the salience evidence (≈3–4×) is the reason to anchor α on the *tax-induced* Martinsson
value rather than the energy-price analogs — a carbon tax is the permanent, salient, high-response
case, the same argument that sends σ to its long-run value and ρ toward 1.

---

## 7. Treatment-effect / mechanism studies — NOT parameter-targeting (validation only)

These are the EU ETS firm-level and China-ETS studies. They report a **binary "% reduction"** for
regulated vs. unregulated firms, **not an elasticity** — you can only back out an implied α by
imposing the price and a functional form, so they are **not** the source for the plug-in value. They
do two other jobs worth keeping: (a) they **validate the model's "abate at no output cost"
assumption** by showing the EU ETS cut is **technique, not scale**; and (b) they give a
**magnitude cross-check** at known EU ETS prices.

- **Colmer, Martin, Muûls & Wagner (2024, *REStud*)** — France: regulated firms cut CO₂ **−14%
  (Ph I) / −16% (Ph II)** with **output and employment flat** and capital rising; emission intensity
  of value-added **−17%** (Ph II). The clean technique result; cited in the model as
  `colmeretal2023`. **An ATT, not an elasticity.**
- **Dechezleprêtre, Nachtigall & Venmans (2023, *JEEM*)** — 4-country installations: **−10%**
  (−6% Ph I, −15% Ph II), revenue +15%, employment flat; technique *inferred* (no output data).
  Cited as `dechezlepretreetal2023`. **An ATT.**
- **Petrick & Wagner (2014)** — Germany: **~−20%** (Ph II), intensity down at flat output. **An ATT.**
- **Klemetsen, Rosendahl & Jakobsen (2020)** — Norway: the **counter-case**, where the Phase II cut
  ran through **scale, not intensity** (emission intensity per man-hour shows no effect). Useful as
  the honest dissent that the technique response is not universal.
- **Bayer & Aklin (2020, *PNAS*)** — aggregate EU-wide (−3.8%; −11.5% in covered sectors); no
  decomposition, no elasticity.
- **China ETS pilots/national (e.g. Cao et al. 2021, *PNAS*: intensity −9.7%)** — uniformly
  **binary ETS-coverage DiD**, not carbon-price elasticities; the price level is context, not the
  estimand.

**For us:** keep Colmer et al. and Dechezleprêtre et al. as the **technique-vs-scale validation** of
the abatement function and as a **magnitude check** (a calibrated α ≈ 2 should reproduce a ~10–17%
Phase II intensity cut at EU ETS prices). But the *value* of α comes from §3, not from these.

---

## 8. Recommendation for the value of α

1. **Settled values (from the model relation `elasticity = α/(α−1)` ⟹ `α = η/(η−1)`).** Mapping each
   Martinsson elasticity η to α:

   | Firm group | Martinsson η | Implied α | Role |
   |---|---|---|---|
   | Non-leakage sectors | ≈ 2.6 | **1.63** | elastic end |
   | **Aggregate LR (baseline)** | **≈ 2.1** | **1.91** | **central** |
   | Leakage-listed sectors | ≈ 1.9 | 2.11 | — |
   | High-emission leakage / top deciles | ≈ 1.5 | 3.00 | dirty-firm |
   | Dirtiest decile (D9–D10) | ≈ 1.3 | 4.33 | inelastic end |

   **Baseline α ≈ 1.9** (η = 2.1); **sweep α ∈ [1.6, 3.0]** (η ∈ [1.5, 2.6]), extendable to 4.3.
   The current solver value α = 1.727 ⟺ η = 2.38 — slightly more elastic than central, but inside the
   non-leakage range, so defensible; nudge to 1.9 to hit 2.1 exactly. Martinsson is the load-bearing
   (and only direct) citation for these.
2. **Calibrate the global relation, not the local slope.** Because `−τ α² \overline{e}` is only the
   `p_z = 0` derivative and abatement is concave in `p_z`, invert the global FOC
   `κ_i = τ_i p_z α_i \overline{e}_i (1−κ_i)^{α_i−1}` when matching a measured elasticity at realized
   EU ETS prices; the constant-elasticity asymptote `α/(α−1)` is the clean target.
3. **Grade α across the emission distribution (preferred, since the channel is z-weighted).**
   Martinsson finds the elasticity is **lower (≈ 1.3–1.7) for the largest emitters** that carry ~80%
   of emissions — exactly the firms our channel `−Σ_i (τ_i z_i/Z) α_i² \overline{e}_i` loads on. So
   grade α *up* with emission rank: low/mid emitters α ≈ 1.7 (η ≈ 2.5), top deciles α ≈ 3.0
   (η ≈ 1.5). A uniform α ≈ 1.9 overstates aggregate abatement; the graded scheme dampens it exactly
   where emissions concentrate. **Two limits to respect:** (i) η < 1 (financially-constrained firms,
   Martinsson η ≈ 0.4) is *not representable* — `α/(α−1) > 1` for all α > 1, so the model forces a
   gross response and overstates abatement for genuinely inelastic firms; (ii) the `α = η/(η−1)`
   values are an **upper bound** — `α/(α−1)` is the large-price asymptote, so at finite EU-ETS prices
   the realized elasticity is lower and the numerically-calibrated α is somewhat smaller (more
   abatement capacity); the map is also steep near η ≈ 1.3 (`dα/dη ≈ −11`), so the dirtiest-firm α is
   fragile. Confirm against the global solve (`subsec: global`).
4. **Robustness band from the energy-price analogs: intensity elasticity ≈ 0.9–2.** The French
   manufacturing energy-price CO₂ elasticities (Marin–Vona −1.15, Dussaux −0.9) sit just below the
   tax-specific Swedish value; treat them as the **lower anchor** (the salience premium, ≈3–4×, is
   why a carbon tax should land at or above them). Report the abatement channel across this band, as
   the σ note reports the sweep across σ.
5. **State that this is a one-paper literature.** Be explicit in the paper: the carbon-price
   elasticity of firm emission intensity in manufacturing is estimated by exactly one
   quasi-experimental study (Martinsson et al.); the rest is energy-price analogs and treatment
   effects. This is both honest disclosure and a contribution hook — our Belgian B2B + EUTL panel,
   with ETS-induced carbon-cost variation, is the kind of data that could estimate it directly.
6. **Horizon symmetry with the companion notes.** Use the **long-run, tax-induced** value (Martinsson
   long-run ≈ 2), not the short-run/impact (≈ 1) or the generic energy-demand elasticity (Linn
   −0.17), for a *permanent* carbon price — the same horizon argument that sends σ to its long-run
   substitutability and ρ toward complete pass-through.

---

### Sources

- Martinsson, Sajtos, Strömberg & Thomann (2024), *The Effect of Carbon Pricing on Firm Emissions: Evidence from the Swedish CO₂ Tax* (RFS 37(6):1848–1886) — [journal](https://doi.org/10.1093/rfs/hhad097) · [ECGI WP PDF](https://www.ecgi.global/sites/default/files/working_papers/documents/theeffectofcarbonpricingonfirmemissions.pdf)
- Shapiro & Walker (2018), *Why Is Pollution from US Manufacturing Declining?* (AER 108(12):3814–3854) — [AEA](https://doi.org/10.1257/aer.20151272) · [NBER w20879](https://www.nber.org/papers/w20879)
- Marin & Vona (2021), *The Impact of Energy Prices on Socioeconomic and Environmental Performance: Evidence from French Manufacturing Establishments* (EER 135:103739) — DOI 10.1016/j.euroecorev.2021.103739 · [WP PDF](http://www.sustainability-seeds.org/papers/RePec/srt/wpaper/0717.pdf)
- Dussaux (2020), *The Joint Effects of Energy Prices and Carbon Taxes on Environmental and Economic Performance: Evidence from French Manufacturing* (OECD Env. WP 154) — DOI 10.1787/b84b1b7d-en
- Martin, de Preux & Wagner (2014), *The Impact of a Carbon Tax on Manufacturing: Evidence from Microdata* (J. Public Economics 117:1–14) — DOI 10.1016/j.jpubeco.2014.04.016 · [LSE eprint](https://researchonline.lse.ac.uk/id/eprint/57349/)
- von Graevenitz & Rottner (2024), *Energy Use Patterns in German Manufacturing* (ZEW DP 22-038) — [PDF](https://ftp.zew.de/pub/zew-docs/dp/dp22038.pdf)
- Linn (2008), *Energy Prices and the Adoption of Energy-Saving Technology* (Economic Journal 118(533):1986–2012) — [CEEPR PDF](https://ceepr.mit.edu/wp-content/uploads/2023/02/2006-012.pdf)
- Andersson (2019), *Carbon Taxes and CO₂ Emissions: Sweden as a Case Study* (AEJ: Economic Policy 11(4):1–30) — [AEA](https://doi.org/10.1257/pol.20170144)
- Rivers & Schaufele (2015), *Salience of Carbon Taxes in the Gasoline Market* (JEEM 74:23–36) — DOI 10.1016/j.jeem.2015.07.002
- Rafaty, Dolphin & Pretis (2021), *Carbon Pricing and the Elasticity of CO₂ Emissions* (RFF WP 21-33) — [PDF](https://media.rff.org/documents/wp_21-33_tUca3j8.pdf)
- Leroutier (2022), *Carbon Pricing and Power Sector Decarbonization: Evidence from the UK* (JEEM 111:102580) — DOI 10.1016/j.jeem.2021.102580
- Abrell, Kosch & Rausch (2022), *How Effective Is Carbon Pricing? Evidence from the UK Power Sector* (JEEM 112:102589) — DOI 10.1016/j.jeem.2021.102589
- *Treatment-effect / validation studies (§7, NOT parameter-targeting):* Colmer, Martin, Muûls & Wagner (2024, REStud 92(3):1625–1660, `colmeretal2023`); Dechezleprêtre, Nachtigall & Venmans (2023, JEEM 118:102758, `dechezlepretreetal2023`); Petrick & Wagner (2014, Kiel WP); Klemetsen, Rosendahl & Jakobsen (2020, Climate Change Economics 11(1):2050006); Bayer & Aklin (2020, PNAS 117(16):8804–8812); Cao, Ho, Jorgenson et al. (2021, PNAS) and the China-ETS DiD literature.

# Evidence on the Pass-Through of Cost Shocks

This note collects the best-identified empirical estimates of **cost pass-through** — how much
of a change in a firm's marginal cost shows up in its price — relevant for disciplining the
price response in our model. It is the companion to `ELASTICITY_SUBSTITUTION.md`: that note pins
down *how buyers substitute* when a taxed firm's price rises (σ); this one pins down *how much
the price rises in the first place* (pass-through ρ). Together they govern the size of the
carbon-cost shock that actually propagates through the network. It focuses on papers that
estimate a well-identified pass-through using quasi-experimental or IV methods.

**Why this matters for us.** A carbon tax is, mechanically, a **marginal-cost shock** to targeted
firms. Our counterfactual propagates that shock along the network: firm *i*'s price rises, which
raises *i*'s buyers' costs, which raises *their* prices, and so on — the Leontief
`\Psi=(I-\Omega)^{-1}` machinery. **Every link in that chain is a pass-through coefficient.** Our
model is competitive (price = marginal cost, no variable markups — see `ELASTICITY_SUBSTITUTION.md`
§8.1), so it *assumes* complete pass-through, ρ = 1, at every node. The question this note answers
is: **is ρ = 1 a defensible benchmark for a carbon tax, and where does it break?** The literature
says the answer depends on three things — whether the cost shock is **common or idiosyncratic**
across a firm's competitors, the firm's **market power**, and the **horizon / position in the
chain** — and that for the configuration a carbon tax actually is, ρ is *near* complete.

---

## TL;DR — the one fact that organizes everything

Measured cost pass-through ranges from **~0.3** (Indian trade reform, De Loecker et al.) to **~1.0
and beyond** (electricity carbon costs, Fabra–Reguant; cement, Ganapati–Shapiro–Walker over-shoots
to 1.8). The reconciling variable is **whether the cost shock is common or idiosyncratic to a
firm's competitors**, interacted with **market power**. The cleanest statement of the mechanism is
Amiti–Itskhoki–Konings's decomposition of a firm's price response into an **own-cost** term and a
**competitor-price** (strategic-complementarity) term:

&nbsp;&nbsp;&nbsp;&nbsp;Δp_i = β·Δmc_i + γ·Δp_{−i},&nbsp;&nbsp; with **β ≈ 0.6, γ ≈ 0.5, and β + γ ≈ 1.**

- For an **idiosyncratic** cost shock (only firm *i* is hit), competitors' prices don't move, so
  only β ≈ 0.6 operates and pass-through is **incomplete** — the missing ~40% is **markup
  absorption**. This is the regime of De Loecker et al. (0.3) and the large-firm AIK estimates (0.5).
- For a **common** cost shock (every competitor is hit at once), competitors' prices rise *too*, so
  the γ·Δp_{−i} term *adds back in* and total pass-through climbs toward **β + γ ≈ 1**. This is the
  regime of Fabra–Reguant (carbon cost on all generators → ρ ≈ 0.86–1.0) and tariff pass-through at
  the border (Cavallo et al. → ~0.95).

**A broad carbon tax is the common-shock case.** It raises marginal cost for *every* firm in a
targeted sector simultaneously, permanently, and is perceived as such. So the strategic-
complementarity channel pushes pass-through **up toward 1**, not down. The "incomplete pass-through,
firms eat the cost in markups" intuition — correct for an idiosyncratic input-price shock — **does
not transfer** to economy-wide carbon pricing. This is *reassuring* for our competitive full-pass-
through benchmark: the empirical configuration closest to a carbon tax (Fabra–Reguant) delivers
ρ ≈ 1 even in a market where the top two firms hold >80% share.

**Where ρ = 1 breaks — the second-order story we should flag, not bury.** Two deviations:
1. **Market power + demand curvature → over-shifting (ρ > 1).** Ganapati–Shapiro–Walker find energy-
   cost pass-through *exceeds* 1 in concentrated industries (cement 1.78, boxes 1.44) because a
   common cost shock acts as a collusion-coordinating device under convex demand (Weyl–Fabinger).
   So market power doesn't only *dampen* pass-through (idiosyncratic case) — for a common shock it
   can *amplify* it past 1.
2. **Position in the chain / horizon attenuates it downstream.** Tariff pass-through is ~0.95 to the
   immediate buyer's cost (the border) but only ~0.05-of-tariff to the final consumer (the store) in
   the short run, as large retailers absorb the wedge in margins. For us the relevant number is the
   **upstream, marginal-cost** one (~0.95) — that *is* the network-propagation coefficient — but the
   final-consumer incidence is muted on impact.

**Takeaway for calibration.** Use **ρ ≈ 1 (complete) as the central benchmark** for carbon-cost
propagation along the network — it is what our competitive model assumes *and* what the closest
empirical analogue (common, permanent cost shock) delivers. Treat **incomplete pass-through
(ρ ≈ 0.3–0.7) as the idiosyncratic-shock / market-power lower case** and **over-shifting (ρ > 1) as
the concentrated-sector upper case**, and note that both are markup phenomena our price-taking model
switches off. See §6 for the recommendation.

| Paper | Cost shock | Object / level | Setting & identification | Pass-through |
|---|---|---|---|---|
| **Fabra & Reguant (2014)** | CO₂ permit cost (EU ETS) | Equilibrium price PT in a wholesale electricity auction | Spain 2004–06; IV using permit price (Spain ≈ price-taker in EU permit mkt) + structural bidding model | **ρ ≈ 0.86 avg; ≈1.0 peak, ≈0.6 off-peak** — *the carbon, common-shock case* |
| **Ganapati, Shapiro & Walker (2020)** | Energy prices | Marginal-cost PT into product price, plant level | 6 US homogeneous-good mfg industries 1972–97; shift-share fuel-mix IV | **ρ ≈ 0.70 avg; range 0.36 → 1.78**; over-shifts under market power |
| **Cavallo, Gopinath, Neiman & Tang (2021)** | Import tariffs (2018–19) | PT to **border** price vs **retail** price | US–China trade war; diff across tariffed/untariffed goods | **≈0.95 at border; ~1% retail for a 20% tariff** (≈ incomplete downstream) |
| **Amiti, Itskhoki & Konings (2019)** | Marginal cost (imported inputs) | Own-cost PT + strategic complementarity, firm-product | Belgian mfg 1995–2007; IV w/ imported-input costs & FX | **β ≈ 0.6 own-cost; 1.0 small firms, 0.5 large**; β+γ≈1 |
| **De Loecker, Goldberg, Khandelwal & Pavcnik (2016)** | Input-tariff → marginal cost | Cost-to-price PT; markup absorption, firm-product | India 1991 liberalization; IV input tariffs + lagged mc | **ρ ≈ 0.30–0.40**; ~60–70% absorbed into markups |

---

## 1. Fabra & Reguant (2014), "Pass-Through of Emissions Costs in Electricity Markets" — *AER*

**This is the single most directly relevant paper: it is literally the pass-through of a *carbon
cost* into prices, and it is the cleanest case of the common-shock regime our carbon tax inhabits.**

- **What cost shock.** The **marginal emissions cost** = (EU ETS permit price τ) × (emissions rate
  *e* of the price-setting plant). Permits were almost entirely **grandfathered (free)** in Phase I,
  so this is the *opportunity cost* of a permit — exactly a marginal-cost wedge of the kind a carbon
  tax imposes. Emission costs were **15–30% of total marginal cost** (so they use a levels, not
  log-log, spec to avoid the non-traded-cost bias that plagues exchange-rate studies).
- **Setting & data.** Spanish **day-ahead wholesale electricity auction** (uniform-price,
  multi-unit), **Jan 2004 – Feb 2006** (790 days, ~16,200 hourly obs), straddling the 2005 ETS launch
  when permit prices jumped from ≈€0 to €20–30/ton. They observe the **full hourly bid and demand
  schedules of every firm and the identity of the price-setting unit** — unusually complete data.
  Market is concentrated: top two firms >80% of production, four integrated incumbents own 61 of 89
  thermal units.
- **Identification.** Two complementary approaches. **(a) Reduced-form IV:** regress hourly price on
  marginal emissions cost, *instrumenting* with the **permit price τ** — exogenous to Spain, a tiny
  part of the EU-wide permit market (first-stage F ≈ 122–130). This is essential because the marginal
  emissions *rate* is endogenous (dirty cheap coal is marginal in low-demand hours), so a naïve OLS
  gives a perverse **−0.17 to −0.22**. **(b) Structural bidding model:** estimate the auction
  optimality condition (bid = marginal cost + markup) and test **γ = 1**, i.e. whether firms fully
  internalize the permit opportunity cost in their bids.
- **Findings.** Headline equilibrium pass-through **ρ = 0.862** (s.e. 0.181), robust across specs to
  **0.77–0.86** — *almost complete*, which the authors note is "the exception rather than the rule"
  in the pass-through literature. The structural test **cannot reject γ = 1**: firms fully price the
  free permit's opportunity cost. Mechanism: pass-through is near-complete because **(i) the cost
  shock is common across firms, (ii) short-run demand is near-perfectly inelastic, and (iii) repricing
  frictions are negligible** (firms re-bid on 70–90% of days). Markup adjustment after a €1 carbon
  increase is **< 1%**; aggregate demand falls only 0.2%. Free allocation ≈ auctioning for short-run
  price effects.
- **HETEROGENEITY.**
  - *By time / demand conditions — the central cut.* **Peak hours: ρ ≈ 0.99–1.09 (full).**
    **Off-peak: ρ ≈ 0.44–0.64 (~60%).** The off-peak shortfall is **not** market power — it comes from
    **dynamic operating constraints** (start-up/ramping/min-load costs) that make firms bid below
    marginal cost at night to avoid shutting down. When those bind, emission costs aren't fully priced.
  - *By firm size / market power.* Internalization **γ ≈ 1 for the three largest firms** (and ~0.78–
    0.83 for the smallest). Market power shows up in **residual-demand elasticity** (small firms face
    elasticities of 4.5–6.5, big firms much less), **not** in whether the carbon cost is passed through.
    Markup adjustment is economically negligible *for all firms* because rivals face the same shock and
    demand is inelastic.
  - *By technology.* Coal ≈ 0.9–1.0 tons CO₂/MWh, gas (CCGT) ≈ 0.35 — coal up to 3× dirtier and cheaper,
    which is the *source of the endogeneity* (handled by IV), not a separate pass-through estimate.
  - *By horizon.* **Short-run only** — this is a market-clearing auction with no investment/entry margin
    (they cite Fowlie et al. 2012 for the long-run).

**For us:** this is the load-bearing reference for the **central ρ ≈ 1 benchmark**. It is the carbon
case, the common-shock case, and the configuration matching our model's assumptions (price ≈ marginal
cost, full pass-through), and it delivers near-complete pass-through *even under high concentration*.
The honest caveat is that deviations below 1 (the off-peak 0.6) come from **operating
constraints/dynamics**, a margin our static model omits — so 0.6–1.0 is a defensible short-run band,
with the upper end the right central value for a permanent policy.

---

## 2. Ganapati, Shapiro & Walker (2020), "Energy Cost Pass-Through in US Manufacturing" — *AEJ: Applied*

The paper whose subtitle is literally "…and Implications for Carbon Taxes" — the canonical source
for **energy-cost pass-through and carbon-tax incidence under imperfect competition.**

- **What cost shock.** Energy-price–driven changes in plant **marginal cost** (MC recovered from
  estimated time-varying markups à la De Loecker–Warzynski; average markup 1.15, so MC = price/markup).
- **Setting & data.** **Six homogeneous-product US manufacturing industries** — boxes, bread, cement,
  ready-mix concrete, gasoline refining, plywood — chosen because Census collects **physical quantity**
  data so unit prices are observable. **Plant-year, 1972–1997**, Census of Manufactures / ASM at RDCs;
  ~5,683 plant-years.
- **Identification.** Regress log(unit price) on log(marginal cost), instrumenting MC with **two
  shift-share (Bartik) instruments**: (a) state electricity-generation fuel shares × national fuel
  prices (regional variation, relevant to RGGI/AB32-type policies; weaker first stage); (b) industry
  fuel-expenditure shares × national leave-out fuel prices (national variation, relevant to a national
  carbon tax; stronger, F ≈ 66–137). β is the pass-through *elasticity*; pass-through *rate* = β × markup.
- **Findings.**
  - **Central pass-through rate ≈ 0.70** (elasticity ~0.6 × markup 1.15); robust 0.59–0.83 across
    instruments. (NB: the often-cited "0.3" is *only* gasoline refining, the lowest single industry —
    not the sample average.)
  - **Incidence:** consumers bear **25–75% of the welfare burden** (firms bear the rest) — they
    **reject 100% consumer incidence in all six industries**, and reject 100% producer incidence in
    all but gasoline. So the standard CGE assumption of complete pass-through + perfect competition is
    rejected: **in the short-to-medium run, firm owners bear more of a carbon tax than textbook models
    imply.**
  - **Over-shifting (ρ > 1)** in **boxes (1.44), cement (1.78), plywood (1.07)** — driven by oligopoly
    + **convex demand** (Weyl–Fabinger 2013: convex demand → ρ > 1). A common cost shock acts as a
    coordinating device that lets oligopolists achieve the output restraint they couldn't coordinate
    alone; producer surplus can even *rise*.
- **HETEROGENEITY.**
  - *By market power — the organizing axis.* Pass-through and incidence are explicit functions of market
    structure: **higher Lerner index + more inelastic demand → higher pass-through, up to over-shifting.**
    Lerner indices: cement 0.57 (most concentrated → the 1.78 outlier), boxes/plywood 0.32, gasoline/
    concrete 0.10–0.11. Perfect-competition accounting *overstates* the consumer share for all six.
  - *By industry — wide.* Pass-through **elasticities 0.33 (gasoline) → 0.96 (boxes)**; **rates ~0.36
    (gasoline) → ~1.78 (cement)**. (Bread and gasoline have weak first stages, F ≈ 1.7–2.4 — treat
    cautiously.)
  - *By horizon.* **Short-to-medium run only.** They note a cross-sectional spec would give a longer-run
    rate under stronger assumptions, and flag long-run pass-through under imperfect competition as future
    work. No dynamic estimate.
  - *By shock size.* **Not tested** — pass-through is a constant log-log elasticity per industry; the only
    nonlinearity is incidence as a function of the Lerner index, not of shock magnitude.

**For us:** the key reference for **dispersion around ρ = 1 and the market-power story**. Two lessons.
(i) The *average* energy-cost pass-through (~0.7) is the natural lower-central anchor for a carbon
shock when markups are present. (ii) **Concentration doesn't only dampen pass-through — for a common
cost shock under convex demand it can push ρ > 1 (over-shifting).** Our price-taking model has no
markups, so it sits exactly at the ρ = 1 knife-edge between the GSW under- and over-shifting cases;
worth stating that we abstract from both, and that the sign of the bias depends on demand curvature
and concentration in the targeted sector.

---

## 3. Cavallo, Gopinath, Neiman & Tang (2021), "Tariff Pass-Through at the Border and at the Store" — *AER: Insights*

*(Identity note: the PDF lives on Felix Tintelnot's site as a course reading — **Tintelnot is not an
author**. His own washing-machine paper, Flaaen–Hortaçsu–Tintelnot 2019, is cited in it.)*

The cleanest demonstration that **pass-through to the immediate buyer's marginal cost and pass-through
to the final consumer are different objects an order of magnitude apart** — the distinction that
matters most for a *network* model.

- **What cost shock.** US import tariffs in the **2018–19 trade war** (China waves of 25%/25%/10→25%,
  plus steel and washing machines), and foreign retaliatory tariffs (~15%). A permanent, policy-driven
  cost shock — close in kind to a carbon tax.
- **Setting & data.** **Border:** BLS International Pricing Program micro prices of identical goods,
  concorded to HS6 × country, 2005–2019. **Retail:** Billion Prices Project + scraped daily prices from
  two top-10 US retailers (~92,600 products), 2017–2019, plus bills-of-lading for import tonnage.
- **Identification.** Compare ex-tariff prices of **affected vs. unaffected** goods (product × origin ×
  time), regressing Δlog ex-tariff price on contemporaneous + 11 lagged log tariff changes with sector
  FE; the 1-year cumulative pass-through is the sum of coefficients. A coefficient of 0 on the ex-tariff
  price ⇒ full pass-through to the US importer; −1 ⇒ foreign exporter fully absorbs.
- **Findings.**
  - **Border / marginal-cost pass-through ≈ 0.95 (nearly complete).** Ex-tariff Chinese import prices
    barely moved (coefficient −0.018 to −0.079, ≈ 0 with controls) — **Chinese exporters did not cut
    dollar prices; incidence is on US importers.** A 20% tariff ⇒ ~18.5% rise in the price the US buyer
    pays. (For contrast, **exchange-rate** pass-through in the same data is only **~22%** — tariffs pass
    through far more, plausibly because perceived as more persistent. A useful caution: not all cost
    shocks are equal; a permanent policy shock like carbon is the high-pass-through kind.)
  - **Retail / consumer pass-through ≈ incomplete & muted.** A 20% tariff raised affected retail prices
    only **~0.9%** (household goods) to **~1.4%** (electronics) in the short run — **retailers absorbed
    the wedge in margins.**
  - **US exporters** facing retaliation cut ex-tariff prices ~**50–54%** pass-through — but almost
    entirely in **undifferentiated/agricultural** goods (substitutable → exporters must eat it);
    differentiated goods showed no concession. (A direct illustration of the σ–pass-through interaction:
    pass-through is higher where substitutability is lower.)
- **HETEROGENEITY.**
  - *By firm size / market power.* The muted retail result **"largely reflects prices set by the largest
    firms"** (two top-10 retailers); their **buyer power** is the leading explanation for margin
    absorption. On the import side, restricting to **large importers** moves the ex-tariff coefficient
    from ≈0 to **−0.112** — bigger importers extracted modestly *more* exporter concession (slightly less
    pass-through *to* them).
  - *By sector / product.* Retail pass-through ranges from **high (washing machines: +5–15% inflation,
    incl. spillover onto untariffed US brands) → ~zero (bicycles)**, with tires/handbags lagged-high. Tied
    to **differentiation/substitutability**.
  - *By horizon.* **Short-run only, and possibly transient.** Retailers **front-ran** the tariffs
    (China import tonnage up 20–40% from the 2017 announcement to build inventory) and **diverted** sourcing
    (China share of the two retailers' tonnage fell ~97%→80%). The authors warn the modest retail
    pass-through **"may not persist"** — expecting higher consumer pass-through over the longer run.
  - *By shock size.* Suggestive nonlinearity (tires/handbags responded only after the 10%→25% step-up) but
    **not formally tested**.

**For us:** the crucial methodological lesson. In a production-network model, the relevant pass-through
is the **upstream, marginal-cost** one — how a carbon cost on firm *i* raises *i*'s buyers' costs — and
*that* number is **near-complete (~0.95)**, validating full pass-through along network links. The
**final-consumer** incidence is a *separate, smaller, attenuating-downstream* object, muted on impact by
inventory/margin/diversion dynamics our static model omits. Keep the two explicitly distinct: ρ ≈ 1 for
network propagation, < 1 (short-run) for ultimate household incidence.

---

## 4. Amiti, Itskhoki & Konings (2019), "International Shocks, Variable Markups, and Domestic Prices" — *REStud*

**The paper that supplies the decomposition organizing this whole note** (the β/γ split in the TL;DR),
and the cleanest evidence on the **firm-size gradient** in pass-through — on *Belgian* data, our exact
setting.

- **What cost shock & object.** It does **not** regress price on a single shock; it decomposes a firm's
  price change into **own-marginal-cost pass-through (β)** and a **response to competitors' prices (γ,
  strategic complementarity = pure markup adjustment)**. Exchange rates and imported-input costs are the
  *instruments*, not the object. "Own-cost pass-through holding competitor prices fixed" is precisely the
  *idiosyncratic*-shock pass-through.
- **Setting & data.** **Belgian manufacturing 1995–2007**, firm-product (PRODCOM PC-8, >1,500 products),
  ~65k obs, merging production unit values, customs import/export, and income statements. Marginal cost
  proxied by average variable cost; the imported-input cost component (firm × country × CN-8 unit values)
  is the precisely-measured instrument.
- **Identification.** Estimate Δp_it = β·Δmc_it + γ·Δp_{−it} + ξ. Instrument competitor prices Δp_{−it}
  with competitors' marginal-cost proxies (their imported-input costs, euro-zone export prices, bilateral
  exchange rates); instrument own MC with the imported-input component (measurement-error fix). Year/
  industry FE; weak-IV F > 100; passes overid tests.
- **Findings.** **Average own-cost pass-through β ≈ 0.6** (0.59–0.65), **strategic complementarity γ ≈
  0.5** (0.48–0.55), and they **cannot reject β + γ = 1**. So an idiosyncratic cost shock passes through
  ~60%, with ~40% absorbed in markups; but a 10% rise in *competitors'* prices raises the firm's own price
  4–5% (all markup). OLS attenuates β to ~0.35 (sum ~0.7); IV roughly doubles it. The incompleteness is a
  **variable-markup** phenomenon, cleanly separated from marginal cost.
- **HETEROGENEITY.**
  - *By firm size / market share — the signature result.* Pass-through falls and strategic complementarity
    rises **monotonically with market share**:

    | Group | Own-cost PT β | Strategic complementarity γ |
    |---|---|---|
    | **Small firms** | **≈ 1.0** (0.97) — like a textbook CES competitor | **≈ 0** (−0.05) |
    | **Large firms** (≥100 FTE, ≈ market share ≥2%, >60% of mfg sales) | **≈ 0.5** (0.48) | **≈ 0.6** (0.65) |

    Small firms have **complete pass-through and constant markups**; large firms have **half pass-through
    and strong markup adjustment**. β + γ = 1 holds within each group. Robust to alternative size cutoffs.
  - *By sector.* **None significant** — no differential effect by differentiated-vs-homogeneous (Rauch),
    R&D intensity, or Broda–Weinstein elasticity. The heterogeneity is a **size story, not a sector story.**
  - *By horizon.* Annual baseline; **two-year differences give β ≈ 0.66, γ ≈ 0.39 — "very similar"**, so
    price stickiness doesn't materially bias the static estimates. No full impulse-response of pass-through.
  - *By shock size.* Not structurally estimated; they flag (fn. 42) that input-sourcing/quality
    re-optimization occur "over longer horizons and in response to **larger devaluations**" — a potential
    nonlinearity, not a measured one.

**For us:** three uses. (i) The **β + γ ≈ 1 decomposition is the engine** of this note's central
argument — it is *why* a common shock (carbon) passes through near-fully while an idiosyncratic one
(0.6) doesn't. (ii) The **firm-size gradient** says aggregate pass-through is pulled toward the
large-firm value because large firms dominate sales — relevant if our Belgian targeting hits big
emitters. (iii) It is **on Belgian manufacturing micro-data**, so it is the most direct evidence on
markup behavior in our actual economy — useful for justifying (or bounding the error of) the
price-taking assumption.

---

## 5. De Loecker, Goldberg, Khandelwal & Pavcnik (2016), "Prices, Markups, and Trade Reform" — *Econometrica*

The reference case for the **idiosyncratic-shock, incomplete-pass-through, markup-absorption** regime —
the *opposite* end of the range from Fabra–Reguant, and a caution about reading any one number as "the"
pass-through.

- **What cost shock & object.** Input-tariff cuts that lowered firms' **marginal costs**; the object is
  how much of that cost decline reached **prices** vs. was absorbed into **markups**. Markups recovered
  via the De Loecker–Warzynski production approach (markup = output elasticity of materials ÷ materials'
  revenue share — no demand/market-structure assumptions); MC = price/markup, with corrections for
  unobserved within-firm input allocation and input-price/quality bias.
- **Setting & data.** **India's 1991 trade liberalization**; **Prowess** firm-product panel (1989–2003,
  estimation 1989–97 when tariff cuts were uncorrelated with industry characteristics). ~1,400 products,
  ~1,970 firms/year. Mostly medium-large firms (factory-gate prices).
- **Identification.** Regress log price, log MC, log markup on output and input tariffs with firm-product
  + sector-year FE; the identity ln P = ln mc + ln μ makes the coefficients sum. Pass-through ζ from
  ln P = a + ζ·ln(mc), **instrumenting MC with input tariffs + lagged MC** (purges measurement error,
  first-stage F = 98).
- **Findings.** Over the reform (output tariffs −62pp, input tariffs −24pp): **prices fell ~18%, marginal
  costs fell ~31%, markups rose ~13%** — the markup rise offsets ~half the cost decline. The direct
  **pass-through rate ζ = 0.30 (OLS 0.34, IV 0.31–0.41)** — robustly incomplete; **~60–70% of a cost shock
  is absorbed into markups.** Mean markup 2.70 (median 1.34). Cost decline driven by *input* tariffs
  (cheaper imported inputs), not output tariffs.
- **HETEROGENEITY.**
  - *By firm / product scope.* Markups are higher and MC lower on products near a firm's **core competency**;
    fall with distance from the core.
  - *By initial markup (pro-competitive).* Output-tariff cuts compress markups **far more for initially
    high-markup products** (top-decile products lose an extra ~3.14% markup per 10pp cut) — pass-through
    heterogeneity concentrated in the right tail.
  - *By sector.* Wide dispersion in markup levels and output elasticities.
  - *By horizon.* **Short-run** (1989–97); longer-run consumer gains may arrive via quality upgrading and
    new-product introduction (firms with bigger markup gains introduced more new products — profits
    financing development), but those are conjectural here.
  - *Market structure.* Incomplete pass-through only **requires demand elasticity increasing in price**
    (variable markups); they stay **agnostic** about the specific demand system.

**For us:** the **lower-bound / cautionary case**. ζ ≈ 0.3 is what pass-through looks like for a cost
shock to a firm with market power *whose competitors are not all hit the same way at the same time*, and
it is a warning that markup absorption can swallow most of a cost change. A carbon tax differs on exactly
the dimension that matters — it is **common and permanent** — which is why we don't take 0.3 as the central
value. But it is the right number to cite for "how incomplete can pass-through get, and why" and for the
honest caveat that **if targeted firms have market power and the tax is narrow/firm-specific, our full-
pass-through model overstates the price (and hence the network) response.**

---

## 6. Recommendation for our calibration

**The four scenarios (committed).** Because our model is competitive (price = marginal cost),
**ρ = 1 is not a dial we turn — it is a structural implication** (dp/dmc = 1 mechanically). Departures
are therefore *robustness scenarios*, each implicitly a different markup assumption, and each grounded in
a specific empirical estimate. We report four, alongside (and secondary to) the σ-sweep. We never use
ρ > 1 — over-shooting requires a baseline markup buffer our competitive model doesn't have.

| # | Scenario | ρ | Empirical grounding |
|---|---|---|---|
| **1** | **Full pass-through (benchmark)** | ρ_i = 1 ∀i | Fabra–Reguant (carbon = common shock, 0.86–1.0); Cavallo border (~0.95); also the competitive model's own implication |
| **2** | **Uniform incomplete** | ρ_i = 0.7 ∀i | GSW average energy-cost pass-through rate (~0.70) |
| **3** | **Uniform strong absorption** | ρ_i = 0.5 ∀i | AIK large-firm own-cost pass-through (~0.5), applied economy-wide |
| **4** | **Heterogeneous by firm size** | ρ_i = 0.5 for large firms, 1.0 for small firms (split *within each sector*) | AIK firm-size gradient: large firms β ≈ 0.5, small firms β ≈ 1.0 |

**Why these four, and what each comparison buys.** Scenarios 1→2→3 trace a uniform pass-through ladder
(full → GSW → AIK-large), bounding the aggregate effect of incompleteness. Scenarios **3 vs. 4 share the
same 0.5 anchor** and differ only in *who* absorbs — everyone (3) vs. only the large firms (4) — so the
comparison **isolates the effect of the heterogeneity itself** from the effect of the level. Scenario 4 is
the empirically faithful one (AIK is literally a within-sector firm-size split on Belgian data), and it is
the scenario that interacts with our leakage channel: if the *targeted* large emitters absorb cost (low ρ)
while their small same-sector competitors pass through fully, relative prices move differently than under
any uniform ρ. The **β + γ ≈ 1 logic** still frames all four as conservative: for a common shock the
strategic-complementarity term adds the competitor-price response back in, pulling true ρ toward 1, so
scenarios 2–4 are *pessimistic bounds*, not rival centers.

*Implementation note for scenario 4.* "Large within sector" should follow AIK's operationalization —
they used ≥100 FTE ≈ top 20% by sales ≈ within-market share ≥ ~2%. With the B2B data, define large by
within-sector market share (or sales rank) and set ρ_i = 0.5 above the cutoff, 1.0 below; report
robustness to the cutoff.

**The heterogeneity twist that matters — targeted sectors *are* the concentrated sectors.**
Pass-through heterogeneity is a **market-power story, not a sector-fundamentals story** (AIK find no
sector heterogeneity once firm size is controlled). The dimension to index ρ on is therefore
concentration / market share, not sector per se. But the sharp point for carbon policy: the *targeted*
EITE sectors — **cement, steel, refining, basic chemicals** — are exactly GSW's high-Lerner,
**over-shooting** industries (cement is the 1.78 outlier). So the empirically relevant heterogeneity
does **not** spread ρ below 1 in the targeted nodes; if anything it pushes ρ **≥ 1** there. The
heterogeneity cuts *toward* the baseline (or over-shooting), not toward absorption — a point worth a
sentence because it preempts the obvious "but pass-through is incomplete" objection precisely where it
would bite.

1. **Central benchmark: ρ ≈ 1 (complete pass-through) along network links.** A carbon tax is a *common,
   permanent, marginal-cost* shock to a targeted sector — the configuration that produces the **highest**
   empirical pass-through (Fabra–Reguant 0.86–1.0 for the actual carbon case; Cavallo et al. ~0.95 to the
   immediate buyer's cost). This is also exactly what our competitive, price-taking model assumes, so the
   model's benchmark is *empirically defensible*, not a convenient simplification. Lead with this.
2. **Distinguish the two pass-through objects explicitly.** Pass-through into the **immediate buyer's
   marginal cost** (the network-propagation coefficient, ≈ 0.95–1.0) is *much* higher than pass-through to
   the **final consumer** (attenuates downstream, muted on impact). Our `\Psi=(I-\Omega)^{-1}` propagation
   uses the *former*; household incidence is the *latter*. State which one each result speaks to.
3. **Lower case: ρ ≈ 0.6–0.7** (AIK own-cost 0.6; GSW average 0.7). The regime where the shock is more
   idiosyncratic, or large firms with market power absorb part of it in markups. The honest reading: this is
   the bias direction *if* targeted emitters have market power and the tax is narrow. Report it as the
   sensitivity floor.
4. **Upper case: ρ > 1 (over-shifting), up to ~1.5–1.8** in concentrated sectors with convex demand
   (GSW: cement 1.78). Our price-taking model sits at the ρ = 1 knife-edge between GSW's under- and
   over-shifting; **the sign of the markup bias is not signable a priori** — it depends on concentration and
   demand curvature in the targeted sector. (Consistent with the house rule against claiming an unsigned
   bias direction — see the imputation-bias memory.) Flag this rather than asserting full pass-through is
   conservative.
5. **The β + γ ≈ 1 framing is the paper-ready justification.** If a referee objects that "pass-through is
   incomplete (De Loecker 0.3), so your full-pass-through model is wrong," the answer is the AIK
   decomposition: incompleteness is the *idiosyncratic-shock* β; a common carbon tax also fires the
   strategic-complementarity γ, and β + γ ≈ 1. One sentence converts the standard objection into support for
   the benchmark. Pair it with Fabra–Reguant as the empirical existence proof.
6. **Horizon.** Every clean estimate here is **short-to-medium run** (Fabra–Reguant auction; GSW plant
   panel; Cavallo et al. flag transience; De Loecker 1989–97). For a *permanent* carbon price the long-run
   pass-through is, if anything, **higher** (entry/exit and full re-optimization tend to push competitive
   pass-through toward 1; the downstream-attenuation in Cavallo et al. is explicitly expected to fade). So
   the short-run estimates bound ρ from *below* for our permanent-policy question — symmetric to the σ
   horizon logic in `ELASTICITY_SUBSTITUTION.md`.
7. **Sectoral heterogeneity.** Pass-through dispersion is a **market-power / market-structure** story (GSW
   Lerner gradient; AIK firm-size gradient), **not** a sector-fundamentals story (AIK finds no sector
   heterogeneity once size is controlled). For Belgium, if ETS targeting concentrates on a few large emitters
   with market power, expect pass-through pulled toward the large-firm value (AIK ≈ 0.5) or, under convex
   demand, over-shooting (GSW cement). A market-power-indexed ρ is the natural robustness cut if results are
   sensitive — analogous to the sector-specific-σ suggestion in the companion note.

---

## 7. Implementing it: a reduced-form pass-through wedge (the planned approach)

The plan is to **leave the model structurally untouched** — price = marginal cost in steady state, so
the baseline calibration is unchanged — and impose the pass-through wedge **only on the counterfactual
response**.

**Specification.**

&nbsp;&nbsp;&nbsp;&nbsp;Steady state: **p_i = mc_i** (markup ≡ 1, baseline untouched).

&nbsp;&nbsp;&nbsp;&nbsp;Counterfactual response: **Δlog p_i = ρ_i · Δlog mc_i**,&nbsp; ρ_i ∈ (0, ~1.8].

Firm *i* absorbs a fraction (1 − ρ_i) of its marginal-cost change in an implicit markup that is *flat at
baseline but responds to cost*. This is exactly a **first-order (AIK-style) linearization of a
variable-markup model around a point where the markup equals 1** — i.e., "no markups in levels, variable
markups in the response." It is internally consistent: the steady state needs no markup data, and the
response inherits the empirical ρ.

**How it enters the network propagation.** Marginal cost responds to input prices and the direct carbon
cost,

&nbsp;&nbsp;&nbsp;&nbsp;**Δlog mc = Ω · Δlog p + s**,

where Ω is the matrix of input cost shares and s_i is firm *i*'s **direct carbon-cost exposure**
(emissions intensity × tax, i.e. Ω_iz · Δp_z). With full pass-through (ρ = 1) this is the familiar
Leontief solution Δp = (I − Ω)⁻¹ s = Ψ s. With the wedge Δp = R · Δmc, where **R ≡ diag(ρ_i)**:

&nbsp;&nbsp;&nbsp;&nbsp;Δp = R(Ω Δp + s)&nbsp; ⟹ &nbsp;**Δp = (I − RΩ)⁻¹ R s ≡ Ψ_ρ s.**

So the wedge simply replaces the Leontief inverse Ψ = (I − Ω)⁻¹ with **Ψ_ρ = (I − RΩ)⁻¹ R**. Uniform ρ:
**Ψ_ρ = ρ(I − ρΩ)⁻¹**. This is a *one-line modification to the propagation step* — no re-calibration, no
re-solve of the steady state.

**It reproduces downstream attenuation for free.** Expanding the uniform case,
Ψ_ρ = ρ · Σ_{k≥0} (ρΩ)ᵏ : a cost shock originating *k* production stages upstream reaches firm *i* scaled
by ρ^{k+1}. So pass-through **decays geometrically with network distance** — which is precisely the
Cavallo et al. **border-vs-store** result (near-complete to the immediate buyer, attenuating downstream
through additional stages). ρ < 1 in the network is the structural counterpart of their downstream
attenuation, and Cavallo's per-stage border number (~0.95) is a direct empirical anchor for a single ρ
step. The reduced form thus has a clean micro-empirical reading, not just a dampening knob.

**The σ channel is untouched and separable.** ρ scales the *size* of the price changes; σ governs how
buyers *substitute given* those changes. They enter at different points (ρ in the price/propagation
block, σ in the demand/reallocation block), so the σ-sweep and the ρ-scenarios are **orthogonal
robustness axes** — runnable on a grid without interaction artifacts.

**Caveats to state explicitly.**
1. **Not derived from optimization → handle welfare carefully.** The markup wedge is imposed, not
   micro-founded, so it carries no welfare interpretation on its own. If you report welfare/incidence,
   **book the absorbed (1 − ρ_i)·Δmc_i as a change in firm *i*'s profits** (a transfer to owners) rather
   than letting it vanish, or the resource constraint leaks. Innocuous for the emissions / reallocation
   counterfactual; *not* innocuous for incidence.
2. **Local, first-order only.** p = mc in levels with Δp = ρΔmc is a kink (markup ≡ 1 at baseline, nonzero
   cost-derivative). Valid for a marginal counterfactual; don't push it to large shocks, where AIK (fn. 42)
   flag nonlinearity (input re-sourcing, quality re-optimization at large devaluations).
3. **ρ_i is calibrated in, not identified.** It is not separately identified inside the model — it is set
   from the pass-through literature. Be transparent that it is an input, and report the scenario set, not
   a point.
4. **If you index ρ_i, pick the keying variable and defend its sign.** The empirically supported dimension
   is concentration / market share (you have B2B market shares). But the sign is ambiguous: *absorption*
   (AIK large-firm ρ ≈ 0.5, decreasing in share) vs *over-shooting* (GSW concentrated ρ > 1, increasing in
   share). Since targeted EITE sectors are the concentrated ones, a defensible default is **ρ_i ≥ 1 (or = 1)
   in targeted nodes, = 1 elsewhere** — i.e. start uniform, introduce a gradient only if the uniform sweep
   shows sensitivity.
5. **Spectral-radius condition.** (I − RΩ)⁻¹ exists only if the spectral radius of RΩ is < 1. Uniform
   ρ ≤ 1 is always fine; with over-shooting ρ_i > 1 in some nodes, verify ρ_max · spr(Ω) < 1 so the
   multiplier doesn't diverge.

**The committed scenario set** (see §6 for grounding): three uniform — **ρ = 1, 0.7, 0.5** — plus one
**heterogeneous** — ρ = 0.5 for large firms / 1.0 for small firms, split within each sector. ρ ≤ 1
throughout (no over-shooting). Each is implemented via the §7.1 pricing rule; scenarios 1–3 set a scalar
ρ, scenario 4 sets ρ_i by within-sector firm size. Run each against the σ-sweep as orthogonal axes.

### 7.1 Global / large-shock implementation: a cost-dependent markup

The Δp = ρΔmc form above is first-order — fine for intuition and for the covariance/sufficient-statistic
expressions, but the model is also **solved globally (nonlinearly) for large shocks**, where the
linearization doesn't hold. Incomplete pass-through carries over to the global solve cleanly by promoting
the wedge from a *shock-scaling* (ill-defined globally) to a **pricing rule** the solver evaluates each
iteration. Replace the price-setting condition p_i = mc_i with a **constant-pass-through-elasticity** rule
anchored at the baseline:

&nbsp;&nbsp;&nbsp;&nbsp;**p_i = (mc_i⁰)^{1−ρ_i} · (mc_i)^{ρ_i}**,&nbsp; equivalently&nbsp; μ_i(mc_i) ≡ p_i/mc_i = (mc_i / mc_i⁰)^{ρ_i−1},&nbsp; ρ_i ∈ (0, 1],

where mc_i⁰ is firm *i*'s baseline marginal cost. Properties:
- **In logs:** log p_i = (1 − ρ_i)·log mc_i⁰ + ρ_i·log mc_i — linear with slope ρ_i, so **d log p_i / d log mc_i = ρ_i
  exactly and globally**, not just locally; to first order it reduces to Δp_i = ρ_i Δmc_i.
- At baseline (mc_i = mc_i⁰): μ_i = 1, p_i = mc_i — **baseline calibration untouched**.
- It is *literally* De Loecker et al.'s estimating equation log p = const + ζ·log mc with **ζ = ρ_i** — the
  functional form the literature estimates, not an ad-hoc knob. Positivity is automatic; monotone,
  log-concave in mc.

**Drop-in to the solver.** The global fixed point keeps the same dimension: instead of solving
p_i = mc_i(p, τ) (the existing CES cost function, which carries σ), solve
**p_i = (mc_i⁰)^{1−ρ_i} · mc_i(p, τ)^{ρ_i}** — one extra composition in the price block, no re-calibration,
no steady-state re-solve. σ stays in the cost/demand block, so **ρ (price block) and σ (reallocation block)
remain separable**. Because ρ_i ≤ 1 dampens propagation (the ρ^k decay over network distance from §7), the
global map is **more contractive** than the ρ = 1 case — incomplete pass-through makes the solve easier,
not harder.

**The consistency point — innocuous in this project (no welfare).** With baseline markup = 1 there is no
buffer to absorb into, so for a cost *increase* ρ < 1 forces **p_i < mc_i** (μ_i < 1): the firm prices below
marginal cost, a negative unit margin. The rule is therefore a *calibrated empirical pricing relation*, not
a profit-maximizing optimum — which **does not matter here, because we do not compute welfare**. Every object
we report — prices, quantities, emissions, the reallocation/leakage decomposition — takes the equilibrium
price vector as given and is unaffected by how the implied profit term would be booked. (For the record:
were welfare ever needed, the absorbed (mc_i − p_i)·y_i is exactly the producer's share of the carbon cost —
GSW's "firms bear 25–75%" — and would have to enter as a profit change π_i = (p_i − mc_i)·y_i with a closure
rule, e.g. lump-sum rebate to households. We don't, so it doesn't bind.) The only abstraction that survives
regardless is **exit**: sustained-loss firms would shut down, an extensive margin the static model omits.

**Optimizing alternative (not needed here).** A genuine optimizing equilibrium would require calibrating a
baseline markup μ_i⁰ > 1 from a demand elasticity so there *is* a buffer and p stays above mc — but that
abandons the p = mc baseline and adds a markup-level calibration. Since we don't do welfare, the calibrated
power-law rule on a p = mc baseline is the right, lighter choice; the optimizing-markup version is noted only
for completeness.

**Levels vs. logs.** A levels rule p_i = mc_i⁰ + ρ_i·(mc_i − mc_i⁰) matches the Fabra–Reguant / GSW *levels*
specs; the power-law (log) rule matches the De Loecker / AIK *log* specs. The log form is cleaner in a
multiplicative CES network (stays in logs/shares, guarantees positivity); the two agree for moderate ρ. Use
the log/power-law form by default.

---

### Sources

- Fabra & Reguant (2014), *Pass-Through of Emissions Costs in Electricity Markets* (AER 104(9):2872–2899) — [AEA](https://www.aeaweb.org/articles?id=10.1257/aer.104.9.2872) · [NBER w19613](https://www.nber.org/system/files/working_papers/w19613/w19613.pdf)
- Ganapati, Shapiro & Walker (2020), *Energy Cost Pass-Through in U.S. Manufacturing: Estimates and Implications for Carbon Taxes* (AEJ: Applied 12(2):303–342) — [AEA](https://www.aeaweb.org/articles?id=10.1257/app.20180474) · [NBER w22281](https://www.nber.org/papers/w22281)
- Cavallo, Gopinath, Neiman & Tang (2021), *Tariff Pass-Through at the Border and at the Store: Evidence from US Trade Policy* (AER: Insights 3(1):19–34) — [AEA](https://www.aeaweb.org/articles?id=10.1257/aeri.20190536) · [NBER w26396](https://www.nber.org/papers/w26396)
- Amiti, Itskhoki & Konings (2019), *International Shocks, Variable Markups, and Domestic Prices* (REStud 86(6):2356–2402) — [author PDF](https://itskhoki.com/papers/DomesticPrices.pdf) · DOI 10.1093/restud/rdz005
- De Loecker, Goldberg, Khandelwal & Pavcnik (2016), *Prices, Markups, and Trade Reform* (Econometrica 84(2):445–510) — [Econometrica](https://onlinelibrary.wiley.com/doi/abs/10.3982/ECTA11042) · [NBER w17925](https://www.nber.org/papers/w17925)
- *Theory anchor (not reviewed above):* Weyl & Fabinger (2013), *Pass-Through as an Economic Tool* (JPE 121(3)) — pass-through as a sufficient statistic for incidence; demand curvature → over- vs under-shifting.

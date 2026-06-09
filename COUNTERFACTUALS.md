# Counterfactual Exercises

This file specifies the counterfactual exercises the quantitative section of the paper
(`sections/quantitative.tex`, formerly the "application" section) will run. It defines, for
each exercise, the baseline, what is held fixed, what varies, the structural parameters
plugged in, and the outcome metrics reported.

It is the counterfactual analog of [REALLOCATION_FINDINGS.md](REALLOCATION_FINDINGS.md): that
file documents a *purely mechanical, model-free* accounting decomposition of what happened in
Belgium; this file documents the *structural, model-based* exercises that ask what *would have*
happened under different carbon prices and different targeting.

---

## 1. Motivation and the two departures from the data

### What we learned from the carbon-leakage decomposition

The mechanical decomposition (see [REALLOCATION_FINDINGS.md](REALLOCATION_FINDINGS.md)) establishes
two facts about Belgium, 2005–2022:

1. **Emission reduction was almost exclusively abatement (technique).** Within-firm intensity
   declines account for the bulk of the ~46% ETS emissions decline. Within-sector reallocation —
   dirty firms losing market share to clean firms in the same narrow sector — is essentially **zero**
   on every margin we measured, and is **uncorrelated** with carbon-cost exposure across sector-years.

2. **This is *not* evidence of a low elasticity of substitution.** The reallocation channel is
   governed by the input elasticity $\sigma$ (it multiplies the covariance sufficient statistic in
   `sections/model.tex` §2.3.2), but it is *also* governed by how large the cross-firm gaps in
   effective prices are.
   In Belgium the EUA price stayed too low, for too long, to open up price gaps big enough to either
   (a) move market shares or (b) let us *identify* $\sigma_i$ from observed reallocation. A near-zero
   reallocation response is consistent with a high $\sigma$ and small price gaps just as much as with
   a low $\sigma$.

### Consequence 1 — we plug $\sigma$ from the literature, we do not estimate it

Because the Belgian experience does not identify the input elasticity of substitution, we **do not
estimate $\sigma$**. Instead we run every counterfactual across a grid of $\sigma$ values taken from
the literature, and report how the results move with $\sigma$. The elasticity is a **dial we turn**,
not a number we claim to have measured. See [§3](#3-structural-parameters-plugged-in).

### Consequence 2 — the benchmark is the actual Belgian experience, and counterfactuals depart from it in two dimensions

Our benchmark is **what actually happened in Belgium, 2005–2022**: the realized EUA price path and
the realized EU ETS targeting vector. At this benchmark the model should reproduce the mechanical
finding — emission reduction dominated by abatement, reallocation ≈ 0. Reproducing this is a
**validation check** ([§5](#5-benchmark-validation)), not a counterfactual.

To make the reallocation channel quantitatively relevant — which is the entire point of a
production-network paper — we must leave the benchmark behind in **two distinct ways**:

- **(A) Higher carbon prices.** Crank $p_z$ above the realized path so that cross-firm price gaps
  become large enough for reallocation to bite.
- **(B) Different targeting.** Change the targeting vector $\tau$ away from the realized EU ETS scheme
  (universal industrial coverage, centrality-based, sector-level — see [§4](#4-the-targeting-schemes)).

**Transparency requirement.** Because every interesting counterfactual moves *both* dials at once,
when we report results we must always **decompose the total change into the part due to higher prices
and the part due to different targeting**. A reader must never be left wondering whether an effect
came from the price or the targeting. The reporting protocol that enforces this is in
[§6](#6-reporting-protocol-separating-price-from-targeting).

---

## 2. The policy levers, in model notation

A carbon policy is the **pair $(p_z, \tau)$** (`sections/model.tex`, §2.1.1); all carbon revenue
$T=\sum_i p_z\tau_i z_i$ is rebated lump-sum to the household, and firms price at marginal cost
($p_i = mc_i$, no markups, full pass-through in the baseline).

| Lever | Symbol | Role in the counterfactuals |
|-------|--------|------------------------------|
| Carbon price | $p_z$ | **Dimension (A).** Swept over a grid from the realized path up to high values. |
| Targeting vector | $\tau = \{\tau_i\}_{i\in\mathcal N}$, $\tau_i\in\{0,1\}$ | **Dimension (B).** One vector per targeting scheme in [§4](#4-the-targeting-schemes). |
| Revenue rebate | (lump-sum) | **Fixed by the model**, not a free lever: revenue is always rebated lump-sum, so the way revenue is spent never contaminates emissions through a scale channel. At first order $d\log Y/d\log p_z = 0$ (`sections/model.tex` §2.2.1, application of Baqaee–Farhi 2020 Thm 1). |
| Cost pass-through | $\rho\in(0,1]$ | **Swept over a grid** (see [§3](#3-structural-parameters-plugged-in)). Baseline is full pass-through ($p_i=mc_i$, so the wedge propagates one-for-one via the forward equation $d\log\mathbf p/d\log p_z = \Psi(\tau\circ\mathcal E)$). The literature (AIK, Belgian data) finds pass-through < 1, which **dampens how much of the carbon wedge reaches downstream prices** and so dampens both channels. Implemented as the firm passing through only a fraction $\rho$ of its own carbon bill ($p_i=mc_i^{\text{non-carbon}}+\rho\,mrc_i$), which linearizes to $\Psi(\rho\,\tau\circ\mathcal E)$ — see [§3](#3-structural-parameters-plugged-in). |

The object we decompose is the change in aggregate emissions (`sections/model.tex`, §2.2.1):

$$
\frac{d\log Z}{d\log p_z}
= \underbrace{\frac{d\log Y}{d\log p_z}}_{\text{scale}}
+ \underbrace{\sum_{i}\frac{z_i}{Z}\frac{d\log e_i}{d\log p_z}}_{\text{abatement (technique)}}
+ \underbrace{\sum_{i}\frac{z_i}{Z}\frac{d\log(\lambda_i/p_i)}{d\log p_z}}_{\text{reallocation (composition)}}
$$

In the CES economy this collapses to a sharp form (`sections/model.tex`, §2.3.2):

$$
\frac{d\log Z}{d\log p_z} \;=\; [\text{abatement}] \;-\; \sigma\cdot[\text{reallocation}],
\qquad
[\text{reallocation}] = \sum_i \frac{x_i}{Z}\,\mathbb C_{\Omega(i,:)}\!\big(\text{Exposure}_i,\ \text{Network-adj. EI}_i\big)
$$

**Two elasticities, two channels, cleanly separated:**
- **Abatement** is governed by the firm's abatement elasticity $\alpha_i>1$. Emission intensity is
  $e_i=(1-\kappa_i)^{\alpha_i}\bar e_i$ (firm spends $\kappa_i$ to abate), giving
  $d\log e_i/d\log p_z=\alpha_i/(1-\alpha_i)<0$ (intensity falls as the price rises). It does **not**
  depend on $\sigma$.
- **Reallocation** is governed by $\sigma$, which enters **linearly** as a multiplier on the covariance
  sufficient statistic. $\sigma$ **scales** this channel; it does **not** set its sign — the covariance
  does (Cov $>0$ amplifies abatement, Cov $<0$ dampens it).

With lump-sum rebate the scale term vanishes at first order, so the action is abatement ($\alpha$)
vs. reallocation ($\sigma\times$Cov).

> **Solution method: global, not approximate.** Because cranking $p_z$ "high" pushes us far from the
> point of approximation, we **solve the CES economy globally** for every counterfactual cell, rather
> than relying on the first-/second-order expansions around the no-policy equilibrium. The first-order
> objects (the forward/backward propagation equations, the centrality measure, the reallocation
> sufficient statistic) are retained for **interpretation** and as the **cheap, policy-implementable**
> targeting device whose accuracy we then test against the global solution (see
> [§5b](#5b-does-first-order-centrality-survive-the-global-solution)).

---

## 3. Structural parameters plugged in

> **STRATEGY UPDATE (supersedes the α-calibration discussion below).** We no longer calibrate α
> to match the Belgian aggregate emission change. Doing so was *internally inconsistent*: it treats
> the whole −24pp as carbon-caused (forcing α to absorb it), yet the model delivers that ΔZ together
> with a large output/reallocation response that Belgium did **not** have (firms abated *in place*;
> Colmer: no contraction) — so we'd be believing the model's emission response while disowning its
> output response (same equilibrium). Instead (**Option B**): **set α and ρ externally from the
> literature, sweep σ.** The *benchmark* is just the model at the realized price under those
> parameters — internally consistent, and we don't pretend to reproduce every Belgian moment.
> - **α** (abatement elasticity): Martinsson (2024), |d log e/d log p| = α/(α−1). All-firm ≈2.08 → α≈1.9;
>   top-decile ≈1.3 → α≈4.3. Our regulated firms are top emitters → **default α = 4** (robustness {2, 4}).
> - **ρ** (pass-through): AIK own-cost ≈0.5–0.7 → **default ρ = 0.5** (robustness {0.5, 0.75, 1}).
> - **σ**: unidentified in Belgium → **swept** {0.034, 0.5, 1, 4.7} (the headline).
> The model's reallocation/output is then a *prediction* (governed by σ, ρ), reported and compared to
> Belgium's ≈0 — not something α is forced to cancel. The calibration text below (§ "Calibration plan")
> is retained for the record but no longer the strategy. See COMPUTATIONAL.md for the decomposition.

| Parameter | Symbol | How we set it | Source |
|-----------|--------|---------------|--------|
| Input elasticity of substitution | $\sigma_i$ | **Plugged from literature, swept over $\{0.034, 0.5, 1, 4.7\}$** — near-Leontief to highly substitutable. $\sigma$ enters reallocation **linearly** (it scales the channel; the covariance sets the sign). Common $\sigma$ first; sector-specific later. | Atalay (2017); Baqaee–Farhi (2019); Peter & Ruane (2023) — see below |
| Abatement elasticity | $\alpha_i>1$ (giving $d\log e_i/d\log p_z = \tfrac{\alpha_i}{1-\alpha_i}<0$) | **Set externally** (Option B, see box above): default $\alpha=4$ (top-emitter), robustness $\{2,4\}$. Governs the technique channel via $e_i=(1-\kappa_i)^{\alpha_i}\bar e_i$; independent of $\sigma,\rho$. ($\alpha>1$ for convexity.) | Martinsson et al. (2024) emission-price elasticity → $\alpha=m/(m-1)$ |
| Cost pass-through | $\rho$ | **Set externally** (Option B): default $\rho=0.5$, robustness $\{0.5,0.75,1\}$. | Amiti–Itskhoki–Konings (own-cost ≈0.5–0.7) |
| No-policy emission intensity | $\bar e_i$ | Firm's pre-policy emissions per unit output; heterogeneous within sector. Measured. | EUTL + imputation |
| Emission intensity vector | $\mathcal E=[e_1,\dots,e_N]$; network-adjusted $\Psi_e=\Psi\mathcal E$ | Measured intensities; network-adjusted via the Leontief inverse. | EUTL + imputation |
| CES input productivities | $\theta_{ij}$ | Calibrated from observed expenditure shares (standard CES inversion). **Note:** in the current model $\theta$ is *input productivity*, not an emission weight. | NBB B2B (cost shares) |
| I-O matrix / Leontief inverse | $\Omega$, $\Psi$ | From the Belgian firm-to-firm B2B network. | NBB B2B |
| Cost pass-through | $\rho$ | **Swept over an even grid $\{0.25, 0.5, 0.75, 1\}$.** Full pass-through ($\rho=1$) is the model's baseline ($p_i=mc_i$); the literature (AIK, GSW) says the realistic range is below 1. The only price–cost wedge we explore (the current model has no markups). | AIK (2019/2014); Ganapati–Shapiro–Walker (2020) |

**$\sigma$ grid — verified.** The object we need is the **producer-level elasticity of substitution
across intermediate inputs** (Baqaee–Farhi call this $\theta$; Atalay calls it $\varepsilon_M$). The
literature cleanly separates this from the value-added-vs-intermediates nest and the consumption nest —
we must not borrow values across nests. Verified primary-source anchors:

| Sweep point | $\sigma$ | Source | Object |
|-------------|----------|--------|--------|
| **Low** (near-Leontief) | **0.034** | Atalay (2017), *AEJ: Macro* 9(3), Table 2 ($\varepsilon_M$) | Substitution across intermediate inputs — exact target object, industry-level (BEA/KLEMS) |
| **Middle** (default) | **0.5** | Baqaee & Farhi (2019), *Econometrica* 87(4), benchmark $(\sigma,\theta,\varepsilon)=(0.9,0.5,0.001)$ ($\theta$) | Substitution across intermediate inputs in exactly our model class — the natural default |
| **Unit** (Cobb–Douglas) | **1.0** | Familiar benchmark, not an estimate | Standard Cobb–Douglas reference point. *Not* a special knife-edge here — $\sigma$ enters reallocation linearly, so nothing flips at $\sigma=1$ |
| **High** (substitutes) | **≈4.7** | Peter & Ruane (2023), NBER WP 31233, "The Aggregate Importance of Intermediate Input Substitutability" | Plant-level substitution across 8 material-input categories, identified off India's 1990s tariff cuts — exact target object, **micro-estimated** |

**Substantive flag — the literature disagrees by two orders of magnitude on this *exact* object.**
Atalay's $\varepsilon_M = 0.034$ (near-Leontief) and Peter & Ruane's $\approx 4.7$ (highly
substitutable) estimate the *same* parameter — producer-level substitution across intermediate inputs —
yet straddle $1$ massively. This is the strongest possible justification for our "sweep, don't
estimate" strategy. In the current model $\sigma$ enters the reallocation channel **linearly** (the CES
result $d\log Z/d\log p_z = [\text{abatement}] - \sigma\cdot[\text{reallocation}]$), so the grid
**scales** the reallocation contribution by a factor of $\sim$140 from the low to the high anchor —
from *reallocation is negligible* ($\sigma=0.034$) to *reallocation is first-order* ($\sigma=4.7$). It
does **not** flip the sign: the amplify-vs-dampen verdict is set by the covariance sufficient statistic
(whether targeted firms' exposure covaries positively or negatively with network-adjusted emission
intensity), independent of $\sigma$. Reporting the low and high anchors side by side is the honest way
to show that *how much* reallocation matters hinges on a parameter the Belgian data cannot pin down.

> **Nesting caveat.** Peter & Ruane's $\approx 4.7$ is substitutability *across material inputs*; they
> find **complementarity** across the broader energy–materials–services nest. Our single $\sigma$
> governs substitution across all intermediate suppliers, so the right anchor depends on whether the
> margin carbon pricing reshuffles is materials-like (substitutable, $\sim$4.7) or energy/services-like
> (complementary, $<1$). Treat the high cell as a *materials-substitution upper bound* rather than a
> universal estimate; a future nested-CES extension would let different input nests carry different
> $\sigma$. Note this concerns **only** the reallocation channel — in the current model abatement is a
> separate technology governed by $\alpha$, not by input substitution, so this caveat does not touch
> the technique channel.

Because we cannot identify $\sigma$ in the Belgian data, the honest framing is: *"reallocation matters
this much if $\sigma$ is this big."* The grid is the deliverable, not a point estimate.

**Abatement elasticity $\alpha$ — calibrated per $(\sigma,\rho)$.** In the current model abatement
is a *standalone technology*, not input substitution: a firm spends resources $\kappa_i$ to lower its
emission intensity, $e_i=(1-\kappa_i)^{\alpha_i}\bar e_i$, where $\bar e_i$ is its no-policy intensity
and $\alpha_i$ is the **abatement elasticity** (`sections/model.tex` §2.1.2). Optimizing $\kappa_i$
against the carbon bill gives the response

$$ \frac{d\log e_i}{d\log p_z} = \frac{\alpha_i}{1-\alpha_i} \;<\;0 \quad (\alpha_i>1). $$

> **$\alpha_i>1$ is required.** With linear abatement cost $\kappa_i$ and $e_i=(1-\kappa_i)^{\alpha_i}\bar e_i$,
> the firm's objective $\kappa + \tau p_z\bar e (1-\kappa)^{\alpha}$ has
> $g''=-\alpha(1-\alpha)\tau p_z\bar e(1-\kappa)^{\alpha-2}$. Only $\alpha>1$ makes it **convex**
> (interior optimum) and makes intensity **fall** with the price. The elasticity *magnitude* is then
> $\alpha/(\alpha-1)>1$ — consistent with the data, where all measured carbon-price elasticities exceed 1.
> *Deferred:* replace the linear cost with a convex abatement cost (e.g. $c\,a^{\beta},\ \beta>1$), the
> more standard MAC shape, which would also admit interior abatement without needing $\alpha>1$.

**Assumption 1 makes $e_i$ independent of the input bundle**, so $\alpha$ and $\sigma$ are *separate*
structural parameters — there is no nesting to resolve and no version of the model in which abatement is
tied to $\sigma$. More than that: the abatement choice **separates** from input choice, so own-firm
intensity $e_i^\*$ is a clean function of $(\tau_i p_z, \alpha_i, \bar e_i)$ **independent of $\sigma$
and $\rho$** even in GE (the FOC is $(1-\kappa_i)^{\alpha-1} = 1/(\tau_i p_z\,\alpha\,\bar e_i)$). $\sigma$
and $\rho$ enter aggregate emissions **only** through how each firm's *scale* $x_i$ is reweighted — i.e.
through the reallocation channel.

**Calibration plan — pin $\alpha(\sigma,\rho)$ by matching the Belgian experience.** Rather than sweep
$\alpha$ independently, we **calibrate** it: for each $(\sigma,\rho)$ on the grid, find the $\alpha$ such
that the globally-solved model reproduces Belgium's observed emission change under the EU ETS,
2005–2022. We then carry the calibrated triple $(\sigma,\rho,\alpha(\sigma,\rho))$ into the
counterfactuals. Three design choices make this well-posed:

1. **What to match — the *within-cell* change ($\approx -24$pp), not the total $-46\%$.** The total
   includes between-sector reallocation ($\approx -20$pp) that [REALLOCATION_FINDINGS.md](REALLOCATION_FINDINGS.md)
   attributes to the **energy transition and the 2008 crisis, not carbon**. Matching the total would
   force $\alpha$ and the reallocation channel to absorb non-carbon structural shifts. The within-cell
   change (within-firm $-25.6$pp + within-sector $\approx 0$) is the carbon-attributable piece the model
   should reproduce.

2. **What price to feed — a single endpoint, because the model is *static and stateless*.** With no
   capital and no abatement stock, emissions in any period depend only on that period's price, so the
   2005–2022 *path* carries no information the model can use: "replicate 2005→2022" collapses to one
   comparative static, **solve at $p_z(2022)$ vs. the 2005 base**. The dynamic-price worry dissolves —
   a stateless model cannot see anything but endpoint prices.

3. **The representative price is a *robustness axis*, not one number.** Statelessness is also the catch:
   real abatement is partly durable investment built up over years of rising prices (Colmer's clean-tech
   channel), and a static model loads all of it onto the terminal *spot* price — **biasing $\alpha$
   downward**. So calibrate at two endpoints and report $\alpha(\sigma,\rho)$ under each:
   **terminal** ($\approx$€80, 2022) → lower-bound $\alpha$; **period-average marginal price**
   ($\approx$€19 over 2005–22) → higher $\alpha$ (verified numbers and 2005 base in the price-levels
   table below). Since $\alpha$ scales roughly inversely with the assumed $\Delta\log p_z$, this brackets
   the bias honestly.

> **Diagnostic — how much does $\alpha$ actually move with $(\sigma,\rho)$?** Because own-abatement is
> $\sigma,\rho$-independent, $\alpha(\sigma,\rho)$ varies **only** to the extent the $(\sigma,\rho)$ pair
> produces reallocation at the calibration price. If it comes out nearly **flat**, reallocation is
> quiescent at 2022 prices — consistent with the data (within-sector reallocation $\approx 0$) — and we
> effectively recover a single $\alpha$. If it **varies a lot**, high $\sigma$ is predicting reallocation
> Belgium did not exhibit, which is itself a finding (tension between high $\sigma$ and the data).

> **Caveat on the moment (still applies).** The within-cell change is *mechanical, not causal* — it
> attributes intensity decline to "technique" regardless of cause, so the calibrated $\alpha$ is an
> **upper bound** on the carbon-driven elasticity, not a clean estimate.

For an initial solver pass before the calibration is wired up, a manual grid
$\alpha\in\{1.5, 2, 4\}$ (elasticity magnitudes $\alpha/(\alpha-1)=3.0 / 2.0 / 1.33$) brackets the
literature-realistic range $\alpha\approx 1.9$–$4.3$ (mapped below).

**Homogeneous $\alpha$ for now.** We use a single $\alpha$ common to all firms in this version
(consistent with "common $\sigma$ first, sector-specific later"). The known limitation: Martinsson finds
**top emitters abate the *least*** (elasticity ~1.3 vs ~6.7 for low emitters), and our regulated firms
*are* top emitters — who dominate aggregate emissions. So a homogeneous $\alpha$ set to a pooled average
**overstates** the abatement of precisely the firms that drive the aggregate. Mitigating factor for our
story: homogeneous $\alpha$ mostly affects the *abatement* term; the *reallocation* sufficient statistic
(the paper's focus) is driven by cross-firm heterogeneity in network-adjusted EI and exposure, not by
$\alpha$ heterogeneity. Cheap first refinement (deferred): a coarse top-emitter-vs-rest split, or
$\alpha_i$ declining in size/$\bar e_i$.

Literature anchors for the eventual disciplining (our ETS firms are *large* emitters, where abatement
elasticities are **lowest**):

| Source | Estimate | Object |
|--------|----------|--------|
| Martinsson et al. (2024), *RFS* | **≈1.30** top-decile emitters (≈2.08 all firms; large heterogeneity) | Emission-intensity elasticity to carbon pricing, Swedish CO₂ tax, 26-yr panel |
| Colmer et al. (2024), *ReStud* | **14–16%** emission reduction / **17.4%** intensity reduction (Phase II); MAC ≤ \$53/ton | Matched DiD, French EU ETS firms |

**Mapping to $\alpha$.** The elasticity *magnitude* is $|d\log e/d\log p_z| = \alpha/(\alpha-1)$ for
$\alpha>1$, so $\alpha = m/(m-1)$ where $m$ is the reported elasticity. Note the inversion: a **lower**
elasticity maps to a **higher** $\alpha$. Martinsson's top-emitter $m\approx 1.3$ implies
$\alpha\approx 1.3/0.3\approx 4.3$; the all-firm $m\approx 2.08$ implies $\alpha\approx 2.08/1.08\approx 1.9$.
(Still map Martinsson's exact regressor — a marginal-cost-share transform — carefully before taking the
number literally. Note $\alpha$ is ill-conditioned for $m$ near 1, where $\alpha\to\infty$.)

**Cross-check on the calibration.** The literature-implied $\alpha$ is a sanity check on the calibrated
$\alpha(\sigma,\rho)$: since our regulated firms are top emitters (low elasticity $\Rightarrow$ high
$\alpha$), and the aggregate is dominated by them, a credible calibration should land toward the
**high** end, $\alpha\approx 4$ (top-emitter), with $\alpha\approx 1.9$ (all-firm) as the other anchor.
If the calibrated value comes out far outside $[1.9, 4.5]$, suspect the price input or the matched
moment. **Still deferred:** heterogeneous $\alpha_i$ (a top-emitter-vs-rest split, or $\alpha_i$
declining in size/$\bar e_i$) — the calibration above pins a single common $\alpha(\sigma,\rho)$.

**Pass-through grid.** The baseline model has full pass-through ($p_i=mc_i$), but the Belgian-data
literature (Amiti–Itskhoki–Konings) finds firms pass through less than 100% of a marginal-cost shock
into their prices. Incomplete pass-through $\rho<1$ means less of the carbon wedge reaches downstream
buyers, so both the propagated abatement incentive and the cross-firm price gaps that drive reallocation
are muted.

We sweep $\rho$ over a **simple even grid $\{0.25, 0.5, 0.75, 1\}$** rather than literature-specific
point values — $\rho$ is a transparency dial, and an even grid makes the comparative static legible.
The empirical literature only needs to justify that $\rho<1$ is the relevant range: Amiti–Itskhoki–
Konings (2019, *RES*; 2014, *AER*) find own-cost pass-through around $0.5$–$0.6$ for large/import-
intensive Belgian firms, and Ganapati–Shapiro–Walker (2020, *AEJ: Applied*) find energy-cost
pass-through around $0.7$ in US manufacturing (the most directly carbon-relevant estimate). So the
realistic mass of the grid is the $0.5$–$0.75$ interior, with $\rho=1$ retained as the model's
full-pass-through benchmark and $\rho=0.25$ as a stress case. Note $\rho$ and $\sigma$ interact: lower
pass-through shrinks the price gaps that a high $\sigma$ would otherwise act on, so the two dials are
not separable and should be reported jointly.

**Reduced-form implementation (reconciling $p_i=mc_i$ with $\rho<1$).** Incomplete pass-through is *not*
captured by $p_i=\rho\,mc_i$ with constant $\rho$: that puts $p_i<mc_i$ at the no-policy baseline
(loss-making) and, in proportional terms, still passes cost *changes* through one-for-one. Instead the
firm passes through only a fraction $\rho$ of its **carbon-induced** cost, leaving the baseline
undistorted ($p_i^0=mc_i^0$):
$$ p_i = mc_i^{\text{non-carbon}} + \rho\,mrc_i, \qquad mrc_i = \tau_i\,p_z\,e_i. $$
The firm absorbs $(1-\rho)$ of its carbon bill in its margin. At $\rho=1$ this is $p_i=mc_i$ (full
pass-through); linearized it is exactly the $\Psi(\rho\,\tau\circ\mathcal E)$ scaling used above
(carbon wedge damped by $\rho$, input-cost pass-through left full — the **non-compounding** choice, which
keeps $\rho$ a single transparent dial). A fully-compounding alternative — every firm passes $\rho$ of
*all* cost changes, giving $d\log\mathbf p/d\log p_z=\rho(I-\rho\Omega)^{-1}(\tau\circ\mathcal E)$ —
attenuates pass-through along the chain; available if wanted, but heavier and less transparent.

### Price levels — calibration endpoints and counterfactual grid

Carbon prices are EUA spot/front-December levels in EUR/tCO₂. Endpoints and post-2018 years are
well-sourced; the quiet middle years (2010–2016) are milestone-anchored estimates (see data caveat).

**Calibration endpoints (the 2005→2022 comparative static):**

| Role | $p_z$ | Basis |
|------|-------|-------|
| **2005 base** | **€18** | Phase-1 launch level; traded €7–30 before the 2006 verified-emissions crash. |
| **2022 terminal** | **€80** | European Commission / EIA; the €95–97 spike in early 2022 was transitory and excluded. |
| **Period-average 2005–22** | **≈€19** (simple mean; "typical realized cost" €15–20) | Heavily pulled down by the 2007 Phase-1 collapse (~€0.7) and the 2012–17 surplus trough. An emissions-weighted average is *even lower* (high prices arrived only after emissions had already fallen). |

The **two-point robustness axis** for the calibration (§3, calibration plan): terminal **€80** → lower
bound on $\alpha$; period-average **≈€19** → higher $\alpha$. The wide gap (€80 vs €19) is the honest
measure of how much the static-model price assumption moves the inferred abatement elasticity.

**Counterfactual high-price grid (round numbers, SCC as reference):** **€100, €150, €200, €250**, with
the realized **€80** as the low column. Round numbers per the design choice; the social cost of carbon
is reported alongside, not used as the anchor:

| Counterfactual $p_z$ | Reference point |
|----------------------|-----------------|
| €80 (realized 2022) | calibration terminal |
| €100 | round "policy-credible" floor above current spot |
| €150 | approaches central SCC |
| €200 | brackets central SCC — Rennert et al. (2022, *Nature*) ≈€170; US EPA 2023 central ≈€175 |
| €250 | EPA low-discount / future-year SCC and the upper tail (EPA range ≈€110–315 across discount rates) |

> **Data caveat (finalize before the paper ships).** The middle-year (2010–2016) annual averages above
> are estimates anchored to sourced milestones, not audited annual means. For the period-average that
> enters the calibration, download the **Ember/Sandbag Carbon Price Viewer CSV** (or the **ICAP
> Allowance Price Explorer** export), average per calendar year, and replace ≈€19 with the audited
> figure. The endpoints (€18, €80) and the counterfactual grid do not depend on this.

*Sources: European Commission "Trends in carbon intensity…"; EIA; EEA EU-ETS price series; Ember/Sandbag
Carbon Price Viewer; Rennert et al. (2022, Nature); US EPA 2023 SCGHG report.*

---

## 4. The targeting schemes

These are the targeting vectors $\tau$ from `THIS_PROJECT.md §3`. Each defines one column of the
counterfactual matrix in [§5](#5-the-counterfactual-matrix).

| # | Scheme | Definition of $\tau$ | Question it answers |
|---|--------|----------------------|----------------------|
| T0 | **Actual EU ETS** (benchmark) | $\tau_i = 1$ iff firm $i$ is an actual EU ETS installation. | What did the realized policy do? (validation + baseline) |
| T1 | **Universal industrial** | $\tau_i = 1$ for all industrial emitters from day 1. | What if all industrial emissions had been priced from the start? |
| T2 | **Centrality-targeted** | $\tau_i = 1$ for the highest-network-centrality firms, holding **the same % of emissions covered** as T0. | For a fixed coverage budget, does targeting the network-central firms beat targeting the biggest emitters? |
| T3 | **Sector-level (all-or-nothing)** | $\tau_i = 1$ for *every* firm in a targeted NACE sector, $0$ otherwise; sectors chosen to match a coverage target. | What is lost/gained by regulating at the sector level instead of the firm level? |

**Coverage normalization.** T2 (and optionally T3) must be defined to hold a coverage statistic fixed
against T0 — otherwise differences confound "who" with "how much." Default normalization: **share of
total (private-sector) emissions covered**. Robustness: share of firms, or share of output. State
explicitly which is held fixed in every table.

**Centrality measure (T2).** Uses the network-centrality object from the model
(`sections/quantitative.tex` — the input-weighted position summarized by the relevant Leontief-inverse
column / the sufficient-statistic weights), **not** raw emissions or raw size. The companion
characterization exercise (`THIS_PROJECT.md §3`) regresses this centrality measure on
policymaker-observables — size, emissions, emission intensity, upstream emissions, sector — to ask how
far a real-world regulator could approximate T2 with observables. That regression is a prerequisite for
T2 being policy-relevant and is tracked as its own deliverable ([§7](#7-open-questions--todos)).

---

## 5. The counterfactual matrix

Every counterfactual is one cell of a **price × targeting** grid; within each cell the structural
parameters are $(\sigma, \rho)$ with $\alpha = \alpha(\sigma,\rho)$ **pinned by the calibration** in
[§3](#3-structural-parameters-plugged-in) (so $\alpha$ is not a free third sweep — it is determined once
$(\sigma,\rho)$ are chosen).

```
                 €80 (realized)    €100        €150        €200   (€250)   (price columns →)
              ┌──────────────────┬──────────┬──────────┬──────────┐
  T0 EU ETS   │  BENCHMARK (§5)  │          │          │          │
  T1 universal│                  │          │          │          │
  T2 central. │                  │          │          │          │
  T3 sector   │                  │          │          │          │
              └──────────────────┴──────────┴──────────┴──────────┘
   each cell → decompose d log Z into {scale, abatement, reallocation},
               over (σ, ρ) with α = α(σ,ρ) pinned by calibration
```

- **Rows** = targeting schemes (dimension B).
- **Columns** = carbon-price levels (dimension A).
- **Within each cell** = the scale/abatement/reallocation decomposition over $(\sigma,\rho)$, with
  $\alpha=\alpha(\sigma,\rho)$ pinned by calibration (§3) — not a free dimension.

**Managing the combinatorics (deadline-aware).** Calibrating $\alpha$ removes a dimension: the
parameter grid is $4\,(\sigma)\times 4\,(\rho)$ with $\alpha$ determined, $\times 2$ for the
representative-price robustness axis (terminal vs. period-average). Still, the full
$\sigma\times\rho\times|\text{price}|\times 4\,(\text{targeting})$ product is large. For the first
version, fix a **default pair** $(\sigma,\rho)=(0.5,1)$ (with its calibrated $\alpha$) for the headline
price×targeting tables, then show **one-at-a-time** sweeps in $\sigma$ and in $\rho$ around that default
(each re-pinning $\alpha$) rather than the full grid. Legible output, bounded runtime, sensitivity still
shown on every dial.

### Benchmark validation

The top-left cell — **T0 at the 2022 price vs. the 2005 base** — *is* the calibration target, and it
does double duty. (i) **Calibration:** for each $(\sigma,\rho)$ we choose $\alpha(\sigma,\rho)$ precisely
so this cell reproduces Belgium's within-cell emission change ($\approx -24$pp; [§3](#3-structural-parameters-plugged-in)).
(ii) **Validation:** with $\alpha$ pinned, check that the model also reproduces the *shape* of the
mechanical decomposition — reduction dominated by abatement, reallocation $\approx 0$ — confirming
consistency with [REALLOCATION_FINDINGS.md](REALLOCATION_FINDINGS.md). The calibration fixes the level by
construction; the validation is that the *channels* line up. If the calibrated $\alpha$ lands far
outside the literature-credible $[0.5,0.7]$, or the model insists on large reallocation at 2022 prices
where the data show none, stop and re-examine before running counterfactuals (per the project's "if
something goes sideways, STOP and re-plan" rule).

---

## 5b. Does first-order centrality survive the global solution?

The centrality measure (and the reallocation sufficient statistic) is a **first-order** object: it
ranks firms by their *marginal* contribution to aggregate emission reduction, evaluated at the
no-policy equilibrium. It is cheap to compute and implementable by a regulator with network data. The
T2 scheme ([§4](#4-the-targeting-schemes)) bets that targeting the high-centrality firms delivers large
reductions. **This exercise tests that bet against the global solve.**

**Procedure.** (i) Rank firms by the first-order centrality / predicted-marginal-reduction measure.
(ii) Form the T2 target set (top firms holding coverage fixed vs. T0). (iii) Solve the CES economy
**globally** under T2 at each price/$\sigma$/$\rho$, and compare the realized global emission reduction
against (a) T0 and (b) alternative target sets (e.g. top-by-emissions, top-by-size, random sets at the
same coverage). The question: **does the first-order ranking still pick the high-reduction firms once
we account for curvature and network interactions?**

**What I expect — and why the answer is informative either way.** A *strong positive but imperfect*
relationship. The first-order ranking should preserve the **direction** and the **coarse ordering**
across very different firms, but I expect **reorderings near the top**, for three reasons:

1. **Curvature in the global response.** The centrality measure is a *marginal* object evaluated at the
   base equilibrium, but global emission reduction is the integral of that margin along the whole price
   path. The abatement technology $e_i=(1-\kappa_i)^{\alpha_i}\bar e_i$ has a constant *local* elasticity
   ($\alpha_i/(1-\alpha_i)$) but a curved *global* response, and its trajectory differs across firms by
   $\alpha_i$ and $\bar e_i$. So a firm that is high-leverage at the base point — large, dirty, central —
   may abate its own intensity away at high prices and cease to be the firm whose reallocation matters
   most, while another firm climbs the ranking. Rank preservation would require homogeneous curvature,
   which the heterogeneity in $(\alpha_i,\bar e_i)$ rules out.
2. **Non-additivity of set targeting.** The centrality measure ranks firms *individually*, but T2
   targets a *set*. Joint interventions interact through the network — taxing $i$ and $j$ together is
   not the sum of taxing each (a Baqaee-2019-style higher-order point) — so the best *set* need not be
   the top-$k$ of the individual ranking.
3. **The sufficient statistic is evaluated at base-period shares.** As shares reallocate under high
   prices, the covariance that governs reallocation moves — and can flip sign for some firms.

**Either outcome is a result.** If the first-order ranking still selects the high-reduction firms in
the global solve, we have license to recommend the cheap, implementable centrality measure as a
targeting device. If it does not, we have shown that network complementarities make *marginal*
centrality misleading for policy design and that the global interaction structure matters — a stronger
argument for the model. Report the rank correlation (Spearman) between first-order predicted reduction
and global realized reduction, and how it degrades as $p_z$ rises (curvature should bite more at high
prices).

---

## 6. Reporting protocol: separating price from targeting

Because moving from the benchmark to any interesting cell changes **both** the price and the targeting,
every headline counterfactual result must be reported as a **path decomposition** that isolates the two
contributions. Use a fixed two-step path from the benchmark to the target cell:

```
   Benchmark                 Step A: price only            Step B: + targeting
 (T0, p_realized)   ──────▶   (T0, p_high)        ──────▶   (Tk, p_high)
                    Δ due to PRICE                Δ due to TARGETING
```

- **Step A (role of higher prices):** hold targeting at T0, raise $p_z$ to the counterfactual level.
  The change in $Z$ (and its abatement/reallocation split) is attributed to **price**.
- **Step B (role of targeting):** hold $p_z$ at the high level, switch targeting from T0 to Tk.
  The additional change is attributed to **targeting**.

Report both steps side by side. Because the two effects are not additively separable in general
(targeting changes the marginal effect of price and vice versa), also report the **reverse path**
(targeting first, then price) so the interaction/path-dependence is visible. If the two paths differ
materially, report the interaction term explicitly rather than burying it.

> This protocol is the operational form of the transparency requirement in
> [§1](#1-motivation-and-the-two-departures-from-the-data): no number in the counterfactual section
> should be reported without making clear how much of it is price and how much is targeting.

---

## 7. Outcome metrics

For every cell (and every step of the §6 decomposition), report:

1. **Aggregate emissions change** $d\log Z$, and its split into **scale / abatement / reallocation**.
2. **The reallocation sufficient statistic** — the input-weighted covariance between policy exposure
   and network-adjusted emission intensity (`THIS_PROJECT.md §1`; sign tells us whether the network
   amplifies or dampens abatement).
3. **Aggregate real output change** $d\log Y$ (expected ≈ 0 at first order under lump-sum rebate; report
   as a check and where the global solution moves it).
4. **Sensitivity to $(\sigma, \rho)$ — with $\alpha$ re-calibrated at each pair** — the one-at-a-time
   sweeps around the default (§5). The $\sigma$ sweep is the headline (Consequence 1); the $\rho$ sweep
   shows how incomplete pass-through mutes both channels. Also report the **calibrated
   $\alpha(\sigma,\rho)$ surface** itself — how flat it is tells the reader how active reallocation was at
   2022 prices (the diagnostic in §3) — and the spread between the terminal-price and period-average-price
   calibrations.

---

## 8. Open questions / TODOs

- [x] **Solution method.** DECIDED: solve the CES economy **globally** for every cell; keep first-order
      objects for interpretation and as the targeting device tested in [§5b](#5b-does-first-order-centrality-survive-the-global-solution).
- [x] **P3 — model-input assembly** — DONE (dev): [analysis/phase4_assemble_model_inputs.R](analysis/phase4_assemble_model_inputs.R)
      assembles the bundle `{Ω, ē, γ, τ, x, ν}` for a base year by reusing the phase2/phase3 network
      logic. Validated on local **downsampled** 2019 (1,186-firm ETS+neighbors subset; ETS = 97% of
      subset emissions). Cost base = no-carbon, closed (inputs+labor); ē = z/total_cost = E_i under μ=1.
- [x] **P4 — data scope / base year** — DECIDED: final = full private-sector panel (run on RMD with full
      B2B); dev = ETS + direct B2B neighbors subset; base year **2019** (2005 as robustness).
- [x] **RMD runner built** — [analysis/run_counterfactuals_rmd.R](analysis/run_counterfactuals_rmd.R):
      self-contained, chains assembly (full B2B) → solver → calibrate $\alpha(\sigma,\rho)$ →
      price×targeting matrix; writes `cf_calibrated_alpha.csv`, `cf_alpha_robustness.csv`, `cf_results.csv`.
      **Paths auto-detect by machine** via `utils/paths.R` (jardang=RMD, jota_=local) — no editing needed.
      Direct sparse solve for ν; `gc()` between cells (the repeated SuiteSparse LU faults without it).
      Default `SCOPE="ets_neighbors"` (clean + tractable on full B2B); `"full"` for the whole panel.
      Smoke-tested locally end-to-end (completes, writes CSVs; correctly reports target-unreachable on
      the downsampled data).
- [ ] **Run it on RMD** with full B2B: `git pull`, then `Rscript analysis/run_counterfactuals_rmd.R`
      (paths auto-resolve). On clean data the calibration brackets → real $\alpha$ and counterfactual
      numbers; the ē outliers / negative-final-demand artifacts disappear.
- [ ] **(Deferred refactor) single network builder.** phase2/phase3/phase4 each construct the
      cost-share matrix separately. Consolidate into one builder that saves the **raw** expenditure
      matrix + cost components (inputs/wages/carbon split), so every consumer re-normalizes from the same
      ingredients (e.g. no-carbon base at the no-policy point). *Redundancy audit conclusion:* we do
      **not** redundantly form the Leontief inverse — Ψ is only ever applied to vectors, and that
      operator is reused (not recomputed) across the descriptive and counterfactual code; the only new
      heavy compute is the equilibrium solver. phase4 now reuses `ev_s2` for emissions.
- [x] **Solver Piece 1 (abatement + prices)** — DONE: [analysis/phase5_model_solver.R](analysis/phase5_model_solver.R).
      Closed-form abatement ($\alpha>1$) + price fixed point on the bundle. Validated: at $p_z=0$ prices
      return to 1 and $e$ to $\bar e$ exactly; abatement and prices respond sensibly; lower $\rho$ shrinks
      the price rise. (Noted: linear-cost abatement has an extensive-margin threshold $\tau p_z\bar e\alpha\ge1$.)
- [x] **Solver Piece 2 (quantities + market clearing)** — DONE (Cobb-Douglas final demand): CES
      expenditure shares → Leontief sales solve $s=(I-M')^{-1}bE$, labor market pins the scale, $b$
      calibrated from base sales. Produces $x_i$, $Z=\sum_i e_i x_i$, and the decomposition. **Linear
      algebra validated to machine precision** (base reproduces observed $x$, max rel diff $4e{-}14$).
      *Aggregate numbers not yet trustworthy on the downsampled subset* — two pure data artifacts that
      vanish on full data: (i) ~13% of sales mass has negative final demand (within-subset intermediates
      > downsampled revenue) clipped to 0; (ii) 4 ē outliers from the partial cost base dominate the
      emission-weighted technique term. Both resolve with full revenue + full B2B on RMD.
- [x] **$\alpha$-inversion wrapper** — DONE: `calibrate_alpha()` (uniroot on `full_solve`'s $\Delta\log Z$,
      framing exposed via `p_lo`/`p_hi`). Validated by **self-recovery** (recovers a known $\alpha^\*$
      from its own $\Delta\log Z$ to 3 decimals). On the dirty subset the real $-24$pp target is
      correctly reported as out-of-range — resolves with clean data. **Solver toolchain is now
      code-complete** (phase4 assemble → phase5 abatement/prices/quantities/$\alpha$-inversion).
- [x] **Pin the $\sigma$ grid** — DONE: $\{0.034, 0.5, 1, 4.7\}$ from Atalay (2017), Baqaee–Farhi (2019),
      Cobb–Douglas, and Peter & Ruane (2023), verified from primary sources. $\sigma$ enters reallocation
      linearly (scales it; covariance sets the sign). Still TODO: decide whether to allow sector-specific
      $\sigma$.
- [x] **Pin the pass-through $\rho$ grid** — DONE: even sweep $\{0.25, 0.5, 0.75, 1\}$; empirics (AIK,
      GSW) place the realistic range in the $0.5$–$0.75$ interior. **Modeling form DONE:** firm passes a
      fraction $\rho$ of its carbon bill, $p_i=mc_i^{\text{non-carbon}}+\rho\,mrc_i \Rightarrow
      \Psi(\rho\,\tau\circ\mathcal E)$ (non-compounding; compounding variant noted as alternative).
- [x] **Abatement elasticity $\alpha$** — DECIDED: **calibrate** $\alpha(\sigma,\rho)$ per $(\sigma,\rho)$
      to match Belgium's 2005–2022 **within-cell** change ($\approx -24$pp), via a 2005→2022 endpoint
      comparison (static, stateless model ⇒ path collapses to endpoints). Representative price is a
      two-point robustness axis (terminal ≈€80 vs. period-average ≈€19). Requires $\alpha>1$ (convex
      abatement); cross-check the calibrated $\alpha$ against the literature range $[1.9, 4.5]$
      (elasticity $\alpha/(\alpha-1)$; lower elasticity ⇒ higher $\alpha$). *Deferred:* heterogeneous
      $\alpha_i$; and a convex abatement cost (more standard MAC shape, removes the $\alpha>1$ requirement).
- [x] **Prices DONE (provisional):** 2005 base **€18**, 2022 terminal **€80**, period-average **≈€19**;
      counterfactual grid **€100/€150/€200/€250** (SCC ≈€170–175 as reference). One finalization step
      below.
- [ ] **Replace the milestone-estimated middle-year EUA averages** (2010–2016) with audited calendar-year
      means from the Ember/Sandbag CSV or ICAP export, then re-confirm the ≈€19 period-average.
- [ ] **(Optional) nested-CES on the reallocation side** — only to let different *input* nests carry
      different $\sigma$ (materials vs. energy/services; Peter & Ruane caveat). **Not** needed for
      $\alpha$/$\sigma$ separation — that is already built into the current model.
- [ ] **§5b rank-preservation check** — Spearman correlation between first-order predicted reduction and
      global realized reduction, by price level.
- [x] **Price grid DONE:** €80 (realized) → €100 → €150 → €200 (→ €250), round numbers with SCC as
      reference ([§3](#3-structural-parameters-plugged-in), price-levels table).
- [ ] **Centrality characterization regression** — regress the centrality measure on
      size/emissions/intensity/upstream-emissions/sector; needed before T2 is policy-relevant ([§4](#4-the-targeting-schemes)).
- [ ] **Coverage normalization** — lock the statistic held fixed across T0/T2/T3 ([§4](#4-the-targeting-schemes)).
- [x] **Data scope** — DECIDED (see P4 above): full private-sector panel for the final runs so leakage
      to untargeted competitors is in the model; ETS+neighbors subset for development. *Scope limit to
      record:* the model is closed, so imports are dropped — no cross-border (import) leakage channel,
      only domestic reallocation.
- [ ] **Price–wedge robustness** — handled by the $\rho$ sweep (the current model prices at $p_i=mc_i$,
      so there are no markups to vary; markups would be a separate model extension).

---

*Companion files:* [REALLOCATION_FINDINGS.md](REALLOCATION_FINDINGS.md) (model-free benchmark),
[THIS_PROJECT.md](THIS_PROJECT.md) (project overview), [paper/sections/model.tex](paper/sections/model.tex)
(model, decomposition, CES sufficient statistic, and centrality measure referenced above).*

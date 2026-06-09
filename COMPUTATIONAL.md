# Computational Methods: Large Shocks and Exact Decomposition in Production Networks

This note explains (1) the computational problem of solving and decomposing **large** shocks in
CES production-network economies, (2) **Baqaee & Farhi's (2024, *Networks, Barriers, and Trade*)**
solution — *differential exact-hat algebra* — and how it improves on standard exact hat algebra, and
(3) **how we apply the idea** in this project's carbon-network solver. Reading notes for the BF paper,
with verbatim equations and page references, are in
[articles/split_baqaee_farhi_networks_barriers_trade/notes.md](articles/split_baqaee_farhi_networks_barriers_trade/notes.md).

Equations are shown in plain (monospaced) form, not LaTeX.

---

## 1. The problem: large shocks don't linearize

In a CES network, the first-order (Theorem-3-style) response of every price and quantity to an
infinitesimal shock is a clean formula in network objects (Domar weights, the Leontief inverse, an
input-output covariance operator). But a **large** shock — a carbon price going from €0 to €250, a 60%
iceberg cost — is not infinitesimal, so:

- A **one-shot first-order approximation** is biased (BF Figure 5: the first-order RMSE in log welfare
  grows roughly linearly in shock size, reaching ~0.02 dollar-weighted / ~0.03 unweighted at a shock
  "intensity" of 20).
- The **decomposition** of the total change into channels (here: technique vs. reallocation) is
  *path-dependent* for large changes, so a naive base-year-weighted (Laspeyres) split has an
  interaction residual that can dominate and even **flip the sign** of a channel (see §4).

So we need a method that (a) computes the **exact** nonlinear response and (b) **decomposes** it into
channels exactly, with no residual.

---

## 2. Standard exact hat algebra (the baseline) — Dekle–Eaton–Kortum (2008)

The trade literature handles large shocks with **exact hat algebra**: write the *entire* general
equilibrium in changes ("hats", `x̂ = x'/x`) and solve the resulting **nonlinear system in one shot**
with a solver like `fmincon` or Knitro.

```
   Solve once:   the full (C+N+F) x (C+N+F) NONLINEAR equilibrium system
                 (C countries, N goods, F factors)
```

BF document its limitations (Appendix G):

- **Explosive scaling.** Time grows steeply in the number of variables: `fmincon` ≈ **4.5 hours at
  ~350 variables**; Knitro ≈ **5 hours at ~1400 variables** — *even with analytical Jacobians supplied*.
- **Memory.** Their machine "cannot solve the factor-specific version of the model … due to
  insufficient memory."
- **Non-convergence under strong complementarities.** Once the elasticities of substitution are
  **lowered below 0.2**, `fmincon` and Knitro "fail to find a solution at all."
- Net: "computationally impracticable for large models."

This matters for us: our calibrated abatement problem and complements-regime σ (0.034, 0.5) are exactly
the low-elasticity territory where one-shot nonlinear solvers struggle.

---

## 3. BF's improvement: differential exact-hat algebra

BF make hat algebra exact a different way — by **"chaining together infinitesimal effects."** View the
first-order characterization (their Theorem 3) as a **system of ODEs in the shock parameter** and
integrate it numerically (Euler or Runge–Kutta) along the shock path:

```
   Discretize the shock into many small steps.  At each step, on the CURRENT (updated) data:
     1. Solve a SMALL  (C+F) x (C+F)  LINEAR system for d log Λ (factor income shares).
     2. Recover d log p, d log λ, d log χ, d log y from Theorem 3.
     3. UPDATE the IO matrices (Ω̃, Ψ̃, Ω, Ψ) and shares, then take the next step.
```

**Why this is exact, not an approximation** (BF, Appendix G, verbatim): Figure 5's first-order error
"is not relevant for differential exact-hat algebra … because once we iterate on the first-order
approximation, it becomes exact." A single Euler step has O(Δ²) truncation error, but because the
**matrices are re-evaluated/updated between steps**, the scheme converges to the true nonlinear solution
as the step size → 0. It integrates the *exact differential* of the equilibrium map, rather than
truncating a Taylor series of the answer.

**The exact between-step update rules** (BF Appendix F), all built from the same input-output covariance
operator `Cov_Ω̃^(k)(·,·)` that drives Theorem 3:

```
   dΩ̃_ij = (1 − θ_i) · Cov_Ω̃^(i)( d log p , I_(j) )
   dΨ̃_ij = Σ_k Ψ̃_ik (1 − θ_i) · Cov_Ω̃^(k)( d log p , Ψ̃_(j) )
   dΩ_ij  = μ_i^{-1} dΩ̃_ij − d log μ_i
   dΨ_ij  = Σ_k Ψ_ik μ_k^{-1}(1 − θ_k) Cov_Ω̃^(k)( d log p , Ψ_(j) )
            − Σ_k Ψ_ik (Ψ_kj − 1_{k=j}) d log μ_k
```

**The key efficiency claim:** each step is a **small `(C+F)×(C+F)` linear solve**, instead of the big
`(C+N+F)×(C+N+F)` nonlinear solve — the N goods are recovered *after* the factor-share solution. Result
(Figure 6): differential hat-algebra scales **near-linearly** (~1 hour at ~1400 variables) where Knitro
is super-linear (~5 hours), and **it keeps working below θ < 0.2** where the nonlinear solvers fail.

### The three-way distinction

| Method | Exact? | Mechanism | Cost |
|--------|--------|-----------|------|
| **(i) one-shot first/second-order** | **No** — error grows with shock size | apply Theorem 3 once; no updating | cheapest, biased for big shocks |
| **(ii) differential exact-hat algebra** (BF) | **Yes** — exact as step→0 | integrate Theorem 3 along the path; small `(C+F)` linear solve + matrix update each step | many cheap linear solves; near-flat scaling; robust to θ→0 |
| **(iii) standard exact hat algebra** (DEK) | **Yes** | one big `(C+N+F)` nonlinear solve | explosive scaling; OOM; fails below θ<0.2 |

(i) integrates nothing — it evaluates the derivative once at the initial point. (ii) and (iii) reach the
*same* nonlinear equilibrium; (ii) by path-integration of small linear systems, (iii) by one big
nonlinear root-find.

---

## 4. How we apply it

### 4.1 Our setting maps onto BF
- **Shock:** the carbon price `p_z : 0 → target` (in place of tariffs/iceberg costs).
- **Channels:** aggregate emissions `Z = Σ_i e_i x_i` split into **technique** (intensity `e_i`) and
  **reallocation** (physical output `x_i`). Physical output is BF's Corollary 3 object,
  `d log y_i = d log λ_i − d log p_i` — so reallocation runs on *quantities*, and its sign is governed
  by the input-output covariance, not by σ vs. 1 (σ scales the magnitude).

### 4.2 One deliberate departure (and why it's still BF-consistent)
BF's *differential* method re-solves a **small linear** system per step and Euler-updates the matrices —
a **speed** device to avoid the big nonlinear solve. **We don't need that speedup:** our equilibrium
solver is an Anderson-accelerated fixed point that is already fast *and* robust at low σ (it doesn't
suffer the θ<0.2 failure). So we keep BF's **path-integration principle for the decomposition** but
compute the **exact equilibrium at each node** (re-solve, no Euler drift) and finite-difference the
channels between nodes. As the step → 0 this converges to BF's analytic integral; the only difference is
we integrate *realized* equilibrium changes (exact nodes) instead of Euler-stepped first-order changes —
which is, if anything, more robust.

### 4.3 The decomposition (exact, residual-free)
`Z = Σ_i e_i x_i`, so at every point on the path the identity
`d log Z = Σ_i (z_i/Z)(d log e_i + d log x_i)` holds exactly. Integrating each channel along the
economic path `p_z : 0 → target`, with **current** (not base-year) weights:

```
   technique     =  ∫₀^target  Σ_i (z_i/Z) · d log e_i   dp_z
   reallocation  =  ∫₀^target  Σ_i (z_i/Z) · d log x_i   dp_z
   technique + reallocation = d log Z        (exactly — fundamental theorem of calculus)
```

Discretized (`decompose_path` / `decompose_path_grid` in
[analysis/phase5_model_solver.R](analysis/phase5_model_solver.R)): grid `p_z = 0, Δ, …, target`; solve
the exact equilibrium at each node; between adjacent nodes use **multiplicative log-mean (LMDI) weights**
`w_i = L(z_i¹, z_i⁰) / L(Z¹, Z⁰)`, where `L(a,b) = (a−b)/(log a − log b)`. These weights make each step's
split sum to that step's `Δlog Z` **exactly**, so the totals sum to `Δlog Z` for *any* number of steps
K, and as K grows the split converges to the structural Divisia integral.
- **K = 1** reduces to a two-point (endpoint) LMDI.
- **K large** is the path integral.
- The **K=1-vs-large gap** measures genuine path dependence (does the covariance evolve along the path =
  real global nonlinearity), separate from the base-weight artifact.

`decompose_path_grid` solves **one** fine grid `0 → max(price)` and reports the cumulative decomposition
at every node, so all price columns of a counterfactual come from a single path of solves.

### 4.4 What it fixed (the headline)
The base-weight (Laspeyres) split — `technique = Σ (z_i^base/Z^base) Δlog e_i`, `reallocation = ΔlogZ −
technique` — gave a **positive, growing** reallocation (network "dampens" abatement). The exact
path-integral gives a **negative** reallocation (network **amplifies**), the covariance-pinned sign:

```
   (illustrative, σ=0.5, on the dev subset)
   p_z      base-weight realloc      path-integral realloc      residual
   €80          +0.38                    −0.65                   ~1e-16
   €250         +1.03                    −1.01                   ~1e-16
```

The positive sign was almost entirely a **base-weight artifact**; the residual of the path-integral
split is machine-zero, confirming it is exact. This is the principled replacement for the ad-hoc
Laspeyres/LMDI patch and is consistent with BF's method.

### 4.5 Cost and robustness
A counterfactual decomposition costs `K+1` equilibrium solves (K ≈ `max_price / PATH_STEP`, e.g. 26 for
€10 steps to €250). At the RMD per-solve cost (~2 s) that is ~1 minute per (scheme × calibrated cell).
**Step-size robustness** should be checked once on real data (compare PATH_STEP = 10 vs 5); the *sum* is
exact at any step, only the *split* refines.

---

## 5. BF-consistency checklist / open items
- ✅ Decomposition integrates the channels along the shock path (BF differential principle); exact,
  residual-free; reallocation runs on physical output (Corollary 3); sign set by the covariance.
- ✅ Robust at low σ (Anderson solver doesn't hit the θ<0.2 nonlinear-solver failure BF flag).
- ⬜ *Optional, fuller BF fidelity:* integrate the **analytic** first-order channels (the covariance
  sufficient statistic / Theorem-3 marginals and Appendix-F matrix updates) rather than
  finite-differencing realized equilibria. Equivalent in the limit; would tie the reallocation term
  directly to the paper's covariance operator. Not needed for the corrected sign/magnitude.
- ⬜ Pin **step size** (PATH_STEP) by a convergence check on the real data.
- ⬜ Two indexing ambiguities flagged in the BF notes (the `(1−θ_i)` vs `(1−θ_k)` index in the `dΨ̃`
  rule; the `(C+F)` vs `(C+CF)` system-size statement) — only matter if we implement the analytic
  matrix-update version above.

*Sources:* BF (2024) §4 (Theorem 3, covariance operator), §4.3 (differential exact-hat algebra),
Appendix D (code), Appendix F (update rules), Appendix G (efficiency: Figs 5–6, the θ<0.2 result).
Full reading notes: [articles/split_baqaee_farhi_networks_barriers_trade/notes.md](articles/split_baqaee_farhi_networks_barriers_trade/notes.md).

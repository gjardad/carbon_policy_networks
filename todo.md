# TODO — Carbon Policies in Production Networks

Canonical project task list. Domain-specific checklists also live in:
- [COUNTERFACTUALS.md](COUNTERFACTUALS.md) §8 — counterfactual pipeline / solver / calibration checklist.
- [tasks/lessons.md](tasks/lessons.md) — recurring-mistake lessons.

Convention: `- [ ]` open, `- [x]` done. Keep one bold lead per item.

## Sufficient statistics for the reallocation channel

From [paper/thesis/notes/technique_vs_reallocation.tex](paper/thesis/notes/technique_vs_reallocation.tex).

- [ ] **Port the results into the model section** — incorporate the propositions from
      [technique_vs_reallocation.tex](paper/thesis/notes/technique_vs_reallocation.tex) into
      [model.tex](paper/thesis/sections/model.tex) as corollaries to the Theorem: (Object 1) the *sign*
      of reallocation via the within-sector covariance of direct and upstream intensity
      ($\mathbb V(\bar e)+\mathrm{Cov}(\bar e,u)$; partial-coverage form
      $\bar\varphi\,\mathbb V(\Psi_e)+\overline{\Psi_e}\,\mathrm{Cov}(\varphi,\Psi_e)+\text{coskew}$),
      and (Object 2) the *relevance* ratio $R=(\sigma/\alpha^2)\,\bar{\mathbb V}/\mathbb E_\omega[\bar e^2]$.
- [ ] **Measure the derived objects in Belgian data** — directly compute the sufficient-statistic
      ingredients on the firm-to-firm data (RMD for magnitudes, downsampled for the algebra check):
      within-buyer variance of network-adjusted intensity $\bar{\mathbb V}$, mean-square direct intensity
      $\mathbb E_\omega[\bar e^2]$ (hence $R$), the within-sector $\mathrm{Cov}(\bar e,u)$ (sign), and the
      coverage–dirtiness alignment $\mathrm{Cov}(\varphi,\Psi_e)$ (partial coverage).
- [ ] **Add the measurements to the quantitative section and link to the full-model simulation** —
      report $R$ and the sign objects alongside the global counterfactual, and verify the sufficient
      statistics predict the simulated technique/composition split (e.g. $R$ vs the $\approx$12/88
      reallocation share) and flag the backfire firms.
- [ ] **Rewrite the intro around the sufficient statistics** — (a) when describing the model, state that
      we derive sufficient statistics giving both the *sign* and the *quantitative relevance* of the
      reallocation channel; (b) explain correctly why in Belgium technique $\gg$ reallocation
      (own-abatement dominance / dispersion stranded by near single-sourcing), superseding the
      intensity-share framing in `introduction_alternative3.tex`.

## Centrality and the global solution

- [ ] **§5b rank-preservation check** — verify the first-order centrality ranking survives the global
      solution: Spearman correlation between the first-order predicted emission reduction and the global
      realized reduction, by price level. Currently
      [quantitative.tex](paper/thesis/sections/quantitative.tex) computes centrality via the *global*
      solve ($\tau_j=1$ per firm), while the intro claims the *first-order* sufficient statistics are used
      to rank firms — so either run the check to justify the first-order objects as the ranking device, or
      reconcile the intro language to "global centrality." (Migrated from COUNTERFACTUALS.md §8.)

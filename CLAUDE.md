## Project Description

See `THIS_PROJECT.md` for a full description of the project, its research questions, and data sources.

## Hardware Setup

Three desktops are used in this project: local 1, local 2, and RMD (remote desktop).

- **RMD**: Has the full NBB data (B2B, Customs, Annual Accounts, PRODCOM). Accessible only via VPN through local 2. No web browser, but connected to GitHub.
- **Local 1**: Personal desktop with Claude Code and Cursor. Has a **downsampled** version of B2B and Annual Accounts data (not all NBB data is downsampled) and the **full training sample** (copied from RMD via local 2 → cloud → local 1).
- **Local 2**: Bridge machine with VPN access to RMD and regular web browser.

When copying files from RMD to local 1: RMD → local 2 → cloud (Dropbox/Claude) → local 1.

## Cross-Project References: `inferring_emissions/` and `facts-emissions-across-network/`

**HARD CONSTRAINT:** When reading anything from `inferring_emissions/`, ONLY read files under `paper/winter26_version/`. When reading anything from `facts-emissions-across-network/`, ONLY read the paper or documentation files. Do NOT read code, analysis scripts, or earlier paper versions from either project — they may be outdated. This applies to all subagents and exploration tasks. Pass this constraint explicitly when delegating.

**EXCEPTION:** You MAY read code files from `inferring_emissions/` (including `analysis/`, `preprocess/`, `utils/`) or from `facts-emissions-across-network/` when the explicit purpose is to copy or directly adapt that code into this project. This exception exists because this project is meant to be standalone — all necessary code from either reference project should be copied here so that someone can reproduce results without needing those projects.

## Paper / Writing — ALWAYS work in `paper/thesis/`

**HARD RULE:** The live, compiled paper is the thesis build under `paper/thesis/`. **ALWAYS edit
`paper/thesis/sections/*.tex`** — never `paper/sections/*.tex`, which is a stale parallel copy the
compiled paper does not use. (`paper/thesis/main.tex` is the master; it `\input`s `thesis/sections/...`.)
I once wrote an entire section into `paper/sections/quantitative.tex` by mistake; do not repeat it.

When writing, **match the thesis's existing notation and conventions — the model is fully written in
`paper/thesis/sections/model.tex`; read it before adding equations.** Specifically for this build:
- No `cleveref`: use `\ref` with manual prefixes (`Table~\ref`, `Figure~\ref`, `Section~\ref`).
- Custom macros: `\N=\mathcal{N}`, `\U=\mathcal{U}`. Sets: producers `\N`, inputs `\U`, consumption `\{C\}`.
- Notation: input-output covariance operator `\mathbb{C}_{\Omega_{(i,:)}}`; network-adjusted EI `\Psi_e`;
  baseline intensity vector `\overline{\mathcal{E}}` (firm `\overline{e}_i`); price response `\frac{d\log\mathbf{p}}{dp_z}`;
  Leontief `\Psi=(I-\Omega)^{-1}`. The CES section label is `subsec: ces` (with a space).
- Assets: tables in `paper/thesis/tables/`, figures in `paper/thesis/figures/` (PNG, not PDF — `*.pdf`
  is gitignored), bibliography `paper/thesis/sections/refs.bib`.
- Compile-test with a minimal standalone wrapper (define `\N`,`\U`; load amsmath/amssymb/natbib/booktabs/multirow);
  check for `! ` errors and Overfull \hbox > ~3pt.

The paper is a git submodule (`paper/`, repo `carbon_policy_in_networks_paper`, synced to Overleaf):
commit inside `paper/`, `git pull --rebase origin main`, push; then in the superrepo `git add paper`
to bump the pointer. End commit messages with the Co-Authored-By line.

## Workflow Orchestration

### 1. Plan Node Default
- Enter plan mode for ANY non-trivial task (3+ steps or architectural decisions)
- If something goes sideways, STOP and re-plan immediately - don't keep pushing
- Use plan mode for verification steps, not just building
- Write detailed specs upfront to reduce ambiguity

### 2. Subagent Strategy
- Use subagents liberally to keep main context window clean
- Offload research, exploration, and parallel analysis to subagents
- For complex problems, throw more compute at it via subagents
- One task per subagent for focused execution

### 3. Self-Improvement Loop
- After ANY correction from the user: update `tasks/lessons.md` with the pattern
- Write rules for yourself that prevent the same mistake
- Ruthlessly iterate on these lessons until mistake rate drops
- Review lessons at session start for relevant project

### 4. Verification Before Done
- Never mark a task complete without proving it works
- Diff behavior between main and your changes when relevant
- Ask yourself: "Would a staff engineer approve this?"
- Run tests, check logs, demonstrate correctness

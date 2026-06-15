###############################################################################
# profile_solver.R  —  profile the CES solver (fast-code-macro practice).
#
# Run this whenever changing the solver, to see where time actually goes before
# optimizing. Reports per-solve wall time and an Rprof self-time breakdown.
#   Rscript analysis/profile_solver.R
###############################################################################
suppressMessages({ library(Matrix); library(dplyr) })

.cand <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
           "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.cand[file.exists(.cand)][1])
source(file.path(project_root, "analysis", "model_assembly.R"))
source(file.path(project_root, "analysis", "phase5_model_solver.R"))

YEAR <- 2019; SCOPE <- "ets_neighbors"; N <- 20
SB <- 0.1; SW <- 2.5; RHO <- 0.7; ALPHA <- 2                    # nested-CES baseline
# assemble the bundle from raw data (no pre-saved file needed on RMD)
b <- assemble_bundle(YEAR, SCOPE, proc_data, out_data); b <- build_nest(b)
bshare <- base_final_shares(b)
cat(sprintf("Bundle: %d firms, %d ETS\n", length(b$gamma), sum(b$tau)))

invisible(full_solve(80, SB, SW, RHO, ALPHA, b, bshare))        # warm up
t0 <- Sys.time()
for (i in seq_len(N)) invisible(full_solve(80, SB, SW, RHO, ALPHA, b, bshare))
per <- as.numeric(difftime(Sys.time(), t0, units = "secs")) / N
cat(sprintf("\nWall time: %.3f s per full_solve (n=%d)\n", per, length(b$gamma)))
s1 <- solve_prices(80, SB, SW, RHO, ALPHA, b)
cat(sprintf("Price solve: %d iters to converge=%s\n", s1$iters, s1$converged))

pf <- tempfile()
Rprof(pf, interval = 0.005)
for (i in seq_len(N)) invisible(full_solve(80, SB, SW, RHO, ALPHA, b, bshare))
Rprof(NULL)
cat("\n=== Rprof self-time (top 12) ===\n")
print(head(summaryRprof(pf)$by.self, 12))
unlink(pf)

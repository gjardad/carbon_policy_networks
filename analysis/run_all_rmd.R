###############################################################################
# run_all_rmd.R  —  run the WHOLE quantitative pipeline sequentially, unattended.
#
#   Rscript analysis/run_all_rmd.R              (optionally: ... > run_log.txt 2>&1)
#
# Order: diagnose_reallocation -> phase6_centrality -> run_counterfactuals_rmd
#        -> proxy_horserace -> make_cf_tables -> characterize_centrality.
#
# - DELETES the cached bundle first, so the re-assembly picks up the current
#   cost base (60/61 + 62, incl. imports) and the nace4d nest. The cached
#   model_inputs_*.RData may predate these and the in-script guard only checks
#   nace4d presence, not the cost-base version -- so we force a clean rebuild.
# - Runs each step in a FRESH Rscript process (no state leak between scripts).
# - Stops at the first failure and reports which step and the exit code.
# - Prints per-step wall time. Machine-agnostic (paths via utils/paths.R).
###############################################################################

.src <- c("C:/Users/jardang/Documents/carbon_policy_networks/utils/paths.R",
          "c:/Users/jota_/Documents/carbon_policy_networks/utils/paths.R")
source(.src[file.exists(.src)][1])                 # project_root, output_dir
script_dir <- file.path(project_root, "analysis")
rscript <- file.path(R.home("bin"), if (.Platform$OS.type == "windows") "Rscript.exe" else "Rscript")

# Force a clean bundle rebuild (new cost base / nace4d).
cache <- file.path(output_dir, sprintf("model_inputs_%d_%s.RData", 2019, "ets_neighbors"))
if (file.exists(cache)) { file.remove(cache); cat("Deleted stale bundle cache:", cache, "\n") }

steps <- c("diagnose_reallocation.R",      # cost-share tail + per-firm reallocation attribution
           "phase6_centrality.R",          # per-firm centrality -> cf_centrality.csv
           "run_counterfactuals_rmd.R",    # 3 schemes -> cf_results.csv
           "proxy_horserace.R",            # observable-ranking race -> cf_horserace*.csv
           "make_cf_tables.R",             # cf_ets_decomp / price_escalation / schemes_compare .tex
           "characterize_centrality.R")    # descriptives, regression, centrality_plane.png

cat(sprintf("Pipeline: %d steps. Rscript = %s\nStart: %s\n\n",
            length(steps), rscript, format(Sys.time(), "%Y-%m-%d %H:%M:%S")))
t_all <- Sys.time()
for (s in steps) {
  cat(sprintf("================ %-26s %s ================\n", s, format(Sys.time(), "%H:%M:%S")))
  t0 <- Sys.time()
  code <- system2(rscript, shQuote(file.path(script_dir, s)))
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "mins")), 1)
  if (code != 0L) {
    cat(sprintf("\n!! FAILED at %s (exit %d) after %.1f min. Stopping.\n", s, code, dt))
    quit(status = 1L)
  }
  cat(sprintf("---------------- done %s in %.1f min ----------------\n\n", s, dt))
}
cat(sprintf("ALL %d STEPS COMPLETE in %.1f min. Outputs in: %s\n",
            length(steps), as.numeric(difftime(Sys.time(), t_all, units = "mins")), output_dir))
cat("Then: commit output_rmd/ and push so the local side can pull.\n")

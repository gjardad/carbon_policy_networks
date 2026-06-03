###############################################################################
# utils/paths.R
#
# Centralized path resolution for analysis/phase3_*.R scripts.
# Sets the following variables in the calling environment:
#   project_root  -- absolute path to the carbon_policy_networks repo
#   nbb_data      -- absolute path to the NBB data root (parent of "processed/")
#   proc_data     -- file.path(nbb_data, "processed")
#   out_data      -- file.path(project_root, "data", "processed")
#
# Add a new (user, machine) entry to KNOWN_USERS below if you run this on a
# new machine.
###############################################################################

KNOWN_USERS <- list(
  # RMD: code on C:, data on X:
  jardang = list(
    project_root = "C:/Users/jardang/Documents/carbon_policy_networks",
    nbb_data     = "X:/Documents/JARDANG/data"),
  # Local 1
  jota_ = list(
    project_root = "c:/Users/jota_/Documents/carbon_policy_networks",
    nbb_data     = "c:/Users/jota_/Documents/NBB_data")
)

.user <- tolower(Sys.info()[["user"]])
if (!.user %in% names(KNOWN_USERS))
  stop("Unknown user '", Sys.info()[["user"]], "'. Add an entry to ",
       "utils/paths.R :: KNOWN_USERS.")

project_root <- KNOWN_USERS[[.user]]$project_root
nbb_data     <- KNOWN_USERS[[.user]]$nbb_data
proc_data    <- file.path(nbb_data, "processed")
raw_data     <- file.path(nbb_data, "raw")
out_data     <- file.path(project_root, "data", "processed")

# output_dir: scripts write trackable artifacts here (CSVs, .tex tables).
# Split by machine so RMD and local-1 outputs never mingle; both directories
# are tracked in the parent repo. To publish a table into the paper, copy
# from output_{rmd,local}/tables/ into paper/tables/ (and paper/thesis/tables/).
output_dir <- if (.user == "jardang") {
  file.path(project_root, "output_rmd")
} else {
  file.path(project_root, "output_local")
}
dir.create(file.path(output_dir, "tables"),  recursive = TRUE, showWarnings = FALSE)
dir.create(file.path(output_dir, "figures"), recursive = TRUE, showWarnings = FALSE)

if (!dir.exists(project_root))
  stop("project_root not found: ", project_root,
       " (user = ", Sys.info()[["user"]], ")")
if (!dir.exists(nbb_data))
  stop("nbb_data not found: ", nbb_data,
       " (user = ", Sys.info()[["user"]], ")")
dir.create(out_data, recursive = TRUE, showWarnings = FALSE)

rm(.user)

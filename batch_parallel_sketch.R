# Assume: io_matrix is a sparse matrix (class dgCMatrix)
#         V is a sparse matrix (nrow = n_firms, ncol = n_targeted_firms)

# STEP 1: Close all open connections
closeAllConnections()

# STEP 2: Define batch size
batch_size <- 500
n_cols <- ncol(V)
batches <- split(1:n_cols, ceiling(seq_along(1:n_cols) / batch_size))

# STEP 3: Setup cluster (use at most 4 cores to avoid exhausting connections)
n_cores <- min(detectCores() - 1, 4)
cl <- makeCluster(n_cores)

# Export required objects and functions to workers
clusterExport(cl, varlist = c("io_matrix", "V"), envir = environment())
clusterEvalQ(cl, library(Matrix))

# STEP 4: Define processing function for one batch
process_batch <- function(batch_indices) {
  V_batch <- V[, batch_indices, drop = FALSE]
  PsiV_batch <- V_batch  # initialize with I * V
  current_power <- V_batch
  
  for (r in 1:50) {
    current_power <- io_matrix %*% current_power
    PsiV_batch <- PsiV_batch + current_power
  }
  return(PsiV_batch)
}

# STEP 5: Run batches in parallel
results <- parLapply(cl, batches, process_batch)

# STEP 6: Combine results
PsiV <- do.call(cBind, results)

# STEP 7: Clean up
stopCluster(cl)
closeAllConnections()
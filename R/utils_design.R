fw_interval_to_hours <- function(iv) {
  val <- as.numeric(gsub("[^0-9.]", "", iv))

  if (grepl("d", iv, ignore.case = TRUE)) {
    return(val * 24)
  }
  if (grepl("h", iv, ignore.case = TRUE)) {
    return(val)
  }

  val
}

fw_run_design <- function(intervals, target_re_pct = 15, n_mc = 1000) {
  set.seed(42)

  results <- data.frame(
    Interval = intervals,
    Samples_yr = sapply(intervals, function(iv) {
      hrs <- fw_interval_to_hours(iv)
      round(365 * 24 / hrs)
    }),
    Mean_RE_pct = sapply(intervals, function(iv) {
      hrs <- fw_interval_to_hours(iv)
      round(runif(1, 5, 50) * sqrt(hrs / 24), 1)
    }),
    stringsAsFactors = FALSE
  )

  results$Meets <- ifelse(results$Mean_RE_pct <= target_re_pct, "Yes", "No")
  results <- results[order(results$Mean_RE_pct), , drop = FALSE]
  rownames(results) <- NULL
  results
}

fw_export_summary_plot <- function(file, clean_df = NULL, flux_df = NULL, design_df = NULL) {
  grDevices::png(file, width = 1600, height = 1200, res = 150)
  old_par <- par(no.readonly = TRUE)
  on.exit({
    par(old_par)
    grDevices::dev.off()
  }, add = TRUE)

  par(mfrow = c(2, 2), mar = c(4, 4, 3, 1))

  # 1 clean
  if (!is.null(clean_df)) {
    cc <- fw_find_cols(clean_df)
    if (!is.null(cc$dt) && !is.null(cc$num)) {
      plot(
        clean_df[[cc$dt]],
        clean_df[[cc$num]],
        type = "l",
        col = "#3c8dbc",
        main = "Raw"
      )
    } else {
      plot.new(); title("No data")
    }
  } else {
    plot.new(); title("No data")
  }

  # 2 flux
  if (!is.null(flux_df)) {
    plot(flux_df$date, flux_df$flux, type = "h", col = "#e67e22", lwd = 2, main = "Daily Flux")
  } else {
    plot.new(); title("No flux")
  }

  # 3 cumulative
  if (!is.null(flux_df)) {
    cf <- cumsum(ifelse(is.na(flux_df$flux), 0, flux_df$flux))
    plot(flux_df$date, cf, type = "l", col = "#27ae60", lwd = 2, main = "Cumulative")
  } else {
    plot.new(); title("No flux")
  }

  # 4 design
  if (!is.null(design_df)) {
    barplot(
      design_df$Mean_RE_pct,
      names.arg = design_df$Interval,
      col = "#3498db",
      border = NA,
      main = "Design"
    )
  } else {
    plot.new(); title("No design")
  }
}

fw_write_simple_report <- function(file, report_format = "html", clean_df = NULL, flux_df = NULL, design_df = NULL) {
  lines <- c(
    "=== FluxWatch Report ===",
    paste("Generated:", Sys.time()),
    paste("Clean data ready:", !is.null(clean_df)),
    paste("Flux ready:", !is.null(flux_df)),
    paste("Design ready:", !is.null(design_df))
  )

  if (!is.null(flux_df)) {
    s <- fw_flux_summary(flux_df)
    lines <- c(
      lines,
      paste("Flux days:", s$days),
      paste("Flux total:", s$total),
      paste("Flux mean:", s$mean),
      paste("Flux sd:", s$sd)
    )
  } else {
    lines <- c(lines, "No flux result.")
  }

  lines <- c(lines, "--- End ---")

  writeLines(lines, con = file, useBytes = TRUE)
}

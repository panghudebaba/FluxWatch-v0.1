fw_plot_data <- function(df, viz_type = "daily_ts", show_points = TRUE, show_smooth = FALSE) {
  cc <- fw_find_cols(df)

  if (is.null(cc$dt) || is.null(cc$num)) {
    plot.new()
    text(0.5, 0.5, "Need datetime + numeric col")
    return(invisible(NULL))
  }

  x <- df[[cc$dt]]
  y <- df[[cc$num]]

  if (viz_type == "daily_ts") {
    plot(
      x, y,
      type = "l",
      col = "#3c8dbc",
      lwd = 1.5,
      xlab = cc$dt,
      ylab = cc$num,
      main = paste0("Time Series: ", cc$num)
    )

    if (isTRUE(show_points)) {
      points(x, y, pch = 16, cex = 0.5, col = "#e74c3c")
    }

    if (isTRUE(show_smooth)) {
      ok <- complete.cases(x, y)
      lines(
        stats::lowess(as.numeric(x[ok]), y[ok], f = 0.2),
        col = "#2ecc71",
        lwd = 2
      )
    }
  }

  if (viz_type == "cumulative") {
    ok <- order(x)
    cy <- cumsum(ifelse(is.na(y[ok]), 0, y[ok]))

    plot(
      x[ok], cy,
      type = "l",
      col = "#e67e22",
      lwd = 2,
      xlab = cc$dt,
      ylab = paste0("Cumulative ", cc$num),
      main = "Cumulative Flux"
    )

    if (isTRUE(show_points)) {
      points(x[ok], cy, pch = 16, cex = 0.4)
    }
  }

  if (viz_type == "boxplot_month") {
    boxplot(
      y ~ format(x, "%Y-%m"),
      col = "#3498db",
      las = 2,
      xlab = "Month",
      ylab = cc$num,
      main = paste0("Monthly: ", cc$num)
    )
  }

  invisible(NULL)
}

fw_plot_design <- function(df, target_re = 15) {
  df$Interval <- factor(df$Interval, levels = df$Interval)

  barplot(
    df$Mean_RE_pct,
    names.arg = df$Interval,
    col = ifelse(df$Meets == "Yes", "#27ae60", "#e74c3c"),
    border = NA,
    ylab = "RE(%)",
    xlab = "Interval",
    main = "RE vs Sampling Interval"
  )

  abline(h = target_re, lty = 2, col = "#3c8dbc", lwd = 2)

  legend(
    "topright",
    legend = c("Meets", "Exceeds", "Target"),
    fill = c("#27ae60", "#e74c3c", NA),
    border = c(NA, NA, NA),
    lty = c(NA, NA, 2),
    col = c(NA, NA, "#3c8dbc"),
    lwd = c(NA, NA, 2)
  )
}

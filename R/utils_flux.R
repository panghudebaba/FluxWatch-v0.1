if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

fw_as_num <- function(x) {
  if (is.null(x)) return(numeric(0))
  if (is.factor(x)) x <- as.character(x)

  if (is.character(x)) {
    z <- trimws(x)
    z[z %in% c("", "NA", "NaN", "NULL", "null", "-", "--")] <- NA_character_
    z <- gsub(",", ".", z, fixed = TRUE)
    suppressWarnings(as.numeric(z))
  } else {
    suppressWarnings(as.numeric(x))
  }
}

fw_as_date <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(as.Date(x))
  if (inherits(x, "POSIXt")) return(as.Date(x))

  if (is.numeric(x)) {
    out <- as.Date(x, origin = "1970-01-01")
    bad <- is.na(out) | out < as.Date("1900-01-01") | out > as.Date("2200-12-31")
    if (any(bad)) out[bad] <- as.Date(x[bad], origin = "1899-12-30")
    return(out)
  }

  if (is.factor(x)) x <- as.character(x)
  xx <- trimws(as.character(x))
  xx[xx %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_

  out <- rep(as.Date(NA), length(xx))
  fmts <- c(
    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d", "%Y%m%d",
    "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
    "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M"
  )

  for (f in fmts) {
    idx <- is.na(out) & !is.na(xx)
    if (!any(idx)) break
    tmp <- suppressWarnings(as.Date(xx[idx], format = f))
    out[idx] <- tmp
  }

  idx <- is.na(out) & !is.na(xx)
  if (any(idx)) {
    tmp <- suppressWarnings(as.POSIXct(xx[idx], tz = "UTC"))
    out[idx] <- as.Date(tmp)
  }

  idx <- is.na(out) & !is.na(xx)
  if (any(idx)) {
    num <- suppressWarnings(as.numeric(xx[idx]))
    ok <- is.finite(num)
    if (any(ok)) {
      ii <- which(idx)[ok]
      out[ii] <- fw_as_date(num[ok])
    }
  }

  out
}

fw_trim_na <- function(x) {
  if (is.null(x)) return(character(0))
  z <- as.character(x)
  z <- trimws(z)
  z[z %in% c("", "NA", "NaN", "NULL", "null")] <- NA_character_
  z
}

fw_rv_get <- function(x, name) {
  tryCatch(x[[name]], error = function(e) NULL)
}

fw_as_named_table_list <- function(x, default_name = "data") {
  if (is.null(x)) return(list())

  if (is.data.frame(x)) {
    out <- list(x)
    names(out) <- default_name
    return(out)
  }

  if (!is.list(x)) return(list())

  ok <- vapply(x, is.data.frame, logical(1))
  out <- x[ok]
  if (length(out) == 0) return(list())

  nm <- names(out)
  if (is.null(nm)) nm <- rep("", length(out))
  miss <- which(is.na(nm) | nm == "")
  if (length(miss) > 0) nm[miss] <- paste0(default_name, "_", miss)
  names(out) <- nm
  out
}

fw_find_flux_cols <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(list(dt = NULL, q = NULL, c = NULL, station = NULL, wybm = NULL))
  }

  nm <- names(df)
  nml <- tolower(gsub("[^a-z0-9]", "", nm))

  pick <- function(patterns, exclude = NULL) {
    hit <- rep(FALSE, length(nml))
    for (p in patterns) hit <- hit | grepl(p, nml, perl = TRUE)
    if (!is.null(exclude)) {
      for (p in exclude) hit <- hit & !grepl(p, nml, perl = TRUE)
    }
    if (any(hit)) nm[which(hit)[1]] else NULL
  }

  dt <- pick(c("^tm$", "^date$", "datetime", "timestamp", "^time$", "riqi", "^rq$"))
  q  <- pick(c("^q$", "^flow$", "discharge", "streamflow", "liuliang"))
  ccol <- pick(
    c("^c$", "cobs", "conc", "concentration", "^tn$", "^tp$", "nh4", "no3", "cod", "toc", "nh3n"),
    exclude = c("code", "station", "stnm")
  )
  station <- pick(c("^station$", "stnm", "sitename", "^site$", "siteid"))
  wybm <- pick(c("^wybm$", "basincode", "watershed", "subbasin"))

  list(dt = dt, q = q, c = ccol, station = station, wybm = wybm)
}

fw_pick_first_text <- function(df, cand_cols) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NA_character_)
  for (nm in cand_cols) {
    if (nm %in% names(df)) {
      z <- fw_trim_na(df[[nm]])
      z <- z[!is.na(z)]
      if (length(z) > 0) return(as.character(z[1]))
    }
  }
  NA_character_
}

fw_pick_first_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (is.list(x)) {
    ok <- vapply(x, is.data.frame, logical(1))
    if (any(ok)) return(x[[which(ok)[1]]])
  }
  NULL
}

fw_get_step1_qf_wq <- function(rv) {
  # 1) 顶层 rv$QF / rv$WQ
  qf0 <- fw_rv_get(rv, "QF")
  wq0 <- fw_rv_get(rv, "WQ")
  if (!is.null(qf0) && !is.null(wq0)) {
    return(list(
      QF = fw_as_named_table_list(qf0, "QF"),
      WQ = fw_as_named_table_list(wq0, "WQ"),
      source = "rv$QF + rv$WQ"
    ))
  }

  # 2) 常见容器
  cand_names <- c("step1", "step1_data", "step1_result", "clean_list")
  for (nm in cand_names) {
    obj <- fw_rv_get(rv, nm)
    if (is.null(obj) || !is.list(obj)) next

    qf <- obj[["QF"]]
    wq <- obj[["WQ"]]
    if (!is.null(qf) && !is.null(wq)) {
      return(list(
        QF = fw_as_named_table_list(qf, "QF"),
        WQ = fw_as_named_table_list(wq, "WQ"),
        source = paste0("rv$", nm, "$QF/WQ")
      ))
    }

    flow <- obj[["Flow"]]
    wq2 <- obj[["WaterQuality"]]
    if (!is.null(flow) && !is.null(wq2)) {
      return(list(
        QF = fw_as_named_table_list(flow, "Flow"),
        WQ = fw_as_named_table_list(wq2, "WaterQuality"),
        source = paste0("rv$", nm, "$Flow/WaterQuality")
      ))
    }
  }

  NULL
}

fw_get_wq_constituents <- function(wq_df) {
  if (is.null(wq_df) || !is.data.frame(wq_df) || nrow(wq_df) == 0) return(character(0))

  ex <- tolower(c(
    "TM", "time", "date", "datetime", "timestamp",
    "WYBM", "STNM", "WYBM_WQ", "STNM_WQ",
    "year", "month", "day", "doy", "station"
  ))

  cols <- names(wq_df)
  keep <- !(tolower(cols) %in% ex)
  cols <- cols[keep]
  if (length(cols) == 0) return(character(0))

  ok <- vapply(cols, function(cc) {
    x <- fw_as_num(wq_df[[cc]])
    any(is.finite(x))
  }, logical(1))

  cols[ok]
}

fw_build_flux_df_from_clean_list <- function(clean_list, target = "TN") {
  if (is.null(clean_list) || !is.list(clean_list)) return(NULL)

  flow_src <- clean_list$Flow %||% clean_list$QF %||% clean_list$flow
  wq_src   <- clean_list$WaterQuality %||% clean_list$WQ %||% clean_list$waterquality

  flow_df <- fw_pick_first_df(flow_src)

  pick_wq_df <- function(x, target_col) {
    if (is.null(x)) return(NULL)
    if (is.data.frame(x)) return(x)
    if (is.list(x)) {
      idx <- which(vapply(x, function(df) is.data.frame(df) && target_col %in% names(df), logical(1)))
      if (length(idx) > 0) return(x[[idx[1]]])

      ok <- vapply(x, is.data.frame, logical(1))
      if (any(ok)) return(x[[which(ok)[1]]])
    }
    NULL
  }

  wq_df <- pick_wq_df(wq_src, target)

  if (is.null(flow_df) || is.null(wq_df)) return(NULL)

  cf <- fw_find_flux_cols(flow_df)
  cw <- fw_find_flux_cols(wq_df)

  dcol_f <- if ("TM" %in% names(flow_df)) "TM" else cf$dt
  qcol   <- if ("Q" %in% names(flow_df)) "Q" else cf$q
  dcol_w <- if ("TM" %in% names(wq_df)) "TM" else cw$dt

  if (is.null(dcol_f) || is.null(qcol) || is.null(dcol_w)) return(NULL)

  ccol <- target
  if (!(ccol %in% names(wq_df))) {
    cands <- fw_get_wq_constituents(wq_df)
    if (length(cands) == 0) return(NULL)
    ccol <- cands[1]
  }

  q <- data.frame(
    TM = fw_as_date(flow_df[[dcol_f]]),
    Q = fw_as_num(flow_df[[qcol]]),
    stringsAsFactors = FALSE
  )
  c <- data.frame(
    TM = fw_as_date(wq_df[[dcol_w]]),
    C_obs = fw_as_num(wq_df[[ccol]]),
    stringsAsFactors = FALSE
  )

  q <- q[!is.na(q$TM), , drop = FALSE]
  c <- c[!is.na(c$TM), , drop = FALSE]
  if (nrow(q) == 0) return(NULL)

  agg_mean <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
  q <- stats::aggregate(Q ~ TM, data = q, FUN = agg_mean)
  if (nrow(c) > 0) c <- stats::aggregate(C_obs ~ TM, data = c, FUN = agg_mean)

  out <- merge(q, c, by = "TM", all.x = TRUE, sort = TRUE)

  station <- fw_pick_first_text(flow_df, c("station", "STNM", "STNM_WQ", "site", "SITENAME"))
  if (is.na(station) || station == "") {
    station <- fw_pick_first_text(wq_df, c("station", "STNM", "STNM_WQ", "site", "SITENAME"))
  }
  if (is.na(station) || station == "") station <- "ALL"

  wybm <- fw_pick_first_text(flow_df, c("WYBM", "WYBM_WQ", "wybm"))
  if (is.na(wybm) || wybm == "") {
    wybm <- fw_pick_first_text(wq_df, c("WYBM", "WYBM_WQ", "wybm"))
  }

  out$station <- station
  out$WYBM <- if (is.na(wybm) || wybm == "") NA_character_ else wybm
  out
}

fw_fill_daily_id_from_source <- function(daily, src, target_col, cand_cols) {
  if (is.null(daily) || !is.data.frame(daily)) return(daily)
  if (!(target_col %in% names(daily))) daily[[target_col]] <- NA_character_

  miss <- is.na(daily[[target_col]]) | daily[[target_col]] == ""
  if (!any(miss)) return(daily)

  fill_val <- NA_character_
  if (!is.null(src) && is.data.frame(src)) {
    for (nm in cand_cols) {
      if (nm %in% names(src)) {
        z <- fw_trim_na(src[[nm]])
        z <- z[!is.na(z)]
        if (length(z) > 0) {
          fill_val <- as.character(z[1])
          break
        }
      }
    }
  }

  if (is.na(fill_val) || fill_val == "") {
    fill_val <- if (identical(target_col, "station")) "ALL" else NA_character_
  }

  daily[[target_col]][miss] <- fill_val
  daily
}

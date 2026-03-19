# =========================================================
# fct_viz.R
# Visualization helpers for Flow / WaterQuality / Rain
# =========================================================

library(shiny)
library(dplyr)
library(ggplot2)
library(patchwork)
library(scales)
library(readxl)

`%||%` <- function(a, b) if (!is.null(a)) a else b


# ---------------------------------------------------------
# 日期转 POSIXct
# ---------------------------------------------------------
fw_viz_as_posix <- function(x, tz = "Asia/Shanghai") {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXt")) return(as.POSIXct(x, tz = tz))
  if (inherits(x, "Date")) return(as.POSIXct(x, tz = tz))
  as.POSIXct(x, tz = tz)
}


# ---------------------------------------------------------
# 通用：清洗列名
# ---------------------------------------------------------
fw_viz_clean_names <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  names(df) <- trimws(names(df))
  names(df) <- gsub("\\s+", "", names(df))
  df
}


# ---------------------------------------------------------
# 通用：混合日期解析
# ---------------------------------------------------------
fw_viz_parse_date <- function(x) {
  if (inherits(x, "POSIXct")) return(x)
  if (inherits(x, "POSIXt")) return(as.POSIXct(x))
  if (inherits(x, "Date")) return(as.POSIXct(x))

  if (is.numeric(x)) {
    return(as.POSIXct(as.Date(x, origin = "1899-12-30")))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr == ""] <- NA_character_

  x_std <- gsub("\\.", "-", x_chr)
  x_std <- gsub("/", "-", x_std)

  out <- suppressWarnings(as.POSIXct(x_std, format = "%Y-%m-%d", tz = "Asia/Shanghai"))

  idx_na <- is.na(out) & !is.na(x_std)
  if (any(idx_na)) {
    out2 <- suppressWarnings(as.POSIXct(x_std[idx_na], format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Shanghai"))
    out[idx_na] <- out2
  }

  idx_na <- is.na(out) & !is.na(x_std)
  if (any(idx_na)) {
    num_try <- suppressWarnings(as.numeric(x_std[idx_na]))
    ok_num <- !is.na(num_try)
    if (any(ok_num)) {
      out_idx <- which(idx_na)[ok_num]
      out[out_idx] <- as.POSIXct(as.Date(num_try[ok_num], origin = "1899-12-30"), tz = "Asia/Shanghai")
    }
  }

  out
}


# ---------------------------------------------------------
# 根据数据猜测类型
# ---------------------------------------------------------
fw_viz_guess_sheet_type <- function(df) {
  nms <- names(df)

  if (all(c("TM", "WYBM", "STNM", "Q") %in% nms)) return("Flow")
  if (all(c("TM", "WYBM", "STNM", "P") %in% nms)) return("Rain")

  wq_core <- all(c("TM", "WYBM", "STNM") %in% nms)
  wq_vars <- intersect(c("TN", "TP", "NH4N", "NO3N", "COD", "TOC", "DO", "PH"), nms)
  if (wq_core && length(wq_vars) > 0) return("WaterQuality")

  if (all(c("NAME_WQ", "WYBM_WQ", "WYBM_QF") %in% nms)) return("Topology")

  "Unknown"
}


# ---------------------------------------------------------
# 标准化 clean_list
# ---------------------------------------------------------
fw_viz_normalize_clean_list <- function(clean_list) {
  if (is.null(clean_list) || length(clean_list) == 0) return(list())

  out <- clean_list

  for (nm in names(out)) {
    df <- out[[nm]]
    if (!is.data.frame(df)) next

    df <- fw_viz_clean_names(df)

    if ("TM" %in% names(df)) {
      df$TM <- fw_viz_parse_date(df$TM)
    }

    if ("WYBM" %in% names(df)) {
      df$WYBM <- as.character(df$WYBM)
    }

    if ("STNM" %in% names(df)) {
      df$STNM <- as.character(df$STNM)
      df$STNM[is.na(df$STNM) | df$STNM == ""] <- df$WYBM[is.na(df$STNM) | df$STNM == ""]
    }

    if ("Q" %in% names(df)) df$Q <- suppressWarnings(as.numeric(df$Q))
    if ("P" %in% names(df)) df$P <- suppressWarnings(as.numeric(df$P))

    out[[nm]] <- df
  }

  out
}


# ---------------------------------------------------------
# 上传文件 -> clean_list
# 支持 xlsx/xls/rds/csv
# ---------------------------------------------------------
fw_viz_read_uploaded_data <- function(path) {
  ext <- tolower(tools::file_ext(path))

  if (ext %in% c("xlsx", "xls")) {
    if (!exists("fw_read_validate_workbook", mode = "function")) {
      stop("fw_read_validate_workbook() not found. Please source fct_ingest.R first.")
    }
    res <- fw_read_validate_workbook(path, auto_impute_flow = TRUE, target = "TN")
    return(res$clean_list)
  }

  if (ext == "rds") {
    obj <- readRDS(path)

    if (is.list(obj) && any(names(obj) %in% c("Flow", "WaterQuality", "Rain", "Topology", "PP"))) {
      return(fw_viz_normalize_clean_list(obj))
    }

    if (is.data.frame(obj)) {
      tp <- fw_viz_guess_sheet_type(obj)
      if (tp == "Unknown") stop("Cannot infer RDS data type.")
      out <- list()
      out[[tp]] <- obj
      return(fw_viz_normalize_clean_list(out))
    }

    stop("Unsupported RDS object.")
  }

  if (ext == "csv") {
    df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
    df <- fw_viz_clean_names(df)
    tp <- fw_viz_guess_sheet_type(df)
    if (tp == "Unknown") stop("Cannot infer CSV data type.")
    out <- list()
    out[[tp]] <- df
    return(fw_viz_normalize_clean_list(out))
  }

  stop("Unsupported file type.")
}


# ---------------------------------------------------------
# 从 clean_list 中抽取数据
# ---------------------------------------------------------
fw_viz_get_plot_data <- function(clean_list) {
  clean_list <- fw_viz_normalize_clean_list(clean_list)

  out <- list(
    rain = NULL,
    flow = NULL,
    wq = NULL,
    topo = NULL
  )

  nm <- names(clean_list)

  rain_name <- intersect(c("Rain", "Rainfall", "Precip", "Precipitation", "PP"), nm)
  if (length(rain_name) > 0) {
    rain <- clean_list[[rain_name[1]]]
    p_col <- intersect(c("P", "PP", "RAIN", "Rain", "PREC"), names(rain))
    if (length(p_col) > 0) names(rain)[names(rain) == p_col[1]] <- "P"
    if (!("STNM" %in% names(rain))) rain$STNM <- rain$WYBM %||% "Rain"
    out$rain <- rain
  }

  if ("Flow" %in% nm) out$flow <- clean_list$Flow
  if ("WaterQuality" %in% nm) out$wq <- clean_list$WaterQuality
  if ("Topology" %in% nm) out$topo <- clean_list$Topology

  out
}



# ---------------------------------------------------------
# 水质指标候选
# ---------------------------------------------------------

























# ---------------------------------------------------------
# 水质指标候选
# ---------------------------------------------------------
fw_viz_get_wq_vars <- function(wq_df) {
  if (is.null(wq_df) || nrow(wq_df) == 0) return(character(0))

  exclude <- c("TM", "WYBM", "STNM")
  cand <- setdiff(names(wq_df), exclude)

  cand[vapply(wq_df[cand], function(x) is.numeric(x) || is.integer(x), logical(1))]
}


# ---------------------------------------------------------
# 站点 choices
# ---------------------------------------------------------
fw_viz_get_site_choices <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(character(0))
  if (!all(c("WYBM", "STNM") %in% names(df))) return(character(0))

  tmp <- df %>%
    distinct(WYBM, STNM) %>%
    mutate(label = paste0(STNM, " [", WYBM, "]"))

  stats::setNames(as.character(tmp$WYBM), tmp$label)
}


# ---------------------------------------------------------
# 过滤
# ---------------------------------------------------------
fw_viz_filter_time <- function(df, start_time, end_time) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (!("TM" %in% names(df))) return(df)

  st <- fw_viz_as_posix(start_time)
  et <- fw_viz_as_posix(end_time)

  df %>% filter(TM >= st, TM <= et)
}

fw_viz_filter_site <- function(df, site_ids) {
  if (is.null(df) || nrow(df) == 0) return(df)
  if (is.null(site_ids) || length(site_ids) == 0) return(df)
  if (!("WYBM" %in% names(df))) return(df)

  df %>% filter(as.character(WYBM) %in% as.character(site_ids))
}


# ---------------------------------------------------------
# Topology: WQ -> Flow 站点映射
# ---------------------------------------------------------
fw_viz_match_flow_sites_by_topology <- function(clean_list, wq_sites) {
  dd <- fw_viz_get_plot_data(clean_list)
  topo <- dd$topo
  if (is.null(topo) || nrow(topo) == 0) return(character(0))
  if (!all(c("WYBM_WQ", "WYBM_QF") %in% names(topo))) return(character(0))

  topo %>%
    mutate(WYBM_WQ = as.character(WYBM_WQ), WYBM_QF = as.character(WYBM_QF)) %>%
    filter(WYBM_WQ %in% as.character(wq_sites)) %>%
    pull(WYBM_QF) %>%
    unique()
}


# ---------------------------------------------------------
# 图形主题
# ---------------------------------------------------------
fw_viz_theme <- function(base_size = 13, rel_size = 1.05, keep_x_text = TRUE) {
  theme_bw(base_size = base_size) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_text(size = rel(rel_size)),
      axis.text.x = if (isTRUE(keep_x_text)) {
        element_text(size = rel(rel_size), angle = 30, colour = "black", hjust = 1)
      } else {
        element_blank()
      },
      axis.text.y = element_text(size = rel(rel_size), colour = "black"),
      panel.border = element_blank(),
      axis.line = element_line(),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_text(size = rel(rel_size), face = "bold", colour = "black"),
      strip.background = element_rect(fill = "grey95", colour = NA),
      strip.text = element_text(face = "bold"),
      legend.title = element_blank(),
      legend.position = "top",
      legend.background = element_blank()
    )
}


# ---------------------------------------------------------
# 降雨图
# panel_mode: overlay / facet_site
# ---------------------------------------------------------
fw_viz_plot_rain <- function(df,
                             start_time,
                             end_time,
                             panel_mode = "overlay",
                             date_breaks = "1 month",
                             date_labels = "%Y-%m-%d",
                             base_size = 13,
                             rel_size = 1.05) {

  if (is.null(df) || nrow(df) == 0 || !all(c("TM", "P") %in% names(df))) {
    return(ggplot() + annotate("text", x = 1, y = 1, label = "无降雨数据 / No rainfall data") + theme_void())
  }

  maxP <- suppressWarnings(max(df$P, na.rm = TRUE))
  if (!is.finite(maxP)) maxP <- 1
  maxP <- 1.1 * maxP

  p <- ggplot(df, aes(x = TM, y = P)) +
    geom_linerange(aes(ymin = 0, ymax = P, colour = STNM), linewidth = 0.5) +
    labs(y = "降雨量 (mm)") +
    scale_x_datetime(
      limits = c(fw_viz_as_posix(start_time), fw_viz_as_posix(end_time)),
      date_breaks = date_breaks,
      date_labels = date_labels,
      position = "top"
    ) +
    scale_y_reverse(expand = c(0, 0), limits = c(maxP, 0)) +
    fw_viz_theme(base_size = base_size, rel_size = rel_size, keep_x_text = FALSE) +
    theme(
      axis.ticks.x = element_blank()
    )

  if (panel_mode == "facet_site") {
    p <- p + facet_wrap(~ STNM, ncol = 1, scales = "free_y")
  }

  p
}


# ---------------------------------------------------------
# 流量图
# panel_mode: overlay / facet_site
# ---------------------------------------------------------
fw_viz_plot_flow <- function(df,
                             start_time,
                             end_time,
                             panel_mode = "overlay",
                             show_imputed_points = TRUE,
                             date_breaks = "1 month",
                             date_labels = "%Y-%m-%d",
                             base_size = 13,
                             rel_size = 1.05) {

  if (is.null(df) || nrow(df) == 0 || !all(c("TM", "Q") %in% names(df))) {
    return(ggplot() + annotate("text", x = 1, y = 1, label = "无流量数据 / No flow data") + theme_void())
  }

  p <- ggplot(df, aes(x = TM, y = Q, colour = STNM, group = WYBM)) +
    geom_line(linewidth = 0.8, alpha = 0.95) +
    labs(y = "流量 (m³/s)") +
    scale_x_datetime(
      limits = c(fw_viz_as_posix(start_time), fw_viz_as_posix(end_time)),
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    fw_viz_theme(base_size = base_size, rel_size = rel_size, keep_x_text = FALSE)

  if (isTRUE(show_imputed_points) && "is_imputed" %in% names(df)) {
    p <- p +
      geom_point(
        data = df %>% filter(is_imputed %in% TRUE),
        aes(x = TM, y = Q),
        inherit.aes = FALSE,
        shape = 21,
        size = 2.5,
        stroke = 0.8,
        fill = "white",
        colour = "red"
      )
  }

  if (panel_mode == "facet_site") {
    p <- p + facet_wrap(~ STNM, ncol = 1, scales = "free_y")
  }

  p
}


# ---------------------------------------------------------
# 水质长表转换
# ---------------------------------------------------------
fw_viz_wq_to_long <- function(df, wq_vars) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (is.null(wq_vars) || length(wq_vars) == 0) return(NULL)

  keep <- intersect(c("TM", "WYBM", "STNM", wq_vars), names(df))
  if (length(setdiff(c("TM", "WYBM", "STNM"), keep)) > 0) return(NULL)

  out <- do.call(
    rbind,
    lapply(wq_vars, function(vv) {
      if (!(vv %in% names(df))) return(NULL)
      data.frame(
        TM = df$TM,
        WYBM = df$WYBM,
        STNM = df$STNM,
        Indicator = vv,
        Value = suppressWarnings(as.numeric(df[[vv]])),
        stringsAsFactors = FALSE
      )
    })
  )

  out
}


# ---------------------------------------------------------
# 水质图（上下分面版）
# panel_mode:
# - overlay            所有站点叠加，按指标上下分面
# - facet_site         按站点上下分面（颜色区分指标）
# - facet_var          按指标上下分面（颜色区分站点）
# - facet_site_var     站点×指标 全部上下堆叠
# ---------------------------------------------------------
fw_viz_plot_wq <- function(df,
                           wq_vars,
                           start_time,
                           end_time,
                           panel_mode = "facet_site_var",
                           show_smooth = TRUE,
                           add_standard_lines = FALSE,
                           std_lines = NULL,
                           date_breaks = "1 month",
                           date_labels = "%Y-%m-%d",
                           base_size = 13,
                           rel_size = 1.05) {

  long_df <- fw_viz_wq_to_long(df, wq_vars)

  if (is.null(long_df) || nrow(long_df) == 0) {
    return(ggplot() +
             annotate("text", x = 1, y = 1,
                      label = "无水质数据 / No water quality data") +
             theme_void())
  }

  # ---------- 构建 facet 标签列 ----------
  # facet_site_var 模式下需要一个组合标签
  long_df <- long_df %>%
    mutate(
      site_var_label = paste0(STNM, " — ", Indicator)
    )

  # ---------- 基础图层 ----------
  p <- ggplot(long_df, aes(x = TM, y = Value, colour = STNM, shape = STNM)) +
    geom_point(alpha = 1, size = 2.4) +
    scale_x_datetime(
      limits = c(fw_viz_as_posix(start_time), fw_viz_as_posix(end_time)),
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    fw_viz_theme(base_size = base_size, rel_size = rel_size, keep_x_text = TRUE)

  if (isTRUE(show_smooth)) {
    p <- p + stat_smooth(method = "loess", se = FALSE, linewidth = 0.7)
  }

  # ---------- 分面逻辑（全部改为上下堆叠 ncol = 1） ----------
  if (panel_mode == "overlay") {
    # 多指标时按指标上下分面，单指标不分面
    if (length(unique(long_df$Indicator)) > 1) {
      p <- p +
        facet_wrap(~ Indicator, ncol = 1, scales = "free_y") +
        labs(y = "浓度 (mg/L)")
    } else {
      # 单指标：y 轴标签直接用指标名
      p <- p + labs(y = paste0(unique(long_df$Indicator), " (mg/L)"))
    }
  }

  if (panel_mode == "facet_site") {
    p <- p +
      facet_wrap(~ STNM, ncol = 1, scales = "free_y") +
      labs(y = "浓度 (mg/L)")
  }

  if (panel_mode == "facet_var") {
    p <- p +
      facet_wrap(~ Indicator, ncol = 1, scales = "free_y") +
      labs(y = NULL)
    # strip 已标明指标，y 轴留白或写通用标签均可
  }

  if (panel_mode == "facet_site_var") {
    # ★ 核心改动：原 facet_grid(Indicator ~ STNM) → 上下堆叠
    p <- p +
      facet_wrap(~ Indicator + STNM, ncol = 1, scales = "free_y",
                 labeller = label_wrap_gen(multi_line = FALSE)) +
      labs(y = NULL)
  }

  # ---------- 标准线 ----------
  if (isTRUE(add_standard_lines) && !is.null(std_lines) && length(std_lines) > 0) {
    if (!is.null(std_lines[["third"]])) {
      p <- p + geom_hline(yintercept = std_lines[["third"]],
                          colour = "brown", linetype = 3, linewidth = 0.8)
    }
    if (!is.null(std_lines[["fourth"]])) {
      p <- p + geom_hline(yintercept = std_lines[["fourth"]],
                          colour = "blue", linetype = 2, linewidth = 0.8)
    }
    if (!is.null(std_lines[["fifth"]])) {
      p <- p + geom_hline(yintercept = std_lines[["fifth"]],
                          colour = "black", linetype = 1, linewidth = 0.8)
    }
  }

  p
}


# ---------------------------------------------------------
# 流量图 + 水质浓度标记（geom_text）
# 将采样时刻的浓度值以文字形式标注在流量折线上方
# ---------------------------------------------------------
fw_viz_plot_flow_with_conc_labels <- function(flow_df,
                                              wq_df,
                                              wq_vars,
                                              start_time,
                                              end_time,
                                              panel_mode = "overlay",
                                              show_imputed_points = TRUE,
                                              date_breaks = "1 month",
                                              date_labels = "%Y-%m-%d",
                                              base_size = 13,
                                              rel_size = 1.05,
                                              label_size = 3.2,
                                              label_vjust = -0.6) {

  if (is.null(flow_df) || nrow(flow_df) == 0 || !all(c("TM", "Q") %in% names(flow_df))) {
    return(ggplot() +
             annotate("text", x = 1, y = 1, label = "无流量数据 / No flow data") +
             theme_void())
  }

  # ---------- 构建浓度标注数据 ----------
  label_df <- NULL
  if (!is.null(wq_df) && nrow(wq_df) > 0 &&
      !is.null(wq_vars) && length(wq_vars) > 0) {

    valid_vars <- intersect(wq_vars, names(wq_df))

    if (length(valid_vars) > 0) {
      long_wq <- fw_viz_wq_to_long(wq_df, valid_vars)

      if (!is.null(long_wq) && nrow(long_wq) > 0) {

        match_q <- function(tm_vec, wybm_vec, flow_df) {
          vapply(seq_along(tm_vec), function(i) {
            site_flow <- flow_df[as.character(flow_df$WYBM) == as.character(wybm_vec[i]), ]
            if (nrow(site_flow) == 0) site_flow <- flow_df
            if (nrow(site_flow) == 0) return(NA_real_)
            idx <- which.min(abs(as.numeric(site_flow$TM) - as.numeric(tm_vec[i])))
            site_flow$Q[idx]
          }, numeric(1))
        }

        long_wq$Q_matched   <- match_q(long_wq$TM, long_wq$WYBM, flow_df)
        long_wq$Value_label <- ifelse(
          is.na(long_wq$Value), NA_character_,
          formatC(long_wq$Value, digits = 3, format = "g")
        )
        long_wq$full_label  <- paste0(long_wq$Indicator, ": ", long_wq$Value_label)
        label_df <- long_wq[!is.na(long_wq$Value) & !is.na(long_wq$Q_matched), ]
      }
    }
  }

  # ---------- 为指标预分配固定颜色（不依赖 ggnewscale）----------
  indicator_colours <- NULL
  if (!is.null(label_df) && nrow(label_df) > 0) {
    inds <- unique(label_df$Indicator)
    pal  <- scales::hue_pal()(length(inds))        # ggplot2 默认色盘
    indicator_colours <- stats::setNames(pal, inds)
    label_df$label_colour <- indicator_colours[label_df$Indicator]
  }

  # ---------- 基础流量图（colour = STNM，不与标注冲突）----------
  p <- ggplot(flow_df, aes(x = TM, y = Q, colour = STNM, group = WYBM)) +
    geom_line(linewidth = 0.8, alpha = 0.95) +
    labs(y = "流量 (m³/s)") +
    scale_x_datetime(
      limits      = c(fw_viz_as_posix(start_time), fw_viz_as_posix(end_time)),
      date_breaks = date_breaks,
      date_labels = date_labels
    ) +
    scale_y_continuous(breaks = scales::breaks_pretty()) +
    fw_viz_theme(base_size = base_size, rel_size = rel_size, keep_x_text = TRUE)

  # ---------- 插补点 ----------
  if (isTRUE(show_imputed_points) && "is_imputed" %in% names(flow_df)) {
    p <- p +
      geom_point(
        data = flow_df %>% dplyr::filter(is_imputed %in% TRUE),
        aes(x = TM, y = Q),
        inherit.aes = FALSE,
        shape = 21, size = 2.5, stroke = 0.8,
        fill = "white", colour = "red"
      )
  }

  # ---------- 浓度标注（colour 直接用预分配的十六进制色，不走 scale）----------
  if (!is.null(label_df) && nrow(label_df) > 0) {

    # 辅助竖线
    # p <- p +
    #   geom_vline(
    #     data        = label_df %>% dplyr::distinct(TM),
    #     aes(xintercept = TM),
    #     inherit.aes = FALSE,
    #     colour      = "grey70",
    #     linetype    = "dotted",
    #     linewidth   = 0.4
    #   )

    # 逐指标添加 geom_text，每次用固定颜色（绕开双 scale 冲突）
    for (ind in unique(label_df$Indicator)) {
      sub <- label_df[label_df$Indicator == ind, ]
      col <- indicator_colours[[ind]]
      p <- p +
        geom_text(
          data        = sub,
          aes(x = TM, y = Q_matched, label = full_label),
          inherit.aes = FALSE,
          colour      = col,
          size        = label_size,
          vjust       = label_vjust,
          fontface    = "bold",
          family      = "sans"
        )
    }

    # 手动图例（colour legend for indicators）
    p <- p +
      guides(colour = guide_legend(title = "站点 Site", override.aes = list(linewidth = 1)))
  }

  # ---------- 分面 ----------
  if (panel_mode == "facet_site") {
    p <- p + facet_wrap(~ STNM, ncol = 1, scales = "free_y")
  }

  p
}




# ---------------------------------------------------------
# logC-logQ 图
# X 轴: log10(Q)，Y 轴: log10(C)
# 颜色: 站点，分面: 指标
# 可选: 回归线 + 斜率/截距注释
# ---------------------------------------------------------
fw_viz_plot_logC_logQ <- function(flow_df,
                                  wq_df,
                                  wq_vars,
                                  start_time,
                                  end_time,
                                  wq_sites          = NULL,
                                  flow_sites        = NULL,
                                  use_topology_match = TRUE,
                                  clean_list        = NULL,
                                  show_regression   = TRUE,
                                  date_breaks       = "1 month",
                                  date_labels       = "%Y-%m-%d",
                                  base_size         = 13,
                                  rel_size          = 1.05) {

  # ---------- 基础校验 ----------
  if (is.null(flow_df) || nrow(flow_df) == 0 || !("Q" %in% names(flow_df))) {
    return(ggplot() +
             annotate("text", x = 1, y = 1, label = "无流量数据 / No flow data") +
             theme_void())
  }
  if (is.null(wq_df) || nrow(wq_df) == 0) {
    return(ggplot() +
             annotate("text", x = 1, y = 1, label = "无水质数据 / No WQ data") +
             theme_void())
  }
  if (is.null(wq_vars) || length(wq_vars) == 0) {
    return(ggplot() +
             annotate("text", x = 1, y = 1, label = "未选择水质指标 / No WQ indicator selected") +
             theme_void())
  }

  # ---------- 站点过滤 ----------
  if (!is.null(wq_sites) && length(wq_sites) > 0 &&
      isTRUE(use_topology_match) && !is.null(clean_list)) {
    flow_sites2 <- fw_viz_match_flow_sites_by_topology(clean_list, wq_sites)
    if (length(flow_sites2) > 0) flow_sites <- unique(c(flow_sites, flow_sites2))
  }

  flow_df <- fw_viz_filter_site(flow_df, flow_sites)
  wq_df   <- fw_viz_filter_site(wq_df,   wq_sites)
  flow_df <- fw_viz_filter_time(flow_df, start_time, end_time)
  wq_df   <- fw_viz_filter_time(wq_df,   start_time, end_time)

  # ---------- 转长表 ----------
  valid_vars <- intersect(wq_vars, names(wq_df))
  long_wq    <- fw_viz_wq_to_long(wq_df, valid_vars)
  if (is.null(long_wq) || nrow(long_wq) == 0) {
    return(ggplot() +
             annotate("text", x = 1, y = 1, label = "水质数据为空 / WQ data empty") +
             theme_void())
  }

  # ---------- 为每条 WQ 记录匹配最近流量 ----------
  match_q <- function(tm_vec, wybm_vec, flow_df) {
    vapply(seq_along(tm_vec), function(i) {
      site_flow <- flow_df[as.character(flow_df$WYBM) == as.character(wybm_vec[i]), ]
      if (nrow(site_flow) == 0) site_flow <- flow_df
      if (nrow(site_flow) == 0) return(NA_real_)
      idx <- which.min(abs(as.numeric(site_flow$TM) - as.numeric(tm_vec[i])))
      site_flow$Q[idx]
    }, numeric(1))
  }

  long_wq$Q_matched <- match_q(long_wq$TM, long_wq$WYBM, flow_df)

  # ---------- 取对数（剔除非正值）----------
  plot_df <- long_wq %>%
    dplyr::filter(!is.na(Value), !is.na(Q_matched),
                  Value > 0, Q_matched > 0) %>%
    dplyr::mutate(
      logQ = log10(Q_matched),
      logC = log10(Value)
    )

  if (nrow(plot_df) == 0) {
    return(ggplot() +
             annotate("text", x = 1, y = 1,
                      label = "无有效正值数据（log 需正值）\nNo valid positive data for log transform") +
             theme_void())
  }

  # ---------- 构建回归注释 ----------
  reg_labels <- NULL
  if (isTRUE(show_regression)) {
    reg_labels <- plot_df %>%
      dplyr::group_by(Indicator, STNM) %>%
      dplyr::summarise(
        n         = dplyr::n(),
        slope     = if (dplyr::n() > 2) coef(lm(logC ~ logQ))[2]              else NA_real_,
        intercept = if (dplyr::n() > 2) coef(lm(logC ~ logQ))[1]              else NA_real_,
        r2        = if (dplyr::n() > 2) summary(lm(logC ~ logQ))$r.squared    else NA_real_,
        xpos      = min(logQ, na.rm = TRUE),   # ← 改为 min → 左侧
        ypos      = max(logC, na.rm = TRUE),   # 仍取顶部
        .groups   = "drop"
      ) %>%
      dplyr::filter(!is.na(slope)) %>%
      dplyr::mutate(
        reg_text = paste0(
          STNM, "\n",
          "b = ", round(slope, 3),
          ", R² = ", round(r2, 3)
        )
      )
  }


  # ---------- 绘图 ----------
  p <- ggplot(plot_df, aes(x = logQ, y = logC, colour = STNM, shape = STNM)) +
    geom_point(size = 2.4, alpha = 0.85) +
    labs(
      x = expression(log[10](Q ~ "(m"^3 * "/s)")),
      y = expression(log[10](C ~ "(mg/L)"))
    ) +
    fw_viz_theme(base_size = base_size, rel_size = rel_size, keep_x_text = TRUE) +
    theme(
      axis.title.x = element_text(size = rel(rel_size)),  # 恢复 x 轴标题
      strip.text   = element_text(face = "bold", size = rel(rel_size))
    )

  # 回归线
  if (isTRUE(show_regression)) {
    p <- p +
      stat_smooth(
        method    = "lm",
        se        = TRUE,
        linewidth = 0.8,
        alpha     = 0.15,
        formula   = y ~ x
      )
  }

  # 回归注释文字（右上角）
  if (!is.null(reg_labels) && nrow(reg_labels) > 0) {
    p <- p +
      geom_text(
        data        = reg_labels,
        aes(x = xpos, y = ypos, label = reg_text),
        inherit.aes = FALSE,
        colour      = "black",
        size        = base_size * 0.28,
        hjust       = 0,    # ← 改为 0 → 左对齐
        vjust       = 1,
        fontface    = "italic",
        show.legend = FALSE
      )
  }


  # 分面：每个指标一格，自由 Y 轴
  if (length(unique(plot_df$Indicator)) > 1) {
    p <- p +
      facet_wrap(~ Indicator, ncol = 1, scales = "free")
  } else {
    p <- p + labs(y = paste0("log10(", unique(plot_df$Indicator), ") (mg/L)"))
  }

  p
}








# ---------------------------------------------------------
# 同站点 Flow + WQ 联合图
# 如果 use_topology_match = TRUE，则通过 Topology 把 WQ 站映射到 Flow 站
# 输出 patchwork，按站点逐个拼接
# ---------------------------------------------------------
fw_viz_plot_flow_wq_linked <- function(clean_list,
                                       flow_df,
                                       wq_df,
                                       wq_vars,
                                       start_time,
                                       end_time,
                                       wq_sites = NULL,
                                       flow_sites = NULL,
                                       use_topology_match = TRUE,
                                       show_wq_smooth = TRUE,
                                       show_imputed_points = TRUE,
                                       date_breaks = "1 month",
                                       date_labels = "%Y-%m-%d",
                                       base_size = 13,
                                       rel_size = 1.05) {

  if (!is.null(wq_sites) && length(wq_sites) > 0 && isTRUE(use_topology_match)) {
    flow_sites2 <- fw_viz_match_flow_sites_by_topology(clean_list, wq_sites)
    if (length(flow_sites2) > 0) flow_sites <- unique(c(flow_sites, flow_sites2))
  }

  flow_df <- fw_viz_filter_site(flow_df, flow_sites)
  wq_df   <- fw_viz_filter_site(wq_df, wq_sites)

  flow_df <- fw_viz_filter_time(flow_df, start_time, end_time)
  wq_df   <- fw_viz_filter_time(wq_df, start_time, end_time)

  if (is.null(flow_df) || nrow(flow_df) == 0 || is.null(wq_df) || nrow(wq_df) == 0) {
    return(ggplot() + annotate("text", x = 1, y = 1, label = "无可配对的流量/水质数据") + theme_void())
  }

  if (length(wq_vars) == 0) {
    return(ggplot() + annotate("text", x = 1, y = 1, label = "未选择水质指标") + theme_void())
  }

  flow_panel <- fw_viz_plot_flow(
    flow_df,
    start_time = start_time,
    end_time = end_time,
    panel_mode = if (length(unique(flow_df$WYBM)) > 1) "facet_site" else "overlay",
    show_imputed_points = show_imputed_points,
    date_breaks = date_breaks,
    date_labels = date_labels,
    base_size = base_size,
    rel_size = rel_size
  )

  wq_panel <- fw_viz_plot_wq(
    wq_df,
    wq_vars = wq_vars,
    start_time = start_time,
    end_time = end_time,
    panel_mode = if (length(wq_vars) > 1 || length(unique(wq_df$WYBM)) > 1) "facet_site_var" else "overlay",
    show_smooth = show_wq_smooth,
    date_breaks = date_breaks,
    date_labels = date_labels,
    base_size = base_size,
    rel_size = rel_size
  )

  flow_panel / wq_panel + plot_layout(ncol = 1, heights = c(2, 2))
}


# ---------------------------------------------------------
# 主函数
# plot_mode:
# - rain_flow_wq
# - flow_wq
# - linked_flow_wq
# - single
#
# single_type:
# - rain
# - flow
# - wq
# ---------------------------------------------------------
# 主函数
# ---------------------------------------------------------
fw_viz_build_plot <- function(clean_list,
                              plot_mode = "flow_wq",
                              single_type = "flow",
                              rain_sites = NULL,
                              flow_sites = NULL,
                              wq_sites = NULL,
                              wq_vars = NULL,
                              rain_panel_mode = "overlay",
                              flow_panel_mode = "overlay",
                              wq_panel_mode = "facet_site_var",
                              start_time,
                              end_time,
                              use_topology_match = TRUE,
                              show_wq_smooth = TRUE,
                              show_imputed_points = TRUE,
                              add_standard_lines = FALSE,
                              std_lines = NULL,
                              date_breaks = "1 month",
                              date_labels = "%Y-%m-%d",
                              base_size = 13,
                              rel_size = 1.05) {

  dd <- fw_viz_get_plot_data(clean_list)

  rain_df <- fw_viz_filter_site(dd$rain, rain_sites)
  flow_df <- fw_viz_filter_site(dd$flow, flow_sites)
  wq_df   <- fw_viz_filter_site(dd$wq,   wq_sites)

  rain_df <- fw_viz_filter_time(rain_df, start_time, end_time)
  flow_df <- fw_viz_filter_time(flow_df, start_time, end_time)
  wq_df   <- fw_viz_filter_time(wq_df,  start_time, end_time)

  # ── rain_flow_wq ──────────────────────────────────────
  if (plot_mode == "rain_flow_wq") {
    p1 <- fw_viz_plot_rain(rain_df, start_time, end_time,
                           panel_mode  = rain_panel_mode,
                           date_breaks = date_breaks, date_labels = date_labels,
                           base_size = base_size, rel_size = rel_size)
    p2 <- fw_viz_plot_flow(flow_df, start_time, end_time,
                           panel_mode  = flow_panel_mode,
                           show_imputed_points = show_imputed_points,
                           date_breaks = date_breaks, date_labels = date_labels,
                           base_size = base_size, rel_size = rel_size)
    p3 <- fw_viz_plot_wq(wq_df, wq_vars = wq_vars,
                         start_time = start_time, end_time = end_time,
                         panel_mode  = wq_panel_mode,
                         show_smooth = show_wq_smooth,
                         add_standard_lines = add_standard_lines, std_lines = std_lines,
                         date_breaks = date_breaks, date_labels = date_labels,
                         base_size = base_size, rel_size = rel_size)
    return(p1 / p2 / p3 + patchwork::plot_layout(ncol = 1, heights = c(1, 1, 2)))
  }

  # ── flow_wq ───────────────────────────────────────────
  if (plot_mode == "flow_wq") {
    p1 <- fw_viz_plot_flow(flow_df, start_time, end_time,
                           panel_mode  = flow_panel_mode,
                           show_imputed_points = show_imputed_points,
                           date_breaks = date_breaks, date_labels = date_labels,
                           base_size = base_size, rel_size = rel_size)
    p2 <- fw_viz_plot_wq(wq_df, wq_vars = wq_vars,
                         start_time = start_time, end_time = end_time,
                         panel_mode  = wq_panel_mode,
                         show_smooth = show_wq_smooth,
                         add_standard_lines = add_standard_lines, std_lines = std_lines,
                         date_breaks = date_breaks, date_labels = date_labels,
                         base_size = base_size, rel_size = rel_size)
    return(p1 / p2 + patchwork::plot_layout(ncol = 1, heights = c(2, 2)))
  }

  # ── linked_flow_wq ────────────────────────────────────
  if (plot_mode == "linked_flow_wq") {
    return(
      fw_viz_plot_flow_wq_linked(
        clean_list = clean_list,
        flow_df = dd$flow, wq_df = dd$wq,
        wq_vars = wq_vars,
        start_time = start_time, end_time = end_time,
        wq_sites = wq_sites, flow_sites = flow_sites,
        use_topology_match  = use_topology_match,
        show_wq_smooth      = show_wq_smooth,
        show_imputed_points = show_imputed_points,
        date_breaks = date_breaks, date_labels = date_labels,
        base_size = base_size, rel_size = rel_size
      )
    )
  }

  # ── ★ rain_flow_conclabel（独立顶层分支，不在 single 内）──
  if (plot_mode == "rain_flow_conclabel") {
    p1 <- fw_viz_plot_rain(rain_df, start_time, end_time,
                           panel_mode  = rain_panel_mode,
                           date_breaks = date_breaks, date_labels = date_labels,
                           base_size = base_size, rel_size = rel_size)

    p2 <- fw_viz_plot_flow_with_conc_labels(
      flow_df = flow_df, wq_df = wq_df, wq_vars = wq_vars,
      start_time = start_time, end_time = end_time,
      panel_mode  = flow_panel_mode,
      show_imputed_points = show_imputed_points,
      date_breaks = date_breaks, date_labels = date_labels,
      base_size = base_size, rel_size = rel_size)

    return(
      (p1 / p2 + patchwork::plot_layout(ncol = 1, heights = c(1, 3))) +
        patchwork::plot_annotation(
          caption = "注：图中标注数字为水质浓度（mg/L）。Note: Numbers in the figure indicate water quality concentrations (mg/L).",
          theme = theme(
            plot.caption = element_text(
              size    = base_size * 0.85,
              colour  = "grey30",
              hjust   = 0,          # 左对齐
              margin  = margin(t = 6, unit = "pt")
            )
          )
        )
    )
  }

  # ── ★ logC-logQ ───────────────────────────────────────
  if (plot_mode == "logc_logq") {
    return(
      fw_viz_plot_logC_logQ(
        flow_df            = flow_df,
        wq_df              = wq_df,
        wq_vars            = wq_vars,
        start_time         = start_time,
        end_time           = end_time,
        wq_sites           = wq_sites,
        flow_sites         = flow_sites,
        use_topology_match = use_topology_match,
        clean_list         = clean_list,
        show_regression    = TRUE,
        date_breaks        = date_breaks,
        date_labels        = date_labels,
        base_size          = base_size,
        rel_size           = rel_size
      )
    )
  }



  # ── single ────────────────────────────────────────────
  if (plot_mode == "single") {
    if (single_type == "rain") {
      return(fw_viz_plot_rain(rain_df, start_time, end_time,
                              panel_mode  = rain_panel_mode,
                              date_breaks = date_breaks, date_labels = date_labels,
                              base_size = base_size, rel_size = rel_size))
    }
    if (single_type == "flow") {
      return(fw_viz_plot_flow(flow_df, start_time, end_time,
                              panel_mode  = flow_panel_mode,
                              show_imputed_points = show_imputed_points,
                              date_breaks = date_breaks, date_labels = date_labels,
                              base_size = base_size, rel_size = rel_size))
    }
    if (single_type == "wq") {
      return(fw_viz_plot_wq(wq_df, wq_vars = wq_vars,
                            start_time = start_time, end_time = end_time,
                            panel_mode  = wq_panel_mode,
                            show_smooth = show_wq_smooth,
                            add_standard_lines = add_standard_lines, std_lines = std_lines,
                            date_breaks = date_breaks, date_labels = date_labels,
                            base_size = base_size, rel_size = rel_size))
    }
  }

  ggplot() +
    annotate("text", x = 1, y = 1, label = "无可用图形 / No plot available") +
    theme_void()
}

# =====================================================================
# utils_design.R
# 采样优化设计模块 —— 工具函数
# =====================================================================

if (!exists("%||%", mode = "function")) {

  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# -----------------------------------------------------------------
# 1) 从 clean_list 检测可用水质指标
# -----------------------------------------------------------------

utils_design_available_constituents <- function(clean_list) {

  if (is.null(clean_list) || !is.list(clean_list)) return(character(0))
  wq <- clean_list$WaterQuality
  if (is.null(wq) || !is.data.frame(wq) || nrow(wq) == 0) return(character(0))
  cand <- c("TN", "TP", "NH4N", "NO3N", "COD", "TOC",
            "tn", "tp", "nh4n", "no3n", "cod", "toc")
  found <- intersect(cand, names(wq))
  unique(toupper(found))
}

# -----------------------------------------------------------------
# 2) 列名查找辅助
# -----------------------------------------------------------------

utils_design_find_col <- function(df, candidates) {

  hit <- intersect(candidates, names(df))
  if (length(hit) > 0) hit[1] else NULL
}

# -----------------------------------------------------------------
# 3) 合并 Flow + WaterQuality → 通量计算用 data.frame
#    返回: data.frame(date, Q, C, TM, station?, ...)
# -----------------------------------------------------------------

utils_design_merge_flow_wq <- function(clean_list, target) {

  if (is.null(clean_list) || !is.list(clean_list) ||
      is.null(target)     || target == "") {
    return(NULL)
  }

  # 方案 A: 项目级函数

  if (exists("fw_build_flux_df_from_clean_list", mode = "function")) {
    merged <- tryCatch(
      fw_build_flux_df_from_clean_list(clean_list, target = target),
      error = function(e) NULL
    )
    if (!is.null(merged) && is.data.frame(merged) && nrow(merged) > 0)
      return(merged)
  }

  # 方案 B: 手动合并
  flow_df <- clean_list$Flow
  wq_df   <- clean_list$WaterQuality
  if (is.null(flow_df) || !is.data.frame(flow_df) || nrow(flow_df) == 0) return(NULL)
  if (is.null(wq_df)   || !is.data.frame(wq_df)   || nrow(wq_df)   == 0) return(NULL)

  time_cands <- c("TM", "tm", "datetime", "Datetime", "DateTime",
                  "date", "Date", "TIME", "time", "timestamp")
  flow_cands <- c("Q", "q", "Flow", "flow", "FLOW", "discharge", "Discharge")
  st_cands   <- c("station", "Station", "STATION", "site", "Site", "WYBM")

  ft <- utils_design_find_col(flow_df, time_cands)
  fq <- utils_design_find_col(flow_df, flow_cands)
  wt <- utils_design_find_col(wq_df,  time_cands)
  if (is.null(ft) || is.null(fq) || is.null(wt)) return(NULL)

  # 浓度列（大小写不敏感）
  conc_col <- NULL
  for (nm in names(wq_df)) {
    if (toupper(nm) == toupper(target)) { conc_col <- nm; break }
  }
  if (is.null(conc_col)) return(NULL)

  tryCatch({
    flow_sub <- data.frame(
      date = as.Date(flow_df[[ft]]),
      Q    = as.numeric(flow_df[[fq]]),
      stringsAsFactors = FALSE
    )
    wq_sub <- data.frame(
      date = as.Date(wq_df[[wt]]),
      C    = as.numeric(wq_df[[conc_col]]),
      stringsAsFactors = FALSE
    )

    # 站点列
    for (sc in st_cands) {
      if (sc %in% names(flow_df)) {
        flow_sub$station <- as.character(flow_df[[sc]]); break
      }
    }
    if (!("station" %in% names(flow_sub))) {
      for (sc in st_cands) {
        if (sc %in% names(wq_df)) {
          wq_sub$station <- as.character(wq_df[[sc]]); break
        }
      }
    }

    merged <- merge(flow_sub, wq_sub, by = "date", all.x = TRUE,
                    suffixes = c("", ".wq"))
    if ("station.wq" %in% names(merged)) {
      if (!("station" %in% names(merged)))
        names(merged)[names(merged) == "station.wq"] <- "station"
      else
        merged$station.wq <- NULL
    }

    merged <- merged[!is.na(merged$date) & !is.na(merged$Q), , drop = FALSE]
    merged$TM  <- as.POSIXct(merged$date)
    merged$doy <- as.integer(format(merged$date, "%j"))

    if (nrow(merged) == 0) return(NULL)
    merged
  }, error = function(e) {
    message("[utils_design] 合并 Flow+WQ 失败: ", e$message)
    NULL
  })
}

# -----------------------------------------------------------------
# 4) 将合并数据转为每日数据（如有 fw_prepare_flux_data 则用之）
# -----------------------------------------------------------------

utils_design_prepare_daily <- function(merged_df) {
  if (is.null(merged_df) || nrow(merged_df) == 0) return(NULL)
  if (exists("fw_prepare_flux_data", mode = "function")) {
    res <- tryCatch(fw_prepare_flux_data(merged_df), error = function(e) NULL)
    if (!is.null(res) && is.data.frame(res) && nrow(res) > 0) return(res)
  }
  merged_df
}

# -----------------------------------------------------------------
# 5) 为 DT 添加交互列
# -----------------------------------------------------------------

utils_design_add_dt_cols <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE)
  df <- df[, !(names(df) %in% c(".merge_date")), drop = FALSE]
  df <- cbind(Selected = "ok", df, stringsAsFactors = FALSE)
  df$`.row_id` <- paste0("row_", seq_len(nrow(df)))
  df
}

# -----------------------------------------------------------------
# 6) DT JS 代码生成器
# -----------------------------------------------------------------

utils_design_dt_callback_js <- function(ns_excluded_id) {
  c(
    "table.on('click', 'td:nth-child(1)', function(){",
    "  var td = this;",
    "  var cell = table.cell(td);",
    "  if(cell.data() === 'ok'){",
    "    cell.data('remove');",
    "  } else {",
    "    cell.data('ok');",
    "  }",
    "  var $row = $(td).closest('tr');",
    "  $row.toggleClass('excluded');",
    "  var excludedRows = [];",
    "  table.$('tr').each(function(i, row){",
    "    if($(this).hasClass('excluded')){",
    "      excludedRows.push(parseInt($(this).attr('id').split('_')[1]));",
    "    }",
    "  });",
    sprintf("  Shiny.setInputValue('%s', excludedRows);", ns_excluded_id),
    "})"
  )
}

utils_design_dt_render_js <- function() {
  c(
    'function(data, type, row, meta){',
    '  if(type === "display"){',
    '    var color = data === "ok" ? "forestgreen" : "red";',
    '    return "<span style=\\"color:" + color + ";font-size:18px;cursor:pointer\\"><i class=\\"glyphicon glyphicon-" + data + "\\"></i></span>";',
    '  } else {',
    '    return data;',
    '  }',
    '}'
  )
}

utils_design_dt_restore_js <- function(ns_excluded_id) {
  c(
    "function(e, table, node, config){",
    "  table.$('tr').removeClass('excluded').each(function(){",
    "    var td = $(this).find('td').eq(0)[0];",
    "    if(td){ var cell = table.cell(td); cell.data('ok'); }",
    "  });",
    sprintf("  Shiny.setInputValue('%s', null);", ns_excluded_id),
    "}"
  )
}

# -----------------------------------------------------------------
# 7) 方案应用 JS 生成器
# -----------------------------------------------------------------

utils_design_apply_scheme_js <- function(tbl_id, excluded_idx, ns_excluded_id) {
  sprintf(
    "(function(){
       var $t = $('#%s').find('table');
       if(!$t.length) return;
       var tbl = $t.DataTable();
       tbl.$('tr').removeClass('excluded').each(function(){
         var td = $(this).find('td').eq(0)[0];
         if(td){ tbl.cell(td).data('ok'); }
       });
       var excl = %s;
       tbl.$('tr').each(function(){
         var id = $(this).attr('id');
         if(id){
           var idx = parseInt(id.split('_')[1]);
           if(excl.indexOf(idx) !== -1){
             $(this).addClass('excluded');
             var td = $(this).find('td').eq(0)[0];
             if(td){ tbl.cell(td).data('remove'); }
           }
         }
       });
       Shiny.setInputValue('%s', excl.length > 0 ? excl : null);
     })();",
    tbl_id,
    jsonlite::toJSON(as.integer(excluded_idx)),
    ns_excluded_id
  )
}

# -----------------------------------------------------------------
# 8) 方法标签
# -----------------------------------------------------------------

utils_design_method_label <- function(m) {
  if (exists("fw_flux_method_label", mode = "function")) {
    return(fw_flux_method_label(m))
  }
  switch(m,
         "weighted"   = "加权平均法",
         "interp"     = "插值法",
         "ratio"      = "比率法",
         "regression" = "回归法",
         "composite"  = "复合法",
         m)
}

# -----------------------------------------------------------------
# 9) 数据状态描述
# -----------------------------------------------------------------

utils_design_data_status <- function(clean_list, target, daily_df) {
  if (is.null(clean_list) || !is.list(clean_list)) {
    return(list(ok = FALSE, class = "data-status-fail", icon = "times-circle",
                msg = "未加载数据。请先在「数据导入」页上传并校验文件。"))
  }
  if (is.null(target) || target == "") {
    return(list(ok = FALSE, class = "data-status-fail", icon = "exclamation-triangle",
                msg = sprintf("已加载工作表 [%s]，但未检测到可用水质指标。",
                              paste(names(clean_list), collapse = ", "))))
  }
  if (is.null(daily_df) || nrow(daily_df) == 0) {
    return(list(ok = FALSE, class = "data-status-fail", icon = "times-circle",
                msg = sprintf("指标 %s 的合并数据为空。", target)))
  }

  n_q <- sum(!is.na(daily_df$Q))
  n_c <- sum(!is.na(daily_df$C))
  dr  <- if ("date" %in% names(daily_df)) {
    dd <- as.Date(daily_df$date[!is.na(daily_df$date)])
    if (length(dd) > 0) paste(min(dd), "~", max(dd)) else "未知"
  } else "未知"

  list(ok = TRUE, class = "data-status-ok", icon = "check-circle",
       msg = sprintf("数据就绪！指标: %s | 行数: %d | Q有效: %d | C有效: %d | 时段: %s",
                     target, nrow(daily_df), n_q, n_c, dr))
}

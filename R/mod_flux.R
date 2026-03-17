# =====================================================================
# mod_flux.R
# 通量计算模块 —— 主模块 UI + Server（集成所有方法）
# 回归法专属逻辑由 mod_flux_regression.R 提供
# 复合法专属逻辑由 mod_flux_composite.R 提供
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ----------------------------- UI -----------------------------

mod_flux_method_page_ui <- function(ns, key) {
  # 回归法 / 复合法使用专属参数块，其他方法使用通用数据源选择
  left_extra <- if (identical(key, "regression")) {
    fw_regression_left_extra_ui(ns, key)
  } else if (identical(key, "composite")) {
    fw_composite_left_extra_ui(ns, key)
  } else {
    shiny::tagList(
      shiny::selectInput(
        ns(paste0("datasrc_", key)),
        "\u6570\u636e\u9009\u62e9",
        choices = c("\u81ea\u52a8\uff08\u7b2c\u4e00\u6b65\u5904\u7406\u6570\u636e\uff09" = "auto")
      )
    )
  }

  shiny::tabPanel(
    title = fw_flux_method_label(key),
    value = key,
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shinydashboard::box(
          width = 12, status = "warning", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("book"), " \u65b9\u6cd5\u539f\u7406\uff08\u70b9\u51fb\u53f3\u4e0a\u89d2\u5c55\u5f00/\u6536\u8d77\uff09"),
          collapsible = TRUE, collapsed = TRUE,
          shiny::uiOutput(ns(paste0("principle_", key)))
        ),
        shinydashboard::box(
          width = 12, status = "primary", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("sliders-h"), " \u53c2\u6570\u8bbe\u7f6e"),
          left_extra,
          shiny::dateRangeInput(ns(paste0("daterange_", key)), "\u6570\u636e\u65f6\u95f4\u9009\u62e9", start = NULL, end = NULL),
          shiny::numericInput(ns(paste0("param1_", key)), "\u53c2\u65701", value = 1, step = 0.1),
          shiny::numericInput(ns(paste0("param2_", key)), "\u53c2\u65702", value = 1, step = 0.1),
          shiny::actionButton(
            ns(paste0("run_", key)), "\u5f00\u59cb\u8ba1\u7b97",
            icon = shiny::icon("play"), class = "btn-primary btn-block"
          )
        )
      ),
      shiny::column(
        width = 8,
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("calculator"), " \u7ed3\u7b97\u7ed3\u679c"),
          shiny::verbatimTextOutput(ns(paste0("txt_settle_", key))),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "\u6c47\u603b\u8868\uff08\u65e5\u5c3a\u5ea6\uff09",
              shiny::div(style = "margin: 8px 0;",
                         shiny::downloadButton(ns(paste0("download_summary_", key)), "\u4e0b\u8f7d\u6c47\u603b\u8868", class = "btn-default btn-sm")),
              DT::DTOutput(ns(paste0("tbl_summary_", key)))
            ),
            shiny::tabPanel(
              "\u901a\u91cf\u65f6\u5e8f",
              shiny::div(style = "margin: 8px 0;",
                         shiny::downloadButton(ns(paste0("download_ts_", key)), "\u4e0b\u8f7d\u65f6\u5e8f\u6570\u636e", class = "btn-default btn-sm")),
              plotly::plotlyOutput(ns(paste0("plot_ts_", key)), height = "260px")
            ),
            shiny::tabPanel(
              "\u8bca\u65ad\u56fe",
              shiny::div(style = "margin: 8px 0;",
                         shiny::downloadButton(ns(paste0("download_diag_", key)), "\u4e0b\u8f7d\u8bca\u65ad\u56fe(PNG)", class = "btn-default btn-sm")),
              shiny::plotOutput(ns(paste0("plot_diag_", key)), height = "500px")
            )
          ),
          shiny::tags$hr(),
          shiny::h5("\u5386\u53f2\u65b9\u6848\u8bb0\u5f55"),
          DT::DTOutput(ns(paste0("tbl_history_", key))),
          shiny::fluidRow(
            shiny::column(6,
                          shiny::downloadButton(ns(paste0("download_current_", key)), "\u4e0b\u8f7d\u5f53\u524d\u7ed3\u679c", class = "btn-success btn-block")),
            shiny::column(6,
                          shiny::downloadButton(ns(paste0("download_history_", key)), "\u4e0b\u8f7d\u5386\u53f2\u6c47\u603b", class = "btn-default btn-block"))
          )
        )
      )
    )
  )
}

mod_flux_compare_tab_ui <- function(ns) {
  shiny::tabPanel(
    title = "\u65b9\u6cd5\u5bf9\u6bd4", value = "compare",
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          shiny::checkboxGroupInput(
            ns("compare_methods"), "\u9009\u62e9\u8981\u5bf9\u6bd4\u7684\u65b9\u6cd5",
            choices = c("\u52a0\u6743\u5e73\u5747\u6cd5" = "weighted",
                        "\u63d2\u503c\u6cd5" = "interp",
                        "\u6bd4\u7387\u6cd5" = "ratio",
                        "\u56de\u5f52\u6cd5" = "regression",
                        "\u590d\u5408\u65b9\u6cd5" = "composite"),
            selected = NULL
          ),
          shiny::actionButton(ns("compare_run"), "\u66f4\u65b0\u5bf9\u6bd4", class = "btn-primary btn-block"),
          shiny::helpText("\u6ce8\uff1a\u4ec5\u663e\u793a\u5df2\u6210\u529f\u8ba1\u7b97\u7684\u65b9\u6cd5\u3002")
        )
      ),
      shiny::column(
        width = 9,
        plotly::plotlyOutput(ns("compare_plot"), height = "400px"),
        shiny::tags$br(),
        DT::DTOutput(ns("compare_table"))
      )
    )
  )
}

mod_flux_ui <- function(id, tabName = "flux") {
  ns <- shiny::NS(id)
  shinydashboard::tabItem(
    tabName = tabName,
    shiny::tabsetPanel(
      id = ns("method_top"), type = "tabs",
      mod_flux_method_page_ui(ns, "weighted"),
      mod_flux_method_page_ui(ns, "interp"),
      mod_flux_method_page_ui(ns, "ratio"),
      mod_flux_method_page_ui(ns, "regression"),
      mod_flux_method_page_ui(ns, "composite"),
      mod_flux_compare_tab_ui(ns)
    )
  )
}

# ----------------------------- Server -----------------------------

mod_flux_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    method_keys <- c("weighted", "interp", "ratio", "regression", "composite")
    # 非回归、非复合方法共享 datasrc 同步逻辑
    simple_keys <- c("weighted", "interp", "ratio")
    conv_factor_fixed <- 86.4

    # ======== 初始化 flux_current / flux_history ========
    observeEvent(TRUE, {
      if (is.null(rv$flux_current) || !is.list(rv$flux_current)) {
        rv$flux_current <- setNames(vector("list", length(method_keys)), method_keys)
      } else {
        for (k in method_keys) {
          if (!(k %in% names(rv$flux_current))) rv$flux_current[[k]] <- NULL
        }
      }
      if (is.null(rv$flux_history) || !is.list(rv$flux_history)) {
        rv$flux_history <- fw_init_flux_history(method_keys)
      } else {
        for (k in method_keys) {
          if (is.null(rv$flux_history[[k]])) {
            rv$flux_history[[k]] <- list(meta = data.frame(stringsAsFactors = FALSE), items = list())
          }
        }
      }
    }, once = TRUE)

    # ======== 简单方法（weighted/interp/ratio）的数据源选择 ========

    is_flux_compatible <- function(df) {
      if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(FALSE)
      cc <- tryCatch(fw_find_flux_cols(df), error = function(e) NULL)
      if (is.null(cc)) return(FALSE)
      !is.null(cc$dt) && !is.null(cc$q) && !is.null(cc$c)
    }

    get_wq_targets <- function(cl) {
      if (is.null(cl) || !is.list(cl)) return(character(0))
      if (is.null(cl$WaterQuality) || !is.data.frame(cl$WaterQuality)) return(character(0))
      cand <- c("TN", "TP", "NH4N", "NO3N", "COD", "TOC")
      intersect(cand, names(cl$WaterQuality))
    }

    data_choices <- shiny::reactive({
      ch <- c("\u81ea\u52a8\uff08\u7b2c\u4e00\u6b65\u5904\u7406\u6570\u636e\uff09" = "auto")
      add_choice <- function(x, label, value) c(x, stats::setNames(value, label))

      if (!is.null(rv$clean_df) && is_flux_compatible(rv$clean_df)) {
        ch <- add_choice(ch, "clean_df\uff08\u7b2c\u4e00\u6b65\u6e05\u6d17\u7ed3\u679c\uff09", "clean_df")
      }

      cl <- rv$clean_list
      if (!is.null(cl) && is.list(cl)) {
        tgs <- get_wq_targets(cl)
        if (length(tgs) > 0) {
          for (tg in tgs) {
            tmp <- tryCatch(fw_build_flux_df_from_clean_list(cl, target = tg), error = function(e) NULL)
            if (!is.null(tmp) && is_flux_compatible(tmp)) {
              ch <- add_choice(ch, paste0("Flow + WaterQuality\uff08", tg, "\uff09"), paste0("merge::", tg))
            }
          }
        }
        nm <- names(cl)
        if (length(nm) > 0) {
          for (k in nm) {
            obj <- cl[[k]]
            if (is_flux_compatible(obj)) {
              ch <- add_choice(ch, paste0("clean_list$", k), paste0("list::", k))
            }
          }
        }
      }
      ch <- ch[!duplicated(unname(ch))]
      ch
    })

    # datasrc 同步：仅简单方法（weighted/interp/ratio）
    observe({
      ch <- data_choices()
      for (k in simple_keys) {
        id0 <- paste0("datasrc_", k)
        cur <- isolate(input[[id0]])
        if (is.null(cur) || !(cur %in% unname(ch))) cur <- unname(ch)[1]
        shiny::updateSelectInput(session, id0, choices = ch, selected = cur)
      }
    })

    get_source_df <- function(src) {
      cl <- rv$clean_list
      if (is.null(src) || identical(src, "auto")) {
        if (!is.null(rv$clean_df) && is_flux_compatible(rv$clean_df)) return(rv$clean_df)
        if (!is.null(cl) && is.list(cl)) {
          tgs <- get_wq_targets(cl)
          if (length(tgs) > 0) {
            for (tg in tgs) {
              tmp <- tryCatch(fw_build_flux_df_from_clean_list(cl, target = tg), error = function(e) NULL)
              if (!is.null(tmp) && is_flux_compatible(tmp)) return(tmp)
            }
          }
          nm <- names(cl)
          if (length(nm) > 0) {
            for (k in nm) { obj <- cl[[k]]; if (is_flux_compatible(obj)) return(obj) }
          }
        }
        return(NULL)
      }
      if (identical(src, "clean_df")) return(if (is_flux_compatible(rv$clean_df)) rv$clean_df else NULL)
      if (grepl("^merge::", src)) {
        tg <- sub("^merge::", "", src)
        tmp <- tryCatch(fw_build_flux_df_from_clean_list(cl, target = tg), error = function(e) NULL)
        return(if (!is.null(tmp) && is_flux_compatible(tmp)) tmp else NULL)
      }
      if (grepl("^list::", src)) {
        nm <- sub("^list::", "", src)
        obj <- if (!is.null(cl) && nm %in% names(cl)) cl[[nm]] else NULL
        return(if (!is.null(obj) && is_flux_compatible(obj)) obj else NULL)
      }
      NULL
    }

    # ======== 回归法：初始化专属 observers（委托 mod_flux_regression.R）========

    step1_qf_wq <- fw_regression_step1_reactive(rv)
    fw_regression_init_observers(input, session, step1_qf_wq)

    # ======== 复合法：初始化专属 observers（委托 mod_flux_composite.R）========
    # 复用与回归法相同的 step1_qf_wq reactive
    fw_composite_init_observers(input, session, step1_qf_wq, key = "composite")

    # ======== 每个方法页的通用 Server 逻辑 ========

    for (k in method_keys) local({
      key <- k

      # ---- 方法原理（复合法委托专属函数，其余用通用文本 + withMathJax）----
      output[[paste0("principle_", key)]] <- shiny::renderUI({
        if (identical(key, "composite")) {
          return(fw_composite_render_principle(input, key))
        }
        shiny::withMathJax(
          shiny::tags$div(
            style = "font-size:14px; line-height:1.8;",
            fw_flux_method_principle(key)
          )
        )
      })

      # ---- 日数据获取 ----
      daily_all <- shiny::reactive({
        # 回归法：委托专属函数
        if (identical(key, "regression")) {
          return(fw_regression_get_daily_all(input, step1_qf_wq))
        }
        # 复合法：委托专属函数，复用已有的 step1_qf_wq
        if (identical(key, "composite")) {
          return(fw_composite_get_daily_all(input, key = key,
                                            step1_qf_wq = step1_qf_wq))
        }
        # 其他简单方法（weighted / interp / ratio）
        src <- input[[paste0("datasrc_", key)]]
        raw <- get_source_df(src)
        if (is.null(raw)) return(NULL)
        tryCatch(fw_prepare_flux_data(raw), error = function(e) NULL)
      })

      daily_station <- shiny::reactive({
        d <- daily_all()
        if (is.null(d) || nrow(d) == 0) return(NULL)
        if (!("station" %in% names(d))) return(d)
        st <- unique(stats::na.omit(as.character(d$station)))
        if (length(st) == 0) return(d)
        d[d$station == st[1], , drop = FALSE]
      })

      # ---- 日期范围同步 ----
      observeEvent(daily_station(), {
        d <- daily_station()
        if (is.null(d) || nrow(d) == 0) return()
        if (all(is.na(d$date))) return()

        rg <- range(fw_as_date(d$date), na.rm = TRUE)
        cur <- isolate(input[[paste0("daterange_", key)]])

        st <- rg[1]; ed <- rg[2]
        if (!is.null(cur) && length(cur) == 2 && all(!is.na(cur))) {
          st <- max(as.Date(cur[1]), rg[1])
          ed <- min(as.Date(cur[2]), rg[2])
          if (st > ed) { st <- rg[1]; ed <- rg[2] }
        }

        shiny::updateDateRangeInput(session, paste0("daterange_", key),
                                    start = st, end = ed, min = rg[1], max = rg[2])
      }, ignoreInit = FALSE)

      # ---- 开始计算 ----
      observeEvent(input[[paste0("run_", key)]], {
        res <- NULL

        if (identical(key, "regression")) {
          # 委托回归法专属函数
          res <- fw_regression_run_calc(input, step1_qf_wq, key)

        } else if (identical(key, "composite")) {
          # 委托复合法专属函数
          res <- fw_composite_run_calc(input, daily_station, key, conv_factor_fixed)

          # 附加数据源标签（复合法从 QF/WQ/指标 组合生成）
          qf_sel <- input[[paste0("qf_sheet_", key)]] %||% ""
          wq_sel <- input[[paste0("wq_sheet_", key)]] %||% ""
          con    <- input[[paste0("constituent_", key)]] %||% ""
          src_lab <- paste0("QF:", qf_sel, " / WQ:", wq_sel, " / ", con)
          if (!is.null(res)) {
            res$params$data_source       <- paste0(qf_sel, "::", wq_sel, "::", con)
            res$params$data_source_label <- src_lab
          }

        } else {
          # 其他简单方法（weighted / interp / ratio）
          d <- daily_station()
          if (is.null(d) || nrow(d) == 0) {
            shiny::showNotification("\u5f53\u524d\u6570\u636e\u9009\u62e9\u4e0d\u53ef\u7528\u4e8e\u901a\u91cf\u8ba1\u7b97\uff0c\u8bf7\u66f4\u6362\u6570\u636e\u6e90\u3002", type = "error")
            return()
          }

          res <- tryCatch(
            fw_run_flux_with_config(
              dat_daily   = d,
              method      = key,
              date_range  = input[[paste0("daterange_", key)]],
              param1      = input[[paste0("param1_", key)]],
              param2      = input[[paste0("param2_", key)]],
              conv_factor = conv_factor_fixed
            ),
            error = function(e) { shiny::showNotification(e$message, type = "error"); NULL }
          )

          # 附加数据源标签
          src_val <- input[[paste0("datasrc_", key)]]
          ch <- data_choices()
          src_lab <- names(ch)[match(src_val, unname(ch))]
          if (length(src_lab) == 0 || is.na(src_lab)) src_lab <- src_val %||% "auto"
          if (!is.null(res)) {
            res$params$data_source <- src_val
            res$params$data_source_label <- src_lab
          }
        }

        if (is.null(res)) return()

        station_used <- if ("station" %in% names(res$daily) && nrow(res$daily) > 0) {
          as.character(unique(res$daily$station)[1])
        } else "ALL"

        rv$flux_current[[key]] <- res
        rv$flux_history <- fw_append_flux_history(
          history = rv$flux_history, method = key, res = res, station = station_used)
        rv$flux_df <- res$daily
        rv$flux_results <- rv$flux_current

        shiny::showNotification(
          paste0(fw_flux_method_label(key), " \u8ba1\u7b97\u5b8c\u6210\uff0c\u5df2\u4fdd\u5b58\u5386\u53f2\u65b9\u6848\u3002"), type = "message")
      }, ignoreInit = TRUE)

      # ---- 响应式：当前结果 / 历史 ----
      cur_res <- shiny::reactive({
        x <- rv$flux_current
        if (is.null(x) || is.null(x[[key]])) NULL else x[[key]]
      })

      hist_res <- shiny::reactive({
        h <- rv$flux_history
        if (is.null(h) || is.null(h[[key]])) NULL else h[[key]]
      })

      # ---- 结算文本 ----
      output[[paste0("txt_settle_", key)]] <- shiny::renderText({
        res <- cur_res()
        if (is.null(res)) return("\u5c1a\u672a\u8ba1\u7b97\u3002\u8bf7\u5728\u5de6\u4fa7\u8bbe\u7f6e\u540e\u70b9\u51fb\u5f00\u59cb\u8ba1\u7b97\u3002")

        d <- res$daily
        total_flux <- round(sum(d$flux, na.rm = TRUE), 4)
        mean_flux  <- round(mean(d$flux, na.rm = TRUE), 4)

        st_vals <- if ("station" %in% names(d)) unique(stats::na.omit(as.character(d$station))) else character(0)
        st_txt  <- if (length(st_vals) > 0) paste(st_vals, collapse = ", ") else "ALL"

        wy_vals <- if ("WYBM" %in% names(d)) unique(stats::na.omit(as.character(d$WYBM))) else character(0)
        wy_line <- if (length(wy_vals) > 0) paste0("\nWYBM: ", paste(wy_vals, collapse = ", ")) else ""

        d0 <- fw_as_date(d$date); d0 <- d0[!is.na(d0)]
        dline <- if (length(d0) > 0) paste0(as.character(min(d0)), " ~ ", as.character(max(d0))) else "NA ~ NA"

        # 回归法 / 复合法额外信息委托各自专属函数
        extra <- if (identical(key, "regression")) {
          fw_regression_settle_extra(res)
        } else if (identical(key, "composite")) {
          fw_composite_settle_extra(res)
        } else ""

        paste0(
          "\u65b9\u6cd5: ", res$method_label,
          "\n\u6570\u636e\u6e90: ", res$params$data_source_label %||% "auto",
          "\n\u7ad9\u70b9: ", st_txt, wy_line,
          "\n\u65f6\u95f4: ", dline,
          "\n\u53c2\u65701: ", res$params$param1 %||% 1,
          "\uff1b\u53c2\u65702: ", res$params$param2 %||% 1,
          if (key %in% simple_keys)
            paste0("\n\u6362\u7b97\u7cfb\u6570: ", conv_factor_fixed, "\uff08\u56fa\u5b9a\uff09") else "",
          extra,
          "\n\u603b\u901a\u91cf(kg): ", total_flux,
          "\n\u5e73\u5747\u65e5\u901a\u91cf(kg/d): ", mean_flux
        )
      })

      # ---- 汇总表 ----
      output[[paste0("tbl_summary_", key)]] <- DT::renderDataTable({
        res <- cur_res()
        if (is.null(res)) {
          return(DT::datatable(data.frame(note = "\u6682\u65e0\u7ed3\u679c"), rownames = FALSE, options = list(dom = "t")))
        }

        base_tbl <- res$summary %||% fw_make_flux_summary_table(res$daily)
        tbl <- as.data.frame(base_tbl, stringsAsFactors = FALSE)

        keep <- c("calc_result", "date", "station", "WYBM", "Q", "flux")
        keep <- keep[keep %in% names(tbl)]
        if (length(keep) > 0) tbl <- tbl[, keep, drop = FALSE]

        cn_map <- c(calc_result = "\u8ba1\u7b97\u7ed3\u679c", date = "\u65f6\u95f4",
                    station = "\u7ad9\u70b9", WYBM = "WYBM", Q = "\u6d41\u91cf",
                    flux = "\u901a\u91cf")
        for (nm in names(cn_map)) {
          if (nm %in% names(tbl)) names(tbl)[names(tbl) == nm] <- cn_map[[nm]]
        }

        DT::datatable(tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
      })

      # ---- 通量时序图 ----
      output[[paste0("plot_ts_", key)]] <- plotly::renderPlotly({
        res <- cur_res()
        if (is.null(res)) return(plotly::plot_ly())
        fw_plot_flux_ts(res$daily, title = paste0(res$method_label, " - \u5f53\u524d\u65b9\u6848"))
      })

      # ---- 诊断图（复合法使用专属诊断图，其余用通用）----
      output[[paste0("plot_diag_", key)]] <- shiny::renderPlot({
        res <- cur_res()
        shiny::req(res)
        if (identical(key, "composite")) {
          fw_composite_render_diag(res)
        } else {
          fw_plot_flux_diag(res)
        }
      })

      # ---- 历史表 ----
      output[[paste0("tbl_history_", key)]] <- DT::renderDataTable({
        h <- hist_res()
        if (is.null(h) || is.null(h$meta) || nrow(h$meta) == 0) {
          return(DT::datatable(data.frame(note = "\u6682\u65e0\u5386\u53f2\u8bb0\u5f55"), rownames = FALSE, options = list(dom = "t")))
        }
        DT::datatable(h$meta, rownames = FALSE, options = list(pageLength = 6, scrollX = TRUE))
      })

      # ---- 下载: 汇总表 ----
      output[[paste0("download_summary_", key)]] <- shiny::downloadHandler(
        filename = function() paste0("flux_", key, "_summary_daily_", Sys.Date(), ".csv"),
        content = function(file) {
          res <- cur_res()
          if (is.null(res)) {
            utils::write.csv(data.frame(note = "No summary"), file, row.names = FALSE)
          } else {
            tbl <- as.data.frame(res$summary %||% fw_make_flux_daily_summary(res$daily), stringsAsFactors = FALSE)
            utils::write.csv(tbl, file, row.names = FALSE)
          }
        }
      )

      # ---- 下载: 时序 ----
      output[[paste0("download_ts_", key)]] <- shiny::downloadHandler(
        filename = function() paste0("flux_", key, "_timeseries_daily_", Sys.Date(), ".csv"),
        content = function(file) {
          res <- cur_res()
          if (is.null(res) || is.null(res$daily)) {
            utils::write.csv(data.frame(note = "No time-series"), file, row.names = FALSE)
          } else {
            d <- as.data.frame(res$daily, stringsAsFactors = FALSE)
            if (!("date" %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)
            keep <- c("date", "station", "WYBM", "Q", "C_obs", "C_est", "flux", "method", "C_source")
            keep <- keep[keep %in% names(d)]
            if (length(keep) > 0) d <- d[, keep, drop = FALSE]
            utils::write.csv(d, file, row.names = FALSE)
          }
        }
      )

      # ---- 下载: 诊断图（复合法使用专属诊断图）----
      output[[paste0("download_diag_", key)]] <- shiny::downloadHandler(
        filename = function() paste0("flux_", key, "_diagnostic_", Sys.Date(), ".png"),
        content = function(file) {
          res <- cur_res()
          grDevices::png(file, width = 1600, height = 1000, res = 130)
          on.exit(grDevices::dev.off(), add = TRUE)
          if (is.null(res)) {
            graphics::plot.new(); graphics::text(0.5, 0.5, "No diagnostic result")
          } else if (identical(key, "composite")) {
            fw_composite_render_diag(res)
          } else {
            fw_plot_flux_diag(res)
          }
        }
      )

      # ---- 下载: 当前结果 ----
      output[[paste0("download_current_", key)]] <- shiny::downloadHandler(
        filename = function() {
          res <- cur_res()
          st <- "station"
          if (!is.null(res) && is.data.frame(res$daily) && nrow(res$daily) > 0 && "station" %in% names(res$daily)) {
            st <- as.character(unique(res$daily$station)[1])
          }
          st <- gsub("[^[:alnum:]_\\-]", "_", st)
          paste0("flux_", key, "_", st, "_current_", Sys.Date(), ".csv")
        },
        content = function(file) {
          res <- cur_res()
          if (is.null(res)) {
            utils::write.csv(data.frame(note = "No result"), file, row.names = FALSE)
          } else {
            utils::write.csv(res$daily, file, row.names = FALSE)
          }
        }
      )

      # ---- 下载: 历史 ----
      output[[paste0("download_history_", key)]] <- shiny::downloadHandler(
        filename = function() paste0("flux_", key, "_history_", Sys.Date(), ".csv"),
        content = function(file) {
          h <- hist_res()
          if (is.null(h) || is.null(h$meta) || nrow(h$meta) == 0) {
            utils::write.csv(data.frame(note = "No history"), file, row.names = FALSE)
          } else {
            utils::write.csv(h$meta, file, row.names = FALSE)
          }
        }
      )
    })

    # ======== 方法对比 ========

    available_methods <- shiny::reactive({
      cur <- rv$flux_current
      if (is.null(cur) || !is.list(cur)) return(character(0))
      keep <- vapply(method_keys, function(m) {
        x <- cur[[m]]
        !is.null(x) && is.list(x) && !is.null(x$daily) && is.data.frame(x$daily) && nrow(x$daily) > 0
      }, logical(1))
      method_keys[keep]
    })

    observe({
      avail <- available_methods()
      if (length(avail) == 0) {
        shiny::updateCheckboxGroupInput(session, "compare_methods", choices = character(0), selected = character(0))
        return()
      }
      ch <- stats::setNames(avail, vapply(avail, fw_flux_method_label, character(1)))
      sel <- intersect(isolate(input$compare_methods), avail)
      if (length(sel) == 0) sel <- avail[1]
      shiny::updateCheckboxGroupInput(session, "compare_methods", choices = ch, selected = sel)
    })

    compare_long <- shiny::eventReactive(
      list(input$compare_run, available_methods()),
      {
        avail <- available_methods()
        if (length(avail) == 0) return(NULL)
        sel <- input$compare_methods %||% character(0)
        sel <- intersect(sel, avail)
        if (length(sel) == 0) sel <- avail[1]

        out <- lapply(sel, function(m) {
          res <- rv$flux_current[[m]]
          if (is.null(res) || !is.list(res) || is.null(res$daily) || !is.data.frame(res$daily)) return(NULL)
          d <- as.data.frame(res$daily, stringsAsFactors = FALSE)
          if (!("date" %in% names(d)) && "TM" %in% names(d)) d$date <- fw_as_date(d$TM)
          if (!all(c("date", "flux") %in% names(d))) return(NULL)
          dd <- data.frame(date = fw_as_date(d$date), flux = fw_as_num(d$flux),
                           method = m, method_label = fw_flux_method_label(m), stringsAsFactors = FALSE)
          dd <- dd[!is.na(dd$date) & is.finite(dd$flux), , drop = FALSE]
          if (nrow(dd) == 0) NULL else dd
        })
        out <- Filter(Negate(is.null), out)
        if (length(out) == 0) return(NULL)
        dd <- do.call(rbind, out)
        dd <- dd[order(dd$date), , drop = FALSE]; rownames(dd) <- NULL
        dd
      }, ignoreNULL = FALSE, ignoreInit = FALSE
    )

    output$compare_plot <- plotly::renderPlotly({
      dd <- compare_long()
      if (is.null(dd) || nrow(dd) == 0) return(plotly::plot_ly())
      p <- plotly::plot_ly(dd, x = ~date, y = ~flux, color = ~method_label,
                           type = "scatter", mode = "lines",
                           hovertemplate = "\u65e5\u671f: %{x}<br>\u901a\u91cf: %{y:.4f} kg/d<extra></extra>")
      plotly::layout(p, title = "\u65b9\u6cd5\u5bf9\u6bd4\uff1a\u65e5\u901a\u91cf\u65f6\u5e8f",
                     xaxis = list(title = "\u65e5\u671f"), yaxis = list(title = "\u901a\u91cf (kg/d)"),
                     legend = list(orientation = "h"))
    })

    output$compare_table <- DT::renderDT({
      dd <- compare_long()
      if (is.null(dd) || nrow(dd) == 0) {
        return(DT::datatable(data.frame(note = "\u6682\u65e0\u53ef\u5bf9\u6bd4\u6570\u636e"), rownames = FALSE, options = list(dom = "t")))
      }
      sp <- split(dd, dd$method_label)
      one_row <- function(lbl, z) {
        z <- z[order(z$date), , drop = FALSE]; d_ok <- z$date[!is.na(z$date)]
        df <- data.frame(
          method_name = lbl,
          total_flux  = round(sum(z$flux, na.rm = TRUE), 4),
          mean_flux   = round(mean(z$flux, na.rm = TRUE), 4),
          valid_days  = sum(is.finite(z$flux)),
          start_date  = if (length(d_ok) > 0) as.character(min(d_ok)) else NA_character_,
          end_date    = if (length(d_ok) > 0) as.character(max(d_ok)) else NA_character_,
          stringsAsFactors = FALSE)
        names(df) <- c("\u65b9\u6cd5\u540d\u79f0",
                       "\u603b\u901a\u91cf(kg)",
                       "\u5e73\u5747\u65e5\u901a\u91cf(kg/d)",
                       "\u6709\u6548\u5929\u6570",
                       "\u8d77\u59cb\u65e5\u671f",
                       "\u7ed3\u675f\u65e5\u671f")
        df
      }

      tbl <- do.call(rbind, lapply(names(sp), function(lbl) one_row(lbl, sp[[lbl]])))
      rownames(tbl) <- NULL
      DT::datatable(tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
}

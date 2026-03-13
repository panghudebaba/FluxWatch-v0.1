if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ----------------------------- UI -----------------------------
mod_flux_method_page_ui <- function(ns, key) {
  left_extra <- if (identical(key, "regression")) {
    shiny::tagList(
      shiny::helpText("回归方法固定调用第一步处理后的 QF/WQ 数据"),
      shiny::selectInput(ns(paste0("qf_sheet_", key)), "QF数据表", choices = NULL),
      shiny::selectInput(ns(paste0("wq_sheet_", key)), "WQ数据表", choices = NULL),
      shiny::selectInput(ns(paste0("constituent_", key)), "水质指标(j)", choices = NULL),
      shiny::selectInput(
        ns(paste0("reg_model_", key)),
        "回归模型",
        choices = c("季节回归" = "loadLm_season", "线性回归" = "loadLm_simple"),
        selected = "loadLm_season"
      )
    )
  } else {
    shiny::tagList(
      shiny::selectInput(
        ns(paste0("datasrc_", key)),
        "数据选择",
        choices = c("自动（第一步处理数据）" = "auto")
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
          width = 12,
          status = "warning",
          solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("book"), " 方法原理（点击右上角展开/收起）"),
          collapsible = TRUE,
          collapsed = TRUE,
          shiny::uiOutput(ns(paste0("principle_", key)))
        ),
        shinydashboard::box(
          width = 12,
          status = "primary",
          solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("sliders-h"), " 参数设置"),
          left_extra,
          shiny::dateRangeInput(ns(paste0("daterange_", key)), "数据时间选择", start = NULL, end = NULL),
          shiny::numericInput(ns(paste0("param1_", key)), "参数1", value = 1, step = 0.1),
          shiny::numericInput(ns(paste0("param2_", key)), "参数2", value = 1, step = 0.1),
          shiny::actionButton(
            ns(paste0("run_", key)),
            "开始计算",
            icon = shiny::icon("play"),
            class = "btn-primary btn-block"
          )
        )
      ),

      shiny::column(
        width = 8,
        shinydashboard::box(
          width = 12,
          status = "info",
          solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("calculator"), " 结算结果"),
          shiny::verbatimTextOutput(ns(paste0("txt_settle_", key))),

          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel(
              "汇总表（日尺度）",
              shiny::div(
                style = "margin: 8px 0;",
                shiny::downloadButton(
                  ns(paste0("download_summary_", key)),
                  "下载汇总表",
                  class = "btn-default btn-sm"
                )
              ),
              DT::DTOutput(ns(paste0("tbl_summary_", key)))
            ),
            shiny::tabPanel(
              "通量时序",
              shiny::div(
                style = "margin: 8px 0;",
                shiny::downloadButton(
                  ns(paste0("download_ts_", key)),
                  "下载时序数据",
                  class = "btn-default btn-sm"
                )
              ),
              plotly::plotlyOutput(ns(paste0("plot_ts_", key)), height = "260px")
            ),
            shiny::tabPanel(
              "诊断图",
              shiny::div(
                style = "margin: 8px 0;",
                shiny::downloadButton(
                  ns(paste0("download_diag_", key)),
                  "下载诊断图(PNG)",
                  class = "btn-default btn-sm"
                )
              ),
              shiny::plotOutput(ns(paste0("plot_diag_", key)), height = "260px")
            )
          ),

          shiny::tags$hr(),
          shiny::h5("历史方案记录"),
          DT::DTOutput(ns(paste0("tbl_history_", key))),
          shiny::fluidRow(
            shiny::column(
              6,
              shiny::downloadButton(
                ns(paste0("download_current_", key)),
                "下载当前结果",
                class = "btn-success btn-block"
              )
            ),
            shiny::column(
              6,
              shiny::downloadButton(
                ns(paste0("download_history_", key)),
                "下载历史汇总",
                class = "btn-default btn-block"
              )
            )
          )
        )
      )
    )
  )
}

# 新增：方法对比页
mod_flux_compare_tab_ui <- function(ns) {
  shiny::tabPanel(
    title = "方法对比",
    value = "compare",
    shiny::fluidRow(
      shiny::column(
        width = 3,
        shiny::wellPanel(
          shiny::checkboxGroupInput(
            ns("compare_methods"),
            "选择要对比的方法",
            choices = c(
              "加权平均法" = "weighted",
              "插值法" = "interp",
              "比率法" = "ratio",
              "回归法" = "regression",
              "复合方法" = "composite"
            ),
            selected = NULL
          ),
          shiny::actionButton(
            ns("compare_run"),
            "更新对比",
            class = "btn-primary btn-block"
          ),
          shiny::helpText("注：仅显示已成功计算的方法。")
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
      id = ns("method_top"),
      type = "tabs",
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
    non_reg_keys <- setdiff(method_keys, "regression")
    conv_factor_fixed <- 86.4

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
            rv$flux_history[[k]] <- list(
              meta = data.frame(stringsAsFactors = FALSE),
              items = list()
            )
          }
        }
      }
    }, once = TRUE)

    # -------- 非回归方法的数据源选择 --------
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
      ch <- c("自动（第一步处理数据）" = "auto")
      add_choice <- function(x, label, value) c(x, stats::setNames(value, label))

      if (!is.null(rv$clean_df) && is_flux_compatible(rv$clean_df)) {
        ch <- add_choice(ch, "clean_df（第一步清洗结果）", "clean_df")
      }

      cl <- rv$clean_list
      if (!is.null(cl) && is.list(cl)) {
        tgs <- get_wq_targets(cl)
        if (length(tgs) > 0) {
          for (tg in tgs) {
            tmp <- tryCatch(
              fw_build_flux_df_from_clean_list(cl, target = tg),
              error = function(e) NULL
            )
            if (!is.null(tmp) && is_flux_compatible(tmp)) {
              ch <- add_choice(ch, paste0("Flow + WaterQuality（", tg, "）"), paste0("merge::", tg))
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

    observe({
      ch <- data_choices()
      for (k in non_reg_keys) {
        id0 <- paste0("datasrc_", k)
        cur <- isolate(input[[id0]])
        if (is.null(cur) || !(cur %in% unname(ch))) cur <- unname(ch)[1]
        shiny::updateSelectInput(session, id0, choices = ch, selected = cur)
      }
    })

    get_source_df <- function(src) {
      cl <- rv$clean_list

      if (is.null(src) || identical(src, "auto")) {
        if (!is.null(rv$clean_df) && is_flux_compatible(rv$clean_df)) {
          return(rv$clean_df)
        }

        if (!is.null(cl) && is.list(cl)) {
          tgs <- get_wq_targets(cl)
          if (length(tgs) > 0) {
            for (tg in tgs) {
              tmp <- tryCatch(
                fw_build_flux_df_from_clean_list(cl, target = tg),
                error = function(e) NULL
              )
              if (!is.null(tmp) && is_flux_compatible(tmp)) return(tmp)
            }
          }

          nm <- names(cl)
          if (length(nm) > 0) {
            for (k in nm) {
              obj <- cl[[k]]
              if (is_flux_compatible(obj)) return(obj)
            }
          }
        }
        return(NULL)
      }

      if (identical(src, "clean_df")) {
        return(if (is_flux_compatible(rv$clean_df)) rv$clean_df else NULL)
      }

      if (grepl("^merge::", src)) {
        tg <- sub("^merge::", "", src)
        tmp <- tryCatch(
          fw_build_flux_df_from_clean_list(cl, target = tg),
          error = function(e) NULL
        )
        return(if (!is.null(tmp) && is_flux_compatible(tmp)) tmp else NULL)
      }

      if (grepl("^list::", src)) {
        nm <- sub("^list::", "", src)
        obj <- if (!is.null(cl) && nm %in% names(cl)) cl[[nm]] else NULL
        return(if (!is.null(obj) && is_flux_compatible(obj)) obj else NULL)
      }

      NULL
    }

    # -------- 回归方法：第一步 QF/WQ --------
    step1_qf_wq <- shiny::reactive({
      fw_get_step1_qf_wq(rv)
    })

    observe({
      s1 <- step1_qf_wq()
      if (is.null(s1)) {
        shiny::updateSelectInput(session, "qf_sheet_regression", choices = character(0))
        shiny::updateSelectInput(session, "wq_sheet_regression", choices = character(0))
        return()
      }

      qf_list <- fw_as_named_table_list(s1$QF, "QF")
      wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
      if (length(qf_list) == 0 || length(wq_list) == 0) {
        shiny::updateSelectInput(session, "qf_sheet_regression", choices = character(0))
        shiny::updateSelectInput(session, "wq_sheet_regression", choices = character(0))
        return()
      }

      qf_nm <- names(qf_list)
      wq_nm <- names(wq_list)

      cur_qf <- isolate(input$qf_sheet_regression)
      cur_wq <- isolate(input$wq_sheet_regression)
      if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
      if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

      shiny::updateSelectInput(session, "qf_sheet_regression", choices = qf_nm, selected = cur_qf)
      shiny::updateSelectInput(session, "wq_sheet_regression", choices = wq_nm, selected = cur_wq)
    })

    observe({
      s1 <- step1_qf_wq()
      if (is.null(s1)) {
        shiny::updateSelectInput(session, "constituent_regression", choices = character(0))
        return()
      }

      wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
      if (length(wq_list) == 0) {
        shiny::updateSelectInput(session, "constituent_regression", choices = character(0))
        return()
      }

      ws <- input$wq_sheet_regression
      if (is.null(ws) || !(ws %in% names(wq_list))) ws <- names(wq_list)[1]

      cands <- fw_get_wq_constituents(wq_list[[ws]])
      if (length(cands) == 0) cands <- names(wq_list[[ws]])

      cur <- isolate(input$constituent_regression)
      if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

      shiny::updateSelectInput(session, "constituent_regression", choices = cands, selected = cur)
    })

    # -------- 每个方法页 --------
    for (k in method_keys) local({
      key <- k

      output[[paste0("principle_", key)]] <- shiny::renderUI({
        shiny::tags$div(
          style = "font-size:14px; line-height:1.8;",
          fw_flux_method_principle(key)
        )
      })

      daily_all <- shiny::reactive({
        if (identical(key, "regression")) {
          s1 <- step1_qf_wq()
          if (is.null(s1)) return(NULL)

          prep <- tryCatch(
            fw_prepare_regression_input(
              step1_data = s1,
              qf_sheet = input[[paste0("qf_sheet_", key)]],
              wq_sheet = input[[paste0("wq_sheet_", key)]],
              constituent = input[[paste0("constituent_", key)]],
              date_range = NULL
            ),
            error = function(e) NULL
          )
          if (is.null(prep)) return(NULL)

          out <- data.frame(
            station = prep$station,
            WYBM = prep$wybm %||% NA_character_,
            date = prep$dat$TM,
            Q = prep$dat$Q,
            C_obs = prep$dat$conc,
            stringsAsFactors = FALSE
          )
          return(out)
        }

        src <- input[[paste0("datasrc_", key)]]
        raw <- get_source_df(src)
        if (is.null(raw)) return(NULL)

        tryCatch(
          fw_prepare_flux_data(raw),
          error = function(e) NULL
        )
      })

      daily_station <- shiny::reactive({
        d <- daily_all()
        if (is.null(d) || nrow(d) == 0) return(NULL)

        if (!("station" %in% names(d))) return(d)

        st <- unique(stats::na.omit(as.character(d$station)))
        if (length(st) == 0) return(d)

        d[d$station == st[1], , drop = FALSE]
      })

      observeEvent(daily_station(), {
        d <- daily_station()
        if (is.null(d) || nrow(d) == 0) return()
        if (all(is.na(d$date))) return()

        rg <- range(fw_as_date(d$date), na.rm = TRUE)
        cur <- isolate(input[[paste0("daterange_", key)]])

        st <- rg[1]
        ed <- rg[2]

        if (!is.null(cur) && length(cur) == 2 && all(!is.na(cur))) {
          st <- max(as.Date(cur[1]), rg[1])
          ed <- min(as.Date(cur[2]), rg[2])
          if (st > ed) {
            st <- rg[1]
            ed <- rg[2]
          }
        }

        shiny::updateDateRangeInput(
          session,
          inputId = paste0("daterange_", key),
          start = st,
          end = ed,
          min = rg[1],
          max = rg[2]
        )
      }, ignoreInit = FALSE)

      observeEvent(input[[paste0("run_", key)]], {
        res <- NULL

        if (identical(key, "regression")) {
          s1 <- step1_qf_wq()
          if (is.null(s1)) {
            shiny::showNotification("未找到第一步 QF/WQ 数据。", type = "error")
            return()
          }

          res <- tryCatch(
            fw_run_flux_with_config(
              method = "regression",
              date_range = input[[paste0("daterange_", key)]],
              param1 = input[[paste0("param1_", key)]],
              param2 = input[[paste0("param2_", key)]],
              step1_data = s1,
              regression_cfg = list(
                qf_sheet = input[[paste0("qf_sheet_", key)]],
                wq_sheet = input[[paste0("wq_sheet_", key)]],
                constituent = input[[paste0("constituent_", key)]],
                model_choice = input[[paste0("reg_model_", key)]]
              )
            ),
            error = function(e) {
              shiny::showNotification(e$message, type = "error")
              NULL
            }
          )
        } else {
          d <- daily_station()
          if (is.null(d) || nrow(d) == 0) {
            shiny::showNotification("当前“数据选择”不可用于通量计算，请更换数据源。", type = "error")
            return()
          }

          res <- tryCatch(
            fw_run_flux_with_config(
              dat_daily = d,
              method = key,
              date_range = input[[paste0("daterange_", key)]],
              param1 = input[[paste0("param1_", key)]],
              param2 = input[[paste0("param2_", key)]],
              conv_factor = conv_factor_fixed
            ),
            error = function(e) {
              shiny::showNotification(e$message, type = "error")
              NULL
            }
          )

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
        } else {
          "ALL"
        }

        rv$flux_current[[key]] <- res
        rv$flux_history <- fw_append_flux_history(
          history = rv$flux_history,
          method = key,
          res = res,
          station = station_used
        )

        rv$flux_df <- res$daily
        rv$flux_results <- rv$flux_current

        shiny::showNotification(
          paste0(fw_flux_method_label(key), " 计算完成，已保存历史方案。"),
          type = "message"
        )
      }, ignoreInit = TRUE)

      cur_res <- shiny::reactive({
        x <- rv$flux_current
        if (is.null(x) || is.null(x[[key]])) return(NULL)
        x[[key]]
      })

      hist_res <- shiny::reactive({
        h <- rv$flux_history
        if (is.null(h) || is.null(h[[key]])) return(NULL)
        h[[key]]
      })

      output[[paste0("txt_settle_", key)]] <- shiny::renderText({
        res <- cur_res()
        if (is.null(res)) {
          return("尚未计算。请在左侧设置后点击“开始计算”。")
        }

        d <- res$daily
        total_flux <- round(sum(d$flux, na.rm = TRUE), 4)
        mean_flux <- round(mean(d$flux, na.rm = TRUE), 4)

        st_vals <- if ("station" %in% names(d)) unique(stats::na.omit(as.character(d$station))) else character(0)
        st_txt <- if (length(st_vals) > 0) paste(st_vals, collapse = ", ") else "ALL"

        wy_vals <- if ("WYBM" %in% names(d)) unique(stats::na.omit(as.character(d$WYBM))) else character(0)
        wy_line <- if (length(wy_vals) > 0) paste0("\nWYBM: ", paste(wy_vals, collapse = ", ")) else ""

        d0 <- fw_as_date(d$date)
        d0 <- d0[!is.na(d0)]
        dline <- if (length(d0) > 0) {
          paste0(as.character(min(d0)), " ~ ", as.character(max(d0)))
        } else {
          "NA ~ NA"
        }

        extra <- ""
        if (identical(key, "regression")) {
          extra <- paste0(
            "\nQF表: ", res$params$qf_sheet %||% "",
            "\nWQ表: ", res$params$wq_sheet %||% "",
            "\n指标: ", res$params$constituent %||% "",
            "\n模型: ", res$params$model_choice %||% ""
          )
        }

        paste0(
          "方法: ", res$method_label,
          "\n数据源: ", res$params$data_source_label %||% "auto",
          "\n站点: ", st_txt,
          wy_line,
          "\n时间: ", dline,
          "\n参数1: ", res$params$param1 %||% 1, "；参数2: ", res$params$param2 %||% 1,
          if (!identical(key, "regression")) paste0("\n换算系数: ", conv_factor_fixed, "（固定）") else "",
          extra,
          "\n总通量(kg): ", total_flux,
          "\n平均日通量(kg/d): ", mean_flux
        )
      })

      output[[paste0("tbl_summary_", key)]] <- DT::renderDataTable({
        res <- cur_res()
        if (is.null(res)) {
          return(DT::datatable(
            data.frame(note = "暂无结果"),
            rownames = FALSE,
            options = list(dom = "t")
          ))
        }

        base_tbl <- res$summary %||% fw_make_flux_summary_table(res$daily)
        tbl <- as.data.frame(base_tbl, stringsAsFactors = FALSE)

        keep <- c("calc_result", "date", "station", "WYBM", "Q", "flux")
        keep <- keep[keep %in% names(tbl)]
        if (length(keep) > 0) tbl <- tbl[, keep, drop = FALSE]

        cn_map <- c(
          calc_result = "计算结果",
          date = "时间",
          station = "站点",
          WYBM = "WYBM",
          Q = "流量",
          flux = "通量"
        )
        for (nm in names(cn_map)) {
          if (nm %in% names(tbl)) names(tbl)[names(tbl) == nm] <- cn_map[[nm]]
        }

        DT::datatable(tbl, rownames = FALSE, options = list(pageLength = 10, scrollX = TRUE))
      })

      output[[paste0("plot_ts_", key)]] <- plotly::renderPlotly({
        res <- cur_res()
        if (is.null(res)) return(plotly::plot_ly())
        fw_plot_flux_ts(res$daily, title = paste0(res$method_label, " - 当前方案"))
      })

      output[[paste0("plot_diag_", key)]] <- shiny::renderPlot({
        res <- cur_res()
        shiny::req(res)
        fw_plot_flux_diag(res)
      })

      output[[paste0("tbl_history_", key)]] <- DT::renderDataTable({
        h <- hist_res()
        if (is.null(h) || is.null(h$meta) || nrow(h$meta) == 0) {
          return(DT::datatable(
            data.frame(note = "暂无历史记录"),
            rownames = FALSE,
            options = list(dom = "t")
          ))
        }

        DT::datatable(h$meta, rownames = FALSE, options = list(pageLength = 6, scrollX = TRUE))
      })

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

      output[[paste0("download_diag_", key)]] <- shiny::downloadHandler(
        filename = function() paste0("flux_", key, "_diagnostic_", Sys.Date(), ".png"),
        content = function(file) {
          res <- cur_res()
          grDevices::png(file, width = 1400, height = 900, res = 130)
          on.exit(grDevices::dev.off(), add = TRUE)

          if (is.null(res)) {
            graphics::plot.new()
            graphics::text(0.5, 0.5, "No diagnostic result")
          } else {
            fw_plot_flux_diag(res)
          }
        }
      )

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

    # -------- 新增：方法对比逻辑 --------
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
        shiny::updateCheckboxGroupInput(
          session, "compare_methods",
          choices = character(0), selected = character(0)
        )
        return()
      }

      ch <- stats::setNames(avail, vapply(avail, fw_flux_method_label, character(1)))
      sel <- intersect(isolate(input$compare_methods), avail)
      if (length(sel) == 0) sel <- avail[1]

      shiny::updateCheckboxGroupInput(
        session, "compare_methods",
        choices = ch,
        selected = sel
      )
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

          dd <- data.frame(
            date = fw_as_date(d$date),
            flux = fw_as_num(d$flux),
            method = m,
            method_label = fw_flux_method_label(m),
            stringsAsFactors = FALSE
          )

          dd <- dd[!is.na(dd$date) & is.finite(dd$flux), , drop = FALSE]
          if (nrow(dd) == 0) return(NULL)
          dd
        })

        out <- Filter(Negate(is.null), out)
        if (length(out) == 0) return(NULL)

        dd <- do.call(rbind, out)
        dd <- dd[order(dd$date), , drop = FALSE]
        rownames(dd) <- NULL
        dd
      },
      ignoreNULL = FALSE,
      ignoreInit = FALSE
    )

    output$compare_plot <- plotly::renderPlotly({
      dd <- compare_long()
      if (is.null(dd) || nrow(dd) == 0) return(plotly::plot_ly())

      p <- plotly::plot_ly(
        dd,
        x = ~date,
        y = ~flux,
        color = ~method_label,
        type = "scatter",
        mode = "lines",
        hovertemplate = "日期: %{x}<br>通量: %{y:.4f} kg/d<extra></extra>"
      )

      plotly::layout(
        p,
        title = "方法对比：日通量时序",
        xaxis = list(title = "日期"),
        yaxis = list(title = "通量 (kg/d)"),
        legend = list(orientation = "h")
      )
    })

    output$compare_table <- DT::renderDT({
      dd <- compare_long()
      if (is.null(dd) || nrow(dd) == 0) {
        return(DT::datatable(
          data.frame(note = "暂无可对比数据"),
          rownames = FALSE,
          options = list(dom = "t")
        ))
      }

      sp <- split(dd, dd$method_label)

      one_row <- function(lbl, z) {
        z <- z[order(z$date), , drop = FALSE]
        d_ok <- z$date[!is.na(z$date)]
        data.frame(
          方法名称 = lbl,
          `总通量(kg)` = round(sum(z$flux, na.rm = TRUE), 4),
          `平均日通量(kg/d)` = round(mean(z$flux, na.rm = TRUE), 4),
          有效天数 = sum(is.finite(z$flux)),
          起始日期 = if (length(d_ok) > 0) as.character(min(d_ok)) else NA_character_,
          结束日期 = if (length(d_ok) > 0) as.character(max(d_ok)) else NA_character_,
          check.names = FALSE,
          stringsAsFactors = FALSE
        )
      }

      tbl <- do.call(rbind, lapply(names(sp), function(lbl) one_row(lbl, sp[[lbl]])))
      rownames(tbl) <- NULL

      DT::datatable(
        tbl,
        rownames = FALSE,
        options = list(pageLength = 10, scrollX = TRUE)
      )
    })
  })
}

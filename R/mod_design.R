# =====================================================================
# mod_design.R
# 采样优化设计模块 —— UI + Server（精简版，委托 fct / utils）
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}

# ============================= UI =====================================

mod_design_ui <- function(id) {

  ns <- NS(id)

  shinydashboard::tabItem(
    tabName = "design",

    shiny::tags$head(shiny::tags$style(shiny::HTML(
      ".excluded { color: rgb(211,211,211); font-style: italic; }
       .retain-badge  { display:inline-block; padding:4px 10px; border-radius:4px;
                        font-weight:bold; font-size:15px; }
       .retain-good   { background:#d4edda; color:#155724; }
       .retain-warn   { background:#fff3cd; color:#856404; }
       .retain-danger  { background:#f8d7da; color:#721c24; }
       .data-status-ok   { color:#155724; background:#d4edda; padding:6px 10px;
                           border-radius:4px; margin:4px 0; }
       .data-status-fail { color:#721c24; background:#f8d7da; padding:6px 10px;
                           border-radius:4px; margin:4px 0; }
       .method-group-hdr { font-weight:bold; margin-top:6px; color:#555;
                           border-bottom:1px solid #ddd; padding-bottom:2px; }"
    ))),

    shiny::fluidRow(

      # ============ 左侧面板 ============
      shinydashboard::box(
        width = 4, status = "primary", solidHeader = TRUE,
        title = shiny::tagList(shiny::icon("drafting-compass"),
                               " 采样优化设计 Sampling Design"),

        # -- 数据源 --
        shiny::h5(shiny::icon("database"), " 数据来源"),
        shiny::selectInput(ns("design_constituent"), "水质指标 Constituent",
                           choices = c("请先加载数据" = ""), selected = ""),
        shiny::uiOutput(ns("ui_data_status")),

        shiny::tags$hr(),

        # -- 采样方案 --
        shiny::h5(shiny::icon("filter"), " 采样方案"),
        shiny::radioButtons(
          ns("design_scheme"), "删除方式",
          choices = c("手动点选"   = "manual",
                      "等间隔抽稀" = "systematic",
                      "随机抽稀"   = "random",
                      "按时间频率" = "frequency"),
          selected = "manual"
        ),

        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'systematic'", ns("design_scheme")),
          shiny::selectInput(ns("design_sys_interval"), "保留间隔",
                             choices = c("每2个"="2","每3个"="3","每4个"="4","每5个"="5",
                                         "每7个"="7","每10个"="10","每14个"="14","每30个"="30"),
                             selected = "4")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'random'", ns("design_scheme")),
          shiny::sliderInput(ns("design_random_pct"), "保留比例%",
                             min = 10, max = 90, value = 50, step = 5, post = "%"),
          shiny::numericInput(ns("design_random_seed"), "随机种子", value = 42, min = 1, max = 99999)
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] == 'frequency'", ns("design_scheme")),
          shiny::selectInput(ns("design_freq"), "目标频率",
                             choices = c("2h"="2h","4h"="4h","6h"="6h","8h"="8h","12h"="12h",
                                         "24h"="24h","48h"="48h","7d"="7d","14d"="14d","30d"="30d"),
                             selected = "24h")
        ),
        shiny::conditionalPanel(
          condition = sprintf("input['%s'] != 'manual'", ns("design_scheme")),
          shiny::actionButton(ns("btn_apply_scheme"), "应用方案",
                              icon = shiny::icon("magic"), class = "btn-warning btn-block")
        ),

        shiny::tags$hr(),

        # -- 通量方法 --
        shiny::h5(shiny::icon("calculator"), " 通量计算方法"),
        shiny::tags$div(class = "method-group-hdr", "基础方法"),
        shiny::checkboxGroupInput(
          ns("design_methods_basic"), NULL,
          choices  = c("加权平均法"="weighted", "插值法"="interp", "比率法"="ratio","回归法"="regression", "复合法"="composite"),
          selected = c("weighted", "interp", "ratio","regression","composite" )
        ),

        # 回归参数
        shiny::conditionalPanel(
          condition = sprintf(
            "input['%s'] && input['%s'].indexOf('regression') > -1",
            ns("design_methods_advanced"), ns("design_methods_advanced")),
          shiny::wellPanel(
            style = "background:#f8f9fa; padding:8px 12px;",
            shiny::tags$strong(shiny::icon("chart-line"), " 回归法参数"),
            shiny::selectInput(ns("design_reg_submethod"), "子方法",
                               choices = c("线性"="abs_linear","对数线性"="log_linear",
                                           "幂函数"="power","多项式2阶"="poly2"),
                               selected = "abs_linear"),
            shiny::checkboxInput(ns("design_reg_log"), "对浓度取 log", value = FALSE),
            shiny::checkboxInput(ns("design_reg_smear"), "Smearing 校正", value = TRUE)
          )
        ),

        # 复合参数
        shiny::conditionalPanel(
          condition = sprintf(
            "input['%s'] && input['%s'].indexOf('composite') > -1",
            ns("design_methods_advanced"), ns("design_methods_advanced")),
          shiny::wellPanel(
            style = "background:#f8f9fa; padding:8px 12px;",
            shiny::tags$strong(shiny::icon("layer-group"), " 复合法参数"),
            shiny::selectInput(ns("design_comp_submethod"), "子方法",
                               choices = c("Beale"="beale","分层Beale"="stratified_beale",
                                           "回归+插值"="regression_interp"),
                               selected = "beale"),
            shiny::numericInput(ns("design_comp_nstrata"), "分层数", value = 3, min = 2, max = 10)
          )
        ),

        shiny::tags$hr(),
        shiny::actionButton(ns("btn_run_design"), "运行对比计算",
                            icon = shiny::icon("play"), class = "btn-primary btn-block"),
        shiny::tags$hr(),
        shiny::h5(shiny::icon("trophy"), " 最优方案"),
        shiny::verbatimTextOutput(ns("txt_design_best")),
        shiny::downloadButton(ns("download_design_report"), "下载报告 CSV",
                              class = "btn-success btn-block")
      ),

      # ============ 右侧面板 ============
      shiny::column(
        width = 8,

        # -- DT 交互表 --
        shinydashboard::box(
          width = 12, status = "info", solidHeader = TRUE, collapsible = TRUE,
          title = shiny::tagList(
            shiny::icon("table"), " 采样数据（点击 ",
            shiny::tags$span(style = "color:forestgreen; font-size:16px;",
                             shiny::icon("ok", lib = "glyphicon")),
            " 列切换）"
          ),
          shiny::fluidRow(
            shiny::column(4, shiny::tags$label("总行数"),
                          shiny::verbatimTextOutput(ns("txt_total_rows"), placeholder = TRUE)),
            shiny::column(4, shiny::tags$label("已排除"),
                          shiny::verbatimTextOutput(ns("txt_excluded_rows"), placeholder = TRUE)),
            shiny::column(4, shiny::tags$label("保留比例"),
                          shiny::uiOutput(ns("ui_retain_pct")))
          ),
          DT::DTOutput(ns("tbl_design_data")),
          shiny::tags$small(class = "text-muted",
                            "点击首列图标切换保留/删除；或用左侧自动方案。")
        ),

        # -- 对比结果 --
        shinydashboard::box(
          width = 12, status = "success", solidHeader = TRUE,
          title = shiny::tagList(shiny::icon("balance-scale"), " 通量对比"),
          shiny::tabsetPanel(
            type = "tabs",
            shiny::tabPanel("汇总表",
                            shiny::div(style = "margin:8px 0;",
                                       shiny::downloadButton(ns("download_compare_csv"),
                                                             "下载 CSV", class = "btn-default btn-sm")),
                            DT::DTOutput(ns("tbl_compare"))),
            shiny::tabPanel("RE 图",
                            shiny::plotOutput(ns("plot_re_bar"), height = "420px")),
            shiny::tabPanel("时序对比",
                            plotly::plotlyOutput(ns("plot_ts_compare"), height = "380px")),
            shiny::tabPanel("散点对比",
                            shiny::plotOutput(ns("plot_scatter"), height = "400px"))
          )
        )
      )
    )
  )
}

# ============================= Server =================================

mod_design_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {

    ns <- session$ns
    conv_factor_fixed <- 86.4

    # ---- 水质指标下拉更新 ----
    observe({
      targets <- utils_design_available_constituents(rv$clean_list)
      if (length(targets) == 0) {
        shiny::updateSelectInput(session, "design_constituent",
                                 choices = c("请先加载数据" = ""), selected = "")
      } else {
        cur <- isolate(input$design_constituent)
        if (is.null(cur) || !(cur %in% targets)) cur <- targets[1]
        shiny::updateSelectInput(session, "design_constituent",
                                 choices = stats::setNames(targets, targets), selected = cur)
      }
    })

    # ---- 合并数据 ----
    merged_df <- shiny::reactive({
      utils_design_merge_flow_wq(rv$clean_list, input$design_constituent)
    })

    daily_all <- shiny::reactive({
      utils_design_prepare_daily(merged_df())
    })

    # ---- 数据状态提示 ----
    output$ui_data_status <- shiny::renderUI({
      st <- utils_design_data_status(rv$clean_list, input$design_constituent, daily_all())
      shiny::tags$div(class = st$class, shiny::icon(st$icon), st$msg)
    })

    # ---- DT 数据准备 ----
    design_dat <- shiny::reactiveVal(NULL)

    observe({
      d <- daily_all()
      if (is.null(d) || nrow(d) == 0) { design_dat(NULL); return() }
      design_dat(utils_design_add_dt_cols(d))
    })

    # ---- DT 渲染 ----
    output$tbl_design_data <- DT::renderDT({
      dat <- design_dat(); shiny::req(dat)
      id_idx  <- which(names(dat) == ".row_id") - 1
      sel_idx <- which(names(dat) == "Selected") - 1

      DT::datatable(
        dat, rownames = FALSE, extensions = "Buttons", selection = "none",
        callback = DT::JS(utils_design_dt_callback_js(ns("excludedRows"))),
        options = list(
          rowId = DT::JS(sprintf("function(d){return d[%d];}", id_idx)),
          columnDefs = list(
            list(visible = FALSE, targets = id_idx),
            list(className = "dt-center", targets = "_all"),
            list(className = "notselectable", targets = sel_idx),
            list(targets = sel_idx, render = DT::JS(utils_design_dt_render_js()))
          ),
          dom = "Bfrtip", pageLength = 15, scrollX = TRUE,
          buttons = list("copy", "csv",
                         list(extend = "collection", text = "全部保留 Restore All",
                              action = DT::JS(utils_design_dt_restore_js(ns("excludedRows")))))
        )
      )
    }, server = FALSE)

    # ---- 自动方案应用 ----
    observeEvent(input$btn_apply_scheme, {
      dat <- design_dat(); shiny::req(dat)
      excluded_idx <- fct_design_compute_excluded(
        dat       = dat,
        scheme    = input$design_scheme,
        sys_interval = as.integer(input$design_sys_interval %||% "4"),
        random_pct   = input$design_random_pct %||% 50,
        random_seed  = input$design_random_seed %||% 42,
        freq_str     = input$design_freq %||% "24h"
      )
      js <- utils_design_apply_scheme_js(
        tbl_id        = ns("tbl_design_data"),
        excluded_idx  = excluded_idx,
        ns_excluded_id = ns("excludedRows")
      )
      shinyjs::runjs(js)
      shiny::showNotification(
        paste0("排除 ", length(excluded_idx),
               " 行，保留 ", nrow(dat) - length(excluded_idx), " 行。"),
        type = "message")
    })

    # ---- 排除/保留统计 ----
    excluded_rows <- shiny::reactive({
      ex <- input$excludedRows
      if (is.null(ex)) integer(0) else as.integer(ex)
    })
    total_n <- shiny::reactive({
      dat <- design_dat()
      if (is.null(dat)) 0L else nrow(dat)
    })

    output$txt_total_rows    <- shiny::renderText(as.character(total_n()))
    output$txt_excluded_rows <- shiny::renderText(as.character(length(excluded_rows())))
    output$ui_retain_pct     <- shiny::renderUI({
      n <- total_n(); ex <- length(excluded_rows())
      if (n == 0) return(shiny::tags$span("\u2014"))
      pct <- round((n - ex) / n * 100, 1)
      cls <- if (pct >= 60) "retain-good" else if (pct >= 30) "retain-warn" else "retain-danger"
      shiny::tags$span(class = paste("retain-badge", cls), paste0(pct, "%"))
    })

    # ---- 保留后数据 ----
    retained_data <- shiny::reactive({
      dat <- design_dat()
      if (is.null(dat)) return(NULL)
      ex <- excluded_rows()
      if (length(ex) == 0) return(dat)
      dat[!(dat$`.row_id` %in% paste0("row_", ex)), , drop = FALSE]
    })

    # ---- 运行对比计算 ----
    compare_results <- shiny::reactiveVal(NULL)

    observeEvent(input$btn_run_design, {
      dat_full <- design_dat()
      dat_thin <- retained_data()
      methods  <- c(input$design_methods_basic %||% character(0),
                    input$design_methods_advanced %||% character(0))

      shiny::req(dat_full, dat_thin)
      if (length(methods) == 0) {
        shiny::showNotification("请至少选择一种方法。", type = "warning"); return()
      }
      if (nrow(dat_thin) < 3) {
        shiny::showNotification("保留数据太少 (<3行)。", type = "error"); return()
      }

      df_full <- fct_design_clean_df(dat_full)
      df_thin <- fct_design_clean_df(dat_thin)

      comp_res <- NULL
      shiny::withProgress(message = "计算中...", value = 0, {
        comp_res <- fct_design_run_comparison(
          df_full        = df_full,
          df_thin        = df_thin,
          methods        = methods,
          conv_factor    = conv_factor_fixed,
          reg_submethod  = input$design_reg_submethod %||% "abs_linear",
          reg_log        = isTRUE(input$design_reg_log),
          reg_smear      = isTRUE(input$design_reg_smear),
          comp_submethod = input$design_comp_submethod %||% "beale",
          comp_nstrata   = input$design_comp_nstrata %||% 3,
          progress_fn    = function(inc, detail) shiny::incProgress(inc, detail = detail)
        )
      })

      summary_df <- fct_design_build_summary(
        results        = comp_res$results,
        methods        = methods,
        reg_submethod  = input$design_reg_submethod %||% "abs_linear",
        reg_log        = isTRUE(input$design_reg_log),
        comp_submethod = input$design_comp_submethod %||% "beale"
      )

      compare_results(list(
        summary    = summary_df,
        details    = comp_res$results,
        n_full     = nrow(df_full),
        n_thin     = nrow(df_thin),
        retain_pct = round(nrow(df_thin) / nrow(df_full) * 100, 1),
        scheme     = input$design_scheme,
        target     = input$design_constituent,
        errors     = comp_res$errors
      ))

      if (length(comp_res$errors) > 0)
        shiny::showNotification(paste(comp_res$errors, collapse = "\n"),
                                type = "warning", duration = 8)
      shiny::showNotification("对比计算完成！", type = "message")
    })

    # ---- 汇总表 ----
    output$tbl_compare <- DT::renderDT({
      cr <- compare_results()
      if (is.null(cr))
        return(DT::datatable(data.frame(note = "请先运行计算"),
                             rownames = FALSE, options = list(dom = "t")))

      tbl <- cr$summary
      dt <- data.frame(
        `计算方法`       = tbl$method_label,
        `子方法/参数`    = tbl$sub_info,
        `全量样本数`     = tbl$n_full,
        `抽稀样本数`     = tbl$n_thin,
        `全量总通量(kg)` = tbl$flux_full_kg,
        `抽稀总通量(kg)` = tbl$flux_thin_kg,
        `全量日均(kg/d)` = tbl$mean_full_kgd,
        `抽稀日均(kg/d)` = tbl$mean_thin_kgd,
        `绝对误差(kg)`   = tbl$abs_error_kg,
        `相对误差(%)`    = tbl$RE_pct,
        stringsAsFactors = FALSE, check.names = FALSE
      )
      DT::datatable(dt, rownames = FALSE,
                    options = list(pageLength = 10, scrollX = TRUE, dom = "t",
                                   columnDefs = list(
                                     list(className = "dt-center", targets = "_all")
                                   ))) |>
        DT::formatStyle("相对误差(%)",
                        backgroundColor = DT::styleInterval(
                          c(5, 10, 15),
                          c("#d4edda", "#fff3cd", "#f8d7da", "#dc3545")),
                        fontWeight = "bold")
    })

    # ---- RE 柱状图 ----
    output$plot_re_bar <- shiny::renderPlot({
      cr <- compare_results(); shiny::req(cr)
      tbl <- cr$summary[!is.na(cr$summary$RE_pct), , drop = FALSE]
      shiny::req(nrow(tbl) > 0)

      labels <- ifelse(tbl$sub_info == "", tbl$method_label,
                       paste0(tbl$method_label, "\n(", tbl$sub_info, ")"))
      par(mar = c(7, 6, 4, 2))
      cols <- ifelse(tbl$RE_pct <= 5,  "#28a745",
                     ifelse(tbl$RE_pct <= 10, "#ffc107",
                            ifelse(tbl$RE_pct <= 15, "#fd7e14", "#dc3545")))

      bp <- barplot(tbl$RE_pct, names.arg = labels, col = cols, border = NA,
                    ylim = c(0, max(tbl$RE_pct, na.rm = TRUE) * 1.3 + 1),
                    ylab = "相对误差 RE (%)",
                    main = sprintf("指标:%s | 保留%s%% (%d/%d) | 方案:%s",
                                   cr$target %||% "?", cr$retain_pct,
                                   cr$n_thin, cr$n_full,
                                   switch(cr$scheme, "manual"="手动",
                                          "systematic"="等间隔",
                                          "random"="随机", "frequency"="按频率",
                                          cr$scheme)),
                    las = 2, cex.names = 0.9)
      text(bp, tbl$RE_pct, labels = paste0(tbl$RE_pct, "%"), pos = 3, cex = 1, font = 2)
      abline(h = c(5,10,15), lty = c(3,2,2), col = c("#28a745","#ffc107","#dc3545"), lwd = 1.5)
      legend("topright", legend = c("\u22645%","5-10%","10-15%",">15%"),
             fill = c("#28a745","#ffc107","#fd7e14","#dc3545"), border = NA, bty = "n", cex = 0.9)
    })

    # ---- 时序对比图 ----
    output$plot_ts_compare <- plotly::renderPlotly({
      cr <- compare_results()
      if (is.null(cr)) return(plotly::plot_ly())
      traces <- list()
      for (m in names(cr$details)) {
        lbl <- utils_design_method_label(m)
        for (tp in c("full", "thin")) {
          res <- cr$details[[m]][[tp]]
          if (is.null(res) || !is.data.frame(res$daily) || nrow(res$daily) == 0) next
          df <- res$daily
          if (!("date" %in% names(df)))
            for (dc in c("TM","datetime","DateTime"))
              if (dc %in% names(df)) { df$date <- df[[dc]]; break }
          if (!("date" %in% names(df))) next
          tp_lbl <- if (tp == "full") "全量" else "抽稀"
          traces[[paste0(m,"_",tp)]] <- data.frame(
            date = as.Date(df$date), flux = as.numeric(df$flux),
            label = paste0(lbl," (",tp_lbl,")"),
            dash = if (tp=="full") "solid" else "dash",
            stringsAsFactors = FALSE)
        }
      }
      if (length(traces) == 0) return(plotly::plot_ly())
      dd <- do.call(rbind, traces)
      dd <- dd[!is.na(dd$date) & is.finite(dd$flux), , drop = FALSE]
      p <- plotly::plot_ly()
      for (lb in unique(dd$label)) {
        sub <- dd[dd$label == lb, , drop = FALSE]
        p <- plotly::add_trace(p, data = sub, x = ~date, y = ~flux, name = lb,
                               type = "scatter", mode = "lines+markers",
                               line = list(dash = sub$dash[1]), marker = list(size = 2),
                               hovertemplate = "日期:%{x}<br>通量:%{y:.4f} kg/d<extra></extra>")
      }
      plotly::layout(p, title = paste0("全量 vs 抽稀 (", cr$target %||% "", ")"),
                     xaxis = list(title = "日期"), yaxis = list(title = "通量 (kg/d)"),
                     legend = list(orientation = "h", y = -0.25))
    })

    # ---- 散点对比图 ----
    output$plot_scatter <- shiny::renderPlot({
      cr <- compare_results(); shiny::req(cr)
      flux_list <- list()
      for (m in names(cr$details)) {
        rf <- cr$details[[m]]$full
        if (!is.null(rf) && is.data.frame(rf$daily) && nrow(rf$daily) > 0) {
          df <- rf$daily
          if (!("date" %in% names(df)))
            for (dc in c("TM","datetime","DateTime"))
              if (dc %in% names(df)) { df$date <- df[[dc]]; break }
          if ("date" %in% names(df))
            flux_list[[m]] <- data.frame(date = as.Date(df$date),
                                         flux = as.numeric(df$flux),
                                         stringsAsFactors = FALSE)
        }
      }
      mk <- names(flux_list)
      if (length(mk) < 2) {
        graphics::plot.new()
        graphics::text(0.5, 0.5, "需至少2种方法", cex = 1.2); return()
      }
      nm <- length(mk)
      par(mfrow = c(nm-1, nm-1), mar = c(3,3,2,1), oma = c(0,0,3,0))
      for (i in 1:(nm-1)) for (j in 2:nm) {
        if (j <= i) { graphics::plot.new(); next }
        mg <- merge(flux_list[[mk[i]]], flux_list[[mk[j]]], by = "date",
                    suffixes = c(".x",".y"))
        mg <- mg[is.finite(mg$flux.x) & is.finite(mg$flux.y), , drop = FALSE]
        if (nrow(mg) < 2) { graphics::plot.new(); next }
        graphics::plot(mg$flux.x, mg$flux.y,
                       xlab = utils_design_method_label(mk[i]),
                       ylab = utils_design_method_label(mk[j]),
                       pch = 16, col = grDevices::rgb(0.2,0.4,0.8,0.5), cex = 0.8)
        graphics::abline(0, 1, lty = 2, col = "red")
        r <- tryCatch(stats::cor(mg$flux.x, mg$flux.y), error = function(e) NA)
        if (!is.na(r)) graphics::legend("topleft", sprintf("r=%.3f", r),
                                        bty = "n", text.col = "red", cex = 0.9)
      }
      graphics::mtext("方法间全量通量散点对比 (红线=1:1)", outer = TRUE, cex = 1.1)
    })

    # ---- 最优方案文本 ----
    output$txt_design_best <- shiny::renderPrint({
      cat(fct_design_best_text(compare_results()))
    })

    # ---- 下载 ----
    output$download_compare_csv <- shiny::downloadHandler(
      filename = function() paste0("design_compare_", Sys.Date(), ".csv"),
      content = function(file) {
        cr <- compare_results()
        if (is.null(cr)) utils::write.csv(data.frame(note = "No data"), file, row.names = FALSE)
        else utils::write.csv(cr$summary, file, row.names = FALSE)
      }
    )

    output$download_design_report <- shiny::downloadHandler(
      filename = function() paste0("design_report_", Sys.Date(), ".csv"),
      content = function(file) {
        cr <- compare_results()
        if (is.null(cr)) {
          utils::write.csv(data.frame(note = "No data"), file, row.names = FALSE)
        } else {
          meta <- data.frame(
            item = c("target","scheme","n_full","n_thin","retain_pct",
                     if (length(cr$errors)>0) "warnings" else NULL),
            value = c(cr$target %||% "?", cr$scheme, as.character(cr$n_full),
                      as.character(cr$n_thin), paste0(cr$retain_pct,"%"),
                      if (length(cr$errors)>0) paste(cr$errors, collapse = "; ") else NULL),
            stringsAsFactors = FALSE)
          con <- file(file, "w"); on.exit(close(con))
          writeLines("# === Design Meta ===", con)
          utils::write.csv(meta, con, row.names = FALSE)
          writeLines("\n# === Comparison Summary ===", con)
          utils::write.csv(cr$summary, con, row.names = FALSE)
        }
      }
    )
  })
}

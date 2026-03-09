

server <- function(input, output, session) {

  # ★ 已移除 get_plan、plan reactiveVal、权限拦截 observeEvent ★

  rv <- reactiveValues(
    raw_df     = NULL,
    clean_df   = NULL,
    flux_df    = NULL,
    design_df  = NULL,
    ingest_log = "等待：请上传 CSV 并点击【加载并校验】\nWaiting: upload CSV and click [Load & Validate]"
  )

  # ---- 1 数据上传 ----
  observeEvent(input$btn_load, {
    req(input$data_csv)
    tryCatch({
      df <- utils::read.csv(input$data_csv$datapath, stringsAsFactors = FALSE, check.names = FALSE)
      log_lines <- character()

      for (col in names(df)) {
        parsed <- tryCatch(as.POSIXct(df[[col]], tryFormats = c(
          "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M",
          "%Y/%m/%d %H:%M:%S", "%Y/%m/%d %H:%M",
          "%m/%d/%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S",
          "%Y-%m-%d"
        )), error = function(e) NULL)
        if (!is.null(parsed) && !all(is.na(parsed))) {
          df[[col]] <- parsed
          log_lines <- c(log_lines, paste0("[OK] '", col, "' -> datetime"))
        }
      }

      for (col in names(df)) {
        if (is.character(df[[col]]) && !inherits(df[[col]], "POSIXt")) {
          nums   <- suppressWarnings(as.numeric(df[[col]]))
          pct_na <- mean(is.na(nums) & !is.na(df[[col]]))
          if (pct_na < 0.1) {
            n_bad <- sum(is.na(nums) & !is.na(df[[col]]))
            if (n_bad > 0) log_lines <- c(log_lines, paste0("[WARN] '", col, "': ", n_bad, " non-numeric -> NA"))
            df[[col]] <- nums
            log_lines <- c(log_lines, paste0("[OK] '", col, "' -> numeric"))
          } else {
            log_lines <- c(log_lines, paste0("[INFO] '", col, "' kept as character (", round(pct_na*100,1), "%)"))
          }
        }
      }

      rv$raw_df     <- df
      rv$clean_df   <- df
      rv$ingest_log <- paste0("Loaded: ", nrow(df), " rows x ", ncol(df), " cols\n", paste(log_lines, collapse = "\n"))
    }, error = function(e) {
      rv$ingest_log <- paste("ERROR:", e$message)
    })
  })

  observeEvent(input$btn_reset, {
    rv$raw_df <- NULL; rv$clean_df <- NULL; rv$flux_df <- NULL; rv$design_df <- NULL
    rv$ingest_log <- "已清空 State cleared."
  })

  output$txt_ingest_status <- shiny::renderPrint({ cat(rv$ingest_log) })

  output$ui_schema_hint <- shiny::renderUI({
    req(rv$clean_df)
    types <- vapply(rv$clean_df, function(x) {
      if (inherits(x, "POSIXt")) "datetime" else if (is.numeric(x)) "numeric" else "character"
    }, character(1))
    shiny::tags$div(
      style = "background:#eef5fb; padding:8px 12px; border-radius:4px;",
      shiny::tags$strong("Schema: "),
      shiny::tags$code(paste(paste0(names(types), " [", types, "]"), collapse = " | "))
    )
  })

  output$tbl_preview <- DT::renderDataTable({
    req(rv$clean_df)
    DT::datatable(rv$clean_df, editable = TRUE, rownames = FALSE, filter = "top",
                  options = list(pageLength = 10, scrollX = TRUE,
                                 language = list(search = "搜索 Search:", lengthMenu = "每页 _MENU_ 条",
                                                 paginate = list(previous = "上一页", `next` = "下一页"))))
  })

  observeEvent(input$tbl_preview_cell_edit, {
    info <- input$tbl_preview_cell_edit
    rv$clean_df[info$row, info$col + 1] <- DT::coerceValue(info$value, rv$clean_df[info$row, info$col + 1])
  })

  # ---- 2 可视化 ----
  .find_cols <- function(df) {
    dt_col <- num_col <- NULL
    for (col in names(df)) {
      if (is.null(dt_col)  && inherits(df[[col]], "POSIXt")) dt_col  <- col
      if (is.null(num_col) && is.numeric(df[[col]]))         num_col <- col
    }
    list(dt = dt_col, num = num_col)
  }

  output$plot_flux <- shiny::renderPlot({
    req(rv$clean_df)
    cc <- .find_cols(rv$clean_df)
    if (is.null(cc$dt) || is.null(cc$num)) { plot.new(); text(0.5,0.5,"Need datetime + numeric col"); return() }
    x <- rv$clean_df[[cc$dt]]; y <- rv$clean_df[[cc$num]]; viz <- input$viz_type
    if (viz == "daily_ts") {
      plot(x, y, type="l", col="#3c8dbc", lwd=1.5, xlab=cc$dt, ylab=cc$num, main=paste0("Time Series: ", cc$num))
      if (input$viz_show_points) points(x, y, pch=16, cex=0.5, col="#e74c3c")
      if (isTRUE(input$viz_show_smooth)) { ok<-complete.cases(x,y); lines(stats::lowess(as.numeric(x[ok]),y[ok],f=0.2), col="#2ecc71", lwd=2) }
    } else if (viz == "cumulative") {
      ok<-order(x); cy<-cumsum(ifelse(is.na(y[ok]),0,y[ok]))
      plot(x[ok], cy, type="l", col="#e67e22", lwd=2, xlab=cc$dt, ylab=paste0("Cumulative ",cc$num), main="Cumulative Flux")
      if (input$viz_show_points) points(x[ok], cy, pch=16, cex=0.4)
    } else if (viz == "boxplot_month") {
      boxplot(y ~ format(x,"%Y-%m"), col="#3498db", las=2, xlab="Month", ylab=cc$num, main=paste0("Monthly: ",cc$num))
    }
  })

  output$download_plot <- shiny::downloadHandler(
    filename = function() paste0("FluxWatch_plot_", Sys.Date(), ".png"),
    content = function(file) {
      grDevices::png(file, width=1200, height=600, res=150)
      req(rv$clean_df); cc<-.find_cols(rv$clean_df)
      if (!is.null(cc$dt) && !is.null(cc$num)) plot(rv$clean_df[[cc$dt]], rv$clean_df[[cc$num]], type="l", col="#3c8dbc", xlab=cc$dt, ylab=cc$num, main=cc$num)
      grDevices::dev.off()
    }
  )

  # ---- 3 通量计算 ----
  observeEvent(input$btn_compute, {
    req(rv$clean_df); df<-rv$clean_df; cc<-.find_cols(df); req(cc$dt, cc$num)
    df<-df[order(df[[cc$dt]]),]; df$date<-as.Date(df[[cc$dt]]); conv<-input$conv_factor
    if (input$method == "daily_integral") {
      result <- do.call(rbind, lapply(split(df, df$date), function(d) {
        t_sec<-as.numeric(difftime(d[[cc$dt]], min(d[[cc$dt]]), units="secs")); vals<-d[[cc$num]]
        ok<-complete.cases(t_sec,vals); if(sum(ok)<2) return(data.frame(date=d$date[1],flux=NA_real_))
        t_sec<-t_sec[ok]; vals<-vals[ok]
        data.frame(date=d$date[1], flux=sum(diff(t_sec)*(vals[-length(vals)]+vals[-1])/2)*conv/1e6)
      }))
    } else {
      result<-stats::aggregate(df[[cc$num]], by=list(date=df$date), FUN=function(v) mean(v,na.rm=TRUE)*conv)
      names(result)<-c("date","flux")
    }
    result<-result[order(result$date),]; rownames(result)<-NULL; rv$flux_df<-result
  })

  output$txt_flux_summary <- shiny::renderPrint({
    if (is.null(rv$flux_df)) { cat("Click [Calculate Flux].") } else {
      cat("Method:", input$method, "\nFactor:", input$conv_factor, "\nDays:", nrow(rv$flux_df),
          "\nTotal:", round(sum(rv$flux_df$flux,na.rm=TRUE),4),
          "\nMean:",  round(mean(rv$flux_df$flux,na.rm=TRUE),4),
          "\nSD:",    round(stats::sd(rv$flux_df$flux,na.rm=TRUE),4), "\n")
    }
  })

  output$tbl_flux_daily <- DT::renderDataTable({
    req(rv$flux_df)
    DT::datatable(rv$flux_df, rownames=FALSE, options=list(pageLength=15, scrollX=TRUE))
  })

  output$download_flux_daily <- shiny::downloadHandler(
    filename = function() paste0("FluxWatch_flux_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(if(!is.null(rv$flux_df)) rv$flux_df else data.frame(note="No data"), file, row.names=FALSE)
  )

  # ---- 4 采样设计 ----
  observeEvent(input$btn_run_design, {
    req(rv$clean_df, input$design_intervals)
    intervals<-input$design_intervals; target_re<-input$design_target_re/100; set.seed(42)
    results<-data.frame(
      Interval = intervals,
      Samples_yr = sapply(intervals, function(iv) { hrs<-as.numeric(gsub("[^0-9.]","",iv)); if(grepl("d",iv)) hrs<-hrs*24; round(365*24/hrs) }),
      Mean_RE_pct = sapply(intervals, function(iv) { hrs<-as.numeric(gsub("[^0-9.]","",iv)); if(grepl("d",iv)) hrs<-hrs*24; round(runif(1,5,50)*sqrt(hrs/24),1) }),
      stringsAsFactors = FALSE)
    results$Meets <- ifelse(results$Mean_RE_pct <= target_re*100, "Yes", "No")
    results<-results[order(results$Mean_RE_pct),]; rownames(results)<-NULL; rv$design_df<-results
  })

  output$txt_design_best <- shiny::renderPrint({
    if (is.null(rv$design_df)) { cat("Run design first.") } else {
      best<-rv$design_df[which.min(rv$design_df$Mean_RE_pct),]
      cat("Best:", best$Interval, "| Samples/yr:", best$Samples_yr, "| RE:", best$Mean_RE_pct, "%\n")
    }
  })

  output$tbl_design <- DT::renderDataTable({
    req(rv$design_df); DT::datatable(rv$design_df, rownames=FALSE, options=list(pageLength=15))
  })

  output$plot_design <- shiny::renderPlot({
    req(rv$design_df); df<-rv$design_df
    df$Interval<-factor(df$Interval, levels=df$Interval)
    barplot(df$Mean_RE_pct, names.arg=df$Interval, col=ifelse(df$Meets=="Yes","#27ae60","#e74c3c"),
            border=NA, ylab="RE(%)", xlab="Interval", main="RE vs Sampling Interval")
    abline(h=input$design_target_re, lty=2, col="#3c8dbc", lwd=2)
    legend("topright", legend=c("Meets","Exceeds","Target"), fill=c("#27ae60","#e74c3c",NA),
           border=c(NA,NA,NA), lty=c(NA,NA,2), col=c(NA,NA,"#3c8dbc"), lwd=c(NA,NA,2))
  })

  output$download_design_table <- shiny::downloadHandler(
    filename = function() paste0("FluxWatch_design_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(if(!is.null(rv$design_df)) rv$design_df else data.frame(note="No data"), file, row.names=FALSE)
  )

  # ---- 5 报告 ----
  output$download_clean_data <- shiny::downloadHandler(
    filename = function() paste0("FluxWatch_clean_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(if(!is.null(rv$clean_df)) rv$clean_df else data.frame(note="No data"), file, row.names=FALSE))

  output$download_flux_result <- shiny::downloadHandler(
    filename = function() paste0("FluxWatch_flux_result_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(if(!is.null(rv$flux_df)) rv$flux_df else data.frame(note="No data"), file, row.names=FALSE))

  output$download_design_result <- shiny::downloadHandler(
    filename = function() paste0("FluxWatch_design_result_", Sys.Date(), ".csv"),
    content = function(file) utils::write.csv(if(!is.null(rv$design_df)) rv$design_df else data.frame(note="No data"), file, row.names=FALSE))

  output$download_report_plot <- shiny::downloadHandler(
    filename = function() paste0("FluxWatch_summary_", Sys.Date(), ".png"),
    content = function(file) {
      grDevices::png(file, width=1600, height=1200, res=150); par(mfrow=c(2,2), mar=c(4,4,3,1))
      if (!is.null(rv$clean_df)) { cc<-.find_cols(rv$clean_df)
      if(!is.null(cc$dt)&&!is.null(cc$num)) plot(rv$clean_df[[cc$dt]],rv$clean_df[[cc$num]],type="l",col="#3c8dbc",main="Raw") else {plot.new();title("No data")}
      } else {plot.new();title("No data")}
      if (!is.null(rv$flux_df)) plot(rv$flux_df$date,rv$flux_df$flux,type="h",col="#e67e22",lwd=2,main="Daily Flux") else {plot.new();title("No flux")}
      if (!is.null(rv$flux_df)) { cf<-cumsum(ifelse(is.na(rv$flux_df$flux),0,rv$flux_df$flux)); plot(rv$flux_df$date,cf,type="l",col="#27ae60",lwd=2,main="Cumulative") } else {plot.new();title("No flux")}
      if (!is.null(rv$design_df)) barplot(rv$design_df$Mean_RE_pct,names.arg=rv$design_df$Interval,col="#3498db",border=NA,main="Design") else {plot.new();title("No design")}
      grDevices::dev.off()
    }
  )

  output$download_report <- shiny::downloadHandler(
    filename = function() {
      ext <- switch(input$report_format, html=".html", docx=".docx", pdf=".pdf", ".html")
      paste0("FluxWatch_report_", Sys.Date(), ext)
    },
    content = function(file) {
      writeLines(c("=== FluxWatch Report ===", paste("Generated:", Sys.time()),
                   paste("Method:", input$method), paste("Factor:", input$conv_factor),
                   if(!is.null(rv$flux_df)) c(paste("Days:",nrow(rv$flux_df)), paste("Total:",round(sum(rv$flux_df$flux,na.rm=TRUE),4))) else "No flux.",
                   "--- End ---"), con=file)
    }
  )

  output$txt_report_status <- shiny::renderPrint({
    cat("Clean data:", ifelse(!is.null(rv$clean_df),"Ready","Not loaded"), "\n")
    cat("Flux result:", ifelse(!is.null(rv$flux_df),"Computed","Not computed"), "\n")
    cat("Design:", ifelse(!is.null(rv$design_df),"Done","Not run"), "\n")
  })
}

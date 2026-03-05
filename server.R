# 提供服务后台接口



server <- function(input, output, session) {

  # ---- ingest ----
  output$txt_ingest_status <- renderPrint("等待上传 CSV 并点击【加载并校验】")
  output$ui_schema_hint    <- renderUI(tags$small("提示：这里显示字段/单位/必填项等 schema 信息（待实现）"))
  output$tbl_preview       <- renderTable(head(mtcars, 10))

  # ---- flux ----
  output$txt_flux_summary  <- renderPrint("通量计算结果摘要（待实现）")
  output$tbl_flux_daily    <- renderTable(data.frame(date = Sys.Date() + 0:6, flux = round(runif(7, 1, 3), 3)))

  # ---- viz ----
  output$plot_flux <- renderPlot({
    plot(1:10, (1:10)^0.5, main = "示例图：plot_flux（待替换）")
  })

  # ---- unc ----
  output$txt_mc_summary <- renderPrint("Monte Carlo 摘要（待实现）")
  output$plot_mc_dist   <- renderPlot({
    hist(rnorm(500), main = "示例图：MC 分布（待替换）")
  })
  output$tbl_mc_ci      <- renderTable(data.frame(p = c("2.5%", "50%", "97.5%"), value = round(quantile(rnorm(1000), c(.025,.5,.975)), 4)))

  # ---- design ----
  output$txt_design_best <- renderPrint("最佳方案（待实现）")
  output$tbl_design      <- renderTable(data.frame(freq_per_year = c(12, 24, 48), est_re = c(0.18, 0.12, 0.08)))

  # ---- export ----
  output$txt_export_note <- renderPrint("导出按钮将把当前结果写成 CSV（待实现）")

  output$download_flux_daily <- downloadHandler(
    filename = function() "flux_daily.csv",
    content  = function(file) write.csv(data.frame(date = Sys.Date() + 0:6, flux = runif(7)), file, row.names = FALSE)
  )
  output$download_mc_summary <- downloadHandler(
    filename = function() "mc_summary.csv",
    content  = function(file) write.csv(data.frame(metric = c("mean","sd"), value = c(0,1)), file, row.names = FALSE)
  )
  output$download_design_table <- downloadHandler(
    filename = function() "design_table.csv",
    content  = function(file) write.csv(data.frame(freq_per_year = c(12,24,48), est_re = c(.18,.12,.08)), file, row.names = FALSE)
  )

  # ---- status ----
  output$txt_status <- renderPrint({
    list(
      app = "FluxWatch",
      time = Sys.time(),
      nav = input$nav
    )
  })
}

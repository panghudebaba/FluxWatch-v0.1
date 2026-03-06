server <- function(input, output, session) {

  # ----------------- 套餐：basic / pro -----------------
  get_plan <- function() {
    p <- Sys.getenv("FLUXWATCH_PLAN", unset = "basic")
    if (!p %in% c("basic", "pro")) p <- "basic"
    p
  }
  plan <- reactiveVal(get_plan())

  # ----------------- 动态消息 -----------------
  sys_messages <- reactiveValues(
    new_message = NULL,
    new_notification = NULL,
    user_status = "在线"
  )

  observe({
    invalidateLater(5000, session)
    sys_messages$new_message <- paste("新的系统消息 - 时间:", Sys.time())
    sys_messages$new_notification <- paste("新通知 - 系统更新：", Sys.time())
  })

  # ----------------- messages -----------------
  output$messages <- shinydashboard::renderMenu({
    shinydashboard::dropdownMenu(
      type = "messages",
      icon = shiny::icon("envelope"),
      badgeStatus = "info",
      shinydashboard::messageItem(
        from = "系统管理员",
        message = ifelse(is.null(sys_messages$new_message), "没有新消息", sys_messages$new_message),
        icon = shiny::icon("user-shield")
      )
    )
  })

  # ----------------- notifications -----------------
  output$notifications <- shinydashboard::renderMenu({
    shinydashboard::dropdownMenu(
      type = "notifications",
      icon = shiny::icon("bell"),
      badgeStatus = "danger",
      shinydashboard::notificationItem(
        text = ifelse(is.null(sys_messages$new_notification), "没有新通知", sys_messages$new_notification),
        icon = shiny::icon("exclamation-triangle"),
        status = "warning"
      )
    )
  })

  # ----------------- tasks / 用户信息 -----------------
  output$tasks <- shinydashboard::renderMenu({
    shinydashboard::dropdownMenu(
      type = "tasks",
      icon = shiny::icon("user"),
      badgeStatus = "success",

      shinydashboard::taskItem(
        value = 100,
        color = "green",
        paste0("用户 John Doe (", sys_messages$user_status, ")")
      ),

      shinydashboard::taskItem(
        value = 0,
        color = "red",
        shiny::actionLink("logout_btn", "Logout")
      )
    )
  })

  # ----------------- logout 行为 -----------------
  observeEvent(input$logout_btn, {
    session$sendCustomMessage(
      type = "logout",
      message = list(url = "https://example.com/logout")
    )
  })

  # ----------------- JS handler -----------------
  session$onFlushed(function() {
    session$sendCustomMessage(
      type = "initLogoutJS",
      message = list()
    )
  })

  # ----------------- Sidebar -----------------
  output$ui_sidebar <- renderUI({
    p <- plan()

    shinydashboard::sidebarMenu(
      id = "nav",

      shinydashboard::menuItem("1 数据导入", tabName = "ingest", icon = shiny::icon("upload")),
      shinydashboard::menuItem("2 通量计算", tabName = "flux", icon = shiny::icon("calculator")),
      shinydashboard::menuItem("3 可视化", tabName = "viz", icon = shiny::icon("chart-line")),

      if (p == "pro")
        shinydashboard::menuItem("4 不确定性", tabName = "unc", icon = shiny::icon("random")),

      if (p == "pro")
        shinydashboard::menuItem("5 方案设计", tabName = "design", icon = shiny::icon("sliders-h")),

      shinydashboard::menuItem("6 结果导出", tabName = "export", icon = shiny::icon("download")),

      shinydashboard::menuItem(" ", tabName = "sep", icon = shiny::icon("minus")),

      shinydashboard::menuItem("项目状态", tabName = "status", icon = shiny::icon("info-circle"))
    )
  })

  # ----------------- basic 禁止访问 pro -----------------
  observeEvent(input$nav, {

    if (plan() == "basic" && input$nav %in% c("unc", "design")) {

      showModal(
        shiny::modalDialog(
          title = "功能限制",
          "该功能为 Pro 版开放。",
          easyClose = TRUE
        )
      )

      shinydashboard::updateTabItems(session, "nav", "status")

    }

  }, ignoreInit = TRUE)

  # ----------------- 示例输出 -----------------
  output$txt_ingest_status <- renderPrint("等待上传 CSV 并点击【加载并校验】")

  output$ui_schema_hint <- renderUI(
    shiny::tags$small("提示：这里显示字段/单位/必填项等 schema 信息（待实现）")
  )

  output$tbl_preview <- renderTable(head(mtcars, 10))

  output$txt_flux_summary <- renderPrint("通量计算结果摘要（待实现）")

  output$tbl_flux_daily <- renderTable(
    data.frame(date = Sys.Date() + 0:6, flux = round(runif(7, 1, 3), 3))
  )

  output$plot_flux <- renderPlot({
    plot(1:10, (1:10)^0.5, main = "示例图：plot_flux")
  })

  output$txt_mc_summary <- renderPrint("Monte Carlo 摘要（待实现）")

  output$plot_mc_dist <- renderPlot({
    hist(rnorm(500), main = "MC 分布")
  })

  output$tbl_mc_ci <- renderTable(
    data.frame(
      p = c("2.5%", "50%", "97.5%"),
      value = round(quantile(rnorm(1000), c(.025,.5,.975)), 4)
    )
  )

  output$txt_design_best <- renderPrint("最佳方案（待实现）")

  output$tbl_design <- renderTable(
    data.frame(freq_per_year = c(12,24,48), est_re = c(.18,.12,.08))
  )

  output$txt_export_note <- renderPrint("导出按钮将把当前结果写成 CSV（待实现）")

  output$download_flux_daily <- downloadHandler(
    filename = function() "flux_daily.csv",
    content = function(file)
      write.csv(data.frame(date = Sys.Date()+0:6, flux = runif(7)), file, row.names = FALSE)
  )

  output$download_mc_summary <- downloadHandler(
    filename = function() "mc_summary.csv",
    content = function(file)
      write.csv(data.frame(metric=c("mean","sd"),value=c(0,1)), file,row.names=FALSE)
  )

  output$download_design_table <- downloadHandler(
    filename = function() "design_table.csv",
    content = function(file)
      write.csv(data.frame(freq_per_year=c(12,24,48),est_re=c(.18,.12,.08)), file,row.names=FALSE)
  )

  output$txt_status <- renderPrint({
    list(
      app="FluxWatch",
      plan=plan(),
      time=Sys.time(),
      nav=input$nav
    )
  })
}

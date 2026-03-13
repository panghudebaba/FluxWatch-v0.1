app_server <- function(input, output, session) {

  rv <- reactiveValues(
    raw_df     = NULL,
    clean_df   = NULL,
    flux_df    = NULL,
    design_df  = NULL,
    ingest_log = "等待：请上传 CSV 并点击【加载并校验】\nWaiting: upload CSV and click [Load & Validate]"
  )

  mod_welcome_server("welcome", rv = rv)
  mod_ingest_server("ingest", rv = rv)
  mod_viz_server("viz", rv = rv)
  mod_flux_server("flux", rv = rv)
  mod_design_server("design", rv = rv)
  mod_report_server("report", rv = rv)
  mod_about_server("about", rv = rv)
  mod_disclaimer_server("disclaimer", rv = rv)
}

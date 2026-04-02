mod_welcome_ui <- function(id) {
  ns <- shiny::NS(id)

  card_header <- function(icon_name, title_text) {
    shiny::tags$div(
      class = "welcome-card-header",
      shiny::tags$div(
        class = "welcome-card-icon",
        shiny::icon(icon_name)
      ),
      shiny::tags$div(
        class = "welcome-card-title",
        title_text
      )
    )
  }

  feature_item <- function(icon_name, title_cn, title_en, desc_text) {
    shiny::tags$div(
      class = "feature-item",
      shiny::tags$div(
        class = "feature-item-icon",
        shiny::icon(icon_name)
      ),
      shiny::tags$div(
        class = "feature-item-content",
        shiny::tags$div(class = "feature-item-title", title_cn),
        shiny::tags$div(class = "feature-item-subtitle", title_en),
        shiny::tags$p(class = "feature-item-desc", desc_text)
      )
    )
  }

  shinydashboard::tabItem(
    tabName = "welcome",

    shiny::tags$style(shiny::HTML("
      /* ─────────────────────────────────────────
         页面整体
      ───────────────────────────────────────── */
      .content-wrapper,
      .tab-content,
      .tab-pane,
      body {
        background: #f5f7fb !important;
      }

      .welcome-page {
        width: 100%;
        max-width: none;
        margin: 0;
        padding: 10px 8px 28px 8px;
      }

      .welcome-stack {
        display: flex;
        flex-direction: column;
        gap: 20px;
      }

      /* ─────────────────────────────────────────
         顶部品牌区
      ───────────────────────────────────────── */
      .brand-hero {
        text-align: center;
        padding: 8px 0 18px 0;
      }

      .brand-hero img {
        display: block;
        width: 100%;
        max-width: 300px;
        margin: 0 auto 14px auto;
        object-fit: contain;
      }

      .brand-title {
        margin: 0 0 6px 0;
        font-size: 28px;
        font-weight: 700;
        color: #1f2d3d;
        letter-spacing: 0.2px;
      }

      .brand-subtitle {
        margin: 0;
        font-size: 15px;
        line-height: 1.85;
        color: #5b6777;
      }

      /* ─────────────────────────────────────────
         通用卡片样式
      ───────────────────────────────────────── */
      .welcome-card {
        width: 100%;
        background: #ffffff;
        border: 1px solid #e7ecf2;
        border-radius: 12px;
        box-shadow: 0 4px 14px rgba(31, 45, 61, 0.04);
        padding: 20px 22px;
        transition: transform 0.18s ease, box-shadow 0.18s ease, border-color 0.18s ease;
      }

      .welcome-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 12px 28px rgba(31, 45, 61, 0.08);
        border-color: #dce4ee;
      }

      .welcome-card-header {
        display: flex;
        align-items: center;
        gap: 12px;
        margin-bottom: 16px;
      }

      .welcome-card-icon {
        width: 40px;
        height: 40px;
        border-radius: 10px;
        background: #eef5ff;
        color: #2f7dbd;
        display: flex;
        align-items: center;
        justify-content: center;
        font-size: 18px;
        flex: 0 0 40px;
      }

      .welcome-card-title {
        font-size: 18px;
        font-weight: 700;
        line-height: 1.45;
        color: #1f2d3d;
      }

      .welcome-card p {
        margin: 0 0 12px 0;
        font-size: 15px;
        line-height: 1.95;
        color: #334155;
        text-align: justify;
      }

      .welcome-card p:last-child {
        margin-bottom: 0;
      }

      .welcome-card a {
        color: #337ab7;
        text-decoration: none;
      }

      .welcome-card a:hover {
        text-decoration: underline;
      }

      /* ─────────────────────────────────────────
         Tab 简介
      ───────────────────────────────────────── */
      .intro-tabs-wrap .nav-tabs {
        border-bottom: 1px solid #e7ecf2;
        margin-bottom: 16px;
        display: flex;
        gap: 8px;
      }

      .intro-tabs-wrap .nav-tabs > li {
        margin-bottom: -1px;
      }

      .intro-tabs-wrap .nav-tabs > li > a {
        margin-right: 0;
        border-radius: 10px 10px 0 0;
        border: 1px solid transparent;
        background: #f8fafc;
        color: #64748b;
        font-size: 14px;
        font-weight: 600;
        padding: 10px 16px;
      }

      .intro-tabs-wrap .nav-tabs > li > a:hover {
        background: #f1f5f9;
        color: #334155;
        border-color: transparent;
      }

      .intro-tabs-wrap .nav-tabs > li.active > a,
      .intro-tabs-wrap .nav-tabs > li.active > a:focus,
      .intro-tabs-wrap .nav-tabs > li.active > a:hover {
        background: #eef5ff;
        color: #2f7dbd;
        border: 1px solid #d9e7fb;
        border-bottom-color: #eef5ff;
      }

      .intro-tabs-wrap .tab-content > .tab-pane {
        padding-top: 4px;
      }

      .lang-panel {
        border: 1px solid #e9eef5;
        border-radius: 12px;
        background: #fbfcfe;
        padding: 18px 18px;
      }

      .lang-panel.zh {
        border-left: 4px solid #3c8dbc;
      }

      .lang-panel.en {
        border-left: 4px solid #00a65a;
      }

      .lang-tag {
        display: inline-block;
        margin-bottom: 10px;
        padding: 4px 10px;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.3px;
        border-radius: 999px;
      }

      .lang-panel.zh .lang-tag {
        background: #eaf3fb;
        color: #1f4e79;
      }

      .lang-panel.en .lang-tag {
        background: #eaf8f1;
        color: #1e7e4a;
      }

      .lang-panel p {
        margin: 0;
      }

      .lang-panel.en p {
        text-align: left;
      }

      /* ─────────────────────────────────────────
         内容拆分布局
      ───────────────────────────────────────── */
      .split-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 18px;
        align-items: start;
      }

      .split-grid.concept-grid {
        grid-template-columns: 1.15fr 0.85fr;
      }

      .split-grid.feature-layout {
        grid-template-columns: 1.3fr 0.7fr;
      }

      .sub-panel {
        background: #fbfcfe;
        border: 1px solid #edf2f7;
        border-radius: 12px;
        padding: 16px 18px;
        height: 100%;
      }

      .formula-box {
        background: #f7fbff;
        border: 1px solid #d9e9f7;
        border-radius: 10px;
        padding: 12px 16px;
        text-align: center;
        margin: 14px 0 16px 0;
        font-size: 16px;
      }

      .formula-box.subtle {
        background: #fafcff;
      }

      .variable-box {
        background: #fafafa;
        border-left: 4px solid #3c8dbc;
        border-radius: 8px;
        padding: 12px 14px;
        margin-bottom: 14px;
        color: #444444;
        line-height: 1.85;
        font-size: 14.5px;
      }

      .figure-wrap {
        text-align: center;
      }

      .figure-wrap img {
        width: 100%;
        max-width: 620px;
        height: auto;
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        box-shadow: 0 2px 8px rgba(0, 0, 0, 0.05);
      }

      .figure-caption {
        margin-top: 8px;
        font-size: 13px;
        color: #667085;
        text-align: center;
        line-height: 1.7;
      }

      /* ─────────────────────────────────────────
         功能模块
      ───────────────────────────────────────── */
      .feature-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 14px;
        margin-top: 14px;
      }

      .feature-item {
        display: flex;
        gap: 12px;
        align-items: flex-start;
        border: 1px solid #e9eef5;
        border-radius: 12px;
        background: #fbfcfe;
        padding: 14px;
        min-height: 112px;
      }

      .feature-item-icon {
        width: 38px;
        height: 38px;
        border-radius: 10px;
        background: #eef5ff;
        color: #2f7dbd;
        display: flex;
        align-items: center;
        justify-content: center;
        flex: 0 0 38px;
        font-size: 16px;
      }

      .feature-item-title {
        font-size: 15px;
        font-weight: 700;
        line-height: 1.5;
        color: #1f2937;
        margin-bottom: 3px;
      }

      .feature-item-subtitle {
        font-size: 13px;
        color: #64748b;
        line-height: 1.55;
        margin-bottom: 6px;
      }

      .feature-item-desc {
        margin: 0 !important;
        font-size: 14px !important;
        line-height: 1.8 !important;
        color: #475569 !important;
        text-align: left !important;
      }

      /* ─────────────────────────────────────────
         开发说明
      ───────────────────────────────────────── */
      .dev-callout {
        background: #fff8e6;
        border: 1px solid #ffe2a8;
        border-left: 4px solid #f39c12;
        border-radius: 10px;
        padding: 14px 16px;
        margin-bottom: 14px;
        color: #6b4e16;
        line-height: 1.9;
      }

      .dev-callout .fa {
        margin-right: 6px;
      }

      .feedback-list {
        margin: 8px 0 0 20px;
        padding: 0;
      }

      .feedback-list li {
        margin-bottom: 8px;
        font-size: 14.5px;
        line-height: 1.85;
        color: #475569;
      }

      .feedback-list li:last-child {
        margin-bottom: 0;
      }

      .mini-note {
        margin-top: 12px;
        font-size: 14px;
        line-height: 1.8;
        color: #667085;
      }

      /* ─────────────────────────────────────────
         响应式
      ───────────────────────────────────────── */
      @media (max-width: 1200px) {
        .feature-grid {
          grid-template-columns: repeat(2, minmax(0, 1fr));
        }
      }

      @media (max-width: 900px) {
        .split-grid,
        .split-grid.concept-grid,
        .split-grid.feature-layout {
          grid-template-columns: 1fr;
        }
      }

      @media (max-width: 767px) {
        .welcome-page {
          padding: 8px 0 20px 0;
        }

        .brand-hero {
          padding: 4px 0 16px 0;
        }

        .brand-hero img {
          max-width: 220px;
          margin-bottom: 12px;
        }

        .brand-title {
          font-size: 24px;
        }

        .welcome-card {
          padding: 18px 16px;
          border-radius: 10px;
        }

        .welcome-card-title {
          font-size: 17px;
        }

        .feature-grid {
          grid-template-columns: 1fr;
        }

        .intro-tabs-wrap .nav-tabs {
          flex-wrap: wrap;
        }

        .intro-tabs-wrap .nav-tabs > li {
          float: none;
        }
      }
    ")),

    shiny::tags$div(
      class = "welcome-page",

      shiny::tags$div(
        class = "brand-hero",
        shiny::tags$img(
          src = "logo.png",
          alt = "FluxWatch Logo"
        ),
        shiny::tags$h2(
          class = "brand-title",
          "通量宝 FluxWatch"
        ),
        shiny::tags$p(
          class = "brand-subtitle",
          "河流物质通量监测与计算软件 · 面向流域水文水环境领域的数据交互式分析平台"
        )
      ),

      shiny::withMathJax(
        shiny::tags$div(
          class = "welcome-stack",

          # 1. 软件简介（Tab）
          shiny::tags$div(
            class = "welcome-card",
            card_header("info-circle", "1.1 软件简介 Software Introduction"),
            shiny::tags$div(
              class = "intro-tabs-wrap",
              shiny::tabsetPanel(
                id = ns("intro_tabs"),
                type = "tabs",

                shiny::tabPanel(
                  "中文简介",
                  shiny::tags$div(
                    class = "lang-panel zh",
                    shiny::tags$div(class = "lang-tag", "中文简介"),
                    shiny::tags$p(
                      "河流物质通量监测与计算软件（简称：通量宝，FluxWatch）是由南京水利科学研究院生态水文与水资源保护研究团队开发的河流物质通量监测与计算软件，也是面向流域水文水环境领域的数据交互式分析平台。软件可实现流域水文水质数据上传、清洗、统计与可视化分析，支持监测断面物质通量计算与不确定性分析、断面通量监测方案制定与优化，以及数据分析结果导出和研究报告编制等功能。"
                    )
                  )
                ),

                shiny::tabPanel(
                  "English",
                  shiny::tags$div(
                    class = "lang-panel en",
                    shiny::tags$div(class = "lang-tag", "English"),
                    shiny::tags$p(
                      shiny::tags$strong("FluxWatch: "),
                      "An interactive data analysis platform for watershed hydrology and water environment developed by the Ecohydrology and Water Resources Protection Research Team from ",
                      shiny::tags$a(
                        href = "https://www.nhri.cn",
                        target = "_blank",
                        "Nanjing Hydraulic Research Institute (NHRI)"
                      ),
                      ". The platform supports uploading, cleaning, statistical analysis and visualization of watershed hydrological and water quality data, as well as material flux calculation at monitoring sections, uncertainty analysis, monitoring scheme formulation and optimization, data export, and research report compilation."
                    )
                  )
                )
              )
            )
          ),

          # 2. 研发背景与主要目的
          shiny::tags$div(
            class = "welcome-card",
            card_header("book", "1.2 研发背景与主要目的 R&D Background and Main Objectives"),
            shiny::tags$div(
              class = "split-grid",
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$p(
                  "河流通量监测和计算对于水资源保护、水环境治理和水生态修复具有重要的科学意义和管理价值。相比于单纯的物质浓度，物质通量能够更准确地反映水环境系统中物质迁移、转化和归趋的动态过程。"
                ),
                shiny::tags$p(
                  "河流污染物通量监测和计算主要回答“在一定时期内，有多少污染物通过河流断面”这一问题，属于污染物“量”的概念。"
                )
              ),
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$p(
                  "通过监测入湖、入河污染物通量，可以计算维持良好水生态所允许的污染物最大输入量，为污染负荷总量控制、责任认定和治理成效评估提供数据基础。"
                ),
                shiny::tags$p(
                  "本软件以力求科学、客观、精准反映河流断面通量过程为目标，为使用者提供常用的河流物质通量监测、分析、计算工具和监测方案设计方法。"
                )
              )
            )
          ),

          # 3. 关键概念与公式
          shiny::tags$div(
            class = "welcome-card",
            card_header("cubes", "1.3 关键概念与公式 Key Concepts & Formula"),
            shiny::tags$div(
              class = "split-grid concept-grid",
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$p(
                  "通量是指单位时间内通过单位面积的物质或能量，有水通量、离子通量、污染物通量、沉积物-上覆水界面通量等。在水环境领域，物质通量通常是指单位时间内通过单位面积的各种阴阳离子物质或污染物的数量。"
                ),
                shiny::tags$p(
                  "对于某个特定河流断面，通量通常可通过监测物质浓度与流量后计算得到，常用单位有 g/s、t/d 等。"
                )
              ),
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$p("物质通量（简称“通量”）的通用数学表达式为："),
                shiny::tags$div(
                  class = "formula-box",
                  shiny::HTML("$$ F = \\frac{M}{A \\cdot T} \\tag{1-1} $$")
                ),
                shiny::tags$div(
                  class = "variable-box",
                  shiny::HTML("其中：<b>M</b>：物质的量；<b>A</b>：截面积；<b>T</b>：时间")
                ),
                shiny::tags$p(
                  "相比浓度指标，通量更适合描述物质在水环境系统中的迁移、转化和归趋过程。"
                )
              )
            )
          ),

          # 4. 污染负荷、图示与应用意义
          shiny::tags$div(
            class = "welcome-card",
            card_header("line-chart", "1.4 污染负荷、图示与应用意义 Pollutant Load, Illustration & Application"),
            shiny::tags$div(
              class = "split-grid",
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$p(
                  "在水污染治理领域，污染负荷（pollutant load）是常用名词术语，指在一定时间内通过某个断面的污染物质量总和，常用单位有 t、kg 等。污染负荷可以视为污染物通量在时间上的累积值。"
                ),
                shiny::tags$p(
                  "实际工作中，河流断面“污染负荷”与“污染物通量”有时会混合使用。为了说明方便，本软件并未将两者严格区分，其内涵均是指在一段时间内通过某个断面的物质总量。"
                ),
                shiny::tags$p(
                  "断面污染负荷计算方法可以用式 1-2 表示："
                ),
                shiny::tags$div(
                  class = "formula-box subtle",
                  shiny::HTML("$$ L = \\int flux(t) \\ast dt \\tag{1-2} $$")
                ),
                shiny::tags$p(
                  "理论上，只要监测断面通量数据的时间尺度足够细，就能获得污染负荷的“真值”。通量计算可为污染被冲走的责任认定、治理成效评估以及总量削减制度提供可靠的数据基础。"
                )
              ),
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$div(
                  class = "figure-wrap",
                  shiny::tags$img(
                    src = "图1 监测断面负荷是物质通量在时间尺度上累积.jpg",
                    alt = "图1 监测断面负荷是物质通量随时间累积"
                  ),
                  shiny::tags$div(
                    class = "figure-caption",
                    "图1 监测断面污染负荷是污染物通量随时间的累积"
                  )
                )
              )
            )
          ),

          # 5. 功能模块与开发说明
          shiny::tags$div(
            class = "welcome-card",
            card_header("th-large", "1.5 功能模块与开发说明 Functional Modules & Feedback"),
            shiny::tags$div(
              class = "split-grid feature-layout",
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$p(
                  "通量宝软件现有主要功能模块覆盖数据处理、分析计算与成果输出等多个环节，便于用户围绕河流断面物质通量开展连续、系统的研究与应用。"
                ),
                shiny::tags$div(
                  class = "feature-grid",

                  feature_item(
                    "database",
                    "数据上传与整理清洗",
                    "Data Upload, Sorting & Cleaning",
                    "支持水文水质数据上传、规范化整理和基础清洗。"
                  ),

                  feature_item(
                    "bar-chart",
                    "可视分析与统计处理",
                    "Data Visualization & Statistical Analysis",
                    "支持趋势识别、图形展示和统计分析处理。"
                  ),

                  feature_item(
                    "calculator",
                    "通量计算和结果展示",
                    "Flux Calculation & Result Presentation",
                    "支持监测断面物质通量计算及结果展示。"
                  ),

                  feature_item(
                    "sliders",
                    "监测方案设计与优化",
                    "Monitoring Scheme Design & Optimization",
                    "支持通量监测方案制定与参数优化分析。"
                  ),

                  feature_item(
                    "file-text-o",
                    "报告编制与成果输出",
                    "Report Compilation & Result Output",
                    "支持数据分析结果导出与研究报告编制。"
                  )
                )
              ),
              shiny::tags$div(
                class = "sub-panel",
                shiny::tags$p(
                  "本软件主要服务于从事水文水资源和水生态水环境相关领域的学生、科技工作者和管理人员。"
                ),
                shiny::tags$div(
                  class = "dev-callout",
                  shiny::icon("lightbulb-o"),
                  " 本软件正在开发阶段，敬请使用者及时反馈使用过程中出现的问题，以便开发者及时更新完善，共同推动河流通量研究和管理工作。"
                ),
                shiny::tags$ul(
                  class = "feedback-list",
                  shiny::tags$li("如遇数据上传、计算过程或结果展示异常，建议及时反馈。"),
                  shiny::tags$li("欢迎提出功能增强、流程优化与界面体验方面的改进建议。"),
                  shiny::tags$li("建议在正式分析前先熟悉各模块的数据组织方式与功能逻辑。")
                ),
                shiny::tags$div(
                  class = "mini-note",
                  "开发团队将持续迭代完善界面、流程和结果展示体验。"
                )
              )
            )
          )
        )
      )
    )
  )
}

mod_welcome_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}

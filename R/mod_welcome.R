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
      /* =============================================
         全局：铺满整个页面
      ============================================= */
      .content-wrapper, .tab-content, .tab-pane, body {
        background: #f5f7fb !important;
      }
      .content-wrapper {
        padding: 0 !important;
      }
      .content-wrapper > .content {
        padding: 0 !important;
        margin: 0 !important;
      }
      .tab-content > .tab-pane {
        padding: 0 !important;
      }

      .welcome-page {
        width: 100%;
        max-width: none;
        margin: 0;
        padding: 0 0 40px 0;
      }
      .welcome-stack {
        display: flex;
        flex-direction: column;
        gap: 20px;
        padding: 0 20px;
      }

      /* =============================================
         标题栏：纯白背景
      ============================================= */
      .brand-hero {
        width: 100%;
        text-align: center;
        padding: 36px 20px 32px 20px;
        background: #ffffff;
        border-bottom: none;
        margin-bottom: 2px;
      }
      .brand-hero img {
        display: block;
        width: 100%;
        max-width: 720px;
        margin: 0 auto;
        object-fit: contain;
        filter: drop-shadow(0 4px 18px rgba(31,45,61,0.08));
      }

      /* =============================================
         通用卡片
      ============================================= */
      .welcome-card {
        width: 100%;
        background: #ffffff;
        border: 1px solid #e7ecf2;
        border-radius: 12px;
        box-shadow: 0 4px 14px rgba(31,45,61,0.04);
        padding: 24px 28px;
        transition: transform 0.18s ease, box-shadow 0.18s ease;
      }
      .welcome-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 10px 24px rgba(31,45,61,0.07);
      }
      .welcome-card-header {
        display: flex;
        align-items: center;
        gap: 12px;
        margin-bottom: 16px;
      }
      .welcome-card-icon {
        width: 40px; height: 40px;
        border-radius: 10px;
        background: #eef5ff; color: #2f7dbd;
        display: flex; align-items: center; justify-content: center;
        font-size: 18px; flex: 0 0 40px;
      }
      .welcome-card-title {
        font-size: 18px; font-weight: 700;
        line-height: 1.45; color: #1f2d3d;
      }
      .welcome-card p {
        margin: 0 0 12px 0;
        font-size: 15px; line-height: 1.95;
        color: #334155; text-align: justify;
      }
      .welcome-card p:last-child { margin-bottom: 0; }
      .welcome-card a { color: #337ab7; text-decoration: none; }
      .welcome-card a:hover { text-decoration: underline; }

      /* =============================================
         1. 软件简介
      ============================================= */
      .intro-header-row {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 16px;
      }
      .intro-header-row .welcome-card-header {
        margin-bottom: 0;
      }
      .lang-switch-wrap {
        display: flex;
        align-items: center;
        gap: 0;
        background: #f1f5f9;
        border-radius: 8px;
        padding: 3px;
        flex-shrink: 0;
      }
      .lang-switch-wrap .btn-lang {
        padding: 5px 14px;
        font-size: 13px; font-weight: 700;
        border: none; border-radius: 6px;
        cursor: pointer;
        background: transparent; color: #64748b;
        transition: all 0.15s ease;
        line-height: 1.4;
      }
      .lang-switch-wrap .btn-lang.active {
        background: #ffffff;
        color: #2f7dbd;
        box-shadow: 0 1px 4px rgba(31,45,61,0.10);
      }
      .intro-panel {
        background: #fbfcfe;
        border: 1px solid #e9eef5;
        border-radius: 12px;
        padding: 20px 24px;
      }
      .intro-panel.zh { border-left: 4px solid #3c8dbc; }
      .intro-panel.en { border-left: 4px solid #00a65a; }
      .intro-panel p { margin: 0 0 10px 0; }
      .intro-panel p:last-child { margin-bottom: 0; }

      /* =============================================
         2. 研发背景
      ============================================= */
      .narrative-block p {
        text-indent: 2em;
        text-align: justify;
      }
      .narrative-highlight {
        background: #f0f7ff;
        border-left: 4px solid #3c8dbc;
        border-radius: 8px;
        padding: 14px 18px;
        margin: 16px 0;
        color: #334155;
        line-height: 1.9; font-size: 15px;
      }
      .narrative-highlight p {
        text-indent: 0 !important;
        margin: 0 !important;
      }

      /* =============================================
         3. 核心概念  左40 / 右60
      ============================================= */
      .dual-concept {
        display: grid;
        grid-template-columns: 2fr 3fr;
        gap: 22px;
        align-items: start;
      }
      .concept-col {
        display: flex;
        flex-direction: column;
        gap: 16px;
      }
      .concept-block {
        background: #fbfcfe;
        border: 1px solid #edf2f7;
        border-radius: 12px;
        padding: 18px 20px;
      }
      .concept-block-title {
        font-size: 16px; font-weight: 700;
        color: #1f2937;
        margin-bottom: 12px;
        padding-bottom: 10px;
        border-bottom: 2px solid #e7ecf2;
        display: flex; align-items: center; gap: 8px;
      }
      .concept-block-title .badge-num {
        display: inline-flex;
        align-items: center; justify-content: center;
        width: 26px; height: 26px;
        border-radius: 50%;
        background: #2f7dbd; color: #fff;
        font-size: 13px; font-weight: 700;
        flex: 0 0 26px;
      }
      .formula-col {
        display: flex;
        flex-direction: column;
        gap: 12px;
      }
      .formula-box {
        background: #f4f7fb;
        border: 1px solid #d9e9f7;
        border-radius: 10px;
        padding: 14px 20px;
        text-align: center;
        margin: 4px 0;
        font-size: 16px;
      }
      .variable-box {
        background: #fafafa;
        border-left: 4px solid #3c8dbc;
        border-radius: 8px;
        padding: 12px 14px;
        margin-bottom: 4px;
        color: #444; line-height: 1.85;
        font-size: 14.5px;
      }
      .figure-wrap {
        text-align: center;
        margin: 8px 0 0 0;
      }
      .figure-wrap img {
        width: 100%;
        max-width: 100%;
        height: auto;
        border: 1px solid #e5e7eb;
        border-radius: 10px;
        box-shadow: 0 2px 10px rgba(0,0,0,0.06);
      }
      .figure-caption {
        margin-top: 10px;
        font-size: 13px; color: #667085;
        text-align: center; line-height: 1.7;
      }

      /* =============================================
         5. 功能模块
      ============================================= */
      .feature-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 14px;
      }
      .feature-item {
        display: flex; gap: 14px;
        align-items: flex-start;
        border: 1px solid #e9eef5;
        border-radius: 12px;
        background: #fbfcfe;
        padding: 16px;
        min-height: 96px;
        transition: transform 0.15s ease, box-shadow 0.15s ease;
      }
      .feature-item:hover {
        transform: translateY(-2px);
        box-shadow: 0 6px 18px rgba(31,45,61,0.06);
      }
      .feature-item-icon {
        width: 40px; height: 40px;
        border-radius: 10px;
        background: #eef5ff; color: #2f7dbd;
        display: flex; align-items: center; justify-content: center;
        flex: 0 0 40px; font-size: 17px;
      }
      .feature-item-title {
        font-size: 15px; font-weight: 700;
        line-height: 1.5; color: #1f2937;
        margin-bottom: 2px;
      }
      .feature-item-subtitle {
        font-size: 12.5px; color: #64748b;
        line-height: 1.5; margin-bottom: 5px;
      }
      .feature-item-desc {
        margin: 0 !important;
        font-size: 13.5px !important;
        line-height: 1.75 !important;
        color: #475569 !important;
        text-align: left !important;
      }

      /* =============================================
         6. 使用说明
      ============================================= */
      .usage-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 22px;
        align-items: start;
      }
      .usage-panel {
        background: #fbfcfe;
        border: 1px solid #edf2f7;
        border-radius: 12px;
        padding: 18px 20px;
      }
      .usage-panel p {
        font-size: 14.5px;
        line-height: 1.85;
      }
      .usage-panel-title {
        font-size: 15px;
        font-weight: 700;
        color: #1f2937;
        margin-bottom: 12px;
        padding-bottom: 8px;
        border-bottom: 2px solid #e7ecf2;
      }
      .dev-callout {
        background: #fff8e6;
        border: 1px solid #ffe2a8;
        border-left: 4px solid #f39c12;
        border-radius: 10px;
        padding: 14px 16px;
        margin-top: 14px;
        color: #6b4e16;
        line-height: 1.85; font-size: 14.5px;
      }
      .dev-callout .fa, .dev-callout .fas, .dev-callout .far {
        margin-right: 6px;
      }
      .feedback-list {
        margin: 8px 0 0 18px;
        padding: 0;
      }
      .feedback-list li {
        margin-bottom: 7px;
        font-size: 14px; line-height: 1.8;
        color: #475569;
      }
      .feedback-list li:last-child { margin-bottom: 0; }
      .feedback-link-box {
        margin-top: 12px;
        padding: 10px 14px;
        background: #f0f7ff;
        border: 1px solid #d9e9f7;
        border-radius: 8px;
        font-size: 13.5px; color: #334155;
      }
      .feedback-link-box a {
        color: #2f7dbd; font-weight: 600;
      }
      .mini-note {
        margin-top: 10px;
        font-size: 13.5px;
        line-height: 1.75;
        color: #667085;
      }

      /* =============================================
         响应式
      ============================================= */
      @media (max-width: 960px) {
        .dual-concept,
        .usage-grid {
          grid-template-columns: 1fr;
        }
      }
      @media (max-width: 767px) {
        .welcome-stack { padding: 0 10px; }
        .brand-hero { padding: 20px 10px 18px 10px; }
        .brand-hero img { max-width: 400px; }
        .welcome-card { padding: 18px 14px; border-radius: 10px; }
        .welcome-card-title { font-size: 17px; }
        .feature-grid { grid-template-columns: 1fr; }
        .intro-header-row { flex-wrap: wrap; gap: 10px; }
      }
    ")),

    shiny::tags$script(shiny::HTML(sprintf("
      $(document).on('click', '#%s .btn-lang', function() {
        var lang = $(this).data('lang');
        var wrap = $(this).closest('.welcome-card');
        wrap.find('.btn-lang').removeClass('active');
        $(this).addClass('active');
        if (lang === 'en') {
          wrap.find('.intro-panel.zh').hide();
          wrap.find('.intro-panel.en').show();
        } else {
          wrap.find('.intro-panel.en').hide();
          wrap.find('.intro-panel.zh').show();
        }
      });
    ", ns("intro_card")))),

    shiny::tags$div(
      class = "welcome-page",

      # ════════════════════════════════════════
      # 标题栏
      # ════════════════════════════════════════
      shiny::tags$div(
        class = "brand-hero",
        shiny::tags$img(
          src = "logo(1).png",
          alt = "FluxWatch Logo"
        )
      ),

      shiny::withMathJax(
        shiny::tags$div(
          class = "welcome-stack",

          # ════════════════════════════════════════
          # 1. 软件简介
          # ════════════════════════════════════════
          shiny::tags$div(
            id = ns("intro_card"),
            class = "welcome-card",

            shiny::tags$div(
              class = "intro-header-row",
              card_header("circle-info",
                          "1.1 \u8f6f\u4ef6\u7b80\u4ecb Software Introduction"),
              shiny::tags$div(
                class = "lang-switch-wrap",
                shiny::tags$button(
                  class = "btn-lang active",
                  `data-lang` = "zh", "\u4e2d\u6587"
                ),
                shiny::tags$button(
                  class = "btn-lang",
                  `data-lang` = "en", "EN"
                )
              )
            ),

            shiny::tags$div(
              class = "intro-panel zh",
              shiny::tags$p(
                "\u901a\u91cf\u5b9d\uff1a\u5357\u4eac\u6c34\u5229\u79d1\u5b66\u7814\u7a76\u9662",
                "\u751f\u6001\u6c34\u6587\u4e0e\u6c34\u8d44\u6e90\u4fdd\u62a4\u7814\u7a76\u56e2\u961f",
                "\u5f00\u53d1\u7684\u6d41\u57df\u6c34\u6587\u6c34\u73af\u5883\u6570\u636e\u4ea4\u4e92\u5f0f\u5206\u6790\u8f6f\u4ef6\u3002",
                "\u8be5\u8f6f\u4ef6\u53ef\u5b9e\u73b0\u6d41\u57df\u6c34\u6587\u6c34\u8d28\u6570\u636e\u4e0a\u4f20\u3001\u6e05\u6d17\u3001",
                "\u7edf\u8ba1\u4e0e\u53ef\u89c6\u5316\u5206\u6790\uff0c",
                "\u76d1\u6d4b\u65ad\u9762\u7269\u8d28\u901a\u91cf\u8ba1\u7b97\u3001\u4e0d\u786e\u5b9a\u6027\u5206\u6790\u3001",
                "\u65ad\u9762\u901a\u91cf\u76d1\u6d4b\u65b9\u6848\u5236\u5b9a\u548c\u4f18\u5316\uff0c",
                "\u6570\u636e\u5206\u6790\u7ed3\u679c\u5bfc\u51fa\u548c\u7814\u7a76\u62a5\u544a\u7f16\u5236\u3002"
              )
            ),

            shiny::tags$div(
              class = "intro-panel en",
              style = "display: none;",
              shiny::tags$p(
                shiny::tags$strong("FluxWatch: "),
                "An interactive data analysis platform software for watershed ",
                "hydrology and water environment developed by the Ecohydrology ",
                "and Water Resources Protection Research Team from ",
                shiny::tags$a(
                  href = "https://www.nhri.cn",
                  target = "_blank",
                  "Nanjing Hydraulic Research Institute (NHRI)"
                ),
                ". This platform enables the uploading, cleaning, statistical ",
                "analysis, and visualization of watershed hydrological and water ",
                "quality data; material flux calculating at flux monitoring stations ",
                "and the uncertainty analysis; flux monitoring schemes formulation ",
                "and optimization; data analysis results downloading; compilation ",
                "of research reports ",
                shiny::tags$em("etc"), "."
              )
            )
          ),

          # ════════════════════════════════════════
          # 2. 研发背景与主要目的
          # ════════════════════════════════════════

          shiny::tags$div(
            class = "welcome-card",
            card_header("book",
                        "1.2 \u7814\u53d1\u80cc\u666f\u4e0e\u4e3b\u8981\u76ee\u7684 R&D Background & Objectives"),
            shiny::tags$div(
              class = "narrative-block",
              shiny::tags$p(
                "\u6cb3\u6d41\u901a\u91cf\u76d1\u6d4b\u548c\u8ba1\u7b97\u5bf9\u4e8e\u6c34\u8d44\u6e90\u4fdd\u62a4\u3001",
                "\u6c34\u73af\u5883\u6cbb\u7406\u548c\u6c34\u751f\u6001\u4fee\u590d\u5177\u6709\u91cd\u8981\u7684\u79d1\u5b66\u610f\u4e49\u548c\u7ba1\u7406\u4ef7\u503c\u3002",
                "\u6cb3\u6d41\u6c61\u67d3\u7269\u901a\u91cf\u76d1\u6d4b\u548c\u8ba1\u7b97\u4e3b\u8981\u56de\u7b54",
                "\u201c\u5355\u4f4d\u65f6\u95f4\u5185\u6709\u591a\u5c11\u6c61\u67d3\u7269\u901a\u8fc7\u6cb3\u6d41\u65ad\u9762\u201d\u3002",
                "\u6c61\u67d3\u7269\u901a\u91cf\u5728\u67d0\u6bb5\u65f6\u95f4\u5185\u7684\u7d2f\u79ef\u503c\u5373\u4e3a\u6c61\u67d3\u8d1f\u8377\u3002",
                "\u901a\u8fc7\u76d1\u6d4b\u5165\u6e56\u3001\u5165\u6cb3\u7684\u6c61\u67d3\u7269\u901a\u91cf\uff0c",
                "\u53ef\u4ee5\u8ba1\u7b97\u7ef4\u6301\u826f\u597d\u6c34\u751f\u6001\u6c61\u67d3\u7269\u6700\u5927\u5141\u8bb8\u8f93\u5165\u91cf",
                "\uff08TMDL\uff0c\u5373\u6700\u5927\u65e5\u8d1f\u8377\u603b\u91cf\uff09\u3002",
                "\u6c61\u67d3\u7269\u901a\u91cf\u63d0\u4f9b\u4e86\u751f\u6001\u73af\u5883\u7ba1\u7406\u90e8\u95e8",
                "\u201c\u6c61\u67d3\u8d23\u4efb\u8ba4\u5b9a\u201d\u548c\u201c\u6c61\u67d3\u8d1f\u8377\u603b\u91cf\u524a\u51cf\u201d\u7684\u4f9d\u636e\uff0c",
                "\u4e3a\u89e3\u51b3\u201c\u6c61\u67d3\u88ab\u51b2\u8d70\u4e86\u7b97\u8c01\u7684\u201d",
                "\u4ee5\u53ca\u201c\u6cbb\u7406\u6210\u6548\u662f\u5426\u771f\u5b9e\u201d\u7b49\u5b9e\u9645\u95ee\u9898\u63d0\u4f9b\u6570\u636e\u57fa\u7840\uff0c",
                "\u662f\u5f53\u524d\u6c34\u73af\u5883\u6cbb\u7406\u6c61\u67d3\u8d1f\u8377",
                "\u201c\u603b\u91cf\u63a7\u5236\u201d\u5236\u5ea6\u7684\u57fa\u77f3\u3002"
              )
            )
          ),

          # ════════════════════════════════════════════════════
          # 3. 核心概念与计算原理  左40% / 右60%
          # ════════════════════════════════════════════════════
          shiny::tags$div(
            class = "welcome-card",
            card_header("cubes",
                        "1.3 \u6838\u5fc3\u6982\u5ff5\u4e0e\u8ba1\u7b97\u539f\u7406 Key Concepts & Calculation Principles"),
            shiny::tags$div(
              class = "dual-concept",

              # ── 左栏 40%：概念①② + 公式 1-1、1-2、1-3 ──
              shiny::tags$div(
                class = "concept-col",

                # 概念块 ① 流量、流率
                shiny::tags$div(
                  class = "concept-block",
                  shiny::tags$div(
                    class = "concept-block-title",
                    shiny::tags$span(class = "badge-num", "1"),
                    "\u6d41\u91cf\u3001\u6d41\u7387 Flow & Flow Rate"
                  ),
                  shiny::tags$p(
                    "\u76ee\u524d\u56fd\u5185\u5916\u666e\u904d\u63a5\u53d7\u7684\u201c\u6d41\u91cf\u201d\u5b9a\u4e49\u662f",
                    "\u5355\u4f4d\u65f6\u95f4\u5185\u901a\u8fc7\u6cb3\u6d41\u6216\u6e20\u9053\u67d0\u4e00\u8fc7\u6c34\u65ad\u9762\u7684\u6c34\u91cf\uff0c",
                    "\u5355\u4f4d\u662f m\u00b3/s\uff0c\u82f1\u6587\u5e38\u7528 \"flow\" \u8868\u793a\u3002",
                    "\u5728\u82f1\u6587\u79d1\u5b66\u4f53\u7cfb\u4e2d\uff0c\"flow\" \u8fd9\u4e2a\u8bcd\u88ab\u5e7f\u6cdb\u4f7f\u7528\uff0c",
                    "\u5176\u79d1\u5b66\u5185\u6db5\u56e0\u9886\u57df\u800c\u5f02\uff0c",
                    "\u4f46\u5176\u7cbe\u9ad3\u5728\u4e8e\u6355\u6349\u548c\u91cf\u5316\u4e00\u5207\u975e\u9759\u6001\u8fc7\u7a0b\u7684\u52a8\u6001\u6027\u548c\u8fde\u7eed\u6027\u3002",
                    "\u5728\u6c34\u79d1\u5b66\u7814\u7a76\u4e0e\u6c34\u6587\u6c34\u5229\u884c\u4e1a\uff0c",
                    "\u201c\u6d41\u91cf\u201d\u540c\u6837\u4e5f\u662f\u6700\u57fa\u672c\u7684\u540d\u8bcd\u672f\u8bed\u3002",
                    "\u76ee\u524d\u6c34\u6587\u6c34\u5229\u884c\u4e1a\u7684\u201c\u6d41\u91cf\u201d\u5b9a\u4e49\u4e2d\uff0c",
                    "\u4e0d\u4f46\u6709\u201c\u91cf\u201d\u7684\u6db5\u4e49\uff0c\u4e5f\u6709\u201c\u65f6\u95f4\u201d\u7684\u5185\u6db5\u3002"
                  ),
                  shiny::tags$p(
                    "\u4ece\u4e25\u683c\u7684\u540d\u8bcd\u672f\u8bed\u5b9a\u4e49\u6765\u770b\uff0c",
                    "\u73b0\u6709\u7684\u201c\u6d41\u91cf\u201d\u662f\u5173\u4e8e\u6c34\u4f53\u5355\u4f4d\u65f6\u95f4\u901a\u8fc7\u65ad\u9762\u7684\u201c\u7387\u201d\uff0c",
                    "\u800c\u4e0d\u662f\u901a\u8fc7\u65ad\u9762\u6c34\u7684\u603b\u201c\u91cf\u201d\uff0c",
                    "\u82f1\u6587\u4e3a \"flow rate\"\uff08\u201c\u6d41\u7387\u201d\uff09\u66f4\u4e3a\u51c6\u786e\u3002",
                    "\u82f1\u6587\u4e2d \"discharge\" \u901a\u5e38\u662f\u6307\u901a\u8fc7\u67d0\u4e2a\u65ad\u9762\u6c34\u7684\u603b\u201c\u91cf\u201d\u3002"
                  )
                ),

                # 概念块 ② 通量、负荷
                shiny::tags$div(
                  class = "concept-block",
                  shiny::tags$div(
                    class = "concept-block-title",
                    shiny::tags$span(class = "badge-num", "2"),
                    "\u901a\u91cf\u3001\u8d1f\u8377 Flux & Load"
                  ),
                  shiny::tags$p(
                    "\u901a\u91cf\u662f\u6307\u5355\u4f4d\u65f6\u95f4\u5185\u901a\u8fc7\u5355\u4f4d\u9762\u79ef\u7684\u7269\u8d28\u6216\u80fd\u91cf\uff0c",
                    "\u6709\u6c34\u901a\u91cf\u3001\u79bb\u5b50\u901a\u91cf\u3001\u6c61\u67d3\u7269\u901a\u91cf\u3001",
                    "\u6c89\u79ef\u7269-\u4e0a\u8986\u6c34\u754c\u9762\u901a\u91cf\u7b49\u3002",
                    "\u5728\u6c34\u73af\u5883\u9886\u57df\uff0c\u7269\u8d28\u901a\u91cf\u901a\u5e38\u662f\u6307",
                    "\u5355\u4f4d\u65f6\u95f4\u5185\u67d0\u4e2a\u6c34\u6d41\u65ad\u9762\u7684",
                    "\u5404\u79cd\u9634\u9633\u79bb\u5b50\u7269\u8d28\u6216\u6c61\u67d3\u7269\u7684\u603b\u91cf\u3002",
                    "\u76f8\u6bd4\u4e8e\u7269\u8d28\u6d53\u5ea6\uff0c",
                    "\u7269\u8d28\u901a\u91cf\u80fd\u66f4\u51c6\u786e\u5730\u53cd\u6620\u6c34\u73af\u5883\u7cfb\u7edf\u4e2d",
                    "\u7269\u8d28\u8fc1\u79fb\u3001\u8f6c\u5316\u548c\u5f52\u8d8b\u7684\u52a8\u6001\u8fc7\u7a0b\u3002"
                  )
                ),

                # 公式 1-1
                shiny::tags$p(
                  "\u7269\u8d28\u901a\u91cf\uff08\u7b80\u79f0\u201c\u901a\u91cf\u201d\uff09\uff0c",
                  "\u901a\u7528\u6570\u5b66\u8868\u8fbe\u5f0f\u4e3a\uff1a"
                ),
                shiny::tags$div(
                  class = "formula-box",
                  shiny::HTML("$$ F = \\frac{M}{A \\cdot T} \\tag{1-1} $$")
                ),
                shiny::tags$div(
                  class = "variable-box",
                  shiny::HTML(
                    "\u5176\u4e2d\uff1a<b>M</b>\u2014\u7269\u8d28\u7684\u91cf\uff1b",
                    "<b>A</b>\u2014\u622a\u9762\u79ef\uff1b",
                    "<b>T</b>\u2014\u65f6\u95f4"
                  )
                ),

                shiny::tags$p(
                  "\u901a\u91cf\u53ef\u4ee5\u901a\u8fc7\u76d1\u6d4b\u7269\u8d28\u6d53\u5ea6\u4e0e\u6d41\u91cf\u540e\u8ba1\u7b97\u5f97\u5230\uff0c",
                  "\u5bf9\u4e8e\u67d0\u4e2a\u7279\u5b9a\u7684\u6cb3\u6d41\u65ad\u9762\uff0c",
                  "\u901a\u91cf\u5e38\u7528\u5355\u4f4d\u6709 g/s\u3001t/d \u7b49\u3002"
                ),

                shiny::tags$p(
                  "\u5728\u6c34\u6c61\u67d3\u6cbb\u7406\u9886\u57df\uff0c",
                  "\u6c61\u67d3\u8d1f\u8377\uff08", shiny::tags$em("pollutant load"), "\uff09",
                  "\u4e5f\u662f\u5e38\u7528\u540d\u8bcd\u672f\u8bed\u3002",
                  "\u6c61\u67d3\u8d1f\u8377\u6307\u901a\u8fc7\u67d0\u4e2a\u65ad\u9762\u7684\u6c61\u67d3\u7269\u8d28\u91cf\u603b\u548c\uff0c",
                  "\u5e38\u7528\u5355\u4f4d\u6709 t\u3001kg \u7b49\u3002",
                  "\u6c61\u67d3\u8d1f\u8377\u53ef\u4ee5\u770b\u6210\u662f\u6c61\u67d3\u7269\u901a\u91cf\u5728\u65f6\u95f4\u4e0a\u7684\u7d2f\u79ef\u503c\u3002"
                ),

                # 公式 1-2
                shiny::tags$p(
                  "\u5982\u679c\u6211\u4eec\u53ef\u4ee5\u76f4\u63a5\u548c\u8fde\u7eed\u6d4b\u5b9a\u901a\u91cf\uff0c",
                  "\u67d0\u4e2a\u65ad\u9762\u7684\u901a\u91cf\u5206\u5e03\u66f2\u7ebf\u5982\u56fe 1 \u6240\u793a\u3002",
                  "\u76d1\u6d4b\u65f6\u95f4\u8303\u56f4\u5185\u8d1f\u8377\u5c31\u662f\u901a\u91cf\u66f2\u7ebf\u4e0b\u9762\u5305\u542b\u7684\u9634\u5f71\u9762\u79ef\uff0c",
                  "\u53ef\u4ee5\u770b\u6210\u662f\u5ba2\u89c2\u4e0a\u4e8b\u5b9e\u5b58\u5728\u7684\u6c61\u67d3\u8d1f\u8377\u201c\u771f\u503c\u201d\u3002",
                  "\u76d1\u6d4b\u65ad\u9762\u6c61\u67d3\u8d1f\u8377\u53ef\u4ee5\u7528\u5f0f 1-2 \u8868\u793a\uff1a"
                ),
                shiny::tags$div(
                  class = "formula-box",
                  shiny::HTML("$$ L = \\int flux(t) \\, dt \\tag{1-2} $$")
                ),

                # 公式 1-3
                shiny::tags$p(
                  "\u7136\u800c\uff0c\u6c61\u67d3\u7269\u901a\u91cf\u65e0\u6cd5\u76f4\u63a5\u6d4b\u5b9a\uff0c",
                  "\u53ea\u80fd\u901a\u8fc7\u6d4b\u5b9a\u6c61\u67d3\u7269\u6d53\u5ea6\u548c\u6d41\u91cf\uff0c",
                  "\u4e24\u8005\u76f8\u4e58\u503c\u518d\u7d2f\u79ef\u76f8\u52a0\u83b7\u5f97\uff08\u5f0f 1-3\uff09\uff1a"
                ),
                shiny::tags$div(
                  class = "formula-box",
                  shiny::HTML("$$ L = k \\int_t c(t)\\,q(t)\\,dt \\tag{1-3} $$")
                ),
                shiny::tags$div(
                  class = "variable-box",
                  shiny::HTML(
                    "\u5f0f\u4e2d <b>c</b> \u4e3a\u6d53\u5ea6\uff0c",
                    "<b>q</b> \u4e3a\u6d41\u91cf\uff0c",
                    "\u4e24\u8005\u90fd\u662f\u65f6\u95f4 <b>t</b> \u7684\u51fd\u6570\uff0c",
                    "<b>k</b> \u662f\u5355\u4f4d\u8f6c\u6362\u56e0\u5b50\u3002",
                    "\u5f53\u76d1\u6d4b\u65ad\u9762\u901a\u91cf\u76d1\u6d4b\u7684\u65f6\u95f4\u5355\u4f4d\u8d8a\u5c0f\uff0c",
                    "\u8ba1\u7b97\u5f97\u5230\u7684\u6c61\u67d3\u8d1f\u8377\u5c31\u8d8a\u903c\u8fd1\u8be5\u65ad\u9762",
                    "\u76d1\u6d4b\u65f6\u95f4\u6bb5\u5185\u6c61\u67d3\u8d1f\u8377\u7684\u201c\u771f\u503c\u201d\u3002"
                  )
                )
              ),

              # ── 右栏 60%：公式 1-4 + 挑战段落 + 图片 ──
              shiny::tags$div(
                class = "formula-col",

                # 公式 1-4
                shiny::tags$p(
                  "\u6c61\u67d3\u8d1f\u8377\u53ef\u4ee5\u901a\u8fc7\u8fde\u7eed\u6d4b\u91cf\u6216\u6d4b\u5b9a\u7cfb\u5217\u6d41\u91cf\u3001",
                  "\u6c61\u67d3\u7269\u6d53\u5ea6\u6837\u672c\uff0c",
                  "\u91c7\u7528\u4e0b\u5217\u8ba1\u7b97\u516c\u5f0f\u83b7\u5f97\uff08\u5f0f 1-4\uff09\uff1a"
                ),
                shiny::tags$div(
                  class = "formula-box",
                  shiny::HTML("$$ L = k \\sum_{i=1}^{n} c_i\\,q_i\\,\\Delta t \\tag{1-4} $$")
                ),
                shiny::tags$div(
                  class = "variable-box",
                  shiny::HTML(
                    "\u5f0f\u4e2d <b>c<sub>i</sub></b>\u2014\u7b2c <i>i</i> \u4e2a\u65f6\u95f4\u6b65\u957f\u6c61\u67d3\u7269\u6d53\u5ea6\uff1b",
                    "<b>q<sub>i</sub></b>\u2014\u7b2c <i>i</i> \u4e2a\u65f6\u95f4\u6b65\u957f\u6d41\u91cf\uff1b",
                    "<b>\u0394t</b>\u2014\u65f6\u95f4\u6b65\u957f\uff1b",
                    "<b>k</b>\u2014\u5355\u4f4d\u8f6c\u6362\u56e0\u5b50"
                  )
                ),

                shiny::tags$p(
                  "\u7531\u4e8e\u6cb3\u6d41\u65ad\u9762\u6d41\u91cf\u548c\u6d53\u5ea6\u5177\u6709\u9ad8\u5ea6\u7684\u65f6\u95f4\u5f02\u8d28\u6027\uff0c",
                  "\u5982\u4f55\u901a\u8fc7\u91c7\u96c6\u548c\u68c0\u6d4b\u6700\u4f73\u7ec4\u5408\u7684\u79bb\u6563\u6837\u54c1\u96c6",
                  "\u83b7\u5f97\u6700\u4e3a\u51c6\u786e\u7684\u6c61\u67d3\u8d1f\u8377\u4f30\u7b97\u503c",
                  "\u662f\u6c61\u67d3\u7269\u901a\u91cf\u76d1\u6d4b\u548c\u8ba1\u7b97\u5de5\u4f5c\u6700\u4e3b\u8981\u7684\u6311\u6218\u3002",
                  "\u6cb3\u6d41\u65ad\u9762\u6d41\u91cf\u8fc7\u7a0b\u4e0e\u6c34\u8d28\u6d53\u5ea6\u76d1\u6d4b\u5747\u9700\u8017\u8d39\u4e00\u5b9a\u7684\u4eba\u529b\u3001\u7269\u529b\u548c\u8d22\u529b\u3002",
                  "\u901a\u8fc7 ADCP \u8fde\u7eed\u6d4b\u5b9a\u65ad\u9762\u6d41\u901f",
                  "\u6216\u8005\u8fde\u7eed\u6d4b\u5b9a\u6c34\u4f4d\u6362\u7b97\u6210\u6d41\u91cf\uff0c",
                  "\u57fa\u672c\u53ef\u4ee5\u5b9e\u73b0\u5728\u7ebf\u9ad8\u9891\u6b21\u8fde\u7eed\u6d4b\u5b9a\u65ad\u9762\u6d41\u91cf\uff1b",
                  "\u53d7\u73b0\u6709\u6280\u672f\u6761\u4ef6\u548c\u76d1\u6d4b\u6210\u672c\u9650\u5236\uff0c",
                  "\u6cb3\u6d41\u65ad\u9762\u6c34\u8d28\u6d53\u5ea6\u96be\u4ee5\u5b8c\u5168\u505a\u5230",
                  "\u4e0e\u6d41\u91cf\u5b8c\u5168\u76f8\u5339\u914d\u7684\u957f\u65f6\u95f4\u9ad8\u9891\u6b21\u8fde\u7eed\u540c\u6b65\u76d1\u6d4b\uff0c",
                  "\u4eba\u5de5\u5de1\u6d4b\u4ecd\u7136\u662f\u6cb3\u6d41\u65ad\u9762\u6c34\u8d28\u76d1\u6d4b\u7684\u4e3b\u8981\u65b9\u6cd5\uff0c",
                  "\u6c34\u8d28\u6d53\u5ea6\u76d1\u6d4b\u9891\u6b21\u5927\u591a\u8fdc\u4f4e\u4e8e\u6d41\u91cf\u76d1\u6d4b\u9891\u6b21\u3002",
                  "\u5982\u4f55\u5229\u7528\u8f83\u9ad8\u76d1\u6d4b\u9891\u6b21\u7684\u65ad\u9762\u6d41\u91cf\u6570\u636e",
                  "\u548c\u8f83\u4f4e\u76d1\u6d4b\u9891\u6b21\u7684\u65ad\u9762\u6c34\u8d28\u6570\u636e",
                  "\u8ba1\u7b97\u5f97\u5230\u76d1\u6d4b\u65ad\u9762\u901a\u91cf\uff0c",
                  "\u4f7f\u5f97\u6c61\u67d3\u8d1f\u8377\u8ba1\u7b97\u7ed3\u679c\u63a5\u8fd1\u4e8e\u201c\u771f\u503c\u201d",
                  "\u4e00\u76f4\u662f\u4eba\u4eec\u5173\u6ce8\u7684\u91cd\u8981\u79d1\u5b66\u95ee\u9898\u3002"
                ),

                # 示意图
                shiny::tags$div(
                  class = "figure-wrap",
                  shiny::tags$img(
                    src = "\u56fe1 \u76d1\u6d4b\u65ad\u9762\u8d1f\u8377\u662f\u7269\u8d28\u901a\u91cf\u5728\u65f6\u95f4\u5c3a\u5ea6\u4e0a\u7d2f\u79ef.jpg",
                    alt = "\u56fe1 \u76d1\u6d4b\u65ad\u9762\u8d1f\u8377\u793a\u610f\u56fe"
                  ),
                  shiny::tags$div(
                    class = "figure-caption",
                    "\u56fe1  \u76d1\u6d4b\u65ad\u9762\u6c61\u67d3\u8d1f\u8377\u662f\u6c61\u67d3\u7269\u901a\u91cf\u968f\u65f6\u95f4\u7684\u7d2f\u79ef"
                  )
                )
              )
            )
          ),

          # ════════════════════════════════════════════════════
          # 4. 功能模块
          # ════════════════════════════════════════════════════
          shiny::tags$div(
            class = "welcome-card",
            card_header("grip-horizontal",
                        "1.4 \u529f\u80fd\u6a21\u5757 Functional Modules"),
            shiny::tags$p(
              "\u901a\u91cf\u5b9d\u8f6f\u4ef6\u73b0\u6709\u4e3b\u8981\u529f\u80fd\u6a21\u5757\u5305\u62ec\uff1a"
            ),

            shiny::tags$div(
              class = "feature-grid",

              feature_item(
                "database",
                "\u2460 \u6570\u636e\u4e0a\u4f20\u4e0e\u6574\u7406\u6e05\u6d17",
                "Data Upload, Sorting & Cleaning",
                "\u652f\u6301\u6c34\u6587\u6c34\u8d28\u6570\u636e\u4e0a\u4f20\u3001\u89c4\u8303\u5316\u6574\u7406\u548c\u57fa\u7840\u6e05\u6d17\u3002"
              ),

              feature_item(
                "chart-bar",
                "\u2461 \u53ef\u89c6\u5206\u6790\u4e0e\u7edf\u8ba1\u5904\u7406",
                "Data Visualization & Statistical Analysis",
                "\u652f\u6301\u8d8b\u52bf\u8bc6\u522b\u3001\u56fe\u5f62\u5c55\u793a\u548c\u7edf\u8ba1\u5206\u6790\u5904\u7406\u3002"
              ),

              feature_item(
                "calculator",
                "\u2462 \u901a\u91cf\u8ba1\u7b97\u548c\u7ed3\u679c\u5c55\u793a",
                "Flux Calculation & Result Presentation",
                "\u652f\u6301\u76d1\u6d4b\u65ad\u9762\u7269\u8d28\u901a\u91cf\u8ba1\u7b97\u53ca\u7ed3\u679c\u5c55\u793a\u3002"
              ),

              feature_item(
                "sliders",
                "\u2463 \u76d1\u6d4b\u65b9\u6848\u8bbe\u8ba1\u4e0e\u4f18\u5316",
                "Monitoring Scheme Design & Optimization",
                "\u652f\u6301\u901a\u91cf\u76d1\u6d4b\u65b9\u6848\u5236\u5b9a\u4e0e\u53c2\u6570\u4f18\u5316\u5206\u6790\u3002"
              ),

              feature_item(
                "file-lines",
                "\u2464 \u62a5\u544a\u7f16\u5236\u4e0e\u6210\u679c\u8f93\u51fa",
                "Report Compilation & Result Output",
                "\u652f\u6301\u6570\u636e\u5206\u6790\u7ed3\u679c\u5bfc\u51fa\u4e0e\u7814\u7a76\u62a5\u544a\u7f16\u5236\uff0c\u662f\u6574\u4e2a\u5206\u6790\u6d41\u7a0b\u7684\u6700\u7ec8\u6210\u679c\u8f93\u51fa\u73af\u8282\u3002"
              )
            )
          ),

          # ════════════════════════════════════════════════════
          # 5. 使用说明与反馈
          # ════════════════════════════════════════════════════
          shiny::tags$div(
            class = "welcome-card",
            card_header("comment-dots",
                        "1.5 \u4f7f\u7528\u8bf4\u660e\u4e0e\u53cd\u9988 Usage Notes & Feedback"),

            shiny::tags$div(
              class = "usage-grid",

              # 左栏
              shiny::tags$div(
                class = "usage-panel",
                shiny::tags$div(
                  class = "usage-panel-title",
                  "\u670d\u52a1\u5bf9\u8c61\u4e0e\u5f00\u53d1\u8bf4\u660e"
                ),
                shiny::tags$p(
                  "\u672c\u8f6f\u4ef6\u4e3b\u8981\u670d\u52a1\u4e8e\u4ece\u4e8b\u6c34\u6587\u6c34\u8d44\u6e90",
                  "\u548c\u6c34\u751f\u6001\u6c34\u73af\u5883\u76f8\u5173\u9886\u57df\u7684\u5b66\u751f\u3001",
                  "\u79d1\u6280\u5de5\u4f5c\u8005\u548c\u7ba1\u7406\u4eba\u5458\u3002"
                ),
                shiny::tags$div(
                  class = "dev-callout",
                  shiny::icon("lightbulb"),
                  " \u672c\u8f6f\u4ef6\u6b63\u5728\u5f00\u53d1\u9636\u6bb5\uff0c",
                  "\u656c\u8bf7\u4f7f\u7528\u8005\u53ca\u65f6\u53cd\u9988\u4f7f\u7528\u8fc7\u7a0b\u4e2d\u51fa\u73b0\u7684\u95ee\u9898\uff0c",
                  "\u4ee5\u4fbf\u5f00\u53d1\u8005\u53ca\u65f6\u66f4\u65b0\u5b8c\u5584\uff0c",
                  "\u5171\u540c\u63a8\u52a8\u6cb3\u6d41\u901a\u91cf\u7814\u7a76\u548c\u7ba1\u7406\u5de5\u4f5c\u3002"
                ),
                shiny::tags$div(
                  class = "mini-note",
                  "\u5f00\u53d1\u56e2\u961f\u5c06\u6301\u7eed\u8fed\u4ee3\u5b8c\u5584\u754c\u9762\u3001\u6d41\u7a0b\u548c\u7ed3\u679c\u5c55\u793a\u4f53\u9a8c\u3002"
                )
              ),

              # 右栏
              shiny::tags$div(
                class = "usage-panel",
                shiny::tags$div(
                  class = "usage-panel-title",
                  "\u53cd\u9988\u4e0e\u5efa\u8bae"
                ),
                shiny::tags$ul(
                  class = "feedback-list",
                  shiny::tags$li(
                    "\u5982\u9047\u6570\u636e\u4e0a\u4f20\u3001\u8ba1\u7b97\u8fc7\u7a0b\u6216\u7ed3\u679c\u5c55\u793a\u5f02\u5e38\uff0c\u5efa\u8bae\u53ca\u65f6\u53cd\u9988\u3002"),
                  shiny::tags$li(
                    "\u6b22\u8fce\u63d0\u51fa\u529f\u80fd\u589e\u5f3a\u3001\u6d41\u7a0b\u4f18\u5316\u4e0e\u754c\u9762\u4f53\u9a8c\u65b9\u9762\u7684\u6539\u8fdb\u5efa\u8bae\u3002"),
                  shiny::tags$li(
                    "\u5efa\u8bae\u5728\u6b63\u5f0f\u5206\u6790\u524d\u5148\u719f\u6089\u5404\u6a21\u5757\u7684\u6570\u636e\u7ec4\u7ec7\u65b9\u5f0f\u4e0e\u529f\u80fd\u903b\u8f91\u3002")
                ),
                shiny::tags$div(
                  class = "feedback-link-box",
                  "\u8054\u7cfb\u5f00\u53d1\u56e2\u961f\uff1a",
                  shiny::tags$a(
                    href = "https://www.nhri.cn",
                    target = "_blank",
                    "\u5357\u4eac\u6c34\u5229\u79d1\u5b66\u7814\u7a76\u9662 (NHRI)"
                  ),
                  " \u00b7 \u751f\u6001\u6c34\u6587\u4e0e\u6c34\u8d44\u6e90\u4fdd\u62a4\u7814\u7a76\u56e2\u961f"
                )
              )
            )
          )

        ) # end welcome-stack
      ) # end withMathJax
    ) # end welcome-page
  ) # end tabItem
}

mod_welcome_server <- function(id, rv) {
  shiny::moduleServer(id, function(input, output, session) {
  })
}

# =====================================================================
# mod_flux_regression.R
# 回归法 —— UI 参数 + Server 初始化 + 数据获取 + 计算调度
# ★ 核心改动：用单个分组下拉（optgroup）替代原来的"类别+模型"两级联动
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}


# =============================================================
# 原理文本
# =============================================================
# =============================================================
# K. 方法原理介绍（HTML，支持 active_sub 高亮）
# =============================================================
# =============================================================
# ★ 原理文本（带高亮 —— 完整版）
# =============================================================
fw_regression_principle_text <- function(active_sub = NULL) {

  # ---- 高亮样式辅助函数 ----
  hl <- function(keys, html_block) {
    is_active <- !is.null(active_sub) && (active_sub %in% keys)
    style <- if (is_active) {
      "background:#fff3cd; border-left:4px solid #ffc107; padding:10px 14px; margin:8px 0; border-radius:4px;"
    } else {
      "padding:10px 14px; margin:8px 0;"
    }
    badge <- if (is_active) {
      '<span style="background:#ffc107;color:#333;padding:2px 8px;border-radius:3px;font-size:11px;font-weight:bold;margin-left:8px;">\u2714 \u5f53\u524d\u9009\u4e2d</span>'
    } else ""
    paste0('<div style="', style, '">', badge, html_block, '</div>')
  }

  # ================================================================
  # 第一大类：RiverLoad Method 9–12
  # ================================================================
  sec_riverload_header <- '
<h4 style="color:#0d6efd;border-bottom:2px solid #0d6efd;padding-bottom:4px;">
  1. RiverLoad \u8d1f\u8377\u4f30\u7b97\u65b9\u6cd5\uff08Method 9\u201312\uff09
</h4>'

  sec_rating <- hl("rating", '
<h5>\u25b6 Method 9\uff1arating \u2014 \u5e42\u51fd\u6570/\u5bf9\u6570\u56de\u5f52\u8bc4\u7ea7\u66f2\u7ebf</h5>
<div style="text-align:center;margin:6px 0;">
  \\[C = aQ^{b},\\qquad L_{r}=\\sum_{i=1}^{n} C_i Q_i\\]
</div>
<table style="font-size:13px;margin:4px 0 8px 12px;">
  <tr><td>\\(C\\)</td><td>\uff1a\u6d53\u5ea6</td></tr>
  <tr><td>\\(Q\\)</td><td>\uff1a\u6d41\u91cf</td></tr>
  <tr><td>\\(a,b\\)</td><td>\uff1a\u8bc4\u7ea7\u66f2\u7ebf\u53c2\u6570\uff08\u901a\u8fc7\u56de\u5f52\u62df\u5408\u5f97\u5230\uff09</td></tr>
  <tr><td>\\(L_r\\)</td><td>\uff1a\u7531\u8bc4\u7ea7\u66f2\u7ebf\u5f97\u5230\u7684\u8d1f\u8377\u4f30\u8ba1\uff08\u672a\u505a\u56de\u53d8\u6362\u504f\u5dee\u4fee\u6b63\uff09</td></tr>
  <tr><td>\\(n\\)</td><td>\uff1a\u7528\u4e8e\u7d2f\u52a0\u7684\u65f6\u95f4\u6b65/\u6837\u672c\u70b9\u6570\u91cf</td></tr>
</table>
<p><b>\u539f\u7406\uff1a</b>\u7528\u5e42\u51fd\u6570\u523b\u753b \\(C\\)\u2013\\(Q\\) \u5173\u7cfb\uff08\u7b49\u4ef7\u4e8e \\(\\log(C)\\) \u5bf9 \\(\\log(Q)\\) \u7684\u7ebf\u6027\u56de\u5f52\uff09\uff0c\u518d\u5728\u9ad8\u9891 \\(Q\\) \u5e8f\u5217\u4e0a\u9884\u6d4b \\(C\\)\uff0c\u901a\u8fc7 \\(\\sum CQ\\) \u4f30\u7b97\u8d1f\u8377\u3002</p>
<p><b>\u9002\u7528\uff1a</b>\u6d53\u5ea6\u4e0e\u6d41\u91cf\u5173\u7cfb\u8fd1\u4f3c\u5e42\u5f8b\u4e14\u7a33\u5b9a\u3002<br>
<b>\u4e0d\u9002\u5408\uff1a</b>\u5b58\u5728\u663e\u8457\u5b63\u8282\u6027/\u8d8b\u52bf\u6216\u7ba1\u7406\u63aa\u65bd\u5bfc\u81f4\u5173\u7cfb\u6f02\u79fb\u3002</p>')

  sec_rating_period <- hl("rating_period", '
<h5>\u25b6 Method 10\uff1arating.period \u2014 \u5206\u671f\u8bc4\u7ea7\u66f2\u7ebf</h5>
<p>\u5f62\u5f0f\u540c Method 9\uff0c\u4f46 \\(a,b\\) \u6309\u201c\u6708/\u5e74/\u5b63\u5ea6\u201d\u7b49 period \u5206\u6bb5\u5206\u522b\u62df\u5408\uff0c\u5f97\u5230\u5206\u6bb5\u53c2\u6570 \\(a_p, b_p\\)\u3002</p>
<p><b>\u539f\u7406\uff1a</b>\u901a\u8fc7\u5206\u6bb5\u62df\u5408\u6355\u6349\u5b63\u8282/\u5e74\u5ea6\u5dee\u5f02\uff0c\u51cf\u5c11\u201c\u5355\u4e00\u8bc4\u7ea7\u66f2\u7ebf\u201d\u5728\u975e\u5e73\u7a33\u6761\u4ef6\u4e0b\u7684\u7cfb\u7edf\u504f\u5dee\u3002</p>
<p><b>\u9002\u7528\uff1a</b>\u5b63\u8282\u6027\u5f3a\u6216\u5206\u671f\u5dee\u5f02\u660e\u663e\u3002<br>
<b>\u4e0d\u9002\u5408\uff1a</b>\u5206\u6bb5\u540e\u6837\u672c\u592a\u5c11\u5bfc\u81f4\u56de\u5f52\u4e0d\u7a33\u3001\u5916\u63a8\u98ce\u9669\u589e\u5927\u3002</p>')

  sec_ferguson <- hl("ferguson", '
<h5>\u25b6 Method 11\uff1aferguson \u2014 Ferguson \u4fee\u6b63</h5>
<div style="text-align:center;margin:6px 0;">
  \\[C = aQ^{b},\\qquad L_{cr} = L_{r}\\cdot\\exp\\!\\left(2.651\\,s^{2}\\right)\\]
</div>
<table style="font-size:13px;margin:4px 0 8px 12px;">
  <tr><td>\\(L_r\\)</td><td>\uff1a\u672a\u505a\u56de\u53d8\u6362\u504f\u5dee\u4fee\u6b63\u7684\u8bc4\u7ea7\u66f2\u7ebf\u8d1f\u8377\u4f30\u8ba1</td></tr>
  <tr><td>\\(L_{cr}\\)</td><td>\uff1a\u505a Ferguson \u4fee\u6b63\u540e\u7684\u8d1f\u8377\u4f30\u8ba1</td></tr>
  <tr><td>\\(s\\)</td><td>\uff1a\\(\\log_{10}\\) \u7a7a\u95f4\u56de\u5f52\u7684\u6b8b\u5dee\u6807\u51c6\u5dee</td></tr>
</table>
<p><b>\u539f\u7406\uff1a</b>\u5bf9\u6570\u56de\u5f52\u56de\u5230\u539f\u5c3a\u5ea6\u4f1a\u4ea7\u751f\u7cfb\u7edf\u6027\u4f4e\u4f30\uff08\u5bf9\u6570\u6b63\u6001\u56de\u53d8\u6362\u504f\u5dee\uff09\uff1bFerguson \u4fee\u6b63\u7528 \\(\\exp(2.651 s^2)\\) \u8fdb\u884c\u6821\u6b63\u3002</p>
<p><b>\u9002\u7528\uff1a</b>\u91c7\u7528 \\(\\log_{10}\\) \u56de\u5f52\u62df\u5408\u8bc4\u7ea7\u66f2\u7ebf\u5e76\u9700\u56de\u5230\u539f\u5c3a\u5ea6\u3002<br>
<b>\u4e0d\u9002\u5408\uff1a</b>\u6b8b\u5dee\u5206\u5e03\u4e25\u91cd\u504f\u79bb\u5bf9\u6570\u6b63\u6001\u6216\u6a21\u578b\u8bbe\u5b9a\u660e\u663e\u4e0d\u5f53\u3002</p>')

  sec_ferguson_period <- hl("ferguson_period", '
<h5>\u25b6 Method 12\uff1aferguson.period \u2014 \u5206\u671f Ferguson \u4fee\u6b63</h5>
<p>\u4e0e Method 11 \u76f8\u540c\uff0c\u4f46 \\(a,b,s\\) \u5747\u4e3a\u5206\u6bb5\u91cf \\(a_p, b_p, s_p\\)\uff0c\u76f8\u5e94\u5f97\u5230\u5206\u6bb5\u4fee\u6b63\u8d1f\u8377\u5e76\u6c47\u603b\u4e3a\u5168\u671f \\(L_{cr}\\)\u3002</p>
<p><b>\u539f\u7406\uff1a</b>\u5c06\u201c\u5206\u671f\u8bc4\u7ea7\u66f2\u7ebf\u201d\uff08Method 10\uff09\u4e0e\u201c\u56de\u53d8\u6362\u504f\u5dee\u4fee\u6b63\u201d\uff08Method 11\uff09\u7ed3\u5408\u3002</p>
<p><b>\u9002\u7528\uff1a</b>\u65e2\u5b58\u5728\u660e\u663e\u5206\u671f\u5dee\u5f02\u3001\u53c8\u5fc5\u987b\u8fdb\u884c\u5bf9\u6570\u56de\u5f52\u56de\u53d8\u6362\u3002<br>
<b>\u4e0d\u9002\u5408\uff1a</b>\u5206\u671f\u540e\u6837\u672c\u8fc7\u5c11\u5bfc\u81f4 \\(a_p,b_p,s_p\\) \u4e0d\u7a33\u5b9a\u3002</p>')

  # ================================================================
  # 第二大类：LOADEST Model 1–9
  # ================================================================
  sec_loadest_header <- '
<h4 style="color:#198754;border-bottom:2px solid #198754;padding-bottom:4px;">
  2. LOADEST / rloadest \u56de\u5f52\u8d1f\u8377\u6a21\u578b
</h4>'

  sec_loadest_intro <- '
<div style="padding:4px 14px;">
<h5>2.1 \u77ac\u65f6\u8d1f\u8377\u5b9a\u4e49\u4e0e\u79bb\u6563\u7d2f\u8ba1</h5>
<div style="text-align:center;margin:6px 0;">
  \\[L_\\tau = \\int_0^\\tau Q(t)C(t)\\,dt,\\qquad
    \\widehat{L}_\\tau = \\sum_{i=1}^{N_P} \\widehat{L}_i \\Delta t\\]
</div>

<h5>2.2 \u5bf9\u6570\u7ebf\u6027\u56de\u5f52</h5>
<div style="text-align:center;margin:6px 0;">
  \\[\\ln(\\widehat{L}) = a_0 + \\sum_{j=1}^{N_V} a_j X_j\\]
</div>

<h5>2.3 \u4ece\u6307\u6570\u53d8\u6362\u5f97\u5230\u8bc4\u7ea7\u66f2\u7ebf\u8d1f\u8377</h5>
<div style="text-align:center;margin:6px 0;">
  \\[\\widehat{L}_{RC} = \\exp\\!\\left(a_0 + \\sum_{j=1}^{N_V} a_j X_j\\right)\\]
</div>

<h5>2.4 LOADEST \u4e5d\u5f0f\uff08\u5b8c\u6574\u5f62\u5f0f\uff09</h5>
<div style="text-align:center;margin:6px 0;font-size:14px;">
  \\[\\ln(\\widehat{L}) = a_0 + a_1\\ln Q + a_2(\\ln Q)^2 + a_3 T + a_4 T^2 + a_5\\sin(2\\pi T) + a_6\\cos(2\\pi T)\\]
</div>
<p style="font-size:12px;color:#555;">Model 1\u20139 \u4e3a\u4e0a\u8ff0\u5404\u9879\u7684\u5b50\u96c6\uff0c\u914d\u5408 Duan smearing \u504f\u5dee\u4fee\u6b63\u3002
\u5176\u5047\u8bbe\u6b8b\u5dee\u670d\u4ece\u6b63\u6001\u5206\u5e03\u4e14\u9075\u5faa\u540c\u65b9\u5dee\u6027\u3002</p>
</div>'

  # 9 个 LOADEST 子模型
  loadest_formulas <- c(
    "1" = "\\ln(L) = a_0 + a_1\\ln Q",
    "2" = "\\ln(L) = a_0 + a_1\\ln Q + a_2(\\ln Q)^2",
    "3" = "\\ln(L) = a_0 + a_1\\ln Q + a_2\\,dtime",
    "4" = "\\ln(L) = a_0 + a_1\\ln Q + a_2\\sin(2\\pi\\,dtime) + a_3\\cos(2\\pi\\,dtime)",
    "5" = "\\ln(L) = a_0 + a_1\\ln Q + a_2(\\ln Q)^2 + a_3\\,dtime",
    "6" = "\\ln(L) = a_0 + a_1\\ln Q + a_2(\\ln Q)^2 + a_3\\sin(2\\pi\\,dtime) + a_4\\cos(2\\pi\\,dtime)",
    "7" = "\\ln(L) = a_0 + a_1\\ln Q + a_2\\sin(2\\pi\\,dtime) + a_3\\cos(2\\pi\\,dtime) + a_4\\,dtime",
    "8" = "\\ln(L) = a_0 + a_1\\ln Q + a_2(\\ln Q)^2 + a_3\\,dtime + a_4\\,dtime^2",
    "9" = "\\ln(L) = a_0 + a_1\\ln Q + a_2(\\ln Q)^2 + a_3\\sin(2\\pi\\,dtime) + a_4\\cos(2\\pi\\,dtime) + a_5\\,dtime + a_6\\,dtime^2"
  )

  sec_loadest_models <- vapply(seq_along(loadest_formulas), function(i) {
    num <- names(loadest_formulas)[i]
    fml <- loadest_formulas[i]
    key_name <- paste0("loadest_", num)
    desc <- switch(num,
                   "1" = "\u4ec5\u6d41\u91cf",
                   "2" = "\u6d41\u91cf + \u6d41\u91cf\u5e73\u65b9",
                   "3" = "\u6d41\u91cf + \u65f6\u95f4\u8d8b\u52bf",
                   "4" = "\u6d41\u91cf + \u5b63\u8282\u9879",
                   "5" = "\u6d41\u91cf + \u6d41\u91cf\u5e73\u65b9 + \u65f6\u95f4",
                   "6" = "\u6d41\u91cf + \u6d41\u91cf\u5e73\u65b9 + \u5b63\u8282",
                   "7" = "\u6d41\u91cf + \u5b63\u8282 + \u65f6\u95f4",
                   "8" = "\u6d41\u91cf + \u6d41\u91cf\u5e73\u65b9 + \u65f6\u95f4 + \u65f6\u95f4\u5e73\u65b9",
                   "9" = "\u5b8c\u6574\u6a21\u578b\uff08\u5168\u90e8\u53d8\u91cf\uff09")
    hl(key_name, paste0(
      '<h5>\u25b6 LOADEST Model ', num, '\uff1a', desc, '</h5>',
      '<div style="text-align:center;margin:4px 0;">\\[', fml, '\\]</div>'))
  }, character(1))

  sec_loadest_vars <- '
<div style="padding:4px 14px;font-size:13px;">
<h5>\u53d8\u91cf\u4e0e\u7cfb\u6570\u8bf4\u660e</h5>
<table style="margin-left:12px;">
  <tr><td>\\(L\\)</td><td>\uff1a\u8d1f\u8377</td></tr>
  <tr><td>\\(Q\\)</td><td>\uff1a\u6d41\u91cf\uff08m\u00b3/s\uff09</td></tr>
  <tr><td>\\(\\ln Q,\\;(\\ln Q)^2\\)</td><td>\uff1a\u5bf9\u6570\u6d41\u91cf\u53ca\u5176\u4e8c\u6b21\u9879</td></tr>
  <tr><td>\\(dtime,\\;dtime^2\\)</td><td>\uff1a\u65f6\u95f4\u53d8\u91cf\u53ca\u5176\u4e8c\u6b21\u9879</td></tr>
  <tr><td>\\(\\sin(2\\pi\\,dtime),\\;\\cos(2\\pi\\,dtime)\\)</td><td>\uff1a\u5b63\u8282\u6027\u6b63\u4f59\u5f26\u9879</td></tr>
  <tr><td>\\(a_0 \\sim a_6\\)</td><td>\uff1a\u56de\u5f52\u7cfb\u6570</td></tr>
</table>
<p style="margin-top:8px;"><b>\u9002\u5408\uff1a</b>\u6709\u8fde\u7eed\u6d41\u91cf\u5e8f\u5217\u548c\u7a00\u758f\u6d53\u5ea6\u6837\u672c\uff1b\u8d1f\u8377\u4e0e\u6d41\u91cf/\u65f6\u95f4/\u5b63\u8282\u7684\u5173\u7cfb\u53ef\u7528\u8fd9\u4e9b\u57fa\u51fd\u6570\u8f83\u597d\u523b\u753b\u3002<br>
<b>\u98ce\u9669\u70b9\uff1a</b>\u6a21\u578b\u8bbe\u5b9a\u4e0d\u5f53\u4f1a\u5bfc\u81f4\u7cfb\u7edf\u6027\u504f\u5dee\uff1b\u9700\u505a\u6b8b\u5dee\u8bca\u65ad\u4e0e\u6a21\u578b\u9009\u62e9\u3002</p>
</div>'

  # ================================================================
  # 第三大类：loadflex
  # ================================================================
  sec_loadflex_header <- '
<h4 style="color:#6f42c1;border-bottom:2px solid #6f42c1;padding-bottom:4px;">
  3. loadflex \u7ebf\u6027/\u5b63\u8282\u56de\u5f52
</h4>'

  sec_loadLm_simple <- hl("loadLm_simple", '
<h5>\u25b6 loadLm_simple \u2014 \u7ebf\u6027\u56de\u5f52</h5>
<div style="text-align:center;margin:6px 0;">
  \\[\\ln C = a_0 + a_1\\ln Q\\]
</div>
<p>\u6700\u7b80\u5355\u7684\u5bf9\u6570\u7ebf\u6027\u6a21\u578b\uff0c\u4ec5\u8003\u8651\u6d41\u91cf\u4e0e\u6d53\u5ea6\u7684\u5bf9\u6570\u5173\u7cfb\u3002</p>')

  sec_loadLm_season <- hl("loadLm_season", '
<h5>\u25b6 loadLm_season \u2014 \u5b63\u8282\u56de\u5f52</h5>
<div style="text-align:center;margin:6px 0;">
  \\[\\ln C = a_0 + a_1\\ln Q + a_2\\sin\\!\\left(\\frac{2\\pi\\,\\text{doy}}{365}\\right) + a_3\\cos\\!\\left(\\frac{2\\pi\\,\\text{doy}}{365}\\right)\\]
</div>
<p>\u5728\u7ebf\u6027\u6a21\u578b\u57fa\u7840\u4e0a\u52a0\u5165\u5b63\u8282\u6027\u6b63\u4f59\u5f26\u9879\uff0c\u6355\u6349\u5e74\u5468\u671f\u5185\u7684\u6d53\u5ea6\u6ce2\u52a8\u3002</p>')

  sec_loadflex_note <- '
<div style="padding:4px 14px;font-size:13px;">
<p>\u5148\u5c1d\u8bd5\u8c03\u7528 <code>loadflex</code> \u5305\uff0c\u4e0d\u53ef\u7528\u5219\u81ea\u52a8\u56de\u9000\u5230 base R <code>lm()</code> + Duan smearing \u5146\u5e95\u3002</p>
</div>'

  # ================================================================
  # 第四大类：EGRET WRTDS
  # ================================================================
  sec_wrtds_header <- '
<h4 style="color:#dc3545;border-bottom:2px solid #dc3545;padding-bottom:4px;">
  4. EGRET WRTDS\uff08Weighted Regressions on Time, Discharge, and Season\uff09
</h4>'

  sec_wrtds <- hl("wrtds", '
<h5>\u25b6 4.1 \u57fa\u672c\u56de\u5f52\u5f62\u5f0f</h5>
<div style="text-align:center;margin:6px 0;">
  \\[\\ln(C_i)=\\beta_0+\\beta_1 T_i+\\beta_2\\ln(Q_i)+\\beta_3\\sin(2\\pi T_i)+\\beta_4\\cos(2\\pi T_i)+\\varepsilon_i\\]
</div>
<table style="font-size:13px;margin:4px 0 8px 12px;">
  <tr><td>\\(C_i\\)</td><td>\uff1a\u7b2c \\(i\\) \u4e2a\u91c7\u6837\u65f6\u523b\u7684\u6d53\u5ea6\uff08mg/L\uff09</td></tr>
  <tr><td>\\(Q_i\\)</td><td>\uff1a\u7b2c \\(i\\) \u4e2a\u91c7\u6837\u65f6\u523b\u7684\u6d41\u91cf\uff08m\u00b3/s\uff09</td></tr>
  <tr><td>\\(T_i\\)</td><td>\uff1a\u5341\u8fdb\u5236\u65f6\u95f4</td></tr>
  <tr><td>\\(\\beta_0 \\sim \\beta_4\\)</td><td>\uff1a\u8be5\u8282\u70b9\u4e0b\u5c40\u90e8\u56de\u5f52\u5f97\u5230\u7684\u7cfb\u6570</td></tr>
  <tr><td>\\(\\varepsilon_i\\)</td><td>\uff1a\u6b8b\u5dee\uff08\u8fd1\u4f3c\u6b63\u6001\u3001\u5747\u503c 0\uff09</td></tr>
</table>

<h5>\u25b6 4.2 \u4e09\u7ef4\u6743\u91cd\uff08Time\u2013Discharge\u2013Season \u4e58\u79ef\u6743\u91cd\uff09</h5>
<div style="text-align:center;margin:6px 0;">
  \\[w_i = w_{T,i}\\cdot w_{Q,i}\\cdot w_{S,i}\\]
</div>
<p>\u8ddd\u79bb\u5b9a\u4e49\uff1a</p>
<div style="text-align:center;margin:4px 0;">
  \\[d_{T,i}=|T_i - T_0|,\\quad d_{Q,i}=|\\ln Q_i - \\ln Q_0|,\\quad d_{S,i}=\\text{seasonDist}(T_i, T_0)\\]
</div>
<p>Tricube \u6743\u91cd\u51fd\u6570\uff1a</p>
<div style="text-align:center;margin:4px 0;">
  \\[w(d;h) = \\begin{cases} \\left(1-\\left|\\frac{d}{h}\\right|^3\\right)^3, & |d|\\le h \\\\ 0, & |d|>h \\end{cases}\\]
</div>
<table style="font-size:13px;margin:4px 0 8px 12px;">
  <tr><td>\\(d\\)</td><td>\uff1a\u67d0\u4e00\u7ef4\u5ea6\u4e0a\u7684\u8ddd\u79bb</td></tr>
  <tr><td>\\(h\\)</td><td>\uff1a\u8be5\u7ef4\u5ea6\u7684\u7a97\u53e3\u534a\u5bbd\uff08bandwidth\uff09</td></tr>
</table>

<h5>\u25b6 4.3 \u5bf9\u6570\u6b63\u6001\u7ea0\u504f\uff08BCF\uff09</h5>
<div style="text-align:center;margin:6px 0;">
  \\[\\text{BCF}=\\exp\\!\\left(\\frac{SE^2}{2}\\right),\\qquad \\widehat{C}=\\exp(\\hat{y})\\cdot\\text{BCF}\\]
</div>
<table style="font-size:13px;margin:4px 0 8px 12px;">
  <tr><td>\\(SE\\)</td><td>\uff1a\u5bf9\u6570\u7a7a\u95f4\u6b8b\u5dee\u6807\u51c6\u5dee</td></tr>
  <tr><td>\\(\\hat{y}\\)</td><td>\uff1a\\(\\ln(C)\\) \u7684\u62df\u5408\u503c</td></tr>
  <tr><td>\\(\\widehat{C}\\)</td><td>\uff1a\u504f\u5dee\u4fee\u6b63\u540e\u7684\u6d53\u5ea6\u4f30\u8ba1\u503c</td></tr>
</table>

<p style="margin-top:8px;"><b>\u9002\u5408\uff1a</b>\u957f\u671f\u76d1\u6d4b\u3001\u5173\u7cfb\u968f\u65f6\u95f4/\u5b63\u8282\u7f13\u6162\u53d8\u5316\u3001\u91c7\u6837\u65f6\u95f4\u8de8\u5ea6\u8f83\u957f\u3002<br>
<b>\u4e0d\u9002\u5408\uff1a</b>\u6781\u5f3a\u4e8b\u4ef6\u4e3b\u5bfc\u4e14\u91c7\u6837\u65e0\u6cd5\u8986\u76d6\u4e8b\u4ef6\uff1b\u6d53\u5ea6\u4e0e\u6d41\u91cf\u4e4b\u95f4\u5b58\u5728\u9891\u7e41\u4e14\u5267\u70c8\u7684\u7ed3\u6784\u6027\u7a81\u53d8\u3002</p>')

  # ================================================================
  # 拼接
  # ================================================================
  paste0(
    '<div style="font-size:13.5px;line-height:1.7;">',
    sec_riverload_header,
    sec_rating, sec_rating_period, sec_ferguson, sec_ferguson_period,
    sec_loadest_header,
    sec_loadest_intro,
    paste(sec_loadest_models, collapse = "\n"),
    sec_loadest_vars,
    sec_loadflex_header,
    sec_loadLm_simple, sec_loadLm_season, sec_loadflex_note,
    sec_wrtds_header,
    sec_wrtds,
    '</div>')
}




# =============================================================
# ★ 构建分组下拉选项（optgroup）
# =============================================================
fw_reg_grouped_model_choices <- function() {
  list(
    "RiverLoad (\u8bc4\u7ea7\u66f2\u7ebf, Method 9-12)" = c(
      "\u8bc4\u7ea7\u66f2\u7ebf (Method 9: rating)"                  = "rating",
      "\u5206\u671f\u8bc4\u7ea7\u66f2\u7ebf (Method 10: rating.period)" = "rating_period",
      "Ferguson\u4fee\u6b63 (Method 11: ferguson)"                    = "ferguson",
      "\u5206\u671fFerguson\u4fee\u6b63 (Method 12: ferguson.period)" = "ferguson_period"
    ),
    "LOADEST (\u56de\u5f52\u8d1f\u8377, Model 1-9)" = c(
      "LOADEST-1: lnQ"                       = "loadest_1",
      "LOADEST-2: lnQ + lnQ\u00b2"           = "loadest_2",
      "LOADEST-3: lnQ + T"                    = "loadest_3",
      "LOADEST-4: lnQ + sin/cos"              = "loadest_4",
      "LOADEST-5: lnQ + lnQ\u00b2 + T"       = "loadest_5",
      "LOADEST-6: lnQ + lnQ\u00b2 + sin/cos" = "loadest_6",
      "LOADEST-7: lnQ + sin/cos + T"          = "loadest_7",
      "LOADEST-8: lnQ + lnQ\u00b2 + T + T\u00b2" = "loadest_8",
      "LOADEST-9: \u5b8c\u6574\u6a21\u578b (full)" = "loadest_9"
    ),
    "loadflex (\u7ebf\u6027/\u5b63\u8282\u56de\u5f52)" = c(
      "loadflex \u7ebf\u6027\u56de\u5f52 (loadLm_simple)" = "loadLm_simple",
      "loadflex \u5b63\u8282\u56de\u5f52 (loadLm_season)" = "loadLm_season"
    ),
    "EGRET WRTDS (\u5c40\u90e8\u52a0\u6743\u56de\u5f52)" = c(
      "WRTDS \u52a0\u6743\u56de\u5f52 (EGRET)" = "wrtds"
    )
  )
}


# =============================================================
# UI：左侧参数控件
# ★ 核心改动：单个分组下拉替代原来的"类别+模型"两级
# =============================================================
fw_regression_left_extra_ui <- function(ns, key = "regression") {

  # ★ 分组下拉：一次性展示全部 16 种回归子方法
  grouped_choices <- list(
    "RiverLoad (\u8bc4\u7ea7\u66f2\u7ebf, Method 9-12)" = c(
      "\u8bc4\u7ea7\u66f2\u7ebf (Method 9: rating)"                  = "rating",
      "\u5206\u671f\u8bc4\u7ea7\u66f2\u7ebf (Method 10)"             = "rating_period",
      "Ferguson\u4fee\u6b63 (Method 11)"                              = "ferguson",
      "\u5206\u671fFerguson\u4fee\u6b63 (Method 12)"                  = "ferguson_period"
    ),
    "LOADEST (\u56de\u5f52\u8d1f\u8377, Model 1-9)" = c(
      "LOADEST-1"                       = "loadest_1",
      "LOADEST-2"           = "loadest_2",
      "LOADEST-3"                    = "loadest_3",
      "LOADEST-4"              = "loadest_4",
      "LOADEST-5"       = "loadest_5",
      "LOADEST-6" = "loadest_6",
      "LOADEST-7"          = "loadest_7",
      "LOADEST-8" = "loadest_8",
      "LOADEST-9"  = "loadest_9"
    ),
    "loadflex (\u7ebf\u6027/\u5b63\u8282\u56de\u5f52)" = c(
      "loadflex \u7ebf\u6027\u56de\u5f52 (loadLm_simple)" = "loadLm_simple",
      "loadflex \u5b63\u8282\u56de\u5f52 (loadLm_season)" = "loadLm_season"
    ),
    "EGRET WRTDS (\u5c40\u90e8\u52a0\u6743\u56de\u5f52)" = c(
      "WRTDS \u52a0\u6743\u56de\u5f52 (EGRET)" = "wrtds"
    )
  )

  shiny::tagList(
    shiny::helpText("\u56de\u5f52\u65b9\u6cd5\u56fa\u5b9a\u8c03\u7528\u7b2c\u4e00\u6b65\u5904\u7406\u540e\u7684 QF/WQ \u6570\u636e"),

    # ---- 数据表选择 ----
    shiny::selectInput(ns(paste0("qf_sheet_", key)), "QF\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("wq_sheet_", key)), "WQ\u6570\u636e\u8868", choices = NULL),
    shiny::selectInput(ns(paste0("constituent_", key)), "\u6c34\u8d28\u6307\u6807(j)", choices = NULL),

    shiny::tags$hr(),

    shiny::tags$div(
      style = "max-height:600px; overflow-y:auto; border:1px solid #ddd; border-radius:6px; padding:12px; margin-top:8px;",
      shiny::tags$h4(shiny::icon("book"), " \u56de\u5f52\u65b9\u6cd5\u539f\u7406\u4ecb\u7ecd"),
      shiny::uiOutput(ns("regression_principle_panel"))
    ),




    # ★★★ 单个分组下拉：回归子方法 ★★★
    shiny::selectInput(
      ns(paste0("reg_model_", key)),
      shiny::tagList(shiny::icon("cogs"), " \u56de\u5f52\u5b50\u65b9\u6cd5"),
      choices  = grouped_choices,
      selected = "loadLm_season",
      width    = "100%"
    ),

    # ---- 分期方式（rating_period / ferguson_period）----
    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'rating_period' || input['%s'] == 'ferguson_period'",
        ns(paste0("reg_model_", key)), ns(paste0("reg_model_", key))),
      shiny::selectInput(
        ns(paste0("period_col_", key)), "\u5206\u671f\u65b9\u5f0f",
        choices = c("\u6309\u6708" = "month", "\u6309\u5e74" = "year",
                    "\u6309\u5b63\u5ea6" = "quarter"),
        selected = "month")
    ),

    # ---- WRTDS 窗口参数 ----
    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'wrtds'", ns(paste0("reg_model_", key))),
      shiny::tags$div(
        style = "background:#f8f9fa; border-radius:4px; padding:8px; margin-top:4px;",
        shiny::tags$small(shiny::tags$b("WRTDS \u7a97\u53e3\u53c2\u6570")),
        shiny::numericInput(ns(paste0("wrtds_hT_", key)), "h_T (\u5e74)",
                            value = 10, min = 1, max = 30, step = 1),
        shiny::numericInput(ns(paste0("wrtds_hQ_", key)), "h_Q (ln)",
                            value = 2, min = 0.5, max = 5, step = 0.5),
        shiny::numericInput(ns(paste0("wrtds_hS_", key)), "h_S (\u5e74)",
                            value = 0.5, min = 0.1, max = 1, step = 0.1))
    ),

    # ---- 动态提示 ----
    shiny::conditionalPanel(
      condition = sprintf(
        "['rating','rating_period','ferguson','ferguson_period'].indexOf(input['%s']) >= 0",
        ns(paste0("reg_model_", key))),
      shiny::tags$div(
        style = "background:#d1ecf1; border-radius:4px; padding:6px; margin-top:4px; font-size:11px;",
        shiny::icon("info-circle"),
        " RiverLoad \u8bc4\u7ea7\u66f2\u7ebf\u3002Ferguson \u4fee\u6b63: exp(2.651\u00b7s\u00b2)")
    ),

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] && input['%s'].indexOf('loadest_') === 0",
        ns(paste0("reg_model_", key)), ns(paste0("reg_model_", key))),
      shiny::tags$div(
        style = "background:#fff3cd; border-radius:4px; padding:6px; margin-top:4px; font-size:11px;",
        shiny::icon("info-circle"),
        " LOADEST: Duan smearing \u504f\u5dee\u4fee\u6b63\u3002\u82e5\u6709 rloadest \u5305\u81ea\u52a8\u8c03\u7528\u3002")
    ),

    shiny::conditionalPanel(
      condition = sprintf(
        "input['%s'] == 'loadLm_simple' || input['%s'] == 'loadLm_season'",
        ns(paste0("reg_model_", key)), ns(paste0("reg_model_", key))),
      shiny::tags$div(
        style = "background:#d4edda; border-radius:4px; padding:6px; margin-top:4px; font-size:11px;",
        shiny::icon("info-circle"),
        " \u7ebf\u6027: ln(C)~ln(Q)\uff1b\u5b63\u8282: +sin/cos\u3002\u5148\u8bd5 loadflex \u5305\uff0c\u4e0d\u53ef\u7528\u5219 base R\u3002")
    ),

    shiny::conditionalPanel(
      condition = sprintf("input['%s'] == 'wrtds'", ns(paste0("reg_model_", key))),
      shiny::tags$div(
        style = "background:#e2d5f1; border-radius:4px; padding:6px; margin-top:4px; font-size:11px;",
        shiny::icon("info-circle"),
        " WRTDS \u5c40\u90e8\u52a0\u6743\u56de\u5f52\uff0c\u8ba1\u7b97\u8f83\u6162\u3002")
    )
  )
}


# =============================================================
# Server：step1_qf_wq reactive
# =============================================================
fw_regression_step1_reactive <- function(rv) {
  shiny::reactive({ fw_get_step1_qf_wq(rv) })
}


# =============================================================
# Server：联动 observers
# ★ 已移除 reg_category 联动，改为直接使用 reg_model 的值
# =============================================================
# =============================================================
# Server：联动 observers + ★ 方法原理高亮渲染
# =============================================================
fw_regression_init_observers <- function(input, output, session, step1_qf_wq, key = "regression") {

  # ---- QF / WQ 表名同步 ----
  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, paste0("qf_sheet_", key), choices = character(0))
      shiny::updateSelectInput(session, paste0("wq_sheet_", key), choices = character(0))
      return()
    }

    qf_list <- fw_as_named_table_list(s1$QF, "QF")
    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(qf_list) == 0 || length(wq_list) == 0) {
      shiny::updateSelectInput(session, paste0("qf_sheet_", key), choices = character(0))
      shiny::updateSelectInput(session, paste0("wq_sheet_", key), choices = character(0))
      return()
    }

    qf_nm <- names(qf_list); wq_nm <- names(wq_list)
    cur_qf <- shiny::isolate(input[[paste0("qf_sheet_", key)]])
    cur_wq <- shiny::isolate(input[[paste0("wq_sheet_", key)]])
    if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
    if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

    shiny::updateSelectInput(session, paste0("qf_sheet_", key), choices = qf_nm, selected = cur_qf)
    shiny::updateSelectInput(session, paste0("wq_sheet_", key), choices = wq_nm, selected = cur_wq)
  })

  # ---- 水质指标联动 ----
  shiny::observe({
    s1 <- step1_qf_wq()
    if (is.null(s1)) {
      shiny::updateSelectInput(session, paste0("constituent_", key), choices = character(0))
      return()
    }

    wq_list <- fw_as_named_table_list(s1$WQ, "WQ")
    if (length(wq_list) == 0) {
      shiny::updateSelectInput(session, paste0("constituent_", key), choices = character(0))
      return()
    }

    ws <- input[[paste0("wq_sheet_", key)]]
    if (is.null(ws) || !(ws %in% names(wq_list))) ws <- names(wq_list)[1]
    cands <- fw_get_wq_constituents(wq_list[[ws]])
    if (length(cands) == 0) cands <- names(wq_list[[ws]])
    cur <- shiny::isolate(input[[paste0("constituent_", key)]])
    if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

    shiny::updateSelectInput(session, paste0("constituent_", key), choices = cands, selected = cur)
  })

  # ★★★ 方法原理面板：选中子方法时高亮对应段落 ★★★
  output$regression_principle_panel <- shiny::renderUI({
    active_model <- input[[paste0("reg_model_", key)]]
    html_text <- fw_regression_principle_text(active_sub = active_model)
    shiny::withMathJax(shiny::HTML(html_text))
  })
}



# =============================================================
# Server：数据获取
# =============================================================
fw_regression_get_daily_all <- function(input, step1_qf_wq) {
  s1 <- step1_qf_wq()
  if (is.null(s1)) return(NULL)

  prep <- tryCatch(
    fw_prepare_regression_input(
      step1_data = s1, qf_sheet = input$qf_sheet_regression,
      wq_sheet = input$wq_sheet_regression, constituent = input$constituent_regression,
      date_range = NULL),
    error = function(e) NULL)
  if (is.null(prep)) return(NULL)

  data.frame(station = prep$station, WYBM = prep$wybm %||% NA_character_,
             date = prep$dat$TM, Q = prep$dat$Q, C_obs = prep$dat$conc,
             stringsAsFactors = FALSE)
}


# =============================================================
# Server：计算调度
# =============================================================
fw_regression_run_calc <- function(input, step1_qf_wq, key = "regression") {
  s1 <- step1_qf_wq()
  if (is.null(s1)) {
    shiny::showNotification("\u672a\u627e\u5230\u7b2c\u4e00\u6b65 QF/WQ \u6570\u636e\u3002", type = "error")
    return(NULL)
  }

  model_choice <- input[[paste0("reg_model_", key)]]  %||% "loadLm_season"
  period_col   <- input[[paste0("period_col_", key)]]  %||% "month"
  wrtds_hT     <- input[[paste0("wrtds_hT_", key)]]   %||% 10
  wrtds_hQ     <- input[[paste0("wrtds_hQ_", key)]]   %||% 2
  wrtds_hS     <- input[[paste0("wrtds_hS_", key)]]   %||% 0.5

  res <- tryCatch(
    fw_run_flux_regression(
      step1_data   = s1,
      qf_sheet     = input[[paste0("qf_sheet_", key)]],
      wq_sheet     = input[[paste0("wq_sheet_", key)]],
      constituent  = input[[paste0("constituent_", key)]],
      date_range   = input[[paste0("daterange_", key)]],
      model_choice = model_choice,
      period_col   = period_col,
      wrtds_h_T    = wrtds_hT,
      wrtds_h_Q    = wrtds_hQ,
      wrtds_h_S    = wrtds_hS),
    error = function(e) {
      shiny::showNotification(e$message, type = "error")
      NULL
    })
  res
}


# =============================================================
# Server：结算文本额外信息
# =============================================================
fw_regression_settle_extra <- function(res) {
  if (is.null(res)) return("")

  cat_labels <- c(riverload = "RiverLoad", loadest = "LOADEST",
                  loadflex = "loadflex", wrtds = "EGRET WRTDS")
  cat_key <- res$params$model_category %||% ""
  cat_txt <- if (cat_key %in% names(cat_labels)) cat_labels[[cat_key]] else cat_key

  paste0(
    "\nQF\u8868: ",     res$params$qf_sheet   %||% "",
    "\nWQ\u8868: ",     res$params$wq_sheet   %||% "",
    "\n\u6307\u6807: ", res$params$constituent %||% "",
    "\n\u65b9\u6cd5\u7c7b\u522b: ", cat_txt,
    "\n\u56de\u5f52\u5b50\u65b9\u6cd5: ", fw_reg_model_label(res$params$model_choice %||% ""),
    "\n\u540e\u7aef: ", res$params$backend %||% "",
    if (!is.null(res$params$period_col) &&
        (res$params$model_choice %in% c("rating_period", "ferguson_period")))
      paste0("\n\u5206\u671f: ", res$params$period_col) else "")
}

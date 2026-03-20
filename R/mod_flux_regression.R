# =====================================================================
# mod_flux_regression.R
# 回归法 —— 专属 UI 参数 + Server 初始化 + 数据获取 + 计算调度
# =====================================================================

if (!exists("%||%", mode = "function")) {
  `%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
}


# ★ 替换：原 composite 原理文本改为回归法原理
# active_sub 参数保留以兼容调用处，但回归法不使用高亮分支
fw_regression_principle_text <- function(active_sub = NULL) {
  paste0(

    # ================================================================
    # 1. RiverLoad
    # ================================================================
    "<h4>1. RiverLoad \u8d1f\u8377\u4f30\u7b97\u65b9\u6cd5\uff08Method 9&ndash;12\uff09</h4>",

    # ---- Method 9 ----
    "<h5>Method 9\uff1arating &mdash; \u5e42\u51fd\u6570/\u5bf9\u6570\u56de\u5f52\u8bc4\u7ea7\u66f2\u7ebf</h5>",
    "\\[C = aQ^{b},\\qquad L_{r}=\\sum_{i=1}^{n} C_i Q_i\\]",
    "<ul>",
    "<li>\\(C\\)\uff1a\u6d53\u5ea6</li>",
    "<li>\\(Q\\)\uff1a\u6d41\u91cf</li>",
    "<li>\\(a,b\\)\uff1a\u8bc4\u7ea7\u66f2\u7ebf\u53c2\u6570\uff08\u901a\u8fc7\u56de\u5f52\u62df\u5408\u5f97\u5230\uff09</li>",
    "<li>\\(L_r\\)\uff1a\u7531\u8bc4\u7ea7\u66f2\u7ebf\u601d\u8def\u5f97\u5230\u7684\u8d1f\u8377\u4f30\u8ba1\uff08\u672a\u505a\u56de\u53d8\u6362\u504f\u5dee\u4fee\u6b63\uff09</li>",
    "<li>\\(n\\)\uff1a\u65f6\u95f4\u6b65/\u6837\u672c\u70b9\u6570\u91cf</li>",
    "<li>\\(C_i, Q_i\\)\uff1a\u7b2c \\(i\\) \u4e2a\u65f6\u95f4\u6b65\u7684\u6d53\u5ea6\u4e0e\u6d41\u91cf</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u7528\u5e42\u51fd\u6570\u523a\u5212 \\(C\\)&ndash;\\(Q\\) \u5173\u7cfb\uff08\u7b49\u4ef7\u4e8e \\(\\log C\\) \u5bf9 \\(\\log Q\\) \u7684\u7ebf\u6027\u56de\u5f52\uff09\uff0c",
    "\u518d\u5728\u9ad8\u9891 \\(Q\\) \u5e8f\u5217\u4e0a\u9884\u6d4b \\(C\\)\uff0c\u901a\u8fc7 \\(\\sum C_i Q_i\\) \u4f30\u7b97\u8d1f\u8377\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u6d53\u5ea6\u4e0e\u6d41\u91cf\u5173\u7cfb\u8fd1\u4f3c\u5e42\u5f8b\u4e14\u7a33\u5b9a\uff1b\u4e0d\u9002\u5408\u5b58\u5728\u663e\u8457\u5b63\u8282\u6027/\u8d8b\u52bf\u6216\u7ba1\u7406\u63aa\u65bd\u5bfc\u81f4\u5173\u7cfb\u6f02\u79fb\u3002</p>",

    # ---- Method 10 ----
    "<h5>Method 10\uff1arating.period &mdash; \u5206\u671f\u8bc4\u7ea7\u66f2\u7ebf</h5>",
    "<p>\u5f62\u5f0f\u540c Method 9\uff0c\u4f46 \\(a,b\\) \u6309&ldquo;\u6708/\u5e74&rdquo;\u7b49 period \u5206\u6bb5\u5206\u522b\u62df\u5408\uff1a",
    "\\(a_p, b_p\\)\uff08\\(p\\) \u8868\u793a\u67d0\u4e2a period\uff0c\u5982\u67d0\u6708\u6216\u67d0\u5e74\uff09\u3002</p>",
    "<p><b>\u539f\u7406\uff1a</b>\u901a\u8fc7\u5206\u6bb5\u62df\u5408\u6355\u6349\u5b63\u8282/\u5e74\u5ea6\u5dee\u5f02\uff0c\u51cf\u5c11&ldquo;\u5355\u4e00\u8bc4\u7ea7\u66f2\u7ebf&rdquo;\u5728\u975e\u5e73\u7a33\u6761\u4ef6\u4e0b\u7684\u7cfb\u7edf\u504f\u5dee\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u5b63\u8282\u6027\u5f3a\u6216\u5206\u671f\u5dee\u5f02\u660e\u663e\uff1b\u4e0d\u9002\u5408\u5206\u6bb5\u540e\u6837\u672c\u592a\u5c11\u5bfc\u81f4\u56de\u5f52\u4e0d\u7a33\u3001\u5916\u63a8\u98ce\u9669\u589e\u5927\u3002</p>",

    # ---- Method 11 ----
    "<h5>Method 11\uff1aferguson &mdash; Ferguson \u4fee\u6b63</h5>",
    "\\[C=aQ^{b},\\qquad L_{cr}=L_{r}\\cdot\\exp\\!\\left(2.651\\,s^{2}\\right)\\]",
    "<ul>",
    "<li>\\(C,Q,a,b\\)\uff1a\u540c Method 9</li>",
    "<li>\\(L_r\\)\uff1a\u672a\u505a\u56de\u53d8\u6362\u504f\u5dee\u4fee\u6b63\u7684\u8bc4\u7ea7\u66f2\u7ebf\u8d1f\u8377\u4f30\u8ba1</li>",
    "<li>\\(L_{cr}\\)\uff1a\u505a Ferguson \u4fee\u6b63\u540e\u7684\u8d1f\u8377\u4f30\u8ba1</li>",
    "<li>\\(s\\)\uff1a\u5728 \\(\\log_{10}\\) \u7a7a\u95f4\u56de\u5f52\u7684\u6807\u51c6\u8bef\u5dee\uff08\u6b8b\u5dee\u6807\u51c6\u5dee\uff09</li>",
    "</ul>",
    "<p><b>\u539f\u7406\uff1a</b>\u5bf9\u6570\u56de\u5f52\u56de\u5230\u539f\u5c3a\u5ea6\u4f1a\u4ea7\u751f\u7cfb\u7edf\u6027\u4f4e\u4f30\uff08\u5bf9\u6570\u6b63\u6001\u56de\u53d8\u6362\u504f\u5dee\uff09\uff1b",
    "Ferguson \u4fee\u6b63\u7528 \\(\\exp(2.651s^2)\\) \u5bf9\u8be5\u504f\u5dee\u8fdb\u884c\u6821\u6b63\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u91c7\u7528 \\(\\log_{10}\\) \u56de\u5f52\u62df\u5408\u8bc4\u7ea7\u66f2\u7ebf\u5e76\u9700\u56de\u5230\u539f\u5c3a\u5ea6\uff1b\u4e0d\u9002\u5408\u6b8b\u5dee\u5206\u5e03\u4e25\u91cd\u6b63\u6001\u6216\u6a21\u578b\u8bbe\u5b9a\u660e\u663e\u4e0d\u5f53\u3002</p>",

    # ---- Method 12 ----
    "<h5>Method 12\uff1aferguson.period &mdash; \u5206\u671f Ferguson \u4fee\u6b63</h5>",
    "<p>\u5f62\u5f0f\u540c Method 11\uff0c\u4f46 \\(a,b,s\\) \u5747\u4e3a\u5206\u6bb5\u91cf \\(a_p,b_p,s_p\\)\uff0c",
    "\u76f8\u5e94\u5f97\u5230\u5206\u6bb5\u4fee\u6b63\u8d1f\u8377\u5e76\u6c47\u603b\u4e3a\u5168\u671f \\(L_{cr}\\)\u3002</p>",
    "<p><b>\u539f\u7406\uff1a</b>\u5c06&ldquo;\u5206\u671f\u8bc4\u7ea7\u66f2\u7ebf&rdquo;\uff08Method 10\uff09\u4e0e&ldquo;\u56de\u53d8\u6362\u504f\u5dee\u4fee\u6b63&rdquo;\uff08Method 11\uff09\u7ed3\u5408\u3002</p>",
    "<p><b>\u9002\u7528\u60c5\u51b5\uff1a</b>\u65e2\u5b58\u5728\u660e\u663e\u5206\u671f\u5dee\u5f02\u3001\u53c8\u5fc5\u987b\u8fdb\u884c\u5bf9\u6570\u56de\u5f52\u56de\u53d8\u6362\u65f6\uff1b",
    "\u4e0d\u9002\u5408\u5206\u671f\u540e\u6837\u672c\u8fc7\u5c11\u5bfc\u81f4 \\(a_p,b_p,s_p\\) \u4e0d\u7a33\u5b9a\u3002</p>",

    "<hr>",

    # ================================================================
    # 2. LOADEST / rloadest
    # ================================================================
    "<h4>2. LOADEST / rloadest \u56de\u5f52\u8d1f\u8377\u6a21\u578b</h4>",
    "<p>LOADEST \u901a\u8fc7\u5bf9\u6570\u7a7a\u95f4\u56de\u5f52\u77ac\u65f6\u8d1f\u8377\uff08\u6216\u6d53\u5ea6\uff09\uff0c\u518d\u901a\u8fc7\u6307\u6570\u56de\u5230\u539f\u7a7a\u95f4\u5e76\u8fdb\u884c\u504f\u5dee\u4fee\u6b63\u3002</p>",

    # ---- 2.1 ----
    "<h5>2.1 \u77ac\u65f6\u8d1f\u8377\u5b9a\u4e49\u4e0e\u79bb\u6563\u7d2f\u8ba1</h5>",
    "\\[L_\\tau = \\int_0^\\tau Q(t)C(t)\\,dt\\]",
    "\\[\\widehat{L}_\\tau = \\sum_{i=1}^{N_P} \\widehat{L}_i \\Delta t,\\quad \\widehat{L}_i = \\widehat{Q_i C_i}\\]",
    "<ul>",
    "<li>\\(L_\\tau\\)\uff1a\u77ac\u65f6\u8d1f\u8377\uff08\u5e38\u89c1\u5355\u4f4d\uff1akg/d\uff09</li>",
    "<li>\\(\\widehat{L}_\\tau\\)\uff1a\u6a21\u578b\u7ed9\u51fa\u7684\u77ac\u65f6\u8d1f\u8377\u4f30\u8ba1</li>",
    "<li>\\(\\widehat{L}_i\\)\uff1a\u79bb\u6563\u65f6\u95f4\u6b65\u4e0a\u7684\u77ac\u65f6\u8d1f\u8377\u4f30\u8ba1</li>",
    "</ul>",

    # ---- 2.2 ----
    "<h5>2.2 \u5bf9\u6570\u7ebf\u6027\u56de\u5f52\uff08LOADEST \u5f0f 4\uff09</h5>",
    "\\[\\ln(\\widehat{L}) = a_0 + \\sum_{j=1}^{N_V} a_j X_j\\]",
    "<ul>",
    "<li>\\(\\ln(\\widehat{L})\\)\uff1a\u5bf9\u6570\u53d8\u6362\u540e\u7684\u8d1f\u8377\u4f30\u8ba1</li>",
    "<li>\\(a_0, a_j\\)\uff1a\u56de\u5f52\u7cfb\u6570</li>",
    "<li>\\(X_j\\)\uff1a\u89e3\u91ca\u53d8\u91cf</li>",
    "</ul>",

    # ---- 2.3 ----
    "<h5>2.3 \u6307\u6570\u56de\u53d8\u6362\u5f97\u5230\u8bc4\u7ea7\u66f2\u7ebf\u8d1f\u8377\uff08LOADEST \u5f0f 5\uff09</h5>",
    "\\[\\widehat{L}_{RC} = \\exp\\!\\left(a_0 + \\sum_{j=1}^{N_V} a_j X_j\\right)\\]",
    "<ul>",
    "<li>\\(\\widehat{L}_{RC}\\)\uff1a\u8bc4\u7ea7\u66f2\u7ebf\u8d1f\u8377\u4f30\u8ba1\u503c\uff08\u539f\u59cb\u5c3a\u5ea6\uff09</li>",
    "<li>\\(X_j\\)\uff1a\u89e3\u91ca\u53d8\u91cf\uff08\u5982\u6d41\u91cf\u3001\u65f6\u95f4\u3001\u5b63\u8282\u7b49\uff09</li>",
    "<li>\\(N_V\\)\uff1a\u89e3\u91ca\u53d8\u91cf\u6570\u91cf</li>",
    "</ul>",

    # ---- 2.4 ----
    "<h5>2.4 \u5e38\u7528\u201c\u6269\u5c55\u9879\u201d\uff08\u5b63\u8282\u4e0e\u8d8b\u52bf\uff09</h5>",
    "\\[\\ln(\\widehat{L}) = a_0 + a_1\\ln(Q) + a_2(\\ln Q)^2",
    "+ a_3 T + a_4 T^2 + a_5\\sin(2\\pi T) + a_6\\cos(2\\pi T)\\]",
    "<ul>",
    "<li>\\(Q\\)\uff1a\u6d41\u91cf\uff08\u5355\u4f4d\u5e38\u89c1\u4e3a m\\(^3\\)/s\uff09</li>",
    "<li>\\(T\\)\uff1a\u65f6\u95f4\uff08\u5341\u8fdb\u5236\u5e74\u6216\u7c7b\u4f3c\u65f6\u95f4\u8868\u793a\u65b9\u5f0f\uff09</li>",
    "<li>\\(\\sin(2\\pi T),\\cos(2\\pi T)\\)\uff1a\u5b63\u8282\u6027\u6b63\u4f59\u5f26\u9879\uff0c\u8868\u793a\u5e74\u5468\u671f\u5185\u7684\u5b63\u8282\u6027\u53d8\u5316</li>",
    "<li>\\(a_1\\sim a_6\\)\uff1a\u56de\u5f52\u7cfb\u6570</li>",
    "</ul>",
    "<p>\u6269\u5c55\u9879\u5305\u542b\u6d41\u91cf\u5e73\u65b9\u9879\u3001\u65f6\u95f4\u9879\u53ca\u5176\u5e73\u65b9\u9879\u4ee5\u53ca\u5b63\u8282\u9879\uff0c\u80fd\u591f\u66f4\u597d\u5730\u6355\u6349\u6d41\u91cf\u4e0e\u5b63\u8282\u53d8\u5316\u4e4b\u95f4\u7684\u5173\u7cfb\u3002",
    "\u4e00\u822c\u5047\u8bbe\u6b8b\ucha\u8fdf\u670d\u4ece\u6b63\u6001\u5206\u5e03\u4e14\u9075\u5faa\u540c\u65b9\u5dee\u6027\u3002</p>",
    "<p><b>\u9002\u5408\uff1a</b>\u6709\u8fde\u7eed\u6d41\u91cf\u5e8f\u5217\u548c\u7a00\u758f\u6d53\u5ea6\uff08\u6216\u8d1f\u8377\uff09\u6837\u672c\uff1b",
    "\u8d1f\u8377\u4e0e\u6d41\u91cf\u3001\u65f6\u95f4\u3001\u5b63\u8282\u7684\u5173\u7cfb\u53ef\u7528\u8fd9\u4e9b\u57fa\u51fd\u6570\u8f83\u597d\u523a\u5212\u3002</p>",
    "<p><b>\u98ce\u9669\u70b9\uff1a</b>\u6a21\u578b\u8bbe\u5b9a\u4e0d\u5f53\u4f1a\u5bfc\u81f4\u7cfb\u7edf\u6027\u504f\u5dee\uff1b\u9700\u8981\u505a\u6b8b\u5dee\u8bca\u65ad\u4e0e\u6a21\u578b\u9009\u62e9\u3002</p>",

    "<hr>",

    # ================================================================
    # 3. EGRET WRTDS
    # ================================================================
    "<h4>3. EGRET \u4e2d WRTDS \u7684\u8d1f\u8377\u8ba1\u7b97</h4>",
    "<p>WRTDS \u5728\u6bcf\u4e00\u4e2a\u9700\u8981\u4f30\u8ba1\u7684&ldquo;\u8282\u70b9&rdquo;\u4e0a\u505a\u4e00\u5957\u5c40\u90e8\u52a0\u6743\u56de\u5f52\uff0c",
    "\u7528\u65f6\u95f4\u3001log\\(Q\\)\u3001\u4ee5\u53ca\u4e00\u9636\u6b63\u4f59\u5f26\u5b63\u8282\u9879\u5efa\u6a21\uff0c\u5e76\u7528\u4e09\u7ef4\u8ddd\u79bb\u51b3\u5b9a\u6743\u91cd\u3002</p>",

    # ---- 3.1 ----
    "<h5>3.1 \u57fa\u672c\u56de\u5f52\u5f62\u5f0f</h5>",
    "\\[\\ln(C_i) = \\beta_0 + \\beta_1 T_i + \\beta_2\\ln(Q_i)",
    "+ \\beta_3\\sin(2\\pi T_i) + \\beta_4\\cos(2\\pi T_i) + \\varepsilon_i\\]",
    "<ul>",
    "<li>\\(C_i\\)\uff1a\u7b2c \\(i\\) \u4e2a\u91c7\u6837\u65f6\u523b\u7684\u6d53\u5ea6\uff08\u5982 mg/L\uff09</li>",
    "<li>\\(Q_i\\)\uff1a\u7b2c \\(i\\) \u4e2a\u91c7\u6837\u65f6\u523b\u7684\u6d41\u91cf\uff08\u5e38\u7528\u5355\u4f4d m\\(^3\\)/s\uff09</li>",
    "<li>\\(T_i\\)\uff1a\u5341\u8fdb\u5236\u65f6\u95f4</li>",
    "<li>\\(\\beta_0\\sim\\beta_4\\)\uff1a\u8be5\u8282\u70b9\u4e0b\u5c40\u90e8\u56de\u5f52\u5f97\u5230\u7684\u7cfb\u6570\uff08WRTDS \u6bcf\u4e2a\u8282\u70b9\u4e00\u5957\u7cfb\u6570\uff09</li>",
    "<li>\\(\\varepsilon_i\\)\uff1a\u6b8b\u5dee\uff08\u5e38\u5047\u8bbe\u8fd1\u4f3c\u6b63\u6001\u3001\u5747\u503c 0\uff09</li>",
    "</ul>",

    # ---- 3.2 ----
    "<h5>3.2 \u4e09\u7ef4\u6743\u91cd\uff08Time&ndash;Discharge&ndash;Season \u7684\u4e58\u79ef\u6743\u91cd\uff09</h5>",
    "\\[w_i = w_{T,i}\\, w_{Q,i}\\, w_{S,i}\\]",
    "<p>\u8ddd\u79bb\u5b9a\u4e49\uff1a</p>",
    "\\[d_{T,i}=|T_i-T_0|,\\quad",
    "d_{Q,i}=|\\ln(Q_i)-\\ln(Q_0)|,\\quad",
    "d_{S,i}=\\text{seasonDist}(T_i,T_0)\\]",
    "<p>tricube \u6743\u91cd\u51fd\u6570\uff1a</p>",
    "\\[w(d;h)=\\begin{cases}\\left(1-\\left|\\dfrac{d}{h}\\right|^3\\right)^3,&|d|\\leq h\\\\0,&|d|>h\\end{cases}\\]",
    "<ul>",
    "<li>\\(d\\)\uff1a\u67d0\u4e00\u7ef4\u5ea6\u4e0a\u7684\u8ddd\u79bb</li>",
    "<li>\\(h\\)\uff1a\u8be5\u7ef4\u5ea6\u7684\u7a97\u53e3\u534a\u5bbd\uff08bandwidth\uff09</li>",
    "</ul>",
    "<p>\u8ddd\u79bb\u8d8a\u5c0f\u6743\u91cd\u8d8a\u5927\uff0c\u4f7f\u6a21\u578b\u5728\u65f6\u95f4\u3001\u6d41\u91cf\u548c\u5b63\u8282\u53d8\u5316\u4e0a\u66f4\u52a0\u7075\u6d3b\u3002</p>",

    # ---- 3.3 ----
    "<h5>3.3 \u4ece \\(\\ln(C)\\) \u56de\u5230 \\(C\\) \u7684\u504f\u5dee\u4fee\u6b63\uff08\u5bf9\u6570\u6b63\u6001\u7ea0\u504f\uff09</h5>",
    "\\[\\text{BCF} = \\exp\\!\\left(\\frac{SE^2}{2}\\right),\\quad",
    "\\widehat{C} = \\exp(y_{\\text{Hat}})\\cdot\\text{BCF}\\]",
    "<ul>",
    "<li>\\(\\text{BCF}\\)\uff1a\u504f\u5dee\u4fee\u6b63\u56e0\u5b50\uff08Bias Correction Factor\uff09</li>",
    "<li>\\(SE\\)\uff1a\u5bf9\u6570\u7a7a\u95f4\u6b8b\u5dee\u6807\u51c6\u5dee</li>",
    "<li>\\(y_{\\text{Hat}}\\)\uff1a\u8be5\u8282\u70b9\u5904 \\(\\ln(C)\\) \u7684\u62df\u5408\u5747\u503c</li>",
    "<li>\\(\\widehat{C}\\)\uff1a\u504f\u5dee\u4fee\u6b63\u540e\u7684\u6d53\u5ea6\u4f30\u8ba1\u5026\uff08\u539f\u59cb\u7a7a\u95f4\uff0c\u5355\u4f4d\u5982 mg/L\uff09</li>",
    "</ul>",
    "<p>\u5bf9\u6570\u53d8\u6362\u53ef\u80fd\u5bfc\u81f4\u4f4e\u6d53\u5ea6\u5080\u7684\u7cfb\u7edf\u6027\u4f4e\u4f30\uff0c",
    "\u901a\u8fc7\u8ba1\u7b97\u6b8b\u5dee\u6807\u51c6\u5dee\u5e76\u5e94\u7528 BCF\uff0cWRTDS \u5c06\u9884\u6d4b\u6d53\u5ea6\u4fee\u6b63\u56de\u539f\u59cb\u6d53\u5ea6\u7a7a\u95f4\u3002</p>",

    # ---- 3.4 ----
    "<h5>3.4 \u9002\u7528\u60c5\u51b5</h5>",
    "<p><b>\u9002\u5408\uff1a</b>\u957f\u671f\u76d1\u6d4b\u3001\u5173\u7cfb\u968f\u65f6\u95f4/\u5b63\u8282\u7f13\u6162\u53d8\u5316\u3001\u91c7\u6837\u4e0d\u4e00\u5b9a\u5f88\u5bc6\u4f46\u65f6\u95f4\u8de8\u5ea6\u8f83\u957f\uff08WRTDS \u7528\u5c40\u90e8\u56de\u5f52\u5728\u65f6\u95f4&ndash;\u6d41\u91cf&ndash;\u5b63\u8282\u4e0a\u5e73\u6ed1\uff09\u3002</p>",
    "<p><b>\u4e0d\u9002\u5408\uff1a</b>\u6781\u5f3a\u4e8b\u4ef6\u4e3b\u5bfc\u4e14\u91c7\u6837\u65e0\u6cd5\u8986\u76d6\u4e8b\u4ef6\uff08\u4efb\u4f55\u7edf\u8ba1\u6a21\u578b\u90fd\u4f1a\u53d7\u9650\uff09\uff1b",
    "\u6d53\u5ea6\u4e0e\u6d41\u91cf\u4e4b\u95f4\u5b58\u5728\u9891\u7e41\u4e14\u5267\u70c8\u7684\u7ed3\u6784\u6027\u7a81\u53d8\u3002</p>"
  )
}




# ---------- UI 参数块（嵌入通用 mod_flux_method_page_ui 的 left_extra）----------

#' 回归法专属的左侧参数控件
#' @param ns  namespace 函数
#' @param key 方法键名（固定为 "regression"）
#' @return tagList
# ★ 修改：移除原理折叠块（静态 UI 中 MathJax 无法渲染折叠内容）
#          原理内容已移至 Server 端 output[["principle_regression"]] renderUI
fw_regression_left_extra_ui <- function(ns, key = "regression") {
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
}




# ---------- Server：QF/WQ 数据源 reactive ----------

#' 创建 step1_qf_wq reactive（从 rv 中提取第一步 QF/WQ 数据）
#' @param rv reactiveValues
#' @return reactive 返回 list(QF, WQ, source) 或 NULL
fw_regression_step1_reactive <- function(rv) {
  shiny::reactive({
    fw_get_step1_qf_wq(rv)
  })
}

# ---------- Server：QF/WQ 表名与指标联动 observers ----------

#' 注册回归法的 QF 表、WQ 表、水质指标联动 observers
#' @param input, session  Shiny 标准
#' @param step1_qf_wq  reactive（fw_regression_step1_reactive 返回值）
fw_regression_init_observers <- function(input, session, step1_qf_wq) {

  # ---- QF / WQ 表名同步 ----
  shiny::observe({
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

    cur_qf <- shiny::isolate(input$qf_sheet_regression)
    cur_wq <- shiny::isolate(input$wq_sheet_regression)
    if (is.null(cur_qf) || !(cur_qf %in% qf_nm)) cur_qf <- qf_nm[1]
    if (is.null(cur_wq) || !(cur_wq %in% wq_nm)) cur_wq <- wq_nm[1]

    shiny::updateSelectInput(session, "qf_sheet_regression", choices = qf_nm, selected = cur_qf)
    shiny::updateSelectInput(session, "wq_sheet_regression", choices = wq_nm, selected = cur_wq)
  })

  # ---- 水质指标联动 ----
  shiny::observe({
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

    cur <- shiny::isolate(input$constituent_regression)
    if (is.null(cur) || !(cur %in% cands)) cur <- cands[1]

    shiny::updateSelectInput(session, "constituent_regression", choices = cands, selected = cur)
  })
}

# ---------- Server：回归法数据获取 ----------

#' 回归法的 daily_all 数据获取
#' @param input       Shiny input
#' @param step1_qf_wq reactive（同上）
#' @return data.frame(station, WYBM, date, Q, C_obs) 或 NULL
fw_regression_get_daily_all <- function(input, step1_qf_wq) {
  s1 <- step1_qf_wq()
  if (is.null(s1)) return(NULL)

  prep <- tryCatch(
    fw_prepare_regression_input(
      step1_data  = s1,
      qf_sheet    = input$qf_sheet_regression,
      wq_sheet    = input$wq_sheet_regression,
      constituent = input$constituent_regression,
      date_range  = NULL
    ),
    error = function(e) NULL
  )
  if (is.null(prep)) return(NULL)

  data.frame(
    station = prep$station,
    WYBM    = prep$wybm %||% NA_character_,
    date    = prep$dat$TM,
    Q       = prep$dat$Q,
    C_obs   = prep$dat$conc,
    stringsAsFactors = FALSE
  )
}

# ---------- Server：回归法计算调度 ----------

#' 回归法的"开始计算"逻辑
#' @param input       Shiny input
#' @param step1_qf_wq reactive
#' @param key         方法键名 "regression"
#' @return res list（与 fw_run_flux_with_config 返回结构一致）或 NULL
fw_regression_run_calc <- function(input, step1_qf_wq, key = "regression") {
  s1 <- step1_qf_wq()
  if (is.null(s1)) {
    shiny::showNotification("未找到第一步 QF/WQ 数据。", type = "error")
    return(NULL)
  }

  res <- tryCatch(
    fw_run_flux_with_config(
      method     = "regression",
      date_range = input[[paste0("daterange_", key)]],
      step1_data = s1,
      regression_cfg = list(
        qf_sheet     = input[[paste0("qf_sheet_", key)]],
        wq_sheet     = input[[paste0("wq_sheet_", key)]],
        constituent  = input[[paste0("constituent_", key)]],
        model_choice = input[[paste0("reg_model_", key)]]
      )
    ),
    error = function(e) {
      shiny::showNotification(e$message, type = "error")
      NULL
    }
  )

  res
}

# ---------- Server：回归法结算文本额外信息 ----------

#' 回归法结算文本中的额外行
#' @param res  计算结果 list
#' @return character
fw_regression_settle_extra <- function(res) {
  if (is.null(res)) return("")
  paste0(
    "\nQF表: ", res$params$qf_sheet %||% "",
    "\nWQ表: ", res$params$wq_sheet %||% "",
    "\n指标: ", res$params$constituent %||% "",
    "\n模型: ", res$params$model_choice %||% ""
  )
}

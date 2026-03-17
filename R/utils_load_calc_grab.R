


#----基流分割-----

#' 基流分割 - Eckhardt递归数字滤波法 (Eckhardt, 2004)
#' 公式: bk = [(1-BFImax)*a*b_{k-1} + (1-a)*BFImax*yk] / (1 - a*BFImax)
#' 对于日数据 a=0.97, 对于5分钟数据 a=0.97^(1/288)
#' 初始b0取径流最小值
#'
#' @param date_vec Date/POSIXct向量, 日期列
#' @param streamflow numeric向量, 径流量
#' @param BFImax numeric, 最大基流指数 (默认0.80, 渗透性含水层; 0.50硬岩; 0.25多孔)
#' @param alpha numeric, 衰退常数 (默认0.97, 日尺度)
#' @return data.frame 包含 date, streamflow, baseflow, directrunoff, baseflow_ratio
baseflow_separation <- function(date_vec, streamflow, BFImax = 0.80, alpha = 0.97) {
  # first baseflow separation
  # Recursive digital filter method (Eckhardt, 2004)
  # bk = [(1 - BFImax) * a * b_{k-1} + (1 - a) * BFImax * yk] / (1 - a * BFImax)
  # a = 0.97 for daily, a = 0.97^(1/288) for 5-min interval
  # 初始b0设为径流最小值

  n <- length(streamflow)
  baseflow <- numeric(n)
  directrunoff <- numeric(n)

  # 处理缺失值标记
  is_valid <- !is.na(streamflow) & is.numeric(streamflow)

  # 初始b0为径流序列最小值 * 1.1
  valid_sf <- streamflow[is_valid & streamflow > 0]
  if (length(valid_sf) == 0) {
    warning("No valid streamflow data found.")
    return(data.frame(date = date_vec, streamflow = streamflow,
                      baseflow = NA, directrunoff = NA, baseflow_ratio = NA))
  }
  b0 <- round(min(valid_sf, na.rm = TRUE) * 1.1, 3)
  baseflow[1] <- b0

  for (i in 2:n) {
    if (!is_valid[i]) {
      # 缺失数据标红（在R中标记为NA）
      baseflow[i] <- NA
      directrunoff[i] <- NA
    } else if (streamflow[i] == 0) {
      baseflow[i] <- 0
      directrunoff[i] <- 0
    } else if (is_valid[i] && !is.na(baseflow[i - 1]) && streamflow[i] > 0) {
      Y_k <- round(streamflow[i], 3)
      B_k_1 <- round(baseflow[i - 1], 3)
      B_k <- round(((1 - BFImax) * alpha * B_k_1 + (1 - alpha) * BFImax * Y_k) /
                     (1 - alpha * BFImax), 3)
      if (Y_k >= B_k) {
        baseflow[i] <- round(B_k, 3)
        directrunoff[i] <- round(Y_k - B_k, 3)
      } else {
        baseflow[i] <- round(Y_k, 3)
        directrunoff[i] <- 0
      }
    } else {
      baseflow[i] <- streamflow[i]
      directrunoff[i] <- streamflow[i]
    }
  }
  # 第一行直接径流补算
  if (is_valid[1] && streamflow[1] > 0) {
    directrunoff[1] <- max(0, round(streamflow[1] - baseflow[1], 3))
  }

  bf_ratio <- ifelse(streamflow > 0 & !is.na(baseflow), round(baseflow / streamflow, 3), NA)

  return(data.frame(
    date = date_vec,
    streamflow = streamflow,
    baseflow = baseflow,
    directrunoff = directrunoff,
    baseflow_ratio = bf_ratio
  ))
}


#' 按年汇总基流分割结果
#' @param bf_result data.frame, baseflow_separation()的返回值
#' @return data.frame 每年的总径流、总基流、总直接径流、年平均基流比
baseflow_annual_summary <- function(bf_result) {
  # 按年汇总径流、基流、直接径流及年均基流比
  bf_result$year <- format(bf_result$date, "%Y")

  annual <- aggregate(
    cbind(streamflow, baseflow, directrunoff) ~ year,
    data = bf_result, FUN = sum, na.rm = TRUE
  )
  annual$baseflow_ratio_mean <- round(annual$baseflow / annual$streamflow, 2)

  return(annual)
}


#----滑动窗口最大值----

#' 滑动窗口求和排序分析
#' 对每列数据, 以指定窗口宽度求滑动窗口和, 然后降序排列,
#' 并计算每个窗口和占总量的百分比
#'
#' @param df data.frame, 第1列为日期/标签, 其余列为数值数据
#' @param window_width integer, 窗口宽度 (默认17)
#' @param top_n integer, 输出前n个最大窗口 (默认15)
#' @return list, 每个数值列对应一个data.frame(date, window_sum, percent)
moving_window_analysis <- function(df, window_width = 17, top_n = 15) {
  # 输入数据起始和终止行已在df中
  # 第一列数据为时间等标记列
  # 窗口宽度默认17
  # 原始数据与输出数据之间的列间距（VBA概念, R中不需要）

  datarow <- nrow(df)
  datacolumn <- ncol(df)

  results <- list()

  for (y in 2:datacolumn) {
    n_windows <- datarow - window_width + 1
    if (n_windows < 1) {
      warning(sprintf("Column %d: data rows (%d) < window_width (%d), skipped.",
                      y, datarow, window_width))
      next
    }

    window_df <- data.frame(
      date = rep(NA, n_windows),
      window_sum = numeric(n_windows),
      percent = numeric(n_windows)
    )

    # 计算每个窗口的和
    for (x in 1:n_windows) {
      window_df$date[x] <- as.character(df[x, 1])
      window_df$window_sum[x] <- sum(df[x:(x + window_width - 1), y], na.rm = TRUE)
    }

    # 按窗口和降序排列（冒泡排序 -> R直接用order）
    window_df <- window_df[order(-window_df$window_sum), ]

    # 计算占总量百分比
    total_sum <- sum(df[, y], na.rm = TRUE)
    window_df$percent <- round(window_df$window_sum * 100 / total_sum, 2)

    # 只取前top_n个
    window_df <- head(window_df, top_n)
    rownames(window_df) <- NULL

    results[[colnames(df)[y]]] <- window_df
  }

  return(results)
}


#----污染负荷计算-----

#' 污染负荷计算 - 瞬时采样法 (Grab Sampling)
#'
#' 注意: 水质起始时间不能早于流量起始时间！
#' 本程序只能计算两个水质数据之间必须至少有一个流量数据的情况,
#' 不能存在两个流量数据之间有多个水质数据的情况。
#'
#' @param flow_data data.frame, 流量数据. 第1列为POSIXct时间, 第2列为流量(m3/s)
#' @param wq_data data.frame, 水质数据. 第1列为POSIXct时间, 其余列为浓度(mg/L)
#' @return list 包含:
#'   - piecewise: 以流量单元为时间节点的分段结果
#'   - wq_period: 以水质时间段为节点的汇总结果
#'   - daily: 以Julian day为节点的日汇总结果
load_calc_grab_sampling <- function(flow_data, wq_data) {

  # ---- 数据验证 ----
  # 检查时间列是否单调递增
  flow_times <- as.POSIXct(flow_data[, 1])
  wq_times   <- as.POSIXct(wq_data[, 1])

  if (any(diff(flow_times) <= 0, na.rm = TRUE)) {
    bad_rows <- which(diff(flow_times) <= 0)
    warning(sprintf("流量数据第 %s 行时间顺序有误！请检查",
                    paste(bad_rows, collapse = ", ")))
  }
  if (any(diff(wq_times) <= 0, na.rm = TRUE)) {
    bad_rows <- which(diff(wq_times) <= 0)
    warning(sprintf("水质数据第 %s 行时间顺序有误！请检查",
                    paste(bad_rows, collapse = ", ")))
  }

  # 检查数值列
  for (j in 2:ncol(flow_data)) {
    if (!is.numeric(flow_data[, j])) {
      warning(sprintf("流量数据第 %d 列含有非数值数据", j))
    }
  }
  for (j in 2:ncol(wq_data)) {
    if (!is.numeric(wq_data[, j])) {
      warning(sprintf("水质数据第 %d 列含有非数值数据", j))
    }
  }

  n_flow <- nrow(flow_data)
  n_wq   <- nrow(wq_data)
  n_wq_params <- ncol(wq_data) - 1  # 水质参数个数（除时间列）

  flow_times  <- as.POSIXct(flow_data[, 1])
  flow_values <- as.numeric(flow_data[, 2])
  wq_times    <- as.POSIXct(wq_data[, 1])

  # ---- 阶段1: 匹配水质时段到流量时间,计算分段负荷 ----
  # 初始化以流量步长为单元的结果数组
  duration_pw   <- rep(0, n_flow)      # 每个流量单元的持续时间(min)
  flow_mean_pw  <- rep(0, n_flow)      # 每个流量单元的平均流量
  concen_pw     <- matrix(0, nrow = n_flow, ncol = n_wq_params)  # 浓度
  volume_pw     <- rep(0, n_flow)      # 水量 (1000 m3)
  load_pw       <- matrix(0, nrow = n_flow, ncol = n_wq_params)  # 负荷 (kg)

  # 以水质时段汇总的结果
  flow_mean_wq  <- rep(0, n_wq)
  duration_wq   <- rep(0, n_wq)
  concen_mean_wq <- matrix(0, nrow = n_wq, ncol = n_wq_params)
  load_wq       <- matrix(0, nrow = n_wq, ncol = n_wq_params)

  for (y_idx in 1:n_wq_params) {
    y <- y_idx + 1  # wq_data中的列索引
    row_end_flow <- 1

    for (i in 1:(n_wq - 1)) {
      time_start_wq <- wq_times[i]
      time_end_wq   <- wq_times[i + 1]

      # 平均浓度, 单位: mg/L
      concen_mean <- round((as.numeric(wq_data[i, y]) + as.numeric(wq_data[i + 1, y])) / 2, 3)

      j_start <- max(1, row_end_flow)

      # 找到水质起始时间在流量序列中最近的位置
      row_start_in_flow <- NA
      row_end_in_flow   <- NA

      for (j in j_start:(n_flow - 1)) {
        t_ahead <- flow_times[j]
        t_later <- flow_times[j + 1]

        # 匹配起始时间
        if (is.na(row_start_in_flow)) {
          diff_ahead <- as.numeric(difftime(time_start_wq, t_ahead, units = "secs"))
          diff_later <- as.numeric(difftime(time_start_wq, t_later, units = "secs"))

          if (diff_ahead >= 0 && diff_later <= 0) {
            # 水质时间在两个流量时间点之间,取最近的
            if (abs(diff_ahead) <= abs(diff_later)) {
              row_start_in_flow <- j
            } else {
              row_start_in_flow <- j + 1
            }
          }
        }

        # 匹配终止时间
        if (!is.na(row_start_in_flow) && is.na(row_end_in_flow)) {
          diff_ahead_end <- as.numeric(difftime(time_end_wq, flow_times[j], units = "secs"))
          diff_later_end <- as.numeric(difftime(time_end_wq, flow_times[j + 1], units = "secs"))

          if (diff_ahead_end >= 0 && diff_later_end <= 0) {
            if (abs(diff_ahead_end) <= abs(diff_later_end)) {
              row_end_in_flow <- j
            } else {
              row_end_in_flow <- j + 1
            }
          }
        }

        if (!is.na(row_start_in_flow) && !is.na(row_end_in_flow)) break
      }

      if (is.na(row_start_in_flow) || is.na(row_end_in_flow)) next

      # 计算分段负荷
      step_n <- row_end_in_flow - row_start_in_flow
      if (step_n < 1) next

      dur_accum <- 0
      vol_accum <- 0
      load_accum <- 0

      for (p in 1:step_n) {
        idx <- row_start_in_flow + p - 1
        # 计算以流量单元间平均值
        flow_mean_piece <- round((flow_values[idx] + flow_values[idx + 1]) / 2, 3)
        # 计算以流量单元间时间长度 (秒)
        dur_piece <- abs(as.numeric(difftime(flow_times[idx + 1], flow_times[idx], units = "secs")))
        # 计算以流量单元间负荷, 单位为kg
        load_piece <- round(concen_mean * flow_mean_piece * dur_piece / 1000, 3)
        # 单位是1000 m3
        vol_piece  <- round(flow_mean_piece * dur_piece * 0.001, 3)

        # 记录以流量单元起始时间和下一个时刻点之间的负荷, 标记为起始单元负荷
        duration_pw[idx]          <- dur_piece / 60
        flow_mean_pw[idx]         <- round(flow_mean_piece, 3)
        concen_pw[idx, y_idx]     <- round(concen_mean, 3)
        volume_pw[idx]            <- vol_piece
        load_pw[idx, y_idx]       <- round(load_piece, 3)

        dur_accum  <- dur_accum + dur_piece
        vol_accum  <- vol_accum + vol_piece
        load_accum <- load_accum + load_piece
        # 以水质单元起始时间和下一个水质时刻点之间的负荷累积, 标记为起始水质单元负荷
      }

      # 将水质时间段内负荷累积
      if (dur_accum > 0) {
        flow_mean_wq[i]            <- round(vol_accum * 1000 / dur_accum, 3)
        duration_wq[i]             <- round(dur_accum / 60, 3)
        concen_mean_wq[i, y_idx]   <- round(concen_mean, 3)
        load_wq[i, y_idx]          <- round(load_accum, 3)  # 将水质时间段内负荷累积
      }

      row_end_flow <- max(1, row_end_in_flow - 1)
    }
  }

  # ---- 阶段2: 按Julian Day分配日负荷 ----
  # 计算需要分解的天数,并输入数组
  # 找到第一个水质时间在流量中的位置
  count_before_first <- which(flow_times == wq_times[1])
  if (length(count_before_first) == 0) {
    count_before_first <- which.min(abs(as.numeric(difftime(flow_times, wq_times[1], units = "secs"))))
  } else {
    count_before_first <- count_before_first[1]
  }

  # 计算需要分解的天数,并输入数组
  n_julian_days <- as.integer(difftime(as.Date(wq_times[n_wq]), as.Date(wq_times[1]), units = "days"))
  if (n_julian_days < 1) {
    daily_result <- NULL
  } else {
    first_day <- as.Date(wq_times[1])
    julian_dates <- first_day + 0:(n_julian_days - 1)

    # 累积持续时间, 单位: 小时
    dur_julian     <- rep(0, n_julian_days)
    # 平均流量, 单位: m3/s
    flow_julian    <- rep(0, n_julian_days)
    # 平均浓度, 单位: mg/L
    concen_julian  <- matrix(0, nrow = n_julian_days, ncol = n_wq_params)
    # 以流量单元起始时间和下一个时刻点之间的负荷, 标记为起始单元负荷
    load_julian    <- matrix(0, nrow = n_julian_days, ncol = n_wq_params)

    for (e in 1:n_wq_params) {
      m_search_start <- count_before_first

      for (k in 1:(n_julian_days - 1)) {
        t_start_j <- as.POSIXct(julian_dates[k], tz = attr(flow_times, "tzone") %||% "UTC")
        t_end_j   <- as.POSIXct(julian_dates[k + 1], tz = attr(flow_times, "tzone") %||% "UTC")

        vol_accum_j  <- 0
        load_accum_j <- 0

        for (m in m_search_start:(n_flow - 1)) {
          t_m     <- flow_times[m]
          t_m1    <- flow_times[m + 1]

          # 判断该流量段与当日时间窗的重叠
          overlap_start <- max(t_m, t_start_j)
          overlap_end   <- min(t_m1, t_end_j)

          if (overlap_end > overlap_start) {
            # 按重叠比例分配水量和负荷
            # 继续累积水量与负荷
            full_dur <- as.numeric(difftime(t_m1, t_m, units = "secs"))
            if (full_dur > 0) {
              frac <- as.numeric(difftime(overlap_end, overlap_start, units = "secs")) / full_dur
              vol_accum_j  <- vol_accum_j  + volume_pw[m] * frac
              load_accum_j <- load_accum_j + load_pw[m, e] * frac
            }
          }

          if (t_m1 >= t_end_j) {
            m_search_start <- m
            break
          }
        }

        day_dur <- as.numeric(difftime(t_end_j, t_start_j, units = "secs"))
        # 累积持续时间, 单位: 小时
        dur_julian[k] <- day_dur / 3600
        if (day_dur > 0 && vol_accum_j > 0) {
          # 平均流量, 单位: m3/s
          flow_julian[k]      <- round(vol_accum_j * 1000 / day_dur, 3)
          # 平均浓度, 单位: mg/L
          concen_julian[k, e] <- round(load_accum_j / vol_accum_j, 3)
        }
        # 以流量单元起始时间和下一个时刻点之间的负荷, 标记为起始单元负荷
        load_julian[k, e] <- round(load_accum_j, 3)
      }
    }

    daily_result <- data.frame(
      date = julian_dates[1:(n_julian_days - 1)],
      duration_hr = dur_julian[1:(n_julian_days - 1)],
      flow_mean = flow_julian[1:(n_julian_days - 1)]
    )
    for (e in 1:n_wq_params) {
      cname <- ifelse(!is.null(colnames(wq_data)[e + 1]),
                      colnames(wq_data)[e + 1], paste0("param", e))
      daily_result[[paste0("concen_mean_", cname)]] <- concen_julian[1:(n_julian_days - 1), e]
      daily_result[[paste0("load_", cname)]]         <- load_julian[1:(n_julian_days - 1), e]
    }
  }

  # ---- 组装输出 ----
  # 以流量单元为节点的分段结果
  piecewise_result <- data.frame(
    time = flow_times[1:(n_flow - 1)],
    duration_min = duration_pw[1:(n_flow - 1)],
    flow_mean = flow_mean_pw[1:(n_flow - 1)]
  )
  for (e in 1:n_wq_params) {
    cname <- ifelse(!is.null(colnames(wq_data)[e + 1]),
                    colnames(wq_data)[e + 1], paste0("param", e))
    piecewise_result[[paste0("concen_", cname)]] <- concen_pw[1:(n_flow - 1), e]
    piecewise_result[[paste0("load_", cname)]]   <- load_pw[1:(n_flow - 1), e]
  }

  # 以水质时段为节点的汇总结果
  # 输出以水质为时间节点的数据
  wq_period_result <- data.frame(
    time_wq = wq_times[1:(n_wq - 1)],
    duration_min = duration_wq[1:(n_wq - 1)],
    flow_mean = flow_mean_wq[1:(n_wq - 1)]
  )
  for (e in 1:n_wq_params) {
    cname <- ifelse(!is.null(colnames(wq_data)[e + 1]),
                    colnames(wq_data)[e + 1], paste0("param", e))
    wq_period_result[[paste0("concen_mean_", cname)]] <- concen_mean_wq[1:(n_wq - 1), e]
    wq_period_result[[paste0("load_", cname)]]        <- load_wq[1:(n_wq - 1), e]
  }

  # 输出以julian day为时间节点的数据
  return(list(
    piecewise  = piecewise_result,
    wq_period  = wq_period_result,
    daily      = daily_result
  ))
}



# ----生成等间隔时间序列（如每5分钟）----

#' 生成等间隔时间序列（如每5分钟）
#' @param start_date POSIXct/character, 起始日期时间 (如 "2020/1/1 00:00")
#' @param end_date POSIXct/character, 终止日期时间 (如 "2020/12/31 23:55")
#' @param interval_min integer, 时间间隔(分钟), 默认5
#' @return data.frame 含一列datetime
generate_time_sequence <- function(start_date, end_date, interval_min = 5) {
  # 需要手动输入具体时间
  # 生成从start_date到end_date的每interval_min分钟时间序列

  start_dt <- as.POSIXct(start_date, format = "%Y/%m/%d %H:%M", tz = "UTC")
  end_dt   <- as.POSIXct(end_date,   format = "%Y/%m/%d %H:%M", tz = "UTC")

  # 判断是否闰年（仅供显示参考）
  year_val <- as.integer(format(start_dt, "%Y"))
  calcu_days <- ifelse(year_val %% 4 == 0 & (year_val %% 100 != 0 | year_val %% 400 == 0), 366, 365)
  message(sprintf("Year: %d, Days: %d", year_val, calcu_days))

  time_seq <- seq(from = start_dt, to = end_dt, by = paste0(interval_min, " min"))

  result <- data.frame(datetime = format(time_seq, "%Y/%m/%d %H:%M"))

  message("Congratulation! Finished!")
  return(result)
}


#' 每隔一定行数插入空行
#' @param df data.frame, 输入数据
#' @param rows_insert integer, 每次插入的空行数 (默认5)
#' @param first_row integer, 起始行 (默认1)
#' @param end_row integer, 终止行 (默认nrow(df))
#' @return data.frame, 插入空行后的数据
insert_rows <- function(df, rows_insert = 5, first_row = 1, end_row = nrow(df)) {
  # 关闭屏幕刷新 (VBA中的概念, R中无需), 提高代码执行效率

  result <- df[1:first_row, , drop = FALSE]

  for (k in 1:(end_row - first_row)) {
    current_row <- first_row + k
    if (current_row <= nrow(df)) {
      result <- rbind(result, df[current_row, , drop = FALSE])
    }
    # 插入空行
    empty_rows <- as.data.frame(matrix(NA, nrow = rows_insert, ncol = ncol(df)))
    colnames(empty_rows) <- colnames(df)
    result <- rbind(result, empty_rows)
  }

  return(result)
}


#' 删除第一列为空的行
#' @param df data.frame
#' @return data.frame, 删除空行后的数据
delete_empty_rows <- function(df) {
  # 视数据行列格式更换
  keep <- !(is.na(df[, 1]) | trimws(as.character(df[, 1])) == "")
  return(df[keep, , drop = FALSE])
}


# ----缺失数据线性插值填充----


#' 缺失数据线性插值填充
#' 该函数将通过线性插值添加的数据标记出来
#'
#' @param df data.frame, 输入数据
#' @param cols integer/character向量, 需要填充的列（索引或列名）
#' @param startrow integer, 起始行 (默认1)
#' @param endrow integer, 终止行 (默认nrow(df))
#' @param threshold numeric, 流量阈值,小于此值视为缺失 (默认NULL不启用)
#' @return list(data = 填充后的data.frame, filled_positions = 被填充的位置矩阵)
fill_missing_data <- function(df, cols, startrow = 1, endrow = nrow(df), threshold = NULL) {
  # 缺失数据线性插值填充
  # 对选定列中的空白/NA值通过前后非空值进行线性插值
  # 填充的数据位置会被记录返回

  filled_positions <- list()

  for (col in cols) {
    vec <- df[startrow:endrow, col]
    n <- length(vec)

    # 若设置了阈值，将小于阈值的数值置为NA（如流量数据清洗）
    if (!is.null(threshold)) {
      vec[!is.na(vec) & is.numeric(vec) & vec < threshold] <- NA
    }

    filled_idx <- c()
    blankcount <- 0

    for (j in 1:n) {
      if (is.na(vec[j]) || vec[j] == "" || identical(toupper(as.character(vec[j])), "NO DATA")) {
        blankcount <- blankcount + 1
      } else {
        if (blankcount >= 1 && (j - blankcount - 1) >= 1) {
          # 前一个非空值
          value_before <- as.numeric(vec[j - blankcount - 1])
          # 后一个非空值（当前值）
          value_after <- as.numeric(vec[j])
          addstep <- (value_after - value_before) / (blankcount + 1)

          for (k in 1:blankcount) {
            idx <- j - blankcount - 1 + k
            vec[idx] <- round(value_before + addstep * k, 3)
            filled_idx <- c(filled_idx, startrow + idx - 1) # 记录在原df中的行号
          }
        }
        blankcount <- 0
      }
    }

    df[startrow:endrow, col] <- vec
    filled_positions[[as.character(col)]] <- filled_idx
  }

  message(sprintf("填充完成! 共填充 %d 个位置",
                  sum(sapply(filled_positions, length))))

  return(list(data = df, filled_positions = filled_positions))
}


# ----合并多个数据框----
#' 合并多个数据框（对应多个sheet）到一个目标数据框
#' 源表个数为n, 每个表数据起始行为nstart, 从左到右依次排列
#' @param sheet_list list, 每个元素为一个data.frame（对应一个sheet）
#' @param nstart integer, 每个表数据的起始行号（默认4）
#' @return data.frame, 合并后的数据
multisheets_merge <- function(sheet_list, nstart = 4) {
  # sheet_list: 源表列表, 每个元素为一个data.frame
  # nstart: 每个单表数据的起始行数

  result <- data.frame()

  for (i in seq_along(sheet_list)) {
    df <- sheet_list[[i]]

    # 确定有效数据行: 从nstart行开始, 以第1列非空为判断依据
    # 特别注意标记列的行是否有空格，若有空格则中断拷贝
    valid_rows <- which(nchar(trimws(as.character(df[nstart:nrow(df), 1]))) > 0) + nstart - 1

    if (length(valid_rows) > 0) {
      max_row <- max(valid_rows)
      chunk <- df[nstart:max_row, , drop = FALSE]
      result <- rbind(result, chunk)
    }
  }

  return(result)
}


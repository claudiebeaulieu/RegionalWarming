# Changepoint uncertainty for Regional warming paper

ConfidenceIntervalsTS = function(ts, year) {
  
  secondseg = function(data, start, previousbeta, p = 1, conf.level = 0.95) {
    n = length(data)
    t = start:(start + n - 1)
    filtered = data - previousbeta + previousbeta * (t - start) / n
    X = (t - start) / n
    trendfit = lm(filtered ~ -1 + X)
    arfit = try(arima(resid(trendfit), order = c(p, 0, 0), include.mean = FALSE, method = "ML"), silent = TRUE)
    
    if (inherits(arfit, "try-error")) {
      return(list(stat = 10^{16}, trend = 0, ci_low = NA, ci_high = NA, annual_trend = NA, annual_ci_low = NA, annual_ci_high = NA))
    }
    
    rho = arfit$coef[1]
    se_ols = summary(trendfit)$coefficients["X", "Std. Error"]
    se_adjusted = se_ols * sqrt((1 + rho) / (1 - rho))
    
    beta = coef(trendfit)[1]
    crit_val = qnorm(1 - (1 - conf.level) / 2)
    
    # CRITICAL FIX: Trend slope is the change (beta - previousbeta) divided by segment length
    ann_trend = (beta - previousbeta) / n
    
    return(list(
      stat = as.numeric(-2 * logLik(arfit)), 
      trend = beta,  # Keep internal end-value for continuity math
      annual_trend = ann_trend,
      annual_ci_low = ann_trend - (crit_val * se_adjusted) / n,
      annual_ci_high = ann_trend + (crit_val * se_adjusted) / n
    ))
  }
  
  firstseg = function(data, start, p = 1, conf.level = 0.95) {
    n = length(data)
    t = start:(start + n - 1)
    X = (t - start) / n
    trendfit = lm(data ~ X)
    arfit = try(arima(resid(trendfit), order = c(p, 0, 0), include.mean = FALSE, method = "ML"), silent = TRUE)
    
    if (inherits(arfit, "try-error")) {
      return(list(stat = 10^{16}, trend = 0, ci_low = NA, ci_high = NA, annual_trend = NA, annual_ci_low = NA, annual_ci_high = NA))
    }
    
    rho = arfit$coef[1]
    se_ols = summary(trendfit)$coefficients["X", "Std. Error"]
    se_adjusted = se_ols * sqrt((1 + rho) / (1 - rho))
    
    beta = coef(trendfit)[2] + coef(trendfit)[1]
    annual_b = coef(trendfit)[2] / n
    crit_val = qnorm(1 - (1 - conf.level) / 2)
    
    return(list(
      stat = as.numeric(-2 * logLik(arfit)), 
      trend = beta, 
      annual_trend = annual_b,
      annual_ci_low = annual_b - (crit_val * se_adjusted) / n,
      annual_ci_high = annual_b + (crit_val * se_adjusted) / n
    ))
  }
  
  runmin = function(v, k) {
    out = NULL
    for (i in 1:(length(v) - k + 1)) {
      out = c(out, min(v[i:(i + k - 1)], na.rm = TRUE))
    }
    return(out)
  }
  
  minseglen = 10
  n = length(ts)
  test.stat = matrix(NA, nrow = n, ncol = 1)
  
  for (tau in minseglen:(n - minseglen)) {
    first = firstseg(ts[1:tau], start = 0)
    if (first$stat < 10^{16}) {
      second = secondseg(ts[(tau + 1):n], start = tau, previousbeta = first$trend)
      test.stat[tau, 1] = first$stat + second$stat
    }
  }
  
  opt_idx = which.min(test.stat[, 1])
  opt_year = year[opt_idx]
  
  opt_first = firstseg(ts[1:opt_idx], start = 0)
  opt_second = secondseg(ts[(opt_idx + 1):n], start = opt_idx, previousbeta = opt_first$trend)
  
  alpha = 0.05
  cpts = c(0, opt_idx, length(test.stat[, 1]))
  nj.maxi = max(runmin(diff(cpts), 2))
  CS.thresh = 3/2 + 2 * log(2 * nj.maxi) - 2 * log(alpha) - log(n)
  cpt_CI = year[which(test.stat[, 1] < min(test.stat[, 1], na.rm = TRUE) + CS.thresh)]
  
  return(list(
    Estimated_Changepoint_Year = opt_year,
    Changepoint_CI_Years = cpt_CI,
    Pre_Break_Annual_Slope = opt_first$annual_trend,
    Pre_Break_Annual_Slope_CI = c(opt_first$annual_ci_low, opt_first$annual_ci_high),
    Post_Break_Annual_Slope = opt_second$annual_trend,
    Post_Break_Annual_Slope_CI = c(opt_second$annual_ci_low, opt_second$annual_ci_high)
  ))
}


ConfidenceIntervals = function(regional_averages) {
  
  secondseg = function(data, start, previousbeta, p = 1, conf.level = 0.95) {
    n = length(data)
    t = start:(start + n - 1)
    filtered = data - previousbeta + previousbeta * (t - start) / n
    X = (t - start) / n
    trendfit = lm(filtered ~ -1 + X)
    arfit = try(arima(resid(trendfit), order = c(p, 0, 0), include.mean = FALSE, method = "ML"), silent = TRUE)
    
    if (inherits(arfit, "try-error")) {
      return(list(stat = 10^{16}, trend = 0, ci_low = NA, ci_high = NA, annual_trend = NA, annual_ci_low = NA, annual_ci_high = NA))
    }
    
    rho = arfit$coef[1]
    se_ols = summary(trendfit)$coefficients["X", "Std. Error"]
    se_adjusted = se_ols * sqrt((1 + rho) / (1 - rho))
    
    beta = coef(trendfit)[1]
    crit_val = qnorm(1 - (1 - conf.level) / 2)
    
    ann_trend = (beta - previousbeta) / n
    
    return(list(
      stat = as.numeric(-2 * logLik(arfit)), 
      trend = beta,
      annual_trend = ann_trend,
      annual_ci_low = ann_trend - (crit_val * se_adjusted) / n,
      annual_ci_high = ann_trend + (crit_val * se_adjusted) / n
    ))
  }
  
  firstseg = function(data, start, p = 1, conf.level = 0.95) {
    n = length(data)
    t = start:(start + n - 1)
    X = (t - start) / n
    trendfit = lm(data ~ X)
    arfit = try(arima(resid(trendfit), order = c(p, 0, 0), include.mean = FALSE, method = "ML"), silent = TRUE)
    
    if (inherits(arfit, "try-error")) {
      return(list(stat = 10^{16}, trend = 0, ci_low = NA, ci_high = NA, annual_trend = NA, annual_ci_low = NA, annual_ci_high = NA))
    }
    
    rho = arfit$coef[1]
    se_ols = summary(trendfit)$coefficients["X", "Std. Error"]
    se_adjusted = se_ols * sqrt((1 + rho) / (1 - rho))
    
    beta = coef(trendfit)[2] + coef(trendfit)[1]
    annual_b = coef(trendfit)[2] / n
    crit_val = qnorm(1 - (1 - conf.level) / 2)
    
    return(list(
      stat = as.numeric(-2 * logLik(arfit)), 
      trend = beta,
      annual_trend = annual_b,
      annual_ci_low = annual_b - (crit_val * se_adjusted) / n,
      annual_ci_high = annual_b + (crit_val * se_adjusted) / n
    ))
  }
  
  runmin = function(v, k) {
    out = NULL
    for (i in 1:(length(v) - k + 1)) {
      out = c(out, min(v[i:(i + k - 1)], na.rm = TRUE))
    }
    return(out)
  }
  
  minseglen = 10
  n = nrow(regional_averages)
  num_regions = ncol(regional_averages) - 1
  test.stat = matrix(NA, nrow = n, ncol = num_regions)
  
  for (i in 2:ncol(regional_averages)) {
    for (tau in minseglen:(n - minseglen)) {
      first = firstseg(regional_averages[1:tau, i], start = 0)
      if (first$stat < 10^{16}) {
        second = secondseg(regional_averages[(tau + 1):n, i], start = tau, previousbeta = first$trend)
        test.stat[tau, i - 1] = first$stat + second$stat
      }
    }
  }
  
  output_list = list()
  alpha = 0.05
  
  for (j in 1:num_regions) {
    region_name = colnames(regional_averages)[j + 1]
    x = test.stat[, j]
    opt_idx = which.min(x)
    opt_year = regional_averages[opt_idx, 1]
    
    opt_first = firstseg(regional_averages[1:opt_idx, j + 1], start = 0)
    opt_second = secondseg(regional_averages[(opt_idx + 1):n, j + 1], start = opt_idx, previousbeta = opt_first$trend)
    
    cpts = c(0, opt_idx, length(x))
    nj.maxi = max(runmin(diff(cpts), 2))
    CS.thresh = 3/2 + 2 * log(2 * nj.maxi) - 2 * log(alpha) - log(n)
    cpt_CI = regional_averages[which(x < min(x, na.rm = TRUE) + CS.thresh), 1]
    
    output_list[[region_name]] = list(
      Estimated_Changepoint_Year = opt_year,
      Changepoint_CI_Years = cpt_CI,
      Pre_Break_Annual_Slope = opt_first$annual_trend,
      Pre_Break_Annual_Slope_CI = c(opt_first$annual_ci_low, opt_first$annual_ci_high),
      Post_Break_Annual_Slope = opt_second$annual_trend,
      Post_Break_Annual_Slope_CI = c(opt_second$annual_ci_low, opt_second$annual_ci_high)
    )
  }
  
  return(output_list)
}
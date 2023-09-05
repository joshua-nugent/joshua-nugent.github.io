library(tidyverse)
library(emmeans)

sim_data <- function(n = 500, beta_main = .15, beta_baseline = 1,
                     beta_RTM = .5, beta_interaction = 0,
                     baseline_imbalance = T){
  id <- 1:n
  baseline <- runif(n, min = 2, max = 10)
  trt <- rbinom(n, size = 1, prob = .5)
  UY <- rnorm(n, sd = .5)
  if(baseline_imbalance){
    extra_baseline <- rnorm(n, mean = 1, sd = .5)
    baseline[as.logical(trt)] <- baseline[as.logical(trt)] + extra_baseline[as.logical(trt)]
  }
  follow_up1 <- beta_baseline*baseline - beta_RTM*(baseline-mean(baseline)) + beta_main*1 + beta_interaction*baseline*1 + UY
  follow_up0 <- beta_baseline*baseline - beta_RTM*(baseline-mean(baseline)) + beta_main*0 + beta_interaction*baseline*0 + UY
  follow_up <- ifelse(trt, follow_up1, follow_up0)
  change <- follow_up - baseline
  true_diff <- mean(follow_up1-follow_up0)
  return(cbind.data.frame(id, baseline, trt, follow_up, change, true_diff))  
}


run_it <- function(beta_RTM = .5,
                   beta_interaction = 0,
                   beta_main = .15,
                   baseline_imbalance = T,
                   change_score = F, include_baseline = F){
  
  dat <- sim_data(beta_RTM = beta_RTM,
                  beta_interaction = beta_interaction,
                  beta_main = beta_main,
                  baseline_imbalance = baseline_imbalance)
  true_diff <- dat$true_diff[1]
  
  if(beta_interaction != 0){
    emm <- contrast(emmeans(lm(follow_up ~ baseline + trt + baseline*trt, data = dat), ~ trt*baseline),
                    method = "revpairwise", adjust = "none")
    hi <- confint(emm)[["upper.CL"]]
    lo <- confint(emm)[["lower.CL"]]
    est <- confint(emm)[["estimate"]]
    bias <- est - true_diff
    cover <- lo < true_diff & hi > true_diff
    pwr <- (lo > 0 | hi < 0)
  } else {
    if(change_score){
      if(include_baseline){
        mod <- lm(change ~ baseline + trt, data = dat)
      } else {
        mod <- lm(change ~ trt, data = dat)
      }
    } else {
      if(include_baseline){
        mod <- lm(follow_up ~ baseline + trt, data = dat)
      } else {
        mod <- lm(follow_up ~ trt, data = dat)
      }
    }
    lo <- confint(mod)[["trt",1]]
    hi <- confint(mod)[["trt",2]]
    est <- coef(mod)[["trt"]]
    cover <- true_diff > lo & true_diff < hi
    bias <- est - true_diff
    pwr <- (lo > 0 | hi < 0)
  }
  return(c(change_score = change_score, include_baseline = include_baseline, true_diff = true_diff,
           cover = cover, bias = bias, pwr = pwr, lo = lo, hi = hi, est = est))
}





library(tidyverse)

add_lower_upper <- function(dat){
  lower <- vector(length = length(dat$TIE_naive))
  upper <- vector(length = length(dat$TIE_naive))
  for(i in 1:length(dat$TIE_naive)){
    lower[i] <- binom.test(x = round(dat$TIE_naive[i] * dat$nsim[i]), n = dat$nsim[i])[[4]][1]
    upper[i] <- binom.test(x = round(dat$TIE_naive[i] * dat$nsim[i]), n = dat$nsim[i])[[4]][2]
  }
  dat <- cbind.data.frame(dat, lower, upper)
  return(dat)
}

sas_lrt <- readRDS("lrt_sas_10000sims.rds")
sas_lrt <- add_lower_upper(sas_lrt) %>% mutate(Method = "LRT, SAS", package = "SAS", method = "LRT", df = "LRT", Approach = "LRT")
sas_model <- readRDS("model_sas_default_10000sims.rds")
sas_model <- add_lower_upper(sas_model) %>% mutate(Method = "Wald, SAS, default", package = "SAS", method = "Wald", df = "default", Approach = "Wald, residual DF")

sas_model_bw <- readRDS("model_sas_bw_10000sims.rds")
sas_model_bw <- add_lower_upper(sas_model_bw) %>% mutate(Method = "Wald, SAS, BW", package = "SAS", method = "Wald", df = "BW", Approach = "Wald, between-within DF")

sas_model_sat <- readRDS("model_sas_sat_10000sims.rds")
sas_model_sat <- add_lower_upper(sas_model_sat) %>% mutate(Method = "Wald, SAS, Satt", package = "SAS", method = "Wald", df = "Satt", Approach = "Wald, approximate DF")

all_dat <- rbind.data.frame(sas_lrt, sas_model, sas_model_sat,
                            sas_model_bw) %>% mutate(ICC = signif((sb2 / (se2 + sb2)), digits = 2)) %>% mutate(labl = paste0(sb2,"\n",ICC))

mylabel1 <- label_bquote(cols = atop(sigma[b]^2== .(sb2)*"," ~~ sigma^2 == 1, "ICC" ==.(signif((sb2 / (1 + sb2)), digits = 2))), rows = "Obs per cluster" == .(nsub))

ggplot(data = all_dat) +
  geom_hline(yintercept = .05, color = "black", alpha = .6) + 
  geom_line(aes(x = as.factor(nclust*2), y = TIE_naive, group = Approach, color = Approach, linetype = Approach), alpha = .5) +
  geom_pointrange(aes(x = as.factor(nclust*2), y = TIE_naive, ymin = lower, ymax = upper, fatten = 2, color = Approach)) +
  labs(y = "Type I error rate", x = "Clusters") +
  facet_grid(cols = vars(sb2), rows = vars(nsub), labeller = mylabel1) +
  ylim(c(.012, .1)) +
  theme(
    axis.text.x = element_text(size = 10),
    axis.text.y = element_text(size = 10),  
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    strip.text.x = element_text(size = 9),
    strip.text.y = element_text(size = 9),
    legend.text=element_text(size=16),
    legend.position = "bottom",legend.title=element_blank())

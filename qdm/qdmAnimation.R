library(tidyquant)
library(tidyverse)
library(timetk)
library(broom)
library(highcharter)
library(gganimate)
library(vapoRwave)
library(extrafont)

symbols <- c("SPY","EFA", "IJS", "EEM","AGG")

prices <- 
  getSymbols(symbols, src = 'yahoo', 
             from = "2012-12-31",
             to = "2017-12-31",
             auto.assign = TRUE, warnings = FALSE) %>% 
  map(~Ad(get(.))) %>%
  reduce(merge) %>% 
  `colnames<-`(symbols)

w <- c(0.25, 0.25, 0.20, 0.20, 0.10)

asset_returns_long <-  
  prices %>% 
  to.monthly(indexAt = "lastof", OHLC = FALSE) %>% 
  tk_tbl(preserve_index = TRUE, rename_index = "date") %>%
  gather(asset, returns, -date) %>% 
  group_by(asset) %>%  
  mutate(returns = (log(returns) - log(lag(returns)))) %>% 
  na.omit()

portfolio_returns_tq_rebalanced_monthly <- 
  asset_returns_long %>%
  tq_portfolio(assets_col  = asset, 
               returns_col = returns,
               weights     = w,
               col_rename  = "returns",
               rebalance_on = "months")

mean_port_return <- 
  mean(portfolio_returns_tq_rebalanced_monthly$returns)

stddev_port_return <- 
  sd(portfolio_returns_tq_rebalanced_monthly$returns)


simulation_accum_1 <- function(init_value, N, mean, stdev) {
  tibble(c(init_value, 1 + rnorm(N, mean, stdev))) %>% 
    `colnames<-`("returns") %>%
    mutate(growth = 
             accumulate(returns, 
                        function(x, y) x * y)) %>% 
    select(growth)
}

sims <- 51
starts <- 
  rep(1, sims) %>%
  set_names(paste("sim", 1:sims, sep = ""))

monte_carlo_sim_51 <- 
  map_dfc(starts, 
          simulation_accum_1, 
          N = 120, 
          mean = mean_port_return, 
          stdev = stddev_port_return)

monte_carlo_sim_51 <- 
  monte_carlo_sim_51 %>% 
  mutate(month = seq(1:nrow(.))) %>% 
  select(month, everything()) %>% 
  `colnames<-`(c("month", names(starts))) %>% 
  mutate_all(funs(round(., 2))) 

reruns <- 51

monte_carlo_rerun_51 <- 
  rerun(.n = reruns, 
        simulation_accum_1(1, 
                           120,
                           mean_port_return, 
                           stddev_port_return)) %>%
  simplify_all() %>% 
  `names<-`(paste("sim", 1:reruns, sep = " ")) %>%
  as_tibble() %>% 
  mutate(month = seq(1:nrow(.))) %>% 
  select(month, everything())

monte_carlo_sim_51 %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>% 
  ggplot(aes(x = month, y = growth, color = sim)) + 
  geom_line(alpha = .5, size = 1) +
  transition_reveal(month) +
  new_retro() +
  labs(title = "Quantitative\nDecision\nModeling") +
  theme(legend.position = "", legend.background = element_blank(),
        legend.key = element_blank(),  
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.line = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_blank(), 
        plot.caption = element_blank(), 
        strip.background = element_blank(), strip.text = element_blank())

monte_carlo_sim_51 %>% 
  gather(sim, growth, -month) %>% 
  group_by(sim) %>% 
  ggplot(aes(x = month, y = growth, color = sim)) + 
  geom_line(alpha = .5, size = 1) +
  new_retro() +
  labs(title = "Quantitative\nDecision\nModeling") +
  theme(legend.position = "", legend.background = element_blank(),
        legend.key = element_blank(),  
        axis.title = element_blank(), 
        axis.text = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(), 
        axis.ticks = element_blank(), axis.line = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), 
        panel.grid.major.x = element_blank(), 
        panel.border = element_blank(), 
        plot.caption = element_blank(), 
        strip.background = element_blank(), strip.text = element_blank())

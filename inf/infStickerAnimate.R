library(dplyr)

library(glue)

library(gganimate)

library(ggplot2)

library(hexSticker)

library(extrafont)

library(vapoRwave)

testDat = data.frame(x = rnorm(100, mean = 10, sd = 1.5), 
                     y = rnorm(100, mean = 10, sd = 1.5), 
                     z = sample(c("a", "b", "c"), 100, replace = TRUE))

testDat = testDat %>% 
  mutate(x = ifelse(z == "a", x + rnorm(100, mean = 5, 1), x), 
         x = ifelse(z == "c", x - rnorm(100, mean = 5, 1), x), 
         y = ifelse(z == "a", y + rnorm(100, mean = 5, 1), y), 
         y = ifelse(z == "c", y - rnorm(100, mean = 5, 1), y))

testDat[101:500,] = testDat[1:100, ]

dotBoys = c(".", "..", "...",
  "....", ".....")

testDat$time = rep(1:5, each = 100)

testDat = testDat %>% 
  mutate(x = ifelse(time == 2, x + rnorm(100, mean = 2, 1), 
                    ifelse(time == 3, x + rnorm(100, mean = 5, 1), 
                           ifelse(time == 4, x + rnorm(100, mean = 8, 1), 
                                  ifelse(time == 5, x + rnorm(100, mean = 10, 1), x)))), 
         y = ifelse(time == 2, y + rnorm(100, mean = 2.5, 1), 
                    ifelse(time == 3, y + rnorm(100, mean = 5.5, 1), 
                           ifelse(time == 4, y + rnorm(100, mean = 8.5, 1), 
                                  ifelse(time == 5, y + rnorm(100, mean = 10.5, 1), y)))))
testDat %>% 
  filter(time == 1) %>% 
ggplot(., aes(x, y, color = z)) +
  geom_point(size = 2.5, alpha = .75) +
  geom_smooth(size = 1.5, alpha = .5, 
              method = "lm", se = FALSE) +
  new_retro() +
  scale_color_newRetro() +
  labs(title = "Advanced\nStatistical\nInference", 
       subtitle = "Transmission Engaged...") +
  theme(legend.position = "", legend.background = element_blank(),
        legend.key = element_blank(), axis.title = element_blank(), 
        axis.text = element_blank(), axis.text.x = element_blank(), 
        axis.text.y = element_blank(), axis.ticks = element_blank(), 
        axis.line = element_blank(), panel.grid.minor = element_blank(), 
        panel.grid.major.y = element_blank(), panel.grid.major.x = element_blank(), 
        panel.border = element_blank(), plot.caption = element_blank(), 
        strip.background = element_blank(), strip.text = element_blank())

out = ggplot(testDat, aes(x, y, color = z)) +
  geom_point(size = 2.5, alpha = .75) +
  geom_smooth(size = 1.5, alpha = .5, 
              method = "lm", se = FALSE) +
  transition_states(time, 1, 2) +
  shadow_wake(wake_length = 0.1, alpha = FALSE) +
  new_retro() +
  scale_color_newRetro() +
  labs(title = "Advanced\nStatistical\nInference", 
       subtitle = "Transmission Engaged{strrep('.', frame/20)}") +
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

out

gganimate::anim_save("inf.gif", out)

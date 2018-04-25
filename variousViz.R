library(psych)
library(ggridges)
library(viridis)
library(ggplot2)
library(dplyr)

scaleStuff = bfi %>% 
  dplyr::select(starts_with("C")) %>% 
  tidyr::gather(key = question, value = score) %>% 
  ggplot(., aes(score, question, fill = ..x..)) +
  geom_density_ridges_gradient() +
  scale_fill_viridis(option = "B") +
  theme_void() + 
  theme_transparent() +
  theme(legend.position="none")

library(hexSticker)

sticker("bds/Rlogo.png", package="", 
        s_x = 1, s_y = 1.05, 
        s_width = .8, s_height = .8,
        h_fill = "white", h_color = "#1e66b7",
        spotlight = TRUE, l_alpha = .2,
        filename = "bds/logoHex.png")

sticker(scaleStuff, package="", 
        s_x = 1, s_y = 1.00, 
        s_width = 1.3, s_height = 1.3,
        h_fill = "white", h_color = "#1e66b7",
        spotlight = TRUE, l_alpha = .2,
        filename = "bds/test.png")

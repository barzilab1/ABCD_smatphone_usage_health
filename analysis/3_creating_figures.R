library(tidyverse)
library(ggplot2)
library(ggeffects)
library(ggpubr)


source("analysis/analysis_utility_fun.R", echo=TRUE)


#  UNIVERSAL THEME----

theme_abcd <- function(base_size = 18) {
  theme_bw(base_size = base_size) +
    theme(
      axis.text.x  = element_text(size = base_size + 4, face = "bold", color = "black"),
      axis.text.y  = element_text(size = base_size + 4, face = "bold", color = "black"),
      axis.title.x = element_text(size = base_size + 5, face = "bold", color = "black"),
      axis.title.y = element_text(size = base_size + 5, face = "bold", color = "black"),
      legend.title = element_blank(),
      legend.text  = element_text(size = base_size + 3, face = "bold"),
      legend.position = "top",
      panel.grid.major = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      plot.title = element_text(size = base_size + 6, face = "bold", hjust = 0.5)
    )
}


#  FIGURE 1----

df_aim1 <- tibble(
  Outcome = factor(c("Depression", "Obesity", "Insufficient Sleep"),
                   levels = c("Insufficient Sleep", "Obesity", "Depression")),
  OR      = c(1.24, 0.91, 1.37),
  CI_low  = c(0.82, 0.57, 1.06),
  CI_high = c(1.87, 1.45, 1.76),
  N       = c(1515, 1319, 1676)
)

fig_aim1 <- make_forest_plot(
  df_aim1,
  x = OR,
  y = Outcome,
  xmin = CI_low,
  xmax = CI_high,
  n_var = N,
  min_x = 0.4,
  max_x = 2.05,
  x_breaks = seq(0.5, 2.0, 0.5),
  title = "Past-Year Smartphone Acquisition"
)

ggsave("plots/figure1.pdf", fig_aim1, width = 12, height = 8, dpi = 320)



#  FIGURE 2----

df_aim2 <- tibble(
  Outcome = factor(c("Depression", "Obesity", "Insufficient Sleep"),
                   levels = c("Insufficient Sleep", "Obesity", "Depression")),
  OR      = c(1.34, 1.29, 1.35),
  CI_low  = c(1.07, 0.96, 1.15),
  CI_high = c(1.69, 1.73, 1.59),
  N       = c(901, 797, 1006)
)


fig_aim2a <- make_forest_plot(
  df_aim2,
  x = OR,
  y = Outcome,
  xmin = CI_low,
  xmax = CI_high,
  n_var = N,
  min_x = 0.7,
  max_x = 2.05,
  x_breaks = seq(0.5, 2.0, 0.5),
  title = "Total Smartphone-Time"
)


ggsave("plots/figure2.pdf", fig_aim2a, width = 12, height = 8, dpi = 320)


#  FIGURE 3----
df_depression_dos <- tribble(
  ~Category,  ~OR, ~CI_low, ~CI_high, ~N,
  "2-5h/day",   1.61, 0.97, 2.66, 901,
  ">5h/day",    2.87, 1.37, 6.04, 901
) %>%
  mutate(
    Category = factor(Category,
                      levels = c("2-5h/day", ">5h/day"))
  )

fig_depression_dos <- make_forest_plot(
  df_depression_dos,
  x = OR,
  y = Category,
  xmin = CI_low,
  xmax = CI_high,
  n_var = N,
  min_x = 0.5,
  max_x = 7,
  x_breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7),
  title = "Depression",
  y_label = "Daily Smartphone Use Duration"
)



df_sleep_dos <- tribble(
  ~Category,  ~OR, ~CI_low, ~CI_high, ~N,
  "2-5h/day",  1.62, 1.17, 2.23, 1006,
  ">5h/day",   2.03, 1.22, 3.39, 1006
) %>%
  mutate(
    Category = factor(Category,
                      levels = c(">5h/day", "2-5h/day"))
  )

fig_sleep_dos <- make_forest_plot(
  df_sleep_dos,
  x = OR,
  y = Category,
  xmin = CI_low,
  xmax = CI_high,
  n_var = N,
  min_x = 0.5,
  max_x = 7,
  x_breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7),
  title = "Insufficient Sleep",
  y_label = "Daily Smartphone Use Duration"
)


spacer <- ggplot() + theme_void()

row1 <- ggarrange(fig_depression_dos, labels = c("A"), font.label = list(size = 24, color = "black"))
row2 <- ggarrange(fig_sleep_dos, labels = c("B"), font.label = list(size = 24, color = "black"))

figure_3 <- ggarrange(
    row1,
    spacer,
    row2,
    ncol = 1,
    heights = c(1, 0.08, 1),
    align = "hv"
)


ggsave("plots/figure_3.pdf", figure_3, width = 12, height = 14, dpi = 320)


#  FIGURE 4----

df_outside_room <- tibble(
  Outcome = factor(c("Depression", "Insufficient Sleep"),
                   levels = c("Insufficient Sleep", "Depression")),
  OR      = c(0.97, 0.68),
  CI_low  = c(0.56, 0.49),
  CI_high = c(1.66, 0.95),
  N       = c(896, 1002)
)

fig_df_outside_room <- make_forest_plot(
  df_outside_room,
  x = OR,
  y = Outcome,
  xmin = CI_low,
  xmax = CI_high,
  n_var = N,
  min_x = 0.45,
  max_x = 2.05,
  x_breaks = seq(0.5, 2.0, 0.5),
  title = "Smartphone Out of Bedroom"
)


ggsave("plots/figure4.pdf", fig_df_outside_room, width = 12, height = 8, dpi = 320)























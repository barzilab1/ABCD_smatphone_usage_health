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
## No need in the revision # May remove later
# df_aim1 <- tibble(
#   Outcome = factor(c("Depression", "Obesity", "Insufficient Sleep"),
#                    levels = c("Insufficient Sleep", "Obesity", "Depression")),
#   OR      = c(1.24, 0.91, 1.37),
#   CI_low  = c(0.82, 0.57, 1.06),
#   CI_high = c(1.87, 1.45, 1.76),
#   N       = c(1515, 1319, 1676)
# )
# 
# fig_aim1 <- make_forest_plot(
#   df_aim1,
#   x = OR,
#   y = Outcome,
#   xmin = CI_low,
#   xmax = CI_high,
#   n_var = N,
#   min_x = 0.4,
#   max_x = 2.05,
#   x_breaks = seq(0.5, 2.0, 0.5),
#   title = "Past-Year Smartphone Acquisition"
# )
# 
# ggsave("plots/figure1.pdf", fig_aim1, width = 12, height = 8, dpi = 320)



#  FIGURE 1----
## eTable 3
df_aim2 <- tibble(
  Outcome = factor(c("Depression", "Obesity", "Insufficient Sleep"),
                   levels = c("Insufficient Sleep", "Obesity", "Depression")),
  OR      = c(1.22, 1.34, 1.28),
  CI_low  = c(1.005, 1.09, 1.12),
  CI_high = c(1.8, 1.65, 1.47),
  N       = c(1097, 1108, 1228)
)

fig_aim2 <- make_forest_plot(
  df_aim2,
  x = OR,
  y = Outcome,
  xmin = CI_low,
  xmax = CI_high,
  n_var = N,
  min_x = 0.7,
  max_x = 2.05,
  x_breaks = seq(0.5, 2.0, 0.5),
  title = "Total Smartphone-Time",
  y_label = "Health Outcome"
)


ggsave("plots/figure1.pdf", fig_aim2, width = 12, height = 8, dpi = 320)


#  FIGURE 2----
## eTable 6
df_depression_dos <- tribble(
  ~Category,  ~OR, ~CI_low, ~CI_high, ~N,
  "2-5h/day",   1.47, 0.93, 2.33, 1097,
  ">5h/day",    2.27, 1.16, 4.43, 1097
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


df_obesity_dos <- tribble(
  ~Category,  ~OR, ~CI_low, ~CI_high, ~N,
  "2-5h/day",  1.09, 0.68, 1.76, 1108,
  ">5h/day",   2.66, 1.38, 5.13, 1108
) %>%
  mutate(
    Category = factor(Category,
                      levels = c("2-5h/day", ">5h/day"))
  )

fig_obesity_dos <- make_forest_plot(
  df_obesity_dos,
  x = OR,
  y = Category,
  xmin = CI_low,
  xmax = CI_high,
  n_var = N,
  min_x = 0.5,
  max_x = 7,
  x_breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7),
  title = "Obesity",
  y_label = "Daily Smartphone Use Duration"
)



df_sleep_dos <- tribble(
  ~Category,  ~OR, ~CI_low, ~CI_high, ~N,
  "2-5h/day",  1.63, 1.22, 2.18, 1228,
  ">5h/day",   1.99, 1.28, 3.09, 1228
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
row2 <- ggarrange(fig_obesity_dos, labels = c("B"), font.label = list(size = 24, color = "black"))
row3 <- ggarrange(fig_sleep_dos, labels = c("C"), font.label = list(size = 24, color = "black"))

figure_2 <- ggarrange(
    row1,
    spacer,
    row2,
    spacer,
    row3,
    ncol = 1,
    heights = c(1, 0.08, 1, 0.08, 1),
    align = "hv"
)


ggsave("plots/figure_2.pdf", figure_2, width = 12, height = 20, dpi = 320)


#  FIGURE 3----
## eTable 7
df_outside_room <- tibble(
  Outcome = factor(c("Depression", "Obesity", "Insufficient Sleep"),
                   levels = c("Insufficient Sleep", "Obesity", "Depression")),
  OR      = c(0.80, 0.88, 0.64),
  CI_low  = c(0.49, 0.53, 0.47),
  CI_high = c(1.31, 1.46, 0.87),
  N       = c(1097, 1108, 1228)
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
  title = "Smartphone Out of Bedroom",
  y_label = "Health Outcome"
)


ggsave("plots/figure_3.pdf", fig_df_outside_room, width = 12, height = 8, dpi = 320)























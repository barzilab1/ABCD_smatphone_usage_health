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
  CI_low  = c(0.75, 0.52, 1.00),
  CI_high = c(2.05, 1.60, 1.86)
)

fig_aim1 <- make_forest_plot(
  df_aim1,
  color = "#007070",
  min_x = 0.4,
  max_x = 2.05,
  title = "Past-Year Smartphone Acquisition"
)

ggsave("plots/figure1_fixed.pdf", fig_aim1, width = 12, height = 8, dpi = 320)



#  FIGURE 2----

df_aim2 <- tibble(
  Outcome = factor(c("Depression", "Obesity", "Insufficient Sleep"),
                   levels = c("Insufficient Sleep", "Obesity", "Depression")),
  OR      = c(1.34, 1.29, 1.35),
  CI_low  = c(1.01, 0.90, 1.11),
  CI_high = c(1.78, 1.84, 1.64)
)

fig_aim2a <- make_forest_plot(
  df_aim2,
  color = "#007070",
  min_x = 0.7,
  max_x = 2,
  title = "Total Smartphone-Time"
)

ggsave("plots/figure2.pdf", fig_aim2a, width = 12, height = 8, dpi = 320)


#  FIGURE 3----

df_depression_dos <- tribble(
  ~Category,  ~OR, ~CI_low, ~CI_high,
  "<2 hours",   1.00, 1.00, 1.00,
  "2-5h/day",   1.61, 0.90, 2.86,
  ">5h/day",    2.87, 1.23, 6.72
) %>%
  mutate(
    Category = factor(
      Category,
      levels = c(
        "<2 hours",
        "2-5h/day",
        ">5h/day")))

fig_depression_dos <- make_forest_plot(
    df_depression_dos,
    y = Category,
    color = "#1B75BC",
    x_breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7),
    title = "Depression",
    y_title = "Daily Smartphone Use Duration"
    )


df_sleep_dos <- tribble(
  ~Category,  ~OR, ~CI_low, ~CI_high,
  "<2 hours",  1.00, 1.00, 1.00,
  "2-5h/day",  1.62, 1.12, 2.34,
  ">5h/day",   2.03, 1.13, 3.64
) %>%
  mutate(
    Category = factor(
      Category,
      levels = c(
          "<2 hours",
          "2-5h/day",
          ">5h/day")))

fig_sleep_dos <- make_forest_plot(
    df_sleep_dos,
    y = Category,
    color = "#1B75BC",
    x_breaks = c(0.5, 1, 2, 3, 4, 5, 6, 7),
    title = "Insufficient Sleep",
    y_title = "Daily Smartphone Use Duration")


df_outside_room <- tibble(
    Outcome = factor(c("Depression", "Insufficient Sleep"),
                     levels = c("Insufficient Sleep", "Depression")),
    OR      = c(0.97, 0.68),
    CI_low  = c(0.52, 0.46),
    CI_high = c(1.80, 0.99)
)

fig_df_outside_room <- make_forest_plot(
    df_outside_room,
    color = "#007070",
    min_x = 0.45,
    max_x = 2,
    title = "Smartphone Out of Bedroom "
)

spacer <- ggplot() + theme_void()

row1 <- ggarrange(fig_depression_dos, labels = c("A"), font.label = list(size = 24, color = "black"))
row2 <- ggarrange(fig_sleep_dos, labels = c("B"), font.label = list(size = 24, color = "black"))
row3 <- ggarrange(fig_df_outside_room, labels = c("C"), font.label = list(size = 24, color = "black"))

figure_3 <- ggarrange(
    row1,
    spacer,
    row2,
    spacer,
    row3,
    ncol = 1,
    heights = c(1, 0.08, 1, 0.08, 1),
    align = "hv"
)


ggsave("plots/figure_3.pdf",
       figure_3, width = 12, height = 18, dpi = 320)



























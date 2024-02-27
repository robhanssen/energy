library(tidyverse)
library(lubridate)
library(patchwork)

theme_set(
    theme_light() +
    theme(
        plot.title.position = "plot",
        plot.title = element_text(hjust = 0),
        panel.grid.minor = element_blank(),
        legend.position = "none",
        plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0)
    )
)

this_year <- year(today())

energy <- readRDS("Rdata/energy.rds")

byday <- energy %>%
    group_by(date) %>%
    summarize(
        use = sum(energy),
        .groups = "drop"
    ) %>%
    mutate(
        year = year(date),
        month = factor(month(date)),
        day = day(date)
    )

byday %>%
    slice_max(use, n = 30) %>%
    mutate(datelab = format(date, format = "%b %d, %Y")) %>%
    ggplot() +
    aes(fct_reorder(factor(datelab), use), use, fill = factor(year)) +
    geom_col() +
    # scale_x_discrete(labels = "%B %d, %Y") +
    coord_flip()

byday %>%
    slice_min(use, n = 30) %>%
    mutate(datelab = format(date, format = "%b %d, %Y")) %>%
    ggplot() +
    aes(fct_reorder(factor(datelab), use), use, fill = factor(year)) +
    geom_col() +
    # scale_x_discrete(labels = "%B %d, %Y") +
    coord_flip()


byday %>%
    ggplot() +
    aes(y = factor(year), x = use) +
    # geom_violin(draw_quantiles = .5)
    ggridges::geom_density_ridges()


byday %>%
    ggplot() +
    aes(date, use, color = month) +
    geom_point(show.legend = FALSE, shape = 1) + 
    scale_y_continuous(
        limits = c(0, NA),
        breaks = seq(0, 100, 10)
        ) +
    labs(x = "", y = "Daily electricity use (in kWh)",
        title = "Daily electricity use")
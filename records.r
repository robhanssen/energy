library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

this_year <- year(today())

datadir <- "./data-by-year"

energy <- list.files(path = datadir, full.names = TRUE) %>%
        map_df(~ read_csv(.))

byday <- energy %>%
        group_by(date) %>%
        summarize(
                use = sum(energy),
                .groups = "drop") %>%
        mutate(
                year = year(date),
                month = factor(month(date)),
                day = day(date)
        )

byday %>%
    slice_max(use, n = 30) %>%
    mutate(datelab = format(date, format = "%b %d, %Y")) %>%
    ggplot + 
    aes(fct_reorder(factor(datelab), use), use, fill = factor(year)) + 
    geom_col() +
    # scale_x_discrete(labels = "%B %d, %Y") +
    coord_flip()

byday %>%
    slice_min(use, n = 30) %>%
    mutate(datelab = format(date, format = "%b %d, %Y")) %>%
    ggplot + 
    aes(fct_reorder(factor(datelab), use), use, fill = factor(year)) + 
    geom_col() +
    # scale_x_discrete(labels = "%B %d, %Y") +
    coord_flip()


byday %>%
    ggplot +
    aes(x = factor(year), y = use) + 
    geom_violin(draw_quantiles = .5)


byday %>%
    ggplot + 
    aes(date, use, color = month) +
    geom_point()
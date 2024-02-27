library(tidyverse)
library(lubridate)
library(patchwork)
theme_set(theme_light())

this_year <- year(today())

datadir <- "./data-by-year"

energy <- list.files(path = datadir, full.names = TRUE) %>%
    map_df(~ read_csv(., show_col_types = FALSE))

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

model <- lm(use ~ month + day, data = byday)
dt <- model %>%
    broom::augment(interval = "confidence") %>%
    bind_cols(date = byday$date)

byday %>% ggplot() +
    aes(x = date, y = use) +
    geom_point() +
    scale_x_date(date_breaks = "3 months", date_labels = "%B %d") +
    geom_line(data = dt, aes(y = .fitted)) +
    geom_ribbon(
        data = dt,
        aes(y = .fitted, ymax = .upper, ymin = .lower),
        fill = "red",
        alpha = .5
    )
# by month

bymonth <-
    energy %>%
    filter(year != year(today())) %>%
    group_by(year, month) %>%
    summarize(energy = sum(energy), .groups = "drop") %>%
    group_by(month) %>%
    summarize(energy = mean(energy), .groups = "drop") %>%
    mutate(monthname = factor(month.abb[month], levels = month.abb)) %>%
    mutate(totaluse = cumsum(energy))


bymonth_thisyear <-
    energy %>%
    filter(year == year(today())) %>%
    group_by(month) %>%
    summarize(energy = sum(energy), .groups = "drop") %>%
    mutate(monthname = factor(month.abb[month], levels = month.abb)) %>%
    mutate(totaluse = cumsum(energy))

monthgraph <-
    bymonth %>%
    ggplot() +
    aes(x = monthname, y = energy, group = FALSE) +
    # geom_line() +
    geom_ribbon(aes(ymin = 0, ymax = energy), alpha = .5) +
    geom_line(
        data = bymonth_thisyear,
        aes(group = FALSE),
        lty = 2,
        linewidth = 1
    ) +
    geom_point(data = bymonth_thisyear) +
    labs(
        x = "Month",
        y = "Average monthly energy use (in kWh)",
        title = "Comparison of historical vs current monthly energy use"
    ) +
    scale_y_continuous(labels = scales::comma_format())

# cumulative graph
cumgraph <-
    bymonth %>%
    ggplot() +
    aes(x = monthname, y = totaluse, group = FALSE) +
    geom_ribbon(aes(ymin = 0, ymax = totaluse), alpha = .5) +
    geom_line(
        data = bymonth_thisyear,
        aes(group = FALSE),
        lty = 2, linewidth = 1
    ) +
    geom_point(data = bymonth_thisyear) +
    labs(
        x = "Month",
        y = "Average monthly energy use (in kWh)"
    ) +
    scale_y_continuous(labels = scales::comma_format())

plot <- monthgraph + cumgraph
ggsave("graphs/monthcomparison.png", width = 12, height = 6, plot = plot)


# this month

monthsago <- function(x) {
    lubridate::floor_date(
        lubridate::today() - months(x),
        "month"
    )
}

plotrange <- 6

byday %>%
    filter(date > monthsago(plotrange)) %>%
    ggplot() +
    aes(x = date, y = use) +
    geom_point() +
    scale_x_date(
        date_breaks = "3 months",
        date_labels = "%B %d",
        limits = c(monthsago(plotrange), today())
    ) +
    geom_line(data = dt, aes(y = .fitted)) +
    geom_ribbon(
        data = dt, aes(
            y = .fitted,
            ymax = .upper,
            ymin = .lower
        ),
        fill = "red", alpha = .5
    )

#
#  What is the average daily and monthly use?
#

# daily use
dailyuse <- energy %>%
    group_by(date) %>%
    summarize(dailyuse = sum(energy)) %>%
    mutate(runningdate = month(date) * 100 + day(date))

# average use on a single day in the year, and rollmean that over 14 days
averagedailyuse <- dailyuse %>%
    group_by(runningdate) %>%
    summarize(dayuse = mean(dailyuse))

rollingdailyuse <- bind_rows(
    tail(averagedailyuse, 6),
    averagedailyuse,
    head(averagedailyuse, 7)
) %>%
    mutate(rollingmean = zoo::rollmean(dayuse,
        14,
        na.pad = TRUE
    )) %>%
    filter(!is.na(rollingmean))

thisyear <- inner_join(dailyuse, rollingdailyuse) %>%
    filter(date >= floor_date(today(), "1 year")) %>%
    mutate(
        cumuse = cumsum(dailyuse),
        cumeaveuse = cumsum(rollingmean)
    )

thisyear %>%
    ggplot() +
    aes(x = date, y = dailyuse) +
    geom_line(aes(y = rollingmean), color = "red", lty = 1, linewidth = 1) +
    geom_line(aes(y = zoo::rollmean(dailyuse, 7, na.pad = TRUE))) +
    expand_limits(y = 0, x = ceiling_date(today(), "1 month")) +
    theme_light() +
    labs(
        x = paste0("Date (in ", this_year, ")"),
        y = "Daily energy use (in kWh)",
        caption = "Red line is long-term average\nBlack line is current 14-days rolling average" # nolint
    )


ggsave("graphs/thisyearcomparedtoalltime.png", width = 8, height = 6)

thisyear %>%
    ggplot() +
    aes(x = date, y = cumuse) +
    geom_point(color = "black", fill = "black", alpha = .2) +
    geom_line(aes(y = cumeaveuse)) +
    scale_x_date(date_breaks = "2 months", date_label = "%b %Y")

#
# cumulative use with current year highlighted
#

this_year <- year(today())

cumulative_use <-
    energy %>%
    # group_by(date) %>%
    # summarize(
    #         energy = sum(energy),
    #         .groups = "drop"
    # ) %>%
    # mutate(year = year(date)) %>%
    group_by(year) %>%
    mutate(
        yearuse = cumsum(energy),
        color = case_when(
            year == year(today()) ~ "red",
            TRUE ~ "gray50"
        ),
        scaled_date = date + years(this_year - year)
    ) %>%
    ungroup() %>%
    mutate(yearuse = case_when(
        year == 2019 ~ 9313 - 6193 + yearuse,
        TRUE ~ yearuse
    ))

min_year <- min(cumulative_use$year)
max_year <- max(cumulative_use$year)
yearcolorrange <- as_factor(min_year:max_year)
yearcolor <- c(rep("gray50", length(yearcolorrange) - 2), "black", "red")
linetypes <- c(rep("dotted", length(yearcolorrange) - 2), "dashed", "solid")
linsizes <- c(rep(0.5, length(yearcolorrange) - 2), 0.75, 1.5)

cumulative_use %>%
    ggplot() +
    aes(
        x = scaled_date,
        y = yearuse,
        color = factor(year, levels = min_year:max_year),
        linewidth = factor(year, levels = min_year:max_year),
        linetype = factor(year, levels = min_year:max_year)
    ) +
    geom_line() +
    scale_color_manual(values = yearcolor) +
    scale_linetype_manual(values = linetypes) +
    scale_linewidth_manual(values = linsizes) +
    scale_x_date(date_labels = "%b") +
    scale_y_continuous(
        labels = scales::comma_format(),
        breaks = 2000 * 0:10
    ) +
    labs(
        x = "Date",
        y = "Cumulative annual use (in kWh)",
        title = "Comparison of cumulative use of electricity",
        caption = paste0(
            "Red line: ",
            this_year,
            "\nBlack line: ",
            this_year - 1,
            "\nGray lines: ",
            min_year,
            "-",
            max_year - 2
        )
    ) +
    theme(legend.position = "none")

ggsave("graphs/cumulative-use.png", width = 6, height = 6)

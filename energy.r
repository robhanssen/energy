library(tidyverse)
library(lubridate)
theme_set(theme_light())

this_year <- year(today())

datadir <- "./data-by-year"

energy <- list.files(path = datadir, full.names = TRUE) %>%
                map_df(~ read_csv(.))

byday <- energy %>%
            group_by(date) %>%
            summarize(use = sum(energy),
                      year = year,
                      month = factor(month),
                      day = day
                        )

model <- lm(use ~ month + day, data = byday)
dt <- model %>%
        broom::augment(interval = "confidence") %>%
        bind_cols(date = byday$date)

byday %>% ggplot +
            aes(x = date, y = use) +
            geom_point() +
            scale_x_date(date_breaks = "3 months", date_labels = "%B %d") +
            geom_line(data = dt, aes(y = .fitted)) +
            geom_ribbon(data = dt,
                        aes(y = .fitted, ymax = .upper, ymin = .lower),
                        fill = "red",
                        alpha = .5)

# this month

monthsago <- function(x) {
                      lubridate::floor_date(lubridate::today() - months(x),
                                            "month"
                                            )
                }

plotrange <- 6

byday %>%
        filter(date > monthsago(plotrange)) %>%
        ggplot +
            aes(x = date, y = use) +
            geom_point() +
            scale_x_date(date_breaks = "3 months",
                          date_labels = "%B %d",
                          limits = c(monthsago(plotrange), today())) +
            geom_line(data = dt, aes(y = .fitted)) +
            geom_ribbon(data = dt, aes(y = .fitted,
                                      ymax = .upper,
                                      ymin = .lower),
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

rollingdailyuse <- bind_rows(tail(averagedailyuse, 6),
                            averagedailyuse,
                            head(averagedailyuse, 7)) %>%
                    mutate(rollingmean = zoo::rollmean(dayuse,
                                                        14,
                                                        na.pad = TRUE)
                                                        ) %>%
                    filter(!is.na(rollingmean))

thisyear <- inner_join(dailyuse, rollingdailyuse) %>%
            filter(date >= floor_date(today(), "1 year")) %>%
            mutate(cumuse = cumsum(dailyuse),
                   cumeaveuse = cumsum(rollingmean)
                   )

(thisyear %>%
            ggplot()
            + aes(x = date, y = dailyuse)
            + geom_line(aes(y = rollingmean), color = "red", lty = 1, size = 1)
            + geom_line(aes(y = zoo::rollmean(dailyuse, 7, na.pad = TRUE)))
            + expand_limits(y = 0, x = ceiling_date(today(), "1 month"))
            + theme_light()
            + labs(x = paste0("Date (in ", this_year, ")"),
                   y = "Daily energy use (in kWh)",
                   caption = "Red line is long-term average\nBlack line is current 14-days rolling average")
)

ggsave("graphs/thisyearcomparedtoalltime.pdf", width = 11, height = 8)

thisyear %>%
            ggplot +
            aes(x = date, y = cumuse) +
            geom_point(color = "black", fill = "black", alpha = .2) +
            geom_line(aes(y = cumeaveuse)) +
            scale_x_date(date_breaks = "2 months", date_label = "%b %Y")
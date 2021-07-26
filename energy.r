library(tidyverse)
library(lubridate)
library(xml2)

datafile <- "source/energyusage20210726.xml"

data <- read_xml(datafile)

time <- xml_find_all(data, "//espi:start") %>%
                as_list() %>%
                unlist() %>%
                as.numeric()

time <- time[2:length(time)]

value <- xml_find_all(data, "//espi:value") %>%
                as_list() %>% 
                unlist() %>% 
                as.numeric()

energy <- tibble(datetime = as_datetime(time), energy = value) %>%
                mutate(date = as.Date(datetime, format = "%Y-%m-%d"),
                        year = year(datetime),
                        month = month(datetime),
                        day = day(datetime),
                        hour = hour(datetime),
                        minute = minute(datetime),
                        runningdate = 100 * month + day
                )


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
            geom_ribbon(data = dt, aes(y = .fitted, ymax = .upper, ymin = .lower), fill = "red", alpha = .5) 

# this month

monthsago <- function(x) {
                      lubridate::floor_date(lubridate::today() - months(x), 
                                            "month"
                                            )
                }

PLOTRANGE = 6

byday %>%
        filter(date > monthsago(PLOTRANGE)) %>%
        ggplot +
            aes(x = date, y = use) +
            geom_point() +
            scale_x_date(date_breaks = "1 week",
                          date_labels = "%B %d",
                          limits = c(monthsago(PLOTRANGE), today())) +
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
            filter(date > floor_date(today(),"1 year")) %>%
            mutate(cumuse = cumsum(dailyuse),
                   cumeaveuse = cumsum(rollingmean)
                   )

 (thisyear %>%
            ggplot()
            + aes(x = date, y = dailyuse) 
            + geom_point(color = "black", fill = "black", alpha = 0.2)
            + geom_line(aes(y = rollingmean), color = "green")
            + geom_line(aes(y = zoo::rollmean(dailyuse, 14, na.pad = TRUE)))
            + geom_vline(xintercept = as.Date("2021-06-14"))
            + theme_light() 
            + labs(x = "Date", 
                   y = "Daily energy use (in kWh)", 
                   caption = "Green line is long-term average\nBlack line is 14-days rolling average")
)

ggsave("thisyearcomparedtoalltime.pdf", width=11, height=8)

thisyear %>%
            ggplot +
            aes(x = date, y =cumuse) +
            geom_point(color = "black", fill = "black", alpha = .2) +
            geom_line(aes(y = cumeaveuse)) +
            scale_x_date(date_breaks = "2 months", date_label = "%b %Y")


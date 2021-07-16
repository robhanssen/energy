library(tidyverse)
library(lubridate)

datafile <- "source/energyusage.txt"

energy <- read_csv(datafile) %>% 
            select(!starts_with("espi")) %>% 
            mutate(date = as.Date(datetime, format="%Y-%m-%d"), 
                   year=year(datetime),
                   month=month(datetime),
                   day=day(datetime),
                   hour=hour(datetime),
                   minute=minute(datetime)
                )

byday <- energy %>% 
            group_by(date) %>%
            summarize(use=sum(energy), 
                      year=year,
                      month=factor(month),
                      day=day
                        )

model <- lm(use ~ month + day, data=byday)
dt <- model %>% broom::augment(interval="confidence") %>% bind_cols(date=byday$date)

byday %>% ggplot + 
            aes(x=date, y=use) + 
            geom_point() + 
            scale_x_date(date_breaks="3 months", date_labels="%B %d") + 
            geom_line(data=dt, aes(y=.fitted)) + 
            geom_ribbon(data=dt, aes(y=.fitted,ymax=.upper,ymin=.lower), fill="red", alpha=.5) #+
            # geom_smooth(method="loess")

# this month

monthsago <- function(x) 
                {
                    today() - months(x)
                }

PLOTRANGE = 6

byday %>% filter(date > monthsago(PLOTRANGE)) %>%
            ggplot +
            aes(x=date, y=use) + 
            geom_point() + 
            scale_x_date(date_breaks="1 week", date_labels="%B %d", limits=c(monthsago(PLOTRANGE),today())) + 
            
            geom_line(data=dt, aes(y=.fitted)) + 
            geom_ribbon(data=dt, aes(y=.fitted,ymax=.upper,ymin=.lower), fill="red", alpha=.5) #+
            # geom_smooth(method="loess")




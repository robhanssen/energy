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
                   minute=minute(datetime), 
                   runningdate = 100*month + day
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
                    lubridate::floor_date(today() - months(x), "month")
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


# byday2 <- energy %>% 
#             group_by(date) %>%
#             summarize(use=sum(energy), 
#                         ) %>% mutate(runningdate = 100*month(date) + day(date))



# dailyaverage <- energy %>% 
#                   group_by(date) %>%
#                   summarize(use=sum(energy)
#                               ) %>% 
#                   ungroup() %>%
#                   mutate(runningdate = 100*month(date) + day(date)) %>%
#                   group_by(runningdate) %>% 
#                   summarize(dailyaverage=mean(use), dailysd=sd(use), n=n(), lower=dailyaverage-1.96*dailysd/sqrt(n),upper=dailyaverage+1.96*dailysd/sqrt(n) ) %>%
#                   mutate(date=as.Date(paste0("2021-",as.character(runningdate %/% 100),"-",as.character(runningdate %% 100)))) %>%
#                   mutate(rmean = zoo::rollmean(dailyaverage, 14, na.pad=TRUE))

# deviate <- byday2 %>% inner_join(dailyaverage, by="runningdate") %>% mutate(deviation = use - rmean, diff=ifelse(deviation<0,"less","more")) 

# deviate %>% filter(date.x >= monthsago(6)) %>% ggplot + aes(x=date.x, y=deviation, color=diff) + geom_point()

# devmonth <- deviate %>% filter(date.x >= monthsago(6)) %>% group_by(month=month(date.x)) %>% summarize(monthuse = sum(use),rmonthuse=sum(dailyaverage)) 

# monthaverage <- energy %>% 
#                     group_by(year,month) %>% 
#                     summarize(monthlyuse = sum(energy)) %>% ungroup() %>%
#                     group_by(month) %>% 
#                     summarize(monthav = mean(monthlyuse), 
#                               monthsd = sd(monthlyuse),
#                               n = n(),
#                               upper = monthav + 1.96*monthsd/sqrt(n),
#                               lower = monthav - 1.96*monthsd/sqrt(n),
#                               )


# devmonth %>%
#             ggplot + 
#                 aes(x=month, y=monthuse) + 
#                 geom_line()  +
#                 geom_errorbar(data=monthaverage, aes(y=monthav, ymax=upper, ymin=lower), fill="green", alpha=.8)  +
#                 # geom_line(data=monthaverage, aes(y=monthav), color="blue") + 
#                 expand_limits(y=0) +
#                 scale_x_continuous(breaks=1:12)



# 
#  What is the average daily and monthly use?
# 

# daily use
dailyuse <- energy %>%
              group_by(date) %>%
              summarize(dailyuse = sum(energy)) %>% 
              mutate(runningdate = month(date)*100 + day(date))

# average use on a single day in the year, and rollmean that over 14 days
averagedailyuse <- dailyuse %>% 
                      group_by(runningdate) %>% 
                      summarize(dayuse = mean(dailyuse))

rollingdailyuse <- bind_rows(tail(averagedailyuse,6), 
                            averagedailyuse, 
                            head(averagedailyuse,7)) %>% 
                    mutate(rollingmean = zoo::rollmean(dayuse,14, na.pad=TRUE)) %>% 
                    filter(!is.na(rollingmean))

inner_join(dailyuse, rollingdailyuse) %>% 
            filter(date > today() - months(100)) %>%
            ggplot + 
            aes(x=date, y=dailyuse) + 
            geom_point(color="black", fill="black",alpha=.2) + 
            geom_line(aes(y=rollingmean), color="green") +
            geom_line(aes(y=zoo::rollmean(dailyuse,14,na.pad=TRUE))) + 
            geom_vline(xintercept=as.Date("2021-06-14"))
            # geom_line(aes(y=dayuse), color="red")



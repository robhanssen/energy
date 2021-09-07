library(tidyverse)
library(lubridate)
library(broom)
theme_set(theme_light())


this_year <- year(today())

datadir <- "./data-by-year"

energy <- list.files(path = datadir, full.names = TRUE) %>%
                map_df(~ read_csv(.)) %>% filter(datetime >= as.Date("2021-01-01"))

temperature <- read_csv("Outside.csv") %>%
        mutate(date = date(datetime),
               year = year(datetime),
               month = month(datetime),
               day = day(datetime),
               hour = hour(datetime)
        ) %>%
        filter(year == 2021)

byhour <- energy %>%
            group_by(date, hour) %>%
            summarize(use = sum(energy),
                      .groups = "drop"
                        )

energy_temperature <-
        inner_join(byhour, temperature) %>%
        mutate(temp = 5 / 9 * (temp_f - 32.0))

# energy_temperature %>%
#     ggplot + 
#         aes(x = temp_f, y = use) + 
#         geom_point()


poweruse = energy_temperature$use
temp = energy_temperature$temp

k <- 10
fm0 <- lm(poweruse ~ temp, subset = seq(to = length(temp), length = k))
st <- list(C = mean(head(poweruse, k)), A = coef(fm0)[[1]], B = coef(fm0)[[2]])

fm <- nls(poweruse ~ pmax(C, A + B * temp + D*temp^2), start = st)
fm

nlsmod <- fm %>% broom::augment(newdata = tibble(temp = -5:max(temp))) 

usemod <- lm(use ~ temp, data = energy_temperature %>% filter(temp > 18))
usemod_flat <- lm(use ~ temp, data = energy_temperature %>% filter(temp < 18))

usepred <- usemod %>% broom::augment(newdata = tibble(temp = -5:40)) 
usepred_flat <- usemod_flat %>% broom::augment(newdata = tibble(temp = -5:max(nlsmod$temp))) 

use <- full_join(usepred, usepred_flat, by = "temp") %>% 
        group_by(temp) %>%
        mutate(.fitted = max(.fitted.x, .fitted.y))

fm %>% tidy()

usemod %>% broom::glance()
usemod %>% broom::tidy()

usemod_flat %>% broom::glance()
usemod_flat %>% broom::tidy()

with(as.list(coef(fm)), (C - A)/B)

energy_temperature %>%
    ggplot + 
        aes(x = temp, y = use, , color = temp) + 
        labs(x = "Tempeture (C)", 
            y = "Use per houre (kWh)") +
        geom_point(alpha = .8) + 
        scale_color_gradient2(low = "blue", high = "red", mid = "green", midpoint = 15) + 
        #geom_point(data = use, aes(y=.fitted), color = "red", size = 2) +
        geom_line(data = nlsmod, aes(y=.fitted), color = "darkgreen", size = 2)

ggsave("graphs/poweruse-by-hour.png")
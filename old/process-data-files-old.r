library(tidyverse)
library(lubridate)
library(xml2)

rawdatafiles <- list.files(path = "source/",
                        pattern = "*.xml",
                        full.names = TRUE)

for (d in rawdatafiles) {

        rawdata <- read_xml(d)

        time <- xml_find_all(rawdata, "//espi:start") %>%
                        as_list() %>%
                        unlist() %>%
                        as.numeric()

        time <- time[2:length(time)]

        value <- xml_find_all(rawdata, "//espi:value") %>%
                        as_list() %>%
                        unlist() %>%
                        as.numeric()

        energy_tmp <- tibble(datetime = as_datetime(time), energy = value)

        if (!exists("energy")) {
                energy <- energy_tmp
        }
        else {
                energy <- bind_rows(energy, energy_tmp)
        }
}
 
energy <- energy %>%
                filter(!duplicated(datetime)) %>%
                mutate(date = as.Date(datetime, format = "%Y-%m-%d"),
                        year = year(datetime),
                        month = month(datetime),
                        day = day(datetime),
                        hour = hour(datetime),
                        minute = minute(datetime),
                        runningdate = 100 * month + day
                )

years <- unique(energy$year)

for (y in years) {
        energy_by_year <- energy %>%
                        filter(year == y)

        fname <- paste0("data-by-year/",
                        "energyuse-",
                        y,
                        ".csv")

        write_csv(energy_by_year, fname)
}
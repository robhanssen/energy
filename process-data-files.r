library(tidyverse)
library(lubridate)
library(xml2)

rawdatafiles <- list.files(
    path = "source/",
    pattern = "*.xml",
    full.names = TRUE
)

# read_duke_xml <- function(d) {
#     rawdata <- xml2::read_xml(d)

#     time <- xml_find_all(rawdata, "//espi:start") %>%
#         as_list() %>%
#         unlist() %>%
#         as.numeric()

#     time <- time[2:length(time)]

#     value <- xml_find_all(rawdata, "//espi:value") %>%
#         as_list() %>%
#         unlist() %>%
#         as.numeric()

#     energy <-
#         tibble::tibble(time = time, energy = value) %>%
#         dplyr::filter(energy > 0)

#     return(energy)
# }

read_duke_xml <- function(d) {
    rawdata <- xml2::read_xml(d)

    time <- xml2::xml_find_all(rawdata, "//espi:IntervalReading") %>%
        as_list()

    energy <-
        tibble::tibble(time) %>%
        unnest_wider(time) %>%
        mutate(
            across(everything(), unlist),
            across(everything(), as.numeric)
        ) %>%
        rename(time = timePeriod, energy = value) %>%
        filter(energy > 0)

    return(energy)
}



write_data_file <- function(tbl, yr) {
    energy_by_year <-
        tbl %>%
        filter(year == yr)
    fname <- paste0(
        "data-by-year/",
        "energyuse-",
        yr,
        ".csv"
    )
    readr::write_csv(energy_by_year, fname)
}


energy_raw <-
    map_df(rawdatafiles, ~ read_duke_xml(.x)) %>%
    select(-readingQuality)

energy <-
    energy_raw %>%
    filter(!duplicated(time)) %>%
    arrange(time) %>%
    mutate(
        datetime = as_datetime(time),
        date = as.Date(datetime),
        year = year(datetime),
        month = month(datetime),
        day = day(datetime),
        hour = hour(datetime),
        minute = minute(datetime),
        runningdate = 100 * month + day
    )

years <- unique(energy$year)

saveRDS(energy, file = "Rdata/energy.rds")

walk(years, ~ write_data_file(energy, .x))

library(tidyverse)
library(lubridate)
library(xml2)

analyze_data_file <- function(f) {

        rawdata <- xml2::read_xml(f)

        time <- xml2::xml_find_all(rawdata, "//espi:start") %>%
                        as_list() %>%
                        unlist() %>%
                        as.numeric()

        time <- time[2:length(time)]

        earliest <- lubridate::as_datetime(min(time))
        latest <- lubridate::as_datetime(max(time))

        tibble::tibble(filename = f, earliest_time = earliest, latest_time = latest) %>%
        mutate(across(ends_with("time"), ~format(.x, format = "%b %d, %Y")))
}

map_df(
        list.files(
                path = "source/",
                pattern = "*.xml",
                full.names = TRUE
        ),
        ~ analyze_data_file(.x)
) %>%
        knitr::kable()
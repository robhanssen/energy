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

        earliest <- as_datetime(min(time))
        latest <- as_datetime(max(time))
        msg <- paste(d, earliest, latest, length(time))
        print(msg)
}

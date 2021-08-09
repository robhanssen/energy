library(xml2)
library(dplyr)
library(lubridate)

data <- read_xml("source/energyusage-mwe.xml")


time <- xml_find_all(data, "//espi:start") %>% as_list() %>% unlist() %>% as.numeric()
value <- xml_find_all(data, "//espi:value") %>% as_list() %>% unlist() %>% as.numeric()

time <- time[2:length(time)]

dd <- tibble(time=as_datetime(time), value=value)


library(tsbox)
library(lubridate)
library(tidyverse)
library(fredr)
library(forecast)
library(tseries)

# https://fred.stlouisfed.org
# fredr_set_key("set_fred_key")

fredr_set_key("5348a4120a46bbd80c1cd3c29fb1104e")

icnsa_data <- fredr(series_id = "ICNSA", observation_start = as.Date("2000-01-01"))

head(icnsa_data)


library(data.table)
library(tidyverse)
library(skimr)
library(janitor)

data <- fread("data.csv", header = T)
country <- fread("country.csv")
series <- fread("series.csv")

skim(data)
data <- data %>% clean_names()
country <- country %>% clean_names()
series <- series %>% clean_names()

skim(data)
data$year <- data$series_code
g_data <- data %>% gather(key = year, value = "value",  names(data[,7:28]))
skim(g_data)

g_data$year <- g_data$year %>% str_replace("x", "")
g_data <- g_data %>% mutate(decimals = as.numeric(decimals), scale = as.numeric(scale), value = as.numeric(value))
skim(g_data)

skim(country)
table(country$capital_city)
country[country == ".."] <- NA
country[country == ""] <- NA

skim(series)
df <- inner_join(g_data, country, by = "country_code")
skim(df)

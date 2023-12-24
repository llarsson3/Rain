rm(list = ls())

# Data manipulation to get the dataset in shape for the data dashboard

library(haven)
library(labelled)
library(foreign)
library(sjlabelled)
library(gtsummary)
library(naniar)
library(lubridate)
library(lisa)
library(ggpubr)
library(gt)
library(knitr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(ggforce)
library(ggdist)
library(gghalves)
library(magrittr)
library(RColorBrewer)

rain <- read_excel("BdL_Rain.xlsx")

# Generate date-based variables
rain$datetoday <- Sys.Date()
rain$MonthNum <- format(rain$Date, "%m")
rain$Year <- format(rain$Date, "%Y")
rain$Year <- as.numeric(rain$Year)

rain <- rain %>% mutate(Month = case_when(
  MonthNum == "01" ~ "January",
  MonthNum == "02" ~ "February",
  MonthNum == "03" ~ "March",
  MonthNum == "04" ~ "April",
  MonthNum == "05" ~ "May",
  MonthNum == "06" ~ "June",
  MonthNum == "07" ~ "July",
  MonthNum == "08" ~ "August",
  MonthNum == "09" ~ "September",
  MonthNum == "10" ~ "October",
  MonthNum == "11" ~ "November",
  MonthNum == "12" ~ "December"
))

# Define the order of months
month_order <- c("January","February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
rain$Month <- factor(rain$Month, levels = month_order, ordered = TRUE)

# Trend over time 

saveRDS(rain,file="RainForecast.Rds")
save(rain,file="RainForecast.Rda")
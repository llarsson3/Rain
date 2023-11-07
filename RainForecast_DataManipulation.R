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

rain$datetoday <- as.Date("2023-10-31")

saveRDS(rain,file="RainForecast.Rds")
save(rain,file="RainForecast.Rda")
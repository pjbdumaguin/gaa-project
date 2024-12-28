library(tidyverse)
library(readxl)
library(ggtext)
library(scales)

GAA_2024 <-
  read_csv_chunked("processed/GAA-2024.csv",
                   DataFrameCallback$new(function(x, pos) x),
                   chunk_size = 10000)

# path <- "input/excel/gaa/2020-GAA.xls"
# 
# sheet <- path %>%
#   excel_sheets() %>% 
#   set_names()
# 
# sheet %>% 
#   map_if(
#     ~ str_equal(.x, "Sheet 1"),
#     ~ read_excel(path = path, sheet = .x, skip = 1),
#     .else =  ~ read_excel(path = path, sheet = .x)
#   ) %>% #returns a list;
#   bind_rows() %>% #bind to one data frame before writing as csv
#   write_csv(file = "2020.csv")

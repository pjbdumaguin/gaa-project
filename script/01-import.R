#'https://www.dbm.gov.ph/

library(tidyverse)
library(rvest)

base_page <- "https://www.dbm.gov.ph"

# Doesn't work. Some fiscal years have a slightly different link.
gaa_pages <- paste0(base_page, "/index.php/", seq(2020, 2024),
                   "/general-appropriations-act-gaa-fy-", seq(2020, 2024))

# Function that takes all the links of a page then
# filters it with matching patterns.
get_links <- function(page, pattern) {
  Sys.sleep(2) # if called multiple times
  page |>
    read_html() |> 
    html_elements("a") |> 
    html_attr("href") |> 
    str_subset(pattern)
}

# This works.
gaa_pages <- get_links(page = paste0(base_page, "/index.php/budget"),
                       pattern = "\\/general\\-appropriations") |> 
  {\(x) paste0(base_page, x)}()

dl_links <- map_chr(gaa_pages,
                    \(page) get_links(page, "csv|xlsx?") |> first())

  
library(tidyverse)
library(rvest)

base_page <- "https://www.dbm.gov.ph"

# Doesn't work. Some fiscal years have a slightly different link.
gaa_pages <- paste0(base_page, "/index.php/", seq(2020, 2024),
                    "/general-appropriations-act-gaa-fy-", seq(2020, 2024))

# Function that takes all the links of a page then
# filters it with matching patterns.
get_links <- function(page, pattern) {
  Sys.sleep(2) # a little helper if called multiple times
  page |>
    read_html() |> 
    html_elements("a") |> 
    html_attr("href") |> 
    str_subset(pattern)
}

# This works.
gaa_pages <- get_links(page = paste0(base_page, "/index.php/budget"),
                       pattern = "/general-appropriations") |> 
  {\(x) paste0(base_page, x)}()

# Get the download link from each page, take the first result
dl_links <- gaa_pages |> 
  map_chr(\(page) get_links(page, "csv|xlsx?") |> first()) |> 
  {\(x) paste0(base_page, x)}()

# Prepare the directory for downloading files
directory <- "./raw-file/gaa/"

if (!dir.exists(directory)) dir.create(directory)

destfiles <- paste0(directory, basename(dl_links))

curl::multi_download(urls = dl_links,
                     destfiles = destfiles,
                     resume = TRUE)
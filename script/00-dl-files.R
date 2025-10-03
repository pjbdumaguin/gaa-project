library(tidyverse)
library(rvest)

base_page <- "https://www.dbm.gov.ph"

# Doesn't work. Some fiscal years have a slightly different link.
# gaa_pages <- paste0(base_page, "/index.php/", seq(2020, 2024),
#                     "/general-appropriations-act-gaa-fy-", seq(2020, 2024))

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
budget_doc_pages <- get_links(
  page = paste0(base_page, "/index.php/budget"),
  pattern = "/general-appropriations|\\d/national-expenditure"
) |>
  paste0(base_page, link = _)

# remove repeated links
budget_doc_pages <- budget_doc_pages |> unique()

# Get the download link from each page, take the first result
dl_links <- budget_doc_pages |> 
  map_chr(\(page) get_links(page, "csv|xlsx?") |> first()) |> 
  paste0(base_page, link = _)

# Prepare the directory for downloading files
directories <- paste0("raw-file/", c("gaa", "nep"), "/")

walk(directories, \(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
})

destfiles <- 
  if_else(
    str_detect(dl_links, "GAA"),
  paste0(directories[1], basename(dl_links)),
  paste0(directories[2], basename(dl_links))
  )

# curl's multi_download function automatically checks if a file already exists
curl::multi_download(urls = dl_links,
                     destfiles = destfiles,
                     resume = TRUE)

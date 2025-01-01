library(tidyverse)
library(readxl)

# GAA_2024 <-
#   read_csv_chunked("processed/GAA-2024.csv",
#                    DataFrameCallback$new(function(x, pos) x),
#                    chunk_size = 10000)

file_paths <- list.files(
  path = "raw-file/gaa", 
  pattern = ".*GAA.*",
  full.names = TRUE
) |> 
  set_names(
    \(x) str_extract(x, "\\d{4}") |> 
      {\(x) str_c("gaa_", x)}()
  )

xl_sheets <- file_paths |> 
  map(\(path) excel_sheets(path))

# Prepare the directory for converting files
directory <- "./processed/"

if (!dir.exists(directory)) dir.create(directory)

## Convert downloaded excel files to csv for further manipulation.

# Special approach to 2020-GAA.xls. Among the initial set of budget documents
# downloaded, the GAA for 2020 is quite different:
# 1. It's in older format; 2. It has multiple sheets;
# 3. Sheet 1 has extra header
process_gaa_2020 <- function(path, sheet) {
  gaa_2020 <- map_if(
    .x = sheet,
    .p = \(x) str_equal(x, "Sheet 1"),
    .f = \(x) read_xls(path = path, sheet = x, skip = 1),
    .else = \(x) read_xls(path = path, sheet = x)
  ) |> bind_rows()
  old_file <- basename(path)
  new_file <- str_replace(old_file, "xls", "csv")
  write_csv(gaa_2020, str_c(directory, new_file))
  print(glue::glue("{old_file} converted to {new_file}"))
}

# Here, I am using the SheetReader package to load the excel files.
# This is much more memory efficient than readxl.
walk2(
  .x = file_paths, .y = xl_sheets,
  .f = \(x, y) {
    if(!file.exists(x)) {
      if(str_detect(basename(x), "2020")) {
        process_gaa_2020(x, y)
      } else {
        gaa <- SheetReader::read_xlsx(path = x, sheet = y)
        old_file <- basename(x)
        new_file <- str_replace(old_file, "xlsx", "csv")
        write_csv(gaa, str_c(directory, new_file))
        print(glue::glue("{old_file} converted to {new_file}"))
      }
    }
    print("done")
  }
)

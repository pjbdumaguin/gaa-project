library(tidyverse)
library(readxl)

# Prepare the files to be converted
xl_paths <- list.files(path = "raw-file/gaa",
                       pattern = ".*GAA.*",
                       full.names = TRUE)

xl_paths <- xl_paths |> set_names(\(path) str_extract(path, "\\d{4}") |>
                                    str_c("gaa_", yr = _))

# List sheet/s of every excel file
xl_sheets <- xl_paths |> map(\(path) excel_sheets(path))

# Prepare the directory for csv files
csv_dir <- "processed/csv/"
if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE)

# Initialize base column names using the earliest excel document.
# This will be used so that the resulting csv files will have uniform result.
if (file.exists(paste0(csv_dir, "2020-GAA.csv"))) {
  base_col_names <- colnames(read_csv(paste0(csv_dir, "2020-GAA.csv")))
} else { 
  base_col_names <- NULL 
}

## Convert downloaded excel files

# Processes older excel format, has multiple sheets, where the first
# sheet has an extra header (2020-GAA.xls)
process_xls <- function(path, sheets, xl_name, file_year, csv_name) {
  csv_path <- str_c(csv_dir, csv_name)
  map_if(
    .x = sheets,
    .p = \(sheet) str_equal(sheet, "Sheet 1"),
    .f = \(sheet) {
      print(glue::glue("reading {xl_name}, sheet: {sheet}"))
      gaa <- read_xls(path = path, sheet = sheet, col_types = "text", skip = 1) |> 
        mutate(YEAR = file_year, .before = 1)
      print(glue::glue("appending {sheet} to {csv_name}"))
      write_csv(gaa, csv_path, quote = "needed")
      .GlobalEnv$base_col_names <- colnames(gaa)
    },
    .else = \(sheet) {
      print(glue::glue("reading {xl_name}, sheet: {sheet}"))
      gaa <- read_xls(path = path, sheet = sheet, col_types = "text") |> 
        mutate(YEAR = file_year, .before = 1)
      print(glue::glue("appending {sheet} to {csv_name}"))
      write_csv(gaa, csv_path, append = TRUE, quote = "needed")
    }
  )
}

# using memory efficient SheetReader package to load the excel files.
# cons: type guessing is not as robust as readxl, parsing it all as text
process_xlsx <- function(path, sheet, xl_name, file_year, csv_name) {
  col_types <- c(rep("text", 20)) # SheetReader requires character vector
  print(glue::glue("reading {xl_name}, sheet: {sheet}"))
  gaa <- if(file_year != 2021) {
    SheetReader::read_xlsx(path = path, col_types = col_types)
  } else { # For files that have extra headers;
    SheetReader::read_xlsx(path = path, skip_rows = 1,
                           col_types = col_types)
  }
  
  # for uniform result
  gaa <- gaa |> 
    mutate(YEAR = file_year) |> 
    select(any_of(base_col_names))
  
  print(glue::glue("converting {xl_name} to {csv_name}"))
  write_csv(gaa, str_c(csv_dir, csv_name), quote = "needed")
}

# main function
process_gaa <- function(path, sheet) {
  xl_name <- basename(path)
  file_year <- parse_integer(str_extract(xl_name, "\\d{4}"))
  csv_name <- paste0(file_year, "-GAA.csv")
  
  if(!file.exists(str_c(csv_dir, csv_name))) {
    if(file_year != 2020) {
      process_xlsx(path, sheet, xl_name, file_year, csv_name)
    } else{
      process_xls(path, sheet, xl_name, file_year, csv_name)
    }
    print("conversion done", quote = FALSE)
  }
}

# Commence conversion
walk2(xl_paths, xl_sheets, process_gaa)

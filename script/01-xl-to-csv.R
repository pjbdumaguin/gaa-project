library(tidyverse)
library(readxl)

# Prepare the files to be converted
file_paths <- list.files(path = "raw-file/gaa",
                         pattern = ".*GAA.*",
                         full.names = TRUE) |> 
  set_names(\(x) str_extract(x, "\\d{4}") |> 
              {\(x) str_c("gaa_", x)}())

#List sheet/s of every file
xl_sheets <- file_paths |> 
  map(\(path) excel_sheets(path))

# Prepare the directory for converting files
directory <- "processed/csv/"
if (!dir.exists(directory)) dir.create(directory)

# Prepare the base column names. This will be used as the basis on how the
# proceeding CSVs should be written.
if (file.exists(paste0(directory, "2020-GAA.csv"))) {
  base_col_names <- colnames(read_csv(paste0(directory, "2020-GAA.csv")))
} else { 
  base_col_names <- NULL 
}

## Convert downloaded excel files to csv for further manipulation.
# Special approach to 2020-GAA.xls.
# Among the initial set of budget documents downloaded, the GAA for 2020 is
# quite different: 1. It's in older format; 2. It has multiple sheets;
# 3. Sheet 1 has extra headers
process_gaa_2020 <- function(path, sheet) {
  old_filename <- basename(path)
  new_filename <- str_replace_all(old_filename, ".*(\\d{4}).*", "\\1-GAA.csv")
  gaa_2020 <- map_if(
    .x = sheet,
    .p = \(x) str_equal(x, "Sheet 1"),
    .f = \(x) {
      print(glue::glue("reading {old_filename}, sheet {x}"))
      read_xls(path = path, sheet = x, skip = 1)
    },
    .else = \(x) {
      print(glue::glue("reading {old_filename}, sheet {x}"))
      read_xls(path = path, sheet = x)
    }
  ) |> 
    bind_rows() |> 
    mutate(YEAR = 2020, .before = 1)
  
  print(glue::glue("converting {old_filename} to {new_filename}"))
  write_csv(gaa_2020, str_c(directory, new_filename), quote = "all")
  
  .GlobalEnv$base_col_names <- colnames(gaa_2020)
}

# Here, I am using the SheetReader package to load the excel files.
# This is much more memory efficient than readxl.
# Challenges: 1. See: Special approach to 2020-GAA.xls;
# 2. 2021-GAA.xlsx has an extra header;
# 3. SheetReader is having a hard time parsing particular values in
# 2024-GAA.xlsx. My temporary(?) solution is to parse all of it to text and
# let the more powerful packages assign the right data type when reading it.
process_gaa <- function(path, sheet) {
  old_filename <- basename(path)
  new_filename <- str_replace_all(old_filename, ".*(\\d{4}).*", "\\1-GAA.csv")
  
  if(!file.exists(str_c(directory, new_filename))) {
    if(str_detect(basename(path), "2020")) {
      process_gaa_2020(path, sheet)
    } else if(str_detect(basename(path), "2021")) {
      print(glue::glue("reading {old_filename}, sheet {sheet}"))
      gaa <- SheetReader::read_xlsx(path = path, skip_rows = 1) |> 
        mutate(YEAR = 2021) |> 
        select(base_col_names)
      print(glue::glue("converting {old_filename} to {new_filename}"))
      write_csv(gaa, str_c(directory, new_filename), quote = "all")
    } else if(str_detect(basename(path), "2024")) { # temporary? fix
      print(glue::glue("reading {old_filename}, sheet {sheet}"))
      gaa <- SheetReader::read_xlsx(path = path,
                                    col_types = c(rep("text", 19))) |> 
        mutate(YEAR = 2024) |> 
        select(base_col_names)
      print(glue::glue("converting {old_filename} to {new_filename}"))
      write_csv(gaa, str_c(directory, new_filename), quote = "all")
    } else {
      print(glue::glue("reading {old_filename}, sheet {sheet}"))
      gaa <- SheetReader::read_xlsx(path = path) |> 
        mutate(YEAR = parse_integer(str_extract(new_filename, "\\d{4}"))) |> 
        select(base_col_names)
      print(glue::glue("converting {old_filename} to {new_filename}"))
      write_csv(gaa, str_c(directory, new_filename), quote = "all")
    }
    print("done")
  }
}

# Commence conversion
walk2(file_paths, xl_sheets, process_gaa)

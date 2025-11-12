library(tidyverse)
library(readxl)

# Prepare the files to be converted
xl_paths <- list.files(
  path = "raw-file",
  pattern = ".xlsx?$",
  all.files = TRUE,
  full.names = TRUE,
  recursive = TRUE
)

xl_paths <- xl_paths |>
  set_names(\(path) {
    str_c(str_extract(path, "GAA|NEP"), str_extract(path, "\\d{4}"))
  })

# List sheet/s of every excel file
xl_sheets <- xl_paths |> map(\(path) excel_sheets(path))

# manually list hidden sheets
# TODO: automate identifying hidden sheets
hidden_sheets <- list(
  NEP2020 = "Sheet1",
  NEP2021 = c("Sheet1", "Sheet2"),
  NEP2025 = "Sheet1"
)

# remove hidden sheets from list of excel sheets
xl_sheets <- modify2(
  xl_sheets,
  names(xl_sheets),
  \(sheet, sheet_name) {
    if (sheet_name %in% names(hidden_sheets)) {
      setdiff(xl_sheets[[sheet_name]], hidden_sheets[[sheet_name]])
    } else {
      return(sheet)
    }
  }
)

# Prepare the directory for csv files
directories <- file.path("processed/csv", c("gaa", "nep"))

walk(directories, \(directory) {
  if (!dir.exists(directory)) {
    dir.create(directory, recursive = TRUE)
  }
})

## Convert downloaded excel files

# Processes older excel format, has multiple sheets
process_xls <- function(xl_path, xl_sheet, csv_path) {
  # initialize sheet number object
  # this will be used to track whether the sheet being read is first or not
  # important for appending multiple sheets
  track_sheet <- 1

  walk(
    .x = xl_sheet,
    .f = \(sheet) {
      # check for rows that need skipping
      data_preview <- read_xls(
        xl_path,
        sheet = sheet,
        n_max = 5,
        .name_repair = "unique_quiet"
      )
      # take the row no. where the DEPARTMENT column first appeared
      skip_rows <- match("DEPARTMENT", data_preview[[1]])
      # set skip to 0 if the target value wasn't found
      skip_rows <- ifelse(is.na(skip_rows), 0, skip_rows)

      message(sprintf("reading %s, sheet: %s", basename(xl_path), sheet))
      xl_doc <- read_xls(
        path = xl_path,
        sheet = sheet,
        col_types = "text",
        skip = skip_rows,
        .name_repair = "unique_quiet"
      )

      message(sprintf("appending %s to %s", sheet, basename(csv_path)))
      is_first_sheet <- track_sheet != 1
      write_csv(
        xl_doc,
        csv_path,
        append = is_first_sheet, # FALSE if first; retaining column names
        quote = "needed"
      )
      track_sheet <<- track_sheet + 1
    }
  )
}

# using memory efficient SheetReader package to load the excel files.
# cons: type guessing is not as robust as readxl, parsing it all as text
# xlsx budget docs are less complicated; only has one sheet
process_xlsx <- function(xl_path, xl_sheet, csv_path) {
  col_types <- c(rep("text", 21)) # SheetReader requires character vector
  message(sprintf("reading %s", basename(xl_path)))
  doc <- if (!str_detect(basename(xl_path), "2021-GAA")) {
    SheetReader::read_xlsx(
      path = xl_path,
      sheet = xl_sheet,
      col_types = col_types
    )
  } else {
    # For xlsx files that have extra headers eg. 2021-GAA.xlsx
    SheetReader::read_xlsx(
      path = xl_path,
      sheet = xl_sheet,
      skip_rows = 1,
      col_types = col_types
    )
  }

  message(sprintf("converting %s to %s", basename(xl_path), basename(csv_path)))
  write_csv(doc, csv_path, quote = "needed")
}

# main function
process_doc <- function(xl_path, xl_sheet) {
  csv_name <- basename(xl_path)
  year <- str_extract(csv_name, "\\d{4}")
  type <- str_extract(csv_name, "GAA|NEP")
  csv_name <- str_c(year, "-", type, ".csv")
  csv_path <- file.path(
    "processed/csv",
    tolower(str_extract(csv_name, "GAA|NEP")),
    csv_name
  )

  if (file.exists(csv_path)) {
    message(sprintf("%s already exists", basename(csv_path)))
  } else {
    if (excel_format(xl_path) == "xls") {
      process_xls(xl_path, xl_sheet, csv_path)
    } else {
      process_xlsx(xl_path, xl_sheet, csv_path)
    }
    message("conversion done")
  }
}

# Commence conversion
walk2(xl_paths, xl_sheets, process_doc)

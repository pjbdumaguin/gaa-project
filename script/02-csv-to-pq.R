library(tidyverse)
library(arrow)

# Prepare CSV files for conversion

# Unify csv files converted from excel format into one organized dataset
# before converting it into parquet

csv_files <- list.files("processed/csv", full.names = TRUE, recursive = TRUE)
csv_files <- set_names(csv_files, basename(csv_files))

# To retrieve ALL data, take every column headers that exists
all_cols <- map(csv_files, \(csv_file){
  read_csv(csv_file, n_max = 0, show_col_types = FALSE) |> 
    names()
}) |> 
  flatten_chr() |> 
  unique()

# prepare temporary destination
temp_directory <- "processed/temp_csv"
if(!file.exists(temp_directory)) dir.create(temp_directory)

# read, manipulate, and write the temporary 'organized' csv files
# ready for parquet conversion
walk(csv_files, \(csv_file){
  # know which column headers this file doesn't have
  curr_cols <- read_csv(csv_file, n_max = 0, show_col_types = FALSE) |> names()
  diff_cols <- setdiff(all_cols, curr_cols)

  # attach these column headers to the file
  diff_cols <- as_tibble(set_names(map(diff_cols, \(x) NA), diff_cols))
  organized_csv <- read_csv_arrow(csv_file) |> bind_cols(diff_cols)

  # set the type of file and year
  organized_csv <- organized_csv |>
    mutate(
      TYPE = basename(csv_file) |> str_extract("GAA|NEP"),
      YEAR = basename(csv_file) |> str_extract("\\d{4}"),
      .before = 1
    ) |>
    select( # order is important!
      TYPE,
      YEAR,
      LVL,
      SORDER,
      DEPARTMENT,
      UACS_DPT_DSC,
      AGENCY,
      UACS_AGY_DSC,
      PREXC_FPAP_ID,
      PREXC_LEVEL,
      DSC,
      OPERUNIT,
      UACS_OPER_DSC,
      UACS_REG_ID,
      UACS_REG_DSC,
      UACS_OPERDIV_ID,
      UACS_DIV_DSC,
      FUNDCD,
      UACS_FUNDSUBCAT_DSC,
      UACS_EXP_CD,
      UACS_EXP_DSC,
      UACS_SOBJ_CD,
      UACS_SOBJ_DSC,
      UACS_OBJ_CD,
      UACS_OBJ_DSC,
      AMT
    )
  
  temp_file <- file.path(temp_directory, basename(csv_file))
  message(sprintf("Writing %s", temp_file))
  write_csv_arrow(organized_csv, temp_file)
  message("Done")
})

pq_path <- "processed/parquet"
if (!dir.exists(pq_path)) {
  dir.create(pq_path)
}

csvs_to_convert <- open_csv_dataset(
  sources = temp_directory,
  # type guessing is impossible for columns with NA value
  schema = schema(
    TYPE = string(),
    YEAR = int64(),
    LVL = string(),
    SORDER = int64(),
    DEPARTMENT = int64(),
    UACS_DPT_DSC = string(),
    AGENCY = int64(),
    UACS_AGY_DSC = string(),
    PREXC_FPAP_ID = string(),
    PREXC_LEVEL = int64(),
    DSC = string(),
    OPERUNIT = int64(),
    UACS_OPER_DSC = string(),
    UACS_REG_ID = int64(),
    UACS_REG_DSC = string(),
    UACS_OPERDIV_ID = int64(),
    UACS_DIV_DSC = string(),
    FUNDCD = int64(),
    UACS_FUNDSUBCAT_DSC = string(),
    UACS_EXP_CD = int64(),
    UACS_EXP_DSC = string(),
    UACS_SOBJ_CD = int64(),
    UACS_SOBJ_DSC = string(),
    UACS_OBJ_CD = int64(),
    UACS_OBJ_DSC = string(),
    AMT = float()
  ),
  skip = 1, # when data types are set manually
  convert_options = csv_convert_options(strings_can_be_null = TRUE)
)

message("Converting entire dataset to parquet. Please wait.")
write_dataset(
  dataset = csvs_to_convert,
  path = pq_path,
  format = "parquet",
  partitioning = c("TYPE", "YEAR")
)
message("Done")

message("Removing temporary Files")
unlink(temp_directory, recursive = TRUE)
message("Temporary files removed")
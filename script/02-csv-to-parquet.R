library(tidyverse)
library(arrow)

pq_path <- "processed/parquet"
if(!dir.exists(pq_path)) dir.create(pq_path)

csvs_to_convert <- open_csv_dataset(
  sources = "processed/csv",
  schema = schema(YEAR = int64(),
                  DEPARTMENT = int64(),
                  UACS_DPT_DSC = string(),
                  AGENCY = int64(),
                  UACS_AGY_DSC = string(),
                  PREXC_FPAP_ID = string(),
                  DSC = string(),
                  OPERUNIT = int64(),
                  UACS_OPER_DSC = string(),
                  UACS_REG_ID = int64(),
                  FUNDCD = int64(),
                  UACS_FUNDSUBCAT_DSC = string(),
                  UACS_EXP_CD = int64(),
                  UACS_EXP_DSC = string(),
                  UACS_SOBJ_CD = int64(),
                  UACS_SOBJ_DSC = string(),
                  AMT = float()),
  skip = 1, # when data types are set manually
  convert_options = csv_convert_options(strings_can_be_null = TRUE)
)

write_dataset(
  dataset = csvs_to_convert,
  path = pq_path,
  format = "parquet",
  partitioning = "YEAR"
)

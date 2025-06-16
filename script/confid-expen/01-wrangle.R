library(tidyverse)

GAA_2024 <- read_csv_chunked("processed/csv/2024-GAA.csv",
                   DataFrameCallback$new(function(x, pos) x),
                   chunk_size = 10000)

# Select and filter to target data
confid_expen <- GAA_2024 %>%
  select(ends_with("DSC"), AMT) %>% 
  filter(UACS_SOBJ_DSC == "Confidential Expenses", AMT > 0) %>%
  summarise(amount = sum(AMT), .by = c(UACS_DPT_DSC, UACS_AGY_DSC))

# Add shorthand terms to govt departments and agencies.
# Extract already available data; set it otherwise
confid_expen %<>% 
  separate_wider_regex(
    cols = UACS_DPT_DSC,
    patterns = c(department = ".+", "\\s\\(", dpt = ".+", "\\)")
  ) %>% 
  separate_wider_regex(
    cols = UACS_AGY_DSC,
    patterns = c(agency = ".+", "\\s\\(", agy = ".+", "\\)"),
    too_few = "align_start"
  ) %>%
  mutate(
    agy = case_when(
      str_detect(agency, "Secretary") ~ "OSEC",
      str_detect(agency, "Customs") ~ "BOC",
      str_detect(agency, "Ombudsman") ~ "OMB",
      str_detect(agency, "President's") ~ "The President's Offices",
      is.na(agy) ~ str_remove_all(agency, "[^A-Z]"),
      .default = agy
    )
  )

# Set factors; necessary before plotting
confid_expen %<>% 
  mutate(
    department = fct_reorder(department, amount, .fun = sum),
    dpt = fct_reorder(dpt, amount, .fun = sum),
    agency = fct_reorder2(agency, department, amount),
    agy = fct_reorder2(agy, dpt, amount)
  ) %>%
  group_by(department) %>% 
  arrange(agency, amount, .by_group = TRUE)

# Machine is weak; had to remove the main file object
# rm(GAA_2024)
# gc()

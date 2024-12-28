#https://readxl.tidyverse.org/articles/readxl-workflows.html

# read_then_csv <- function(sheet, path) {
#   pathbase <- path %>%
#     basename() %>%
#     tools::file_path_sans_ext()
#   path %>%
#     read_excel(sheet = sheet) %>% 
#     write_csv(paste0(pathbase, "-", sheet, ".csv"))
# }

paths <-
  list.files(path = "input/excel/gaa", 
             pattern = ".*GAA.*",
             full.names = TRUE) %>% 
  set_names(~ str_extract(., "\\d{4}") %>% paste0("gaa_", .))

sheets <- map(paths, ~ excel_sheets(.x) %>% set_names())

xl_files <- list(path = paths, sheet = sheets)

sheets %>% 
  map_if(
    
  )


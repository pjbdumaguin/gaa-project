# Libraries ---------------------------------------------------------------

library(tidyverse)
library(arrow)
library(duckdb)
library(scales)
library(glue)
library(systemfonts)
library(marquee)
library(ggfx)

# File --------------------------------------------------------------------

path <- "processed/parquet"
gaa <- open_dataset(path)

# Data wrangling ----------------------------------------------------------

confid_expen <- gaa |>
  to_duckdb() |>
  select(yr = YEAR, ends_with("DSC"), AMT) |>
  rename_with( ~ str_extract(.x, "UACS_(.+)_DSC", group = 1), ends_with("_DSC")) |>
  rename_with(tolower) |>
  filter(sobj == "Confidential Expenses", amt > 0) |>
  collect()

# confid_expen |>
#   filter(yr %in% c(2022, 2025)) |>
#   # arrange(yr) |>
#   group_by(yr, dpt, agy) |>
#   summarise(amt = sum(amt), .groups = "drop") |> # drop the yr grouping
#   arrange(dpt, agy, yr) |>
#   group_by(dpt, agy) |> # regroup for the next calculation
#   mutate(
#     prev_amt = lag(amt),
#     change = amt - prev_amt,
#     prc_change = (amt - prev_amt) / prev_amt * 100
#   ) |>
#   arrange(desc(change)) |>
#   filter(change > 0 | change < 0, !is.na(change)) |>
#   ggplot(aes(x = fct_reorder(paste(agy, dpt), change), y = change)) +
#   geom_col() +
#   coord_flip()

# ce_22_24 <- confid_expen |>
#   select(yr, dpt, agy, amt) |>
#   filter(yr %in% c(2022:2024)) |>
#   summarise(amt = sum(amt), .by = c(yr, dpt, agy))
#
# ce_25 <- confid_expen |>
#   select(yr, dpt, agy, amt) |>
#   filter(yr == 2025) |>
#   summarise(amt = sum(amt), .by = c(yr, dpt, agy))
#
# anti_join(ce_22_24, ce_25, by = c("agy", "dpt")) |> view()

to_php <- label_currency(
  accuracy = 0.01,
  prefix = "\u20b1",
  big.mark = " ",
  scale_cut = cut_short_scale(space = TRUE),
  drop0trailing = TRUE
)

get_id <- function(dpt, agy) {
  dpt_an <- str_extract(dpt, ".+\\s\\(([:alpha:]+)\\)", group = 1)
  # agy_an <- case_when(
  #   str_detect(agy, "\\(") ~ str_extract(agy, ".+\\s\\(([:alpha:]+)\\)", group = 1),
  #   str_detect(agy, "Secretary") ~ "OSec",
  #   str_detect(agy, "Customs") ~ "BOC",
  #   str_detect(agy, "Ombudsman") ~ "OMB",
  #   str_detect(agy, "Senate") ~ agy,
  #   .default = str_remove_all(agy, "[^A-Z]")
  # )
  glue("**{dpt_an}  \n{agy}**")
}

ce_gainers <- confid_expen |>
  select(yr, dpt, agy, amt) |>
  filter(yr %in% c(2022:max(yr))) |>
  summarise(amt = sum(amt * 1e3), .by = -last_col()) |> # amt is in thousands
  arrange(dpt, agy, yr) |>
  complete(yr, nesting(dpt, agy), fill = list(amt = 0)) |>
  mutate(
    prev_amt = lag(amt),
    change = amt - prev_amt,
    avg_change = mean(change, na.rm = TRUE),
    .by = c(dpt, agy)
  ) |>
  filter(avg_change > 0 | avg_change < 0) |>
  mutate(id = get_id(dpt, agy)) |>
  mutate(id = fct_reorder(id, desc(avg_change))) |>
  arrange(desc(avg_change))

doj_gain <- ce_gainers |>
  filter(yr %in% c(2022, 2025), 
         str_detect(id, "\\*\\*DOJ  \nOffice of the Secretary\\*\\*")) |>
  reframe(amt = amt[yr == 2025] - amt[yr == 2022]) |>
  pull() |>
  to_php()

omb_loss_prc <- ce_gainers |>
  filter(yr %in% c(2022, 2025), 
         str_detect(id, "\\*\\*OMB  \nOffice of the Ombudsman\\*\\*")) |>
  reframe(amt_prc = (amt[yr == 2025] - amt[yr == 2022]) / amt[yr == 2022]) |>
  pull() |>
  abs() |>
  percent(accuracy = 0.01)

ce_gainers |>
  filter(str_detect(id, "DOJ|OMB")) |>
  ggplot(aes(yr, amt)) +
  geom_col(fill = "#9c9bdb") +
  geom_smooth(color = "#fa7405",
              method = "lm",
              se = FALSE) +
  geom_text(
    aes(label = to_php(amt)),
    color = "#9c9bdb",
    family = "Montserrat",
    size = 3.5,
    vjust = -0.2
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
  facet_wrap(vars(id), scales = "free_y") +
  labs(
    x = NULL,
    y = NULL,
    title = glue("**{#fa7405 ↗}DOJ's Confidential Funds Skyrocket;  
                 {#fa7405 ↘}OMB's Allocation Plummets <omb_loss_prc>**",
                 .open = "<",
                 .close = ">"),
    subtitle = "The Department of Justice and its attached agencies have been awarded a significant increase in confidential fund allocations, with the Office of the Secretary alone ballooning to \u20b11.525 billion. In contrast, the Office of the Ombudsman has experienced a loss of 98.06% of its confidential fund budget since the administration transition in 2022.",
    caption = "based on data from _dbm.gov.ph_ annual **General Appropriations Act**",
    tag = "![](image/logo/philvized/symbol_light_transparent.png)**philvized**"
  ) +
  theme(
    text = element_marquee(
      family = "Montserrat",
      color = "#9c9bdb",
      style = classic_style(),
      width = NA
    ),
    axis.title = element_blank(),
    axis.text.x = element_marquee(colour = "#9c9bdb"),
    axis.text.y = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(color = "#9c9bdb", fill = NA),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#131343", colour = NA),
    plot.title = element_marquee(),
    plot.title.position = "plot",
    plot.subtitle = element_marquee(size = 10, width = 1),
    plot.caption = element_marquee(hjust = 1),
    plot.caption.position = "plot",
    plot.tag = element_marquee(size = 10),
    plot.tag.position = "bottomleft",
    plot.tag.location = "plot",
    plot.margin = margin(20, 20, 20, 20),
    strip.background = element_blank(),
    strip.text = element_marquee(color = "#9c9bdb", width = 1,
                                 margin = margin(0,0,0,0))
  )

ggsave(
  filename = "change.png",
  path = "image/",
  scale = 2,
  width = 1080,
  height = 1080,
  units = "px"
)

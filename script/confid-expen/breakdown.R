# Load libraries ----------------------------------------------------------

library(tidyverse)
library(arrow)
library(duckdb)
library(treemapify)
library(ggfx)
library(glue)
library(scales)
library(marquee)
library(systemfonts)

# Wrangle data ------------------------------------------------------------

gaa <- open_dataset("processed/parquet")

confid_expen <- gaa |>
  to_duckdb() |>
  select(yr = YEAR, ends_with("DSC"), AMT) |>
  filter(UACS_SOBJ_DSC == "Confidential Expenses", AMT > 0) |>
  summarise(
    amt = sum(AMT, na.rm = TRUE) * 1e3,
    # data is in thousand
    .by = c(yr, UACS_DPT_DSC, UACS_AGY_DSC)
  ) |>
  collect()

# Extract/add shorthand terms to govt departments and agencies.
confid_expen <- confid_expen |>
  separate_wider_regex(cols = UACS_DPT_DSC,
                       patterns = c(department = ".+", "\\s\\(", dpt = ".+", "\\)")) |>
  separate_wider_regex(
    cols = UACS_AGY_DSC,
    patterns = c(agency = ".+", "\\s\\(", agy = ".+", "\\)"),
    too_few = "align_start"
  ) |>
  mutate(
    agy = case_when(
      str_detect(agency, "Secretary") ~ "OSEC",
      str_detect(agency, "Customs") ~ "BOC",
      str_detect(agency, "Ombudsman") ~ "OMB",
      str_detect(agency, "Senate") ~ agency,
      str_detect(agency, "President's") ~ "The President's Offices",
      is.na(agy) ~ str_remove_all(agency, "[^A-Z]"),
      .default = agy
    )
  ) |>
  mutate(dpt = case_when(
    str_detect(department, " President") ~ "O. President",
    str_detect(department, "Executive") ~ "Other EOs",
    .default = dpt
  ))

# Label -------------------------------------------------------------------

to_php <- label_currency(
  accuracy = 0.01,
  prefix = "\u20b1",
  big.mark = " ",
  scale_cut = cut_short_scale(space = TRUE),
  drop0trailing = TRUE
)

# Color -------------------------------------------------------------------

dpt <- summarise(confid_expen, dpt_amt = sum(amt), .by = dpt) |>
  arrange(dpt_amt) |> pull(dpt)
dpt_n <- length(dpt)
base_colrs <- c("#320d0f", "#9c9bdb", "#6a2e12", "#343369", "#fa7405")
tile_colrs <- pal_gradient_n(base_colrs)(seq(0, 1, length.out = dpt_n)) |>
  set_names(dpt)

# Filter -----------------------------------------------------------------

speckle <- function(x, colour, proportion) {
  raster_dim <- dim(x)
  n_pixels <- prod(raster_dim)
  n_speckles <- n_pixels * proportion
  x[sample(length(x), n_speckles)] <- farver::encode_native(colour)
  x
}

# Plot --------------------------------------------------------------------

gen_plots <- function(fiscal_year) {
  # fiscal_year <- 2020 # test
  ce <- confid_expen |>
    filter(yr == fiscal_year) |>
    group_by(yr, dpt) |>
    mutate(amt_dpt = sum(amt)) |> # for labeling purposes
    ungroup()
  
  p <- ggplot(ce, aes(
    area = amt,
    fill = dpt,
    label = agy,
    subgroup = paste0(dpt, "\n", to_php(amt_dpt)),
  )) +
    with_custom(
      geom_treemap(
        color = "white",
        size = 1,
        linetype = "dotted",
        show.legend = FALSE,
        start = "topleft"
      ),
      filter = speckle,
      colour = '#44446c',
      proportion = 0.1
    ) +
    geom_treemap_text(
      color = "gray10",
      place = "bottomright",
      start = "topleft",
      # reflow = TRUE,
      family = "Montserrat"
    ) +
    geom_treemap_subgroup_border(
      color = "white",
      size = 1.75,
      show.legend = FALSE,
      start = "topleft"
    ) +
    geom_treemap_subgroup_text(
      color = "white",
      family = "Montserrat",
      fontface = "bold",
      place = "topleft",
      start = "topleft"
    )
  
  p + labs(
    title = glue(
      "**Philippine government's
      {.a Confidential Funds}, <fiscal_year>**",
      .open = "<",
      .close = ">"
    ),
    subtitle = glue(
      "**Total:** {#fa7405 **<to_php(sum(ce$amt))>**}",
      .open = "<",
      .close = ">"
    ),
    caption = "based on data from
    _dbm.gov.ph_ annual **General Appropriations Act**",
    tag = "![](image/logo/philvized/symbol_light_transparent.png)**philvized**"
  ) -> p
  
  p + scale_fill_manual(values = tile_colrs) -> p
  
  theme_pv <- theme(
    text = element_marquee(
      family = "Montserrat",
      color = "#9c9bdb",
      style = classic_style(),
      width = 1
    ),
    aspect.ratio = 1,
    panel.background = element_blank(),
    plot.background = element_rect(fill = "#131343", colour = NA),
    plot.title = element_marquee(
      size = 32,
      hjust = 0,
      margin = margin(0, 0, 10, 0),
      style = modify_style(
        classic_style(),
        "a",
        weight = "bold",
        color = "white",
        background = "#fa7405",
        padding = trbl(em(0.01))
      )
    ),
    plot.title.position = "plot",
    plot.subtitle = element_marquee(size = 12),
    plot.caption = element_marquee(
      size = 10,
      hjust = 1,
      margin = margin(t = 30, 0, 0, 0),
      width = NA
    ),
    plot.caption.position = "plot",
    plot.tag = element_marquee(size = 10),
    plot.tag.position = "bottomleft",
    plot.tag.location = "plot"
  )
  
  theme_set(theme_pv)
  
  p
  
  ggsave(
    filename = glue("breakdown_{fiscal_year}.png"),
    path = "image/",
    scale = 2,
    width = 1080,
    height = 1350,
    units = "px"
  )
}

walk(unique(confid_expen$yr), gen_plots)

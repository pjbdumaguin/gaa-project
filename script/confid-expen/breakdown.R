# Load libraries ----------------------------------------------------------

library(tidyverse)
library(arrow)
library(duckdb)
library(treemapify)
library(ggfx)
library(glue)
library(scales)
library(systemfonts)
library(marquee)
library(ggfittext)
library(ggrepel)

# Wrangle data ------------------------------------------------------------

gaa <- open_dataset("processed/parquet")

confid_expen <- gaa |>
  to_duckdb() |>
  select(yr = YEAR, ends_with("DSC"), AMT) |>
  filter(UACS_SOBJ_DSC == "Confidential Expenses", AMT > 0) |>
  summarise(
    # data is in thousand
    amt = sum(AMT, na.rm = TRUE) * 1e3,
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
      str_detect(agency, "Secretary") ~ "OSec",
      str_detect(agency, "Customs") ~ "BOC",
      str_detect(agency, "Ombudsman") ~ "OMB",
      str_detect(agency, "Senate") ~ agency,
      is.na(agy) ~ str_remove_all(agency, "[^A-Z]"),
      .default = agy
    )
  )

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

# Marquee style set -------------------------------------------------------

cust_style <- modify_style(
  classic_style(),
  "a",
  weight = "bold",
  color = "white",
  background = "#fa7405",
  padding = trbl(em(0.01))
)

# Plot --------------------------------------------------------------------

gen_plots <- function(fiscal_year) {
  # fiscal_year <- 2023 # test
  ce <- confid_expen |>
    filter(yr == fiscal_year) |>
    group_by(yr, dpt) |>
    mutate(amt_dpt = sum(amt)) |> # for labeling purposes
    ungroup()
  
  tm_ce <- treemapify(
    ce,
    area = "amt",
    subgroup = "amt_dpt",
    layout = "srow",
    start = "topleft"
  ) |>
    mutate(agy_n = n(), .by = dpt) |>
    mutate(width = xmax - xmin, height = ymax - ymin)
  
  tm_ce_subgroup <- tm_ce |>
    summarise(
      xmin = min(xmin),
      xmax = max(xmax),
      ymin = min(ymin),
      ymax = max(ymax),
      .by = c(department, dpt, amt_dpt)
    ) |>
    mutate(
      width = xmax - xmin,
      height = ymax - ymin,
      x = xmin + ((xmax - xmin) / 2),
      y = ymin + ((ymax - ymin) / 2)
    ) |>
    left_join(enframe(tile_colrs, "dpt", "color"), by = "dpt")
  
  p <- tm_ce |>
    ggplot(aes(
      xmin = xmin,
      xmax = xmax,
      ymin = ymin,
      ymax = ymax
    )) +
    # agency border
    geom_rect(
      aes(fill = dpt),
      color = "gray90",
      linetype = "dotted",
      show.legend = FALSE
    ) +
    # agency label
    geom_fit_text(
      aes(
        label = case_when(
          str_detect(agy, "TPO") ~ agency,
          str_equal(dpt, agy) ~ NA,
          height < 0.0725 & agy_n == 1 ~ NA,
          width < 0.06 & agy_n == 1 ~ NA,
          width > height & height < 0.03 ~ NA,
          height > width & width < 0.03 ~ NA,
          .default = agy
        )
      ),
      na.rm = TRUE,
      min.size = 4,
      place = "bottomright",
      reflow = TRUE,
      contrast = TRUE,
      family = "Montserrat"
    ) +
    # agency label, too short boxes
    geom_fit_text(
      aes(
        label = case_when(
          width > height & height < 0.03 ~ agy,
          .default = NA
        )
      ),
      na.rm = TRUE,
      padding.y = grid::unit(0.1, "mm"),
      min.size = 1,
      place = "right",
      reflow = TRUE,
      contrast = TRUE,
      family = "Montserrat"
    ) +
    # agency label, rotated
    geom_fit_text(
      aes(
        label = case_when(
          height > width & width < 0.03 & !str_detect(agy, "OTS") ~ agy,
          .default = NA
        )
      ),
      na.rm = TRUE,
      # padding.x = grid::unit(0, "mm"),
      # padding.y = grid::unit(0.5, "mm"),
      min.size = 2,
      place = "center",
      reflow = TRUE,
      contrast = TRUE,
      family = "Montserrat",
      angle = 90
    ) +
    # subgroup border
    geom_rect(data = tm_ce_subgroup,
              fill = NA,
              color = "white") +
    # department label full or acronym
    geom_fit_text(
      data = tm_ce_subgroup,
      mapping = aes(
        label =
          case_when(
            width >= 0.25 ~ department,
            between(width, 0.05, 0.25) ~ dpt,
            .default = NA
          )
      ),
      na.rm = TRUE,
      place = "topleft",
      reflow = TRUE,
      hjust = 0,
      formatter = toupper,
      contrast = TRUE,
      family = "Montserrat",
      fontface = "bold"
    ) +
    # department label, rotated
    geom_fit_text(
      data = tm_ce_subgroup,
      mapping = aes(label = ifelse(height > width & width < 0.050, dpt, NA)),
      na.rm = TRUE,
      place = "center",
      reflow = TRUE,
      contrast = TRUE,
      family = "Montserrat",
      fontface = "bold",
      angle = 90
    ) +
    # amount label, center
    geom_fit_text(
      data = tm_ce_subgroup,
      mapping = aes(label = ifelse(height >= 0.075, to_php(amt_dpt), NA)),
      na.rm = TRUE,
      place = "centre",
      contrast = TRUE,
      family = "Montserrat",
      fontface = "bold"
    ) +
    # amount label, bottom
    geom_fit_text(
      data = tm_ce_subgroup,
      mapping = aes(label = ifelse(height <= 0.075 & width >= 0.05,
                                   to_php(amt_dpt), NA)),
      na.rm = TRUE,
      place = "bottom",
      contrast = TRUE,
      family = "Montserrat",
      fontface = "bold"
    ) +
    coord_cartesian(clip = "off") +
    # amount label, outside
    geom_label_repel(
      data = tm_ce_subgroup,
      mapping = 
        aes(x = x, y = y,
            label = 
              case_when(
                between(height, 0.055, 0.08) & width <= 0.05 ~ to_php(amt_dpt),
                height <= 0.055 & width <= 0.05 ~ glue("{dpt}: {to_php(amt_dpt)}"),
                .default = NA
                )
      ), 
      color = "white",
      fill = tm_ce_subgroup$color,
      size = 2.75,
      family = "Montserrat",
      segment.color = "white",
      segment.linetype = "longdash",
      segment.alpha = 0.80,
      segment.size = 0.2,
      # segment.curvature = -1e-20,
      # segment.ncp = 3,
      # segment.angle = 40,
      # segment.square = TRUE,
      # segment.inflect = TRUE,
      force = 1.1,
      force_pull = 0,
      box.padding = 0.1,
      label.padding = 0,
      label.r = 0,
      label.size = 0,
      # xlim = c(0, NA),
      ylim = c(-0.0275, -0.0325),
      na.rm = TRUE,
      show.legend = FALSE,
      max.overlaps = Inf
    ) +
    # could also use coord_cartesian(expand = FALSE)
    scale_y_continuous(expand = expansion(mult = c(0, 0))) +
    scale_x_continuous(expand = expansion(mult = c(0, 0)))
  
  p + labs(
    x = NULL,
    y = NULL,
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
      width = NA
    ),
    # aspect.ratio = 1,
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank(),
    panel.background = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    plot.background = element_rect(fill = "#131343", colour = NA),
    plot.title = element_marquee(
      size = 26,
      hjust = 0,
      margin = margin(0, 0, 0, 0),
      style = cust_style,
      width = 1
    ),
    # plot.title.position = "plot",
    plot.subtitle = element_marquee(
      size = 12,
      hjust = 0,
      margin = margin(12, 0, 0, 0)
    ),
    plot.caption = element_marquee(
      size = 10,
      hjust = 1,
      margin = margin(20, 0, 0, 0)
    ),
    # plot.caption.position = "panel",
    plot.tag = element_marquee(size = 10, margin = margin(20, 0, 0, 0)),
    plot.tag.position = "bottomleft",
    plot.tag.location = "plot",
    plot.margin = margin(20, 20, 10, 20)
  )
  
  theme_set(theme_pv)
  
  p
  
  ggsave(
    filename = glue("breakdown_{fiscal_year}.png"),
    # device = svg,
    path = "image/",
    scale = 2,
    width = 1080,
    height = 1350,
    units = "px"
  )
}

walk(sort(unique(confid_expen$yr)), gen_plots)

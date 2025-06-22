# Load libraries ----------------------------------------------------------

library(tidyverse)
library(arrow)
library(dbplyr)
library(duckdb)
library(ggtext)
library(scales)
library(glue)
library(ggfx)
library(ragg)

# Wrangle Data ------------------------------------------------------------

pq_path <- "processed/parquet"
gaa <- open_dataset(pq_path)

confid_expen <- gaa |>
  to_duckdb() |> 
  select(yr = YEAR, ends_with("DSC"), AMT) |> 
  filter(UACS_SOBJ_DSC == "Confidential Expenses", AMT > 0) |>
  mutate(dpt = if_else(str_detect(UACS_DPT_DSC, "OVP|DepEd"), 
                       UACS_DPT_DSC, "other")) |> 
  summarise(amt = sum(AMT, na.rm = TRUE)*1e3, .by = c(yr, dpt)) |> 
  collect()

# Customize label ---------------------------------------------------------

to_php <- label_currency(
  accuracy = 0.001,
  prefix = "\u20b1",
  big.mark = " ",
  scale_cut = cut_short_scale(space = TRUE),
  drop0trailing = TRUE
)

labels <- labs(
  title = "<b style='color:#fa7405'>■ VP Sara</b>'s **confidential funds**, 
  only a <b style='color:#fa7405'>small</b> chunk of government's",
  subtitle = "<b style='color:#9c9bdb'>■ Philippine government</b>'s confidential 
  expenses over the last six years",
  x = NULL,
  y = NULL,
  caption = "based on **data** from **dbm**.gov.ph annual **General Appropriations Act**",
  tag = "<img src='image/logo/wordmark2_light_transparent.png' height='8.5'/>"
)

# Set Filter --------------------------------------------------------------

speckle <- function(x, colour, proportion) {
  raster_dim <- dim(x)
  n_pixels <- prod(raster_dim)
  n_speckles <- n_pixels * proportion
  x[sample(length(x), n_speckles)] <- farver::encode_native(colour)
  x
}

# Plot --------------------------------------------------------------------

base_plot <- ggplot(confid_expen, aes(x = yr, y =  amt)) +
  with_custom(
    geom_col(
      aes(fill = dpt),
      position = position_stack(reverse = TRUE),
      width = .75,
      show.legend = FALSE
    ),
    filter = speckle,
    colour = '#44446c',
    proportion = 0.075
  ) +
  geom_text(
    aes(label = after_stat(to_php(y))),
    stat = "summary",
    fun = sum,
    vjust = -.5,
    color = "#9c9bdb",
    size = 10,
    size.unit = "pt",
    family = "Montserrat",
    fontface = "bold"
  ) +
  scale_fill_manual(
    values = c("other" = "#9c9bdb",
               "Office of the Vice-President (OVP)" = "#fa7405",
               "Department of Education (DepEd)" = "#6a2e12")
  ) +
  scale_x_continuous(breaks = c(2020, 2023, 2025)) +
  scale_y_continuous(breaks = c(1e9, 4e9, 5e9),
                     labels = to_php) 

# Set theme ---------------------------------------------------------------

theme <- theme(
  text = element_text(color = "#9c9bdb", family = "Montserrat"),
  title = element_markdown(),
  axis.ticks = element_blank(),
  axis.text = element_text(color = "#7879b3"),
  axis.text.y = element_markdown(),
  legend.position = "none",
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_line(color = "#42427eb3", #alpha value b3=70%
                                    linetype = "longdash"),
  panel.background = element_blank(),
  plot.background = element_rect(fill = "#131343",
                                 color = "#131343"),
  plot.title.position = "plot",
  plot.caption = element_markdown(size = 7,
                                  padding = unit(c(15, 0, 0, 0), "pt")),
  plot.caption.position = "plot",
  plot.tag = element_textbox(padding = unit(c(10, 0, 0, 0), "pt")),
  plot.tag.position = "bottomleft", 
  plot.tag.location = "plot",
  plot.margin = margin(30, 30, 20, 30)
)

# Annotate ----------------------------------------------------------------

ovp_amt <- confid_expen |> 
  filter(yr == 2023, str_detect(dpt, "OVP")) |> pull(amt)
deped_amt <- confid_expen |> 
  filter(yr == 2023, str_detect(dpt, "DepEd")) |> pull(amt)

ovp_yend <- ovp_amt/2 + deped_amt
deped_yend <- deped_amt/2

noted_plot <- base_plot + 
  annotate(
    "curve", x = 2023.65, y = ovp_yend, xend = 2023, yend = ovp_yend,
    curvature = .3,
    arrow = arrow(length = unit(5, "pt")),
    color = "#6a2e12"
  ) +
  annotate(
    "richtext", x = 2023.65, y = ovp_yend,
    label = glue("<span style='font-size:8.75pt; font-family:Montserrat'>",
                 "OVP<br>**{to_php(ovp_amt)}**</span>"),
    color = "#fa7405",
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    lineheight = .1,
    hjust = 0
  ) + 
  annotate(
    "curve", x = 2022.4, y = deped_amt, xend = 2023, yend = deped_yend,
    curvature = .5,
    arrow = arrow(length = unit(5, "pt")),
    color = "#fa7405"
  ) +
  annotate(
    "richtext", x = 2022.13, y = deped_amt,
    label = glue("<span style='font-size:8.75pt; font-family:Montserrat'>",
                 "DEPED<br>**{to_php(deped_amt)}**</span>"),
    color = "#6a2e12",
    fill = NA,
    label.color = NA,
    label.padding = grid::unit(rep(0, 4), "pt"),
    lineheight = .1
  )

# Save --------------------------------------------------------------------

noted_plot + labels + theme

ggsave(
  "image/overview.png",
  device = agg_png,
  width = 1080,
  height = 1080,
  units = "px",
  scale = 2
)

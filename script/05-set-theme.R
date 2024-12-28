# Labels------------------------------------------------------------------------
labels <- labs(
  y = NULL,
  title = "Philippine Government's 
  <span style='color:#fa7405;'>**Confidential**</span> Expenses",
  subtitle = "",
  caption = "**source**:
   **D**epartment of **B**udget and **M**anagement<br>
  Annual **G**eneral **A**ppropriations **A**ct",
  tag = "<img style='vertical-align:middle;'
  src='image/logo/logo100x100.jpg' width='25'/>"
)

# Philvized reference palette---------------------------------------------------
ref_color <- c(
  "bunting" = "#131343",
  "rhino" = "#343369",
  "east bay" = "#42427e",
  "fiord" = "#44446c",
  "kimberly" = "#6a6c9c",
  "wild blue yonder" = "#7879b3",
  "cold purple" = "#9c9bdb",
  "chilean fire" = "#fa7405",
  "hairy heath" = "#6a2e12",
  "sambuca" = "#320d0f"
)

# Theme-------------------------------------------------------------------------
theme <- theme(
  text = element_text(color = "#9c9bdb"),
  title = element_markdown(),
  axis.ticks = element_blank(),
  axis.text = element_text(color = "#7879b3"),
  #axis.text.x = element_blank(),
  axis.text.y = element_markdown(),
  legend.position = "none",
  panel.grid.minor = element_blank(),
  panel.grid.major.x = element_line(color = "#42427eb3", #alpha value b3=70%
                                    linetype = "longdash"),
  panel.grid.major.y = element_blank(),
  panel.background = element_blank(),
  plot.background = element_rect(fill = "#131343",
                                 color = "#131343"),
  plot.title = element_markdown(size = 16,
                                padding = unit(c(0, 0, 10, 15), "pt")),
  plot.title.position = "plot",
  plot.caption = element_markdown(size = 7),
  plot.caption.position = "plot",
  plot.tag = element_markdown(padding = unit(c(0, 0, 0, 10), "pt")),
  plot.tag.position = "bottomleft", 
  plot.tag.location = "plot",
  plot.margin = margin(30, 20, 30, 20)
) 

(plot <- base + labels + theme)

ggsave(
  "image/plot.png",
  width = 1080,
  height = 1080,
  units = "px",
  scale = 2
)
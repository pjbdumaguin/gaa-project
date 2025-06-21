library(tidyverse)
library(scales)
library(ggtext)

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

# Plotting the base chart-------------------------------------------------------
# The underlying data (amount) is in thousands.
# Bringing it back to its base form should be helpful when labeling
base <- confid_expen %>% 
  ggplot(aes(x = amount * 1e3, y = dpt)) +
  geom_col(
    aes(fill = agency),
    position = position_stack(reverse = TRUE),
    width = 0.5,
    show.legend = FALSE
  )

# Set bar colors
agy_n <- confid_expen$agency %>% unique() %>% length()
base_colrs <- c("#fa7405", "#6a2e12", "#320d0f")
bar_colrs <- pal_gradient_n(base_colrs)(seq(0, 1, length.out = agy_n))

base <- base + scale_fill_manual(values = bar_colrs)

# This function appends peso symbol \u20b1 to values when called;
# also applies other formats such as trimming large numbers to shorthand names
to_php <- label_currency(
  accuracy = 0.01,
  prefix = "\u20b1",
  big.mark = " ",
  scale_cut = cut_short_scale(space = TRUE),
  drop0trailing = TRUE
)

base <- base + 
  scale_x_continuous(
    name = NULL,
    n.breaks = 3,
    labels = to_php,
    expand = expansion(mult = c(0, 0)),
    position = "top"
  )

base <- base + 
  scale_y_discrete(
    labels  = function(i){
      i %>% case_match(
        "OP" ~ "**OP**<br><span style='font-size:6pt'>
                *Office of the<br>**President***</span>",
        "OEOs" ~ "**OEOs**<br><span style='font-size:6pt'>
                  *<b>Other</b> Exec<br>Offices*</span>",
        .default = paste0("**", i, "**")
      )
    }
  )

# Applying labels---------------------------------------------------------------
# I'd like to append the total amount of confidential expenses by department
# at the end of each bar however, it is not present in the current data frame.
# Calling the stat_summary and using the function sum should give us what we
# are asking. The after_stat function makes ggplot wait for the computation
# before mapping the data.
base <- base +
  geom_text(
    aes(label = to_php(after_stat(x))),
    stat = "summary", 
    fun = sum,
    color = "white", 
    fontface = "bold",
    hjust = "inward",
    size = 3
  )

# This part places labels in each stack. I only want departments with more than
# one agency and has enough bar size to be labeled thus the filters.
base <- base +
  geom_text(
    data = confid_expen %>% 
      mutate(agy_n = n()) %>%
      filter(amount >= 5e4, agy_n > 1), #hide select labels
    mapping = aes(label = agy),
    position = position_stack(vjust = 0),
    color = "white",
    size = 2,
    vjust = -1,
    hjust = 0,
    check_overlap = TRUE
  )

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
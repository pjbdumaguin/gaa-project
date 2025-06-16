library(scales)

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
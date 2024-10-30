########################################
# Purpose:  Simple GANTT chart for research planning. Example for STREEM
#           students.
# Date:     Nov 1, 2022
# Author:   Leon Bremer
########################################

### Load external packages
library(tidyverse)
library(readxl)

### Setup
setwd(paste0("C:/Users/abr448/OneDrive - Vrije Universiteit Amsterdam/VU/TSS/",
            "TSS2024-25"))

### Functions
annotate_wrapper <- function(start, stop) {
  # A simple wrapper around annotate() to avoid repetition of code.
  geom <- annotate(
    geom = "rect",
    xmin = as.Date(start),
    xmax = as.Date(stop),
    ymin = .5,
    ymax = num_stages + .5,
    fill = color_phase,
    alpha = .35
  )
  return(geom)
}

### Data
tbl_dates <- read_excel("data/gantt_dates.xlsx") %>%
  mutate(
    Stage = factor(
      x = Stage,
      levels = 1:6,
      labels = c("Orientation", "ResQuestion", "LitRev", "Method & Data",
                 "Analysis", "Writing")
    ),
    Start = as.Date(Start),
    End = as.Date(End)
  )

### Parameters
num_stages <- tbl_dates %>% pull(Stage) %>% levels() %>% length()
color_phase <- "#6fb3e3"

### Plot
ggplot(
  data = tbl_dates,
  mapping = aes(
    x = Start,
    y = Stage,
    xend = End,
    yend = Stage,
    color = Importance
  )
) +
  annotate_wrapper(start = "2025-01-01", stop = "2025-01-31") +
  annotate_wrapper(start = "2025-03-21", stop = "2025-06-30") +
  geom_segment(linewidth = 7) +
  labs(title = "GANTT chart",
       subtitle = "Planning for STREEM thesis",
       caption = "EXAMPLE by Leon Bremer") +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title = element_blank()
  ) +
  scale_color_continuous(low = "#d9db4b", high = "#e65b05")

ggsave("figures/gantt_example.pdf",
       width = 8,
       height = 3)

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
setwd('C:/Users/abr448/Documents/VU/TSS/TSS2023-24')

### Data
tbl <- read_excel("data/gantt_dates.xlsx") %>%
  mutate(
    Stage = factor(x = Stage,
                   levels = 1:6,
                   labels = c('Orientation', 'ResQuestion', 'LitRev',
                              'Method & Data', 'Analysis', 'Writing')),
    Start = as.Date(Start),
    End = as.Date(End)
  )

### Plot
ggplot(data = tbl,
       mapping = aes(x = Start, xend = End, y = Stage, yend = Stage,
                     color = Importance)) +
  annotate('rect', xmin = as.Date('2024-01-01'), xmax = as.Date('2024-01-31'),
           ymin = .5, ymax = tbl$Stage %>% levels() %>% length() + .5,
           fill = '#6fb3e3', alpha = .35) +
  annotate('rect', xmin = as.Date('2024-03-21'), xmax = as.Date('2024-06-30'),
           ymin = .5, ymax = tbl$Stage %>% levels() %>% length() + .5,
           fill = '#6fb3e3', alpha = .35) +
  geom_segment(size = 7) +
  labs(title = 'GANTT chart',
       subtitle = 'Planning for STREEM thesis',
       caption = 'EXAMPLE by Leon Bremer') +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %y") +
  scale_y_discrete(limits = rev) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    axis.title = element_blank()
  ) +
  scale_color_continuous(low = '#d9db4b', high = '#e65b05')

ggsave('figures/gantt_example.pdf',
       width = 8,
       height = 3)

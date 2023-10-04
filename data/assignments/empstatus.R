library(tidyverse)
library(waffle)
library(MetBrewer)

data <- read.csv("empstatus.csv")

plotdata <- data |> 
  mutate(empstatus = factor(empstatus, levels = c("Full time work","Part time work","Parental leave","Unemployed","Non-active"))) |> 
  pivot_longer(-empstatus, names_to = "gender", values_to = "values")

ggplot(plotdata, aes(fill = empstatus, values = values)) +
  geom_waffle(color = "white", size=1.15, n_rows = 5, na.rm = T) +
  facet_wrap(~gender, ncol=1) +
  scale_fill_met_d(name = "Juarez") +
  coord_equal(expand = F) +
  labs(title = "Gender roles in the compatibility of family and work",
       subtitle = "Employment status of parents with children aged up to 15 years",
       caption="Data: Statistics Austria. Figure: @matschnetzer") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(strip.text.x = element_text(size = 12, hjust = 0, margin = margin(t = 1, unit = "lines")),
        plot.title = element_text(size = 16, margin = margin(b = 0.3, unit = "lines")),
        plot.caption = element_text(margin = margin(t = 1, unit = "lines")),
        legend.title = element_blank()) +
  theme_enhance_waffle() 


ggsave("empstatus.png", width = 8, height = 4.5, dpi = 320, bg = "white")

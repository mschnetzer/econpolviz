library(tidyverse)

# Load data
raw <- read_csv("incomeineq.csv")

# Show all available countries, but we don't want country groups (EA18, EU15, etc.) 
# Filter all countries that have a 2-digit code
unique(raw$geo)
filtered <- raw |> filter(str_length(geo) < 3, time %in% c(2000,2022))

# Not all countries have both years available -> filter those with both values!
filtered |> count(geo)
plotdata <- filtered |> filter(n() > 1, .by = geo)

plotdata |> 
  ggplot(aes(x = geo, y = values)) +
  geom_line(aes(group = geo), linewidth = 3, color = "gray90") +
  geom_point(aes(color = factor(time)), size = 3) +
  scale_color_manual(name = NULL, values = c("goldenrod1","midnightblue"),
                     guide = guide_legend(direction = "horizontal")) + 
  labs(x = NULL, y = "Gini index", title = "Change in income inequality in Europe",
       subtitle = "Gini coefficient of disposable household income, 2000-2022",
       caption = "Source: Eurostat. Figure: Your Name") +
  theme_minimal() +
  theme(legend.position = c(0.8,0.9),
        plot.title.position = "plot",
        plot.caption = element_text(size = 7, margin = margin(t = 1, unit = "lines")),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2))

ggsave("incomeineq.png", width = 8, height = 5, dpi = 320, bg = "white")

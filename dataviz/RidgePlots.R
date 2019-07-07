library(ggplot2)
library(ggridges)
library(plotly)
library(viridis)
lincoln_weather <- lincoln_weather
plot <- ggplot(lincoln_weather, aes(x = `Mean Temperature [F]`, y = `Month`, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_fill_viridis(name = "Temp. [F]", option = "C") +
  labs(title = 'Temperatures in Lincoln NE in 2016')
plotly::ggplotly(plot)

library(DT)
datatable(selected_plot, options = list(pageLength = 25))
library(d3heatmap)
d3heatmap(mtcars, scale="column", colors="Blues")

library(purrr) # map function to make grouped categories argument
library(dplyr) # for select function

data(mpg, package = "ggplot2")

mpgg <- mpg %>%
  filter(class %in% c("suv", "compact", "midsize")) %>%
  group_by(class, manufacturer) %>%
  summarize(count = n())

categories_grouped <- mpgg %>%
  group_by(name = class) %>%
  do(categories = .$manufacturer) %>%
  list_parse()

highchart() %>%
  hc_xAxis(categories = categories_grouped) %>%
  hc_add_series(data = mpgg, type = "bar", hcaes(y = count, color = manufacturer),
                showInLegend = FALSE)

library("highcharter")
data(diamonds, mpg, package = "ggplot2")

hchart(mpg, "scatter", hcaes(x = displ, y = hwy, group = class))

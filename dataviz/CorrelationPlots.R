devtools::install_github("kassambara/ggcorrplot")
devtools::install_github("ropensci/iheatmapr")
library(iheatmapr)
data(measles, package = "iheatmapr")

main_heatmap(measles, name = "Measles<br>Cases", x_categorical = FALSE,
             layout = list(font = list(size = 8))) %>%
  add_col_groups(ifelse(1930:2001 < 1961,"No","Yes"),
                 side = "bottom", name = "Vaccine<br>Introduced?",
                 title = "Vaccine?",
                 colors = c("lightgray","blue")) %>%
  add_col_labels(ticktext = seq(1930,2000,10),font = list(size = 8)) %>%
  add_row_labels(size = 0.3,font = list(size = 6)) %>%
  add_col_summary(layout = list(title = "Average<br>across<br>states"),
                  yname = "summary")  %>%
  add_col_title("Measles Cases from 1930 to 2001", side= "top") %>%
  add_row_summary(groups = TRUE,
                  type = "bar",
                  layout = list(title = "Average<br>per<br>year",
                                font = list(size = 8)))

library(ggplot2)
library(ggcorrplot)
devtools::install_github("hrbrmstr/hrbrthemes")
library(hrbrthemes)
library(gcookbook)
library(tidyverse)
ggplot(mtcars, aes(mpg, wt)) +
  geom_point(aes(color=factor(carb))) +
  labs(x="Fuel efficiency (mpg)", y="Weight (tons)",
       title="Seminal ggplot2 scatterplot example",
       subtitle="A plot that is only useful for demonstration purposes",
       caption="Brought to you by the letter 'g'") +
  scale_color_ipsum() +
  theme_ipsum_rc()


# Correlation matrix
data(mtcars)
corr <- round(cor(mtcars), 1)

# Plot
ggcorrplot(corr, hc.order = TRUE,
           type = "lower",
           lab = TRUE,
           lab_size = 3,
           method="circle",
           colors = c("tomato2", "white", "springgreen3"),
           title="Correlogram of mtcars",
           ggtheme=theme_bw)
library(ggplot2)
theme_set(theme_bw())

# plot
g <- ggplot(mpg, aes(manufacturer, cty))
g + geom_boxplot() +
  geom_dotplot(binaxis='y',
               stackdir='center',
               dotsize = .5,
               fill="red") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) +
  labs(title="Box plot + Dot plot",
       subtitle="City Mileage vs Class: Each dot represents 1 row in source data",
       caption="Source: mpg",
       x="Class of Vehicle",
       y="City Mileage")

p <- ggplot(`Random distribution`, aes_string('Bimodal', 'Normal')) +
  aes_string(colour = 'Class') +
  geom_point() + theme_bw(15)

ggExtra::ggMarginal(
  p,
  type = 'density',
  margins = 'both',
  size = 5,
  colour = '#FF0000',
  groupFill = TRUE
)
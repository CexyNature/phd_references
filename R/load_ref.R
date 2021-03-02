# This script loads all references used in César Herrera's thesis

library(here)
library(bib2df)

here::here()

path <- "data/raw/thesis_references.txt"

df <- bib2df(path)
colnames(df)

df[df$YEAR=="2019},","BIBTEXKEY"]


min(df$YEAR)
max(df$YEAR)

library(dplyr)

ref_x_year <- df %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(n = n())

ref_x_type <- df %>%
  dplyr::group_by(TYPE) %>%
  dplyr::summarise(n = n())


library(ggplot2)
library(extrafont)

loadfonts(device = "win")

# Custom theme:
theme_custom_cesar <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = "Roboto Condensed", color = "#22211d"),
      # Axes
      axis.line.y = element_line(size = 0.51),
      axis.line.x = element_line(size = 0.51),
      axis.text.x = element_text(size = 12),
      axis.text.y = element_text(size = 12),
      axis.ticks = element_line(size = 0.51),
      # axis.ticks.x = element_line(),
      axis.title.x = element_text(size = 12),
      axis.title.y = element_blank(),
      # Grid
      # panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#f5f5f2",color = NA), 
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.background = element_rect(fill = "#f5f5f2", color = NA), 
      panel.spacing = unit(0.2, "lines"),
      # Legend
      legend.background = element_rect(fill = "#f5f5f2", color = NA),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 14, hjust = 0, color = "#4e4d47"),
      legend.position = "bottom",
      # Strip
      strip.text = element_blank(),
      plot.title = element_text(size = 24, hjust = 0.5, color = "#4e4d47"),
      plot.subtitle = element_text(size = 16, hjust = 0.5, color = "#4e4d47", 
                                   margin = margin(b = 0.2, 
                                                   t = 0.1, 
                                                   l = 2, 
                                                   unit = "cm"), 
                                   debug = F),
      plot.caption = element_text(size = 10, 
                                  hjust = .5, 
                                  margin = margin(t = 0.5, 
                                                  b = 0, 
                                                  unit = "cm"), 
                                  color = "#939184"),
      ...
    )
}





ref_per_year_p <- ggplot(ref_x_year, aes(x=YEAR, y=n)) +
  geom_bar(stat = "identity", fill = "#66003A") +
  # Year 1926
  annotate("text", 
           x= 1926.5, 
           y=25,
           label = paste0(df[df$YEAR == 1926, "TITLE"], 
                          " by\n", 
                          unlist(df[df$YEAR == 1926, "AUTHOR"], use.names = F)),
           size = 3.5,
           hjust = 0,
           color = "#66003A") +
  annotate("rect", 
           xmin=1926, 
           xmax=1959, 
           ymin=23.5 , 
           ymax=26.5,
           alpha=0.2,
           color="#66003A",
           fill="#8a817c") +
  geom_segment(aes(x = 1926, y = 26, xend = 1926, yend = 2),
               color = "#66003A",
               size = 0.5,
               arrow = arrow(length = unit(0.2, "cm"))) +
  # Year 1935
  annotate("text", 
           x= 1935.5, 
           y=20,
           label = paste0(df[df$YEAR == 1935, "TITLE"], 
                          " by\n", 
                          unlist(df[df$YEAR == 1935, "AUTHOR"], use.names = F)),
           size = 3.5,
           hjust = 0,
           color = "#66003A") +
  annotate("rect", 
           xmin=1935, 
           xmax=1990, 
           ymin=18.5, 
           ymax=21.5,
           alpha=0.2,
           color="#66003A",
           fill="#8a817c") +
  geom_segment(aes(x = 1935, y = 20, xend = 1935, yend = 2),
               color = "#66003A",
               size = 0.5,
               arrow = arrow(length = unit(0.2, "cm"))) +
  # Year 1943
  annotate("text", 
           x= 1943.5, 
           y=15,
           label = paste0(df[df$YEAR == 1943, "TITLE"], 
                          " by\n", 
                          unlist(df[df$YEAR == 1943, "AUTHOR"], use.names = F)),
           size = 3.5,
           hjust = 0,
           color = "#66003A") +
  annotate("rect", 
           xmin=1943, 
           xmax=1991, 
           ymin=13.5 , 
           ymax=16.5,
           alpha=0.2,
           color="#66003A",
           fill="#8a817c") +
  geom_segment(aes(x = 1943, y = 15, xend = 1943, yend = 2),
               color = "#66003A",
               size = 0.5,
               arrow = arrow(length = unit(0.2, "cm"))) +
  # Year 1961
  annotate("text", 
           x= 1961.5, 
           y=10,
           label = paste0(df[df$YEAR == 1961, "TITLE"], 
                          " by\n", 
                          unlist(df[df$YEAR == 1961, "AUTHOR"], use.names = F)[1],
                          " and ",
                          unlist(df[df$YEAR == 1961, "AUTHOR"], use.names = F)[2]),
           size = 3.5,
           hjust = 0,
           color = "#66003A") +
  annotate("rect", 
           xmin=1961, 
           xmax=1991, 
           ymin=8.5 , 
           ymax=11.5,
           alpha=0.2,
           color="#66003A",
           fill="#8a817c") +
  geom_segment(aes(x = 1961, y = 10, xend = 1961, yend = 2),
               color = "#66003A",
               size = 0.5,
               arrow = arrow(length = unit(0.2, "cm"))) +
  scale_x_continuous(expand = c(0, 0),
                     limits = c(1925, 2021),
                     breaks = seq(1920, 2020, 5),
                     labels = seq(1920, 2020, 5)) + 
  scale_y_continuous(limits = c(0, 30), 
                     breaks = seq(5, 30, 5),
                     expand = c(0, 0)) +
  labs(title = "Standing on the shoulders of giants",
       subtitle = "References used in my thesis per year",
       x = "Year",
       y = "Number of references",
       caption = "CC BY-SA @cexynature") +
  theme_custom_cesar()


ref_per_year_p

ggsave(filename = "analysis/figures/ref_per_year.png", 
       ref_per_year_p, 
       width = 9, height = 8, units = "in", dpi = 600)


library(waffle)
library(hrbrthemes)

ref_x_type_p <- ggplot(ref_x_type, aes(fill = TYPE, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 10, flip = TRUE) +
  # facet_wrap(~category, nrow = 1, strip.position = "bottom") +
  scale_x_discrete() + 
  scale_y_continuous(labels = function(x) x * 10,
                     expand = c(0,0)) +
  scale_fill_manual(name = NULL,
                    values = c("#9e2a2b", "#bc6c25", "#2d6a4f", "#74c69d", "#40916c", "#bcb8b1", "#b7e4c7")) +
  # ggthemes::scale_fill_tableau(name=NULL) +
  coord_equal() +
  labs(title = "Sampling effort",
       # subtitle = paste0("Video success rate ", round(success_rate, 2)),
       subtitle = "",
       x = "", 
       y = "Sampling events") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), 
        axis.ticks.y = element_line(),
        legend.position = "bottom") +
  guides(fill = guide_legend(reverse = FALSE, nrow = 2))

g01

ggsave(filename = "fig/waffle_chart_all_1.png", g01, 
       width = 16, height = 8, units = "in", dpi = 600)





df[df$TYPE == "Online Database", ]

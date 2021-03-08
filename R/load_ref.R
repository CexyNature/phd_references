# This script loads all references used in César Herrera's thesis and
# creates a few visualization summaries

library(here)
library(bib2df)
here::here()

path <- "data/raw/thesis_references.txt"

df <- bib2df(path)
# colnames(df)
# min(df$YEAR)
# max(df$YEAR)

library(dplyr)

ref_x_year <- df %>%
  dplyr::group_by(YEAR) %>%
  dplyr::summarise(n = n())

ref_x_type <- df %>%
  dplyr::group_by(TYPE) %>%
  dplyr::summarise(n = n())

ref_x_journal <- df %>%
  dplyr::filter(TYPE == "Journal Article") %>%
  dplyr::group_by(JOURNAL) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

library(ggplot2)
library(extrafont)
loadfonts()
# loadfonts(device = "win")

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


# Create visualization: number of references per year
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
       subtitle = "References in my thesis per year",
       x = "Year",
       y = "Number of references",
       caption = "CC BY-SA @cexynature") +
  theme_custom_cesar()


ref_per_year_p

ggsave(filename = "analysis/figures/ref_per_year.png", 
       ref_per_year_p, 
       width = 9, height = 8, units = "in", dpi = 600)


# Create visualization: number of references per type
library(waffle)
library(hrbrthemes)

ref_x_type_p <- ggplot(ref_x_type, aes(fill = TYPE, values = n)) +
  geom_waffle(color = "white", size = .25, n_rows = 20, flip = TRUE) +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(labels = function(x) x * 20,
                     expand = c(0,0)) +
  scale_fill_manual(name = NULL,
                    values = c("#c9cba3", "#adc178", "#2d6a4f", 
                               "#ff7aa2", "#b9375e", "#1780A1", 
                               "#ffc300", "#8e9aaf", "#d08c60")) +
  coord_equal() +
  labs(title = "Standing on the shoulders of giants",
       subtitle = "References in my thesis per type",
       caption = "CC BY-SA @cexynature") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(panel.grid = element_blank(), 
        axis.ticks.y = element_line(),
        legend.position = "bottom",
        legend.box = "horizontal") +
  guides(fill = guide_legend(reverse = FALSE, nrow = 3)) +
  theme_custom_cesar()

ref_x_type_p

ggsave(filename = "analysis/figures/ref_per_type.png", ref_x_type_p, 
       width = 9, height = 8, units = "in", dpi = 600)


# Create visualization: number of references per journal (only most cited journals)
# References per journal
# "Australian Journal of Ecology" == "Austral Ecology"
# "ISPRS Annals of Photogrammetry, Remote Sensing and Spatial Information Sciences" == "ISPRS Journal of Photogrammetry and Remote Sensing"
sum(ref_x_journal$n)
ref_x_journal_selection <- ref_x_journal[ref_x_journal$n > 3,]
other_jouranls <- data.frame(JOURNAL = "Other journals", n = sum(ref_x_journal$n) - sum(ref_x_journal_selection$n))
ref_x_journal_selection <- rbind(ref_x_journal_selection, other_jouranls)
ref_x_journal_selection$JOURNAL <- factor(ref_x_journal_selection$JOURNAL, levels = c(ref_x_journal_selection$JOURNAL))

ref_x_journal_p <- ggplot(ref_x_journal_selection, aes(x = JOURNAL, y = n)) +
  geom_bar(stat = "identity", fill = "#b392ac", color = "#4f5d75") +
  scale_x_discrete(expand = c(0,0),
                   limits = rev(levels(ref_x_journal_selection$JOURNAL))) +
  scale_y_continuous(expand = c(0,0), 
                     limits = c(1, 190),
                     trans = "log10",
                     breaks = c(5, 10, 15, 20, 180),
                     labels = c("5", "10", "15", "20", "180")) +
  # geom_hline(yintercept = 20,
  #            linetype = "dotted",
  #            color = "#bfc0c0") +
  # geom_hline(yintercept = 15,
  #            linetype = "dotted",
  #            color = "#bfc0c0") +
  # geom_hline(yintercept = 10,
  #            linetype = "dotted",
  #            color = "#bfc0c0") +
  # geom_hline(yintercept = 5,
  #            linetype = "dotted",
  #            color = "#bfc0c0") +
  coord_flip() +
  labs(title = "Standing on the shoulders of giants",
       subtitle = "References in my thesis per Journal",
       caption = "CC BY-SA @cexynature",
       y = bquote("Number of citations - "~log[10]~" scale")) +
  geom_segment(aes(x = 0, y = 30, xend = 2, yend = 40),
               color = "#f5f5f2",
               size = 2) +
  geom_segment(aes(x = 0, y = 34, xend = 2, yend = 45),
               color = "#f5f5f2",
               size = 2) +
  theme_custom_cesar()

ref_x_journal_p

ggsave(filename = "analysis/figures/ref_per_journal.png", ref_x_journal_p, 
       width = 12, height = 8, units = "in", dpi = 600)


# Create visualization: number of references per author
ls_authors <- unlist(df$AUTHOR)
ls_authors_ln <- gsub(",.*$", "", ls_authors)
ls_authors_ln <- data.frame(author = ls_authors_ln, n = 1)

ls_authors_ln <- ls_authors_ln %>%
  dplyr::group_by(author) %>%
  na.omit() %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

ls_authors_fn <- data.frame(author = ls_authors, n = 1)
ls_authors_fn$author[ls_authors_fn$author == "Sheaves, Marcus"] <- "Sheaves, M."
ls_authors_fn$author[ls_authors_fn$author == "Baker, Ronald"] <- "Baker, R."

ls_authors_fn <- ls_authors_fn %>%
  dplyr::group_by(author) %>%
  na.omit() %>%
  dplyr::summarise(n = n()) %>%
  dplyr::arrange(desc(n))

library(wordcloud2)
colnames(ls_authors_ln) <- c("word", "freq")
ls_authors_ln <- as.data.frame(ls_authors_ln)

# test <- ls_authors_ln[ls_authors_ln$freq >2,]
# library(webshot)
# library(htmlwidgets)
# webshot::install_phantomjs()

out_wc <- wordcloud2(ls_authors_ln, 
           figPath = "data/raw/thank_you.jpg", 
           gridSize = 5,
           size = 0.5,
           minSize = 0.03,
           color = "black",
           shuffle = T,
           backgroundColor="#FFC2D9")

out_wc

out_wc <- wordcloud2(ls_authors_ln, 
                     figPath = "data/raw/thank_you.jpg", 
                     gridSize = 5,
                     size = 0.4,
                     minSize = 0.03,
                     color = "black",
                     shuffle = T,
                     backgroundColor="#FFC2D9")

out_wc

# saveWidget(out_wc, "tmp.html", selfcontained = F)
# webshot("tmp.html","analysis/figures/thank_you_wc.png", delay = 1, vwidth = 1000, vheight=1000)

letterCloud(ls_authors_ln, word = "Thank You", color="black", backgroundColor="#ff70a6")

letterCloud(ls_authors_ln, word = "Thank You", 
            gridSize = 4,
            size = 0.3,
            minSize = 0.03,
            color = "black",
            shuffle = T,
            backgroundColor="#FFC2D9")



with_sheaves <- which(sapply(df$AUTHOR, function(x) "Sheaves, M." %in% x))
with_sheaves <- df[with_sheaves, ]

library(ggplot2)

ggsave_pref <- function(...) {
  
  ggsave(..., width = 16, units = "cm", dpi = 1200)
  
}

d_back <- rgb(242, 244, 239, maxColorValue = 255)
d_text <- rgb(75, 75, 75, maxColorValue = 255)
d_frame <- rgb(65, 65, 65, maxColorValue = 255)

d_1 <- rgb(225, 170, 40, maxColorValue = 255)
d_2 <- rgb(30, 95, 70, maxColorValue = 255)
d_3 <- rgb(126, 143, 117, maxColorValue = 255)
d_4 <- rgb(239, 205, 131, maxColorValue = 255)
d_5 <- rgb(225, 120, 50, maxColorValue = 255)

dc_1 <- d_1
dc_2 <- c(d_1, d_2)
dc_3 <- c(d_1, d_2, d_3)
dc_5 <- c(d_1, d_2, d_3, d_4, d_5)


standard_theme <- theme()
### keinen Rahmen plotten, da Kombination von Plots
theme_set(
  theme(
    text = element_text(family = "Calibri", size = 8),
    plot.title = element_text(size = 7, hjust = 0.5),
    legend.title = element_text(size = 10),
    legend.position = "bottom",
    legend.spacing.x = unit(.1, 'cm'),
    legend.justification = "center",
    legend.margin = margin(0, 0, 0, 0),
    legend.text = element_text(size = 8, family = "Calibri"),
    axis.text = element_text(size = 10, family = "Calibri", hjust = 0.5, vjust = 0.5),
    axis.title = element_text(size = 5.5, family = "Calibri"),
    axis.line.x.bottom = element_blank(),
    axis.title.x = element_text(margin = margin(6, 0, 0, 0), size = 8),
    axis.ticks.length = unit(0.2, "cm"),  # Länge der Ticks anpassen
    axis.ticks.x  = element_line(size = 0.5, colour = "#EEEEEE"), 
    axis.ticks.y = element_blank(),
    axis.line.x = element_line(size = 0.5, color = "#EEEEEE"),
    # Behalte nur die Hauptgitterlinien
    panel.grid.major.y = element_line(colour = "#EEEEEE", linewidth = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),  # Entferne die Nebengitterlinien# Entferne die vertikalen Hauptgitterlinien# Hauptgitterlinien
    plot.margin = unit(c(1, 1, 0.5,0.5), "cm"),
    panel.border = element_blank(),
    
    # Hintergrund
    panel.background = element_rect(fill = "white", color = "white"),
    plot.background = element_rect(fill = "white", color = "white"),  # Weißer Plot-Hintergrund
    legend.background = element_rect(fill = "white", linewidth = 0.2, linetype = "solid", colour = "white"),
    
    strip.placement = "outside",
    strip.background = element_rect(fill = "white"),
    strip.text.y = element_text(margin = margin(0, 0, 0, 0)),
    strip.text = element_text(size = 7, family = "Calibri"),
    legend.key.width = unit(0, "cm"),
    legend.key.height = unit(0.1, "cm")
  )
)

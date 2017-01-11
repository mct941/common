theme_slide <- function (...) {
  theme(strip.text = element_text(size = rel(2)),
        axis.text = element_text(size = rel(2)),
        axis.title = element_text(size = rel(2.25)),
        legend.text = element_text(size = rel(2)),
        #panel.margin = unit(2, "lines"),
        legend.title = element_text(size = rel(2.25)),
        plot.title = element_text(size = rel(3)),...)
}

theme_basic <- function(...) {
  theme_bw() + theme(panel.grid.major.x=element_blank(),panel.grid.major.y=element_blank(),
                     panel.grid.minor.x=element_blank(),panel.grid.minor.y=element_blank())
}

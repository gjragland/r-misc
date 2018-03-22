#R
#GJR 3/22/2018
#ggplot2 theme for black background plots (presentation)

theme_black = function() {
 
  theme_classic() %+replace%
 
    theme(
      # Specify axis options
      axis.line = element_line(color="white"),
      panel.background = element_rect(fill = "black"),
      plot.background = element_rect(fill = "black"),
      axis.text = element_text(color = "white"),
      axis.title = element_text(color = "white"),
      legend.background = element_rect(color = NA, fill = "black"),
      legend.text = element_text(color = "white")
      #guides(colour = guide_legend(override.aes = list(col="white")))
    )
}

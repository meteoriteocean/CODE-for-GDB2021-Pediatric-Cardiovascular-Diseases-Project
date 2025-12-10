#Figure 2
rm(list=ls())

plot1<-readRDS(file = here("/data_processed/", "f2mapA.RDS"))
plot2<-readRDS(file = here("/data_processed/", "fmap19_20.RDS"))
plot3<-readRDS(file = here("/data_processed/", "fmap20_21.RDS"))

combined_plot_1 <- grid.arrange(plot1, plot2, plot3, nrow = 3, heights = c(1, 1, 1))

label_grob <- grobTree(
  textGrob("a", x = 0, y = 1, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("b", x = 0, y = 0.66, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("c", x = 0, y = 0.33, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold"))
)

final_plot_1 <- grid.arrange(combined_plot_1, left = label_grob)

ggsave("/figure 2.png", final_plot_1, width = 13.5, height = 24*13.5/15, units = "in",dpi = 1200)
ggsave("/figure 2.pdf", final_plot_1, width = 13.5, height = 24*13.5/15, units = "in",dpi = 1200)

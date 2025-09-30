rm(list = ls())
df_global_year <- readRDS(file = here("/data_processed/", "df_global_year.RDS"))
df_chd_birth <- readRDS(file = here("/data_processed/","birth.RDS"))
df_chd_birth<-df_chd_birth%>%
  filter(location_name =="Global",metric_name == "Rate")%>%
  mutate(cause_name = ifelse(cause_name=="Congenital heart anomalies","CHD",cause_name))
df_filtered <- df_global_year %>%
  filter(measure_name %in% c("Prevalence", "Incidence"))%>%
  mutate(cause_name=ifelse(cause_name == "Cardiovascular diseases","NC-CVD","CHD"))
df_cvd_inci_rate<-df_filtered %>%
  filter(cause_name == "NC-CVD",measure_name == "Incidence",metric_name == "Rate")
df_inci<-bind_rows(df_cvd_inci_rate,df_chd_birth)
df_pr_CVD <- df_filtered%>%
  filter(measure_name == "Prevalence", cause_name == "NC-CVD")
df_pr_CHA <- df_filtered%>%
  filter(measure_name == "Prevalence",cause_name == "CHD")
df_in_CVD <- df_filtered%>%
  filter(measure_name == "Incidence",cause_name == "NC-CVD")
df_in_CHA <- df_filtered%>%
  filter(measure_name == "Incidence", cause_name == "CHD")
breaks_data <- df_filtered %>%
  filter(year %in% c(1992, 1997, 2002, 2007, 2012, 2017, 2021))
df_target <- df_pr_CHA%>%
  filter(metric_name == "Number",sex_name =="Both")
colors <- c("Male" = "#3b54a6" , "Female" = "#b61642", "Both" = "#288358")
get_limits<-function(data, measure, metric) {
  df_0<-data%>%filter(measure_name == measure, metric_name == metric,cause_name%in%c("NC-CVD","CHD"))
  return(max(df_0$upper)*1.1)
}
create_plot <- function(data, measure, metric, cause, value) {
  max_l<-get_limits(data,measure,metric)
  ggplot(data %>% filter(measure_name == measure, metric_name == metric,cause_name == cause),
         aes(x = year, y = val, color = sex_name)) +
    geom_ribbon(aes(ymin = lower, ymax = upper, fill = sex_name), color = NA, alpha = 0.15) +
    geom_line(linewidth = 1) +
    scale_x_continuous(breaks = c(1992, 1997, 2002, 2007, 2012, 2017, 2021), limits = c(1991, 2022), expand = c(0, 0)) +
    scale_y_continuous(labels = function(x) {
      lab <- scales::scientific_format()(x)
      zero <- x == 0    
      lab[zero] <- "0" 
      idx <- !zero
      lab <- gsub("e+?", "%*%10^", lab)   
      parse(text = lab)                     
    }, limits = c(0, max_l)) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors)  +
    theme_classic() +
    theme(
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 10),
      legend.title = element_text(size = 12, face = "bold"),
      legend.text = element_text(size = 10),
      panel.grid.major = element_line(color = "gray", linetype = "dashed"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      x = "Year",
      y = value,
      color = "Sex",
      fill = "Sex")
}
pc_pr_CVD_N <-create_plot(df_filtered,"Prevalence","Number","NC-CVD","Number")
pc_pr_CHD_N <-create_plot(df_filtered,"Prevalence","Number","CHD","Number")
pc_in_CVD_N <-create_plot(df_filtered,"Incidence","Number","NC-CVD","Number")
pc_in_CHD_N <-create_plot(df_filtered,"Incidence","Number","CHD","Number")
pc_pr_CVD_R <-create_plot(df_filtered,"Prevalence","Rate","NC-CVD","Rate (per 100k per year)")
pc_pr_CHD_R <-create_plot(df_filtered,"Prevalence","Rate","CHD","Rate (per 100k per year)")
pc_in_CVD_R <-create_plot(df_filtered,"Incidence","Rate","NC-CVD","Rate (per 100k per year)")
pc_in_CHD_R <-create_plot(df_chd_birth,"Incidence","Rate","CHD","Rate (per 100k per year)")
yline <- c(1,0.75,0.50,0.25)
xline <- c(0.003,0.503)
label_grob <- grobTree(
  textGrob("a", x = xline[[1]], y = yline[[1]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("b", x = xline[[2]], y = yline[[1]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("c", x = xline[[1]], y = yline[[2]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("d", x = xline[[2]], y = yline[[2]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("e", x = xline[[1]], y = yline[[3]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("f", x = xline[[2]], y = yline[[3]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("g", x = xline[[1]], y = yline[[4]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("h", x = xline[[2]], y = yline[[4]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")))
g <- gridExtra::arrangeGrob(pc_in_CVD_N,pc_in_CHD_N,pc_in_CVD_R,pc_in_CHD_R,pc_pr_CVD_N,pc_pr_CHD_N,pc_pr_CVD_R,pc_pr_CHD_R, ncol = 2)
plot_combined<- grobTree(g, label_grob)  
ggsave("/figure.pdf", plot_combined, width = 11, height = 16, units = "in")
ggsave("/figure.png", plot_combined, width = 11, height = 16, units = "in")
write.csv(df_filtered,file = "/figure 1.csv")


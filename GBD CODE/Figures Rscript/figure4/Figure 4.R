rm(list=ls())

df<- readRDS(file = here("/data_processed/", "df_diseases_1.RDS"))
file_name <- "Panoramic Overview of Different Types of Cardiovascular Diseases"
df_main <- df %>%
  mutate(measure_name = ifelse(measure_name == "DALYs (Disability-Adjusted Life Years)", "DALYs", measure_name)) %>%
  mutate(or = ifelse(cause_name %in% c("Congenital heart anomalies","Cardiovascular diseases"),"NC-CVD & CHD","NC-CVD"))%>%
  mutate(cause_name = ifelse(cause_name=="Cardiovascular diseases","NC-CVD",cause_name))%>%
  filter(measure_name %in% c("Incidence", "Prevalence", "Deaths", "DALYs"),
         location_name == "Global",
         year %in% c(2021),
         age_name %in% c("<20 years"),
         metric_name == "Rate") %>%
  group_by(measure_name, or) %>%
  mutate(total_val = sum(val),
         percentage = (val / total_val) * 100) %>%
  ungroup()

df_filtered_i <- df %>%
  filter(measure_name == "Incidence",
         location_name %in% c("High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"),
         year %in% c(1992, 2021),
         #         age_name %in% c("<20 years"),
         !grepl("Cardiovascular diseases",cause_name),
         !grepl("Congenital heart anomalies",cause_name),
         metric_name == "Rate")%>%
  group_by(location_name,age_name, year) %>%
  mutate(total_val = sum(val),
         percentage = (val / total_val) * 100) %>%
  ungroup()

df_filtered_p <- df %>%
  filter(measure_name == "Prevalence",
         location_name %in% c("High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"),
         year %in% c(1992, 2021),
         #         age_name %in% c("<20 years"),
         !grepl("Cardiovascular diseases",cause_name),
         !grepl("Congenital heart anomalies",cause_name),
         metric_name == "Rate")%>%
  group_by(location_name,age_name, year) %>%
  mutate(total_val = sum(val),
         percentage = (val / total_val) * 100) %>%
  ungroup()

df_filtered_d <- df %>%
  filter(measure_name == "Deaths",
         location_name %in% c("High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"),
         year %in% c(1992, 2021),
         #         age_name %in% c("<20 years"),
         !grepl("Cardiovascular diseases",cause_name),
         !grepl("Congenital heart anomalies",cause_name),
         metric_name == "Rate")%>%
  group_by(location_name,age_name, year) %>%
  mutate(total_val = sum(val),
         percentage = (val / total_val) * 100) %>%
  ungroup()

location_order <- c("High SDI","High-middle SDI","Middle SDI","Low-middle SDI","Low SDI")
df_filtered_i$location_name <- factor(df_filtered_i$location_name, levels = location_order)
df_filtered_p$location_name <- factor(df_filtered_p$location_name, levels = location_order)
df_filtered_d$location_name <- factor(df_filtered_d$location_name, levels = location_order)

measure_order<- c("Incidence", "Prevalence", "Deaths", "DALYs")
df_main$measure_name<-factor(df_main$measure_name,levels = measure_order)


df_filtered_i$combo_location_year <- paste(df_filtered_i$location_name, df_filtered_i$year)
combo_location_year_order <- paste(rep(location_order, each = 2), c(1992, 2021))
df_filtered_i$combo_location_year <- factor(df_filtered_i$combo_location_year, levels = combo_location_year_order)

df_filtered_p$combo_location_year <- paste(df_filtered_p$location_name, df_filtered_p$year)
combo_location_year_order <- paste(rep(location_order, each = 2), c(1992, 2021))
df_filtered_p$combo_location_year <- factor(df_filtered_p$combo_location_year, levels = combo_location_year_order)

df_filtered_d$combo_location_year <- paste(df_filtered_d$location_name, df_filtered_d$year)
combo_location_year_order <- paste(rep(location_order, each = 2), c(1992, 2021))
df_filtered_d$combo_location_year <- factor(df_filtered_d$combo_location_year, levels = combo_location_year_order)

df_main$combo_measure_or <- paste(df_main$measure_name,df_main$or)
combo_measure_or_order <- paste(rep(measure_order,each = 2),c("NC-CVD & CHD","NC-CVD"))
df_main$combo_measure_or <- factor(df_main$combo_measure_or, levels = combo_measure_or_order)
#
cause_totals_i <- df_filtered_i %>%
  group_by(cause_name) %>%
  summarise(total = sum(val)) %>%
  arrange(desc(total))

cause_totals_p <- df_filtered_p %>%
  group_by(cause_name) %>%
  summarise(total = sum(val)) %>%
  arrange(desc(total))

cause_totals_d <- df_filtered_d %>%
  group_by(cause_name) %>%
  summarise(total = sum(val)) %>%
  arrange(desc(total))

cause_totals_m <- df_main %>%
  group_by(cause_name) %>%
  summarise(total = sum(val)) %>%
  arrange(desc(total))

df_filtered_i$cause_name <- factor(df_filtered_i$cause_name, levels = rev(cause_totals_i$cause_name))
df_filtered_p$cause_name <- factor(df_filtered_p$cause_name, levels = rev(cause_totals_p$cause_name))
df_filtered_d$cause_name <- factor(df_filtered_d$cause_name, levels = rev(cause_totals_d$cause_name))
df_main$cause_name <- factor(df_main$cause_name,levels = rev(cause_totals_m$cause_name))

cause_colors <- c(
  "Congenital heart anomalies" = "#cab2d6",
  "NC-CVD" = "#fdbf6f",
  "Ischemic stroke" = "#a6cee3",
  "Intracerebral hemorrhage" = "#1f78b4", 
  "Subarachnoid hemorrhage" = "#b2df8a",
  "Hypertensive heart disease" = "#33a02c",
  "Endocarditis" = "#fb9a99",
  "Other cardiovascular and circulatory diseases" = "#e31a1c",
  "Rheumatic heart disease" = "#fdbf6f",
  "Ischemic heart disease" = "#ff7f00",
  "Non-rheumatic calcific aortic valve disease" = "#6a3d9a",
  "Non-rheumatic degenerative mitral valve disease" = "#ffff99",
  "Other non-rheumatic valve diseases" = "#b15928",
  "Pulmonary Arterial Hypertension" = "#fccde5",
  "Myocarditis" = "#ffed6f",
  "Other cardiomyopathy" = "#f47971",
  "Alcoholic cardiomyopathy" = "#3db64f", 
  "Atrial fibrillation and flutter" = "#6791cd",
  "Lower extremity peripheral arterial disease" = "#d9d9d9"
)



main_plot <- ggplot(subset(df_main, !(measure_name == "Incidence" & or == "NC-CVD & CHD")), aes(x = combo_measure_or, y = percentage, fill = cause_name, alpha = factor(or))) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = cause_colors, guide = guide_legend(reverse = TRUE)) +
  scale_alpha_manual(values = c("NC-CVD & CHD" = 0.7, "NC-CVD" = 1), guide = "none") +
  labs(x = "Measure percentage", y = "Percentage (%)", fill = "Cause") +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
df_filtered_i_2021 <- df_filtered_i %>%
  filter(year == 2021)
df_filtered_p_2021 <- df_filtered_p %>%
  filter(year == 2021)
df_filtered_d_2021 <- df_filtered_d %>%
  filter(year == 2021)
age_order <- c("<20 years","<5 years", "5-9 years", "10-14 years", "15-19 years" )
#i
age_plots_i <- lapply(age_order, function(age) {
  ggplot(df_filtered_i_2021[df_filtered_i_2021$age_name == age,], aes(x = percentage, y = location_name, fill = cause_name)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = cause_colors, guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +  
    labs(title = age, x = "proportion (%)", y = "Location", fill = "Cause") +  
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 45, hjust = 1),
          legend.position = "none")
})
age_plots_i_arranged <- grid.arrange(grobs = age_plots_i, nrow = 1,top =  textGrob("Incidence", gp = gpar(fontsize = 14, fontface = "bold")))

age_plots_p <- lapply(age_order, function(age) {
  ggplot(df_filtered_p_2021[df_filtered_p_2021$age_name == age,], aes(x = percentage, y = location_name, fill = cause_name)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = cause_colors, guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +  
    labs(title = age, x = "proportion (%)", y = "Location", fill = "Cause") +  
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 45, hjust = 1),
          legend.position = "none")
})
age_plots_p_arranged <- grid.arrange(grobs = age_plots_p, nrow = 1,top =  textGrob("Prevalence", gp = gpar(fontsize = 14, fontface = "bold")))

age_plots_d <- lapply(age_order, function(age) {
  ggplot(df_filtered_d_2021[df_filtered_d_2021$age_name == age,], aes(x = percentage, y = location_name, fill = cause_name)) +
    geom_bar(stat = "identity", position = "stack") +
    scale_fill_manual(values = cause_colors, guide = guide_legend(reverse = TRUE)) +
    scale_x_continuous(labels = scales::percent_format(scale = 1)) +  
    labs(title = age, x = "proportion (%)", y = "Location", fill = "Cause") +  
    theme_minimal() +
    theme(axis.text.y = element_text(angle = 45, hjust = 1),
          legend.position = "none")
})
age_plots_d_arranged <- grid.arrange(grobs = age_plots_d, nrow = 1,top =  textGrob("Mortality", gp = gpar(fontsize = 14, fontface = "bold")))

label_grob <- grobTree(
  textGrob("a", x = 0, y = 1, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("b", x = 0, y = 0.6, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("c", x = 0, y = 0.4, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("d", x = 0, y = 0.2, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold"))
)

final_plot_1 <- grid.arrange(main_plot, age_plots_i_arranged,age_plots_p_arranged,age_plots_d_arranged, nrow = 4, heights = c(1, 0.5,0.5,0.5), left = label_grob)
df <- df%>%
  mutate(measure_name = ifelse(measure_name == "DALYs (Disability-Adjusted Life Years)", "DALYs", measure_name)) %>%
  filter(!grepl("Cardiovascular diseases",cause_name),
         !grepl("Congenital heart anomalies",cause_name))

create_line_plot <- function(df, measure, title) {
  df_ranked <- df %>%
    filter(measure_name == measure, 
           year %in% c(1992, 1997, 2002, 2007, 2012, 2017, 2021),
           location_name == "Global", 
           metric_name == "Rate",
           age_name == "<20 years") %>%
    group_by(year) %>%
    mutate(rank = rank(-val, ties.method = "first")) %>%
    filter(rank <= 10) %>%
    ungroup()
  

  df_2021 <- df_ranked %>%
    filter(year == 2021) %>%
    arrange(rank)
  

  cause_levels <- df_2021$cause_name
  
  df_ranked <- df_ranked %>%
    mutate(cause_name = factor(cause_name, levels = cause_levels))
  
  df_wide <- df_ranked %>%
    dplyr::select(rank, year, cause_name) %>%
    pivot_wider(names_from = year, values_from = cause_name, names_prefix = "year_")
  
  df_long <- df_wide %>%
    pivot_longer(cols = starts_with("year_"), names_to = "year", values_to = "cause") %>%
    mutate(year = gsub("year_", "", year))
  
  ggplot(df_long, aes(x = year, y = rank, color = cause, group = cause)) +
    geom_line() +
    geom_point() +
    scale_color_manual(values = cause_colors) +
    scale_y_reverse(breaks = seq(1, 10, by = 1)) + 
    theme_minimal() +
    labs(title = title,
         x = "Year",
         y = "Rank",
         color = "Cause") +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}


p_i <- create_line_plot(df, "Incidence", "Ranked Causes of NC-CVD Incidence")


p_p <- create_line_plot(df, "Prevalence", "Ranked Causes of NC-CVD Prevalence")


p_d <- create_line_plot(df, "Deaths", "Ranked Causes of NC-CVD Deaths")


p_D <- create_line_plot(df, "DALYs", "Ranked Causes of NC-CVD DALYs")

label_grob <- grobTree(
  textGrob("e", x = 0, y = 1, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("f", x = 0, y = 0.75, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("g", x = 0, y = 0.5, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("h", x = 0, y = 0.25, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold"))
)

final_plot_2 <- grid.arrange(p_i, p_p, p_d, p_D, nrow = 4, heights = c(1,1,1,1), left = label_grob)
final_plot <- grid.arrange(final_plot_1,final_plot_2, nrow = 1, widths = c(5,2), top = textGrob(" ", gp = gpar(fontsize = 16, fontface = "bold")))

ggsave(paste0("/",file_name,"_1.pdf"), final_plot, width = 21, height = 15, units = "in")
ggsave(paste0("/",file_name,"_1.png"), final_plot, width = 21, height = 15, units = "in",dpi = 300)


#figure 5
rm(list=ls())

df_cor<-readRDS(file = here("/data_processed/","df_cor.RDS"))
df_c19<-readRDS(file = here("/data_processed/", "covid.RDS"))
df_c19 <- df_c19%>%
  mutate(cause_name = ifelse(cause_name == "Other COVID-19 pandemic-related outcomes", "other c19", cause_name)) %>%
  filter(year %in% c("2019","2020","2021"))


year_cor_test <- function(measure,measure19,cause,age,sex,cortest){

  c19_rate<- df_c19%>%
    filter(measure_name == measure19,cause_name =="COVID-19",
           metric_name =="Rate",age_name == age,year %in% c("2019","2020","2021"),sex_name == sex)%>%
    dplyr::select(location_id,location_name,year,val)%>%
    dplyr::rename(c19  = val)
 
  cvd_rate<-df_cor%>%
    filter(age_name == age,cause_name == cause)%>%
    dplyr::select(location_id,location_name,year,val)
  
  df <- left_join(cvd_rate,c19_rate,by= c("location_id"="location_id","year"="year"))%>%
    mutate(across(c19,~replace_na(., 0)))%>%
    dplyr::rename(location_name = location_name.x)%>%
    dplyr::select(location_id,location_name,year,val,c19)
  
  c_year<- df %>%
    dplyr::filter(year%in%c(2020,2021))%>%
    group_by(year) %>%
    group_map(.,
              ~{
                valid_idx <- is.finite(.x$val) & is.finite(.x$c19)
                if (sum(valid_idx) < 2) {
                  tibble(year = .y$year, correlation = NA, p_value = NA)
                } else {
                  res <- cor.test(.x$val[valid_idx], .x$c19[valid_idx], method = cortest)
                  tibble(year = .y$year, correlation = res$estimate, p_value = res$p.value)
                }
              }
    )
  result_year <- bind_rows(c_year)

  result_year<-result_year%>%
    mutate(cause_name=cause,measure_cvd = measure,measure_c19 = measure19,age_group=age,sex = sex)%>%
    mutate(p_label = ifelse(p_value <= 0.05, "p < 0.05", "p > 0.05"))
}
r<-year_cor_test("Incidence","Incidence","NC-CVD","<20 years","Both","pearson")


colors <- c("Male" = "#3b54a6" , "Female" = "#b61642", "Both" = "#288358")
create_plot <- function(data,corr,fig_name) {
  ggplot(data,
         aes(x = age_group, y = correlation, color = sex)) +
    geom_point( aes(x = age_group, y = correlation, color = sex, shape = p_label),stroke = 1.2,
                fill = "white") +
    ggrepel::geom_text_repel(
      data = dplyr::filter(data, sex == "Both", !is.na(p_value)),
      aes(label = sprintf("p=%.2e", p_value)),
      vjust = -1,
      size = 3,
      show.legend = FALSE,
      na.rm = TRUE
    ) +
    scale_y_continuous(labels = comma) +
    scale_color_manual(values = colors) +
    scale_fill_manual(values = colors) +
    scale_shape_manual(
      
      values = c("p < 0.05" = 16, "p > 0.05" = 21),
      
      breaks = c("p < 0.05", "p > 0.05"),
      
      labels = c("p < 0.05", "p > 0.05")
      
    ) +
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
      x = "age_group",
      y = "Correlation",
      color = "Gender",
      fill = "Cause",
      shape = "p-value") +
    guides(
      shape = guide_legend(order = 1)
    )
}


ls_age<-unique(df_c19$age_name)
ls_cause <- unique(df_c19$cause_name)
ls_sex <- unique(df_c19$sex_name)
draw_plot<- function(cause,cortest){
  m1<- "Incidence"
  m19<- "Incidence"
  result_all_age<-{}
  for (j in ls_age) {
    age_all_sex<-{}
    for (i in ls_sex) {
      r<-year_cor_test(m1,m19,cause,j,i,cortest)
      age_all_sex <- bind_rows(age_all_sex, r)
    }
    result_all_age<- bind_rows(result_all_age,age_all_sex)
  }
  
  
  result_all_age$age_group <- factor(result_all_age$age_group, levels = ls_age)
  result_csv<-result_all_age
  
  re_2020 <- result_all_age%>%
    filter(year == "2020")
  figname1 = paste0("The ",ifelse(cortest=="pearson","Pearson","Spearman")," Correlation between ",cause," ",m1,"(per 100,000 population) and\n Covid-19 ",m19,"(per 100,000 population) in 2020")
  figure_1<-create_plot(re_2020,cortest,figname1)
  
  re_2021 <- result_all_age%>%
    filter(year == "2021")
  figname2 = paste0("The ",ifelse(cortest=="pearson","Pearson","Spearman")," Correlation between ",cause," ",m1,"(per 100,000 population) and\n Covid-19 ",m19,"(per 100,000 population) in 2021")
  figure_2<-create_plot(re_2021,cortest,figname2)
  
  m1<- "Deaths"
  m19<- "Deaths"
  result_all_age<-{}
  for (j in ls_age) {
    age_all_sex<-{}
    for (i in ls_sex) {
      r<-year_cor_test(m1,m19,cause,j,i,cortest)
      age_all_sex <- bind_rows(age_all_sex, r)
    }
    result_all_age<- bind_rows(result_all_age,age_all_sex)
  }
  result_all_age$age_group <- factor(result_all_age$age_group, levels = ls_age)
  result_csv<-rbind(result_csv,result_all_age)
  re_2020 <- result_all_age%>%
    filter(year == "2020")
  figname1 = paste0("The ",ifelse(cortest=="pearson","Pearson","Spearman")," Correlation between ",cause," ",m1,"(per 100,000 population) and\n Covid-19 ",m19,"(per 100,000 population) in 2020")
  figure_3<-create_plot(re_2020,cortest,figname1)
  
  re_2021 <- result_all_age%>%
    filter(year == "2021")
  figname2 = paste0("The ",ifelse(cortest=="pearson","Pearson","Spearman")," Correlation between ",cause," ",m1,"(per 100,000 population) and\n Covid-19 ",m19,"(per 100,000 population) in 2021")
  figure_4<-create_plot(re_2021,cortest,figname2)
  
  ls_final<-list(figure_1,figure_2,figure_3,figure_4)
}
p_chd<-draw_plot("CHD", "spearman")
p_nccvd<-draw_plot("NC-CVD", "spearman")
plot_combined <-( (p_chd[[1]]+p_chd[[2]])/(p_chd[[3]]+p_chd[[4]])/(p_nccvd[[1]]+p_nccvd[[2]])/(p_nccvd[[3]]+p_nccvd[[4]]) ) +
  plot_annotation(theme = theme(plot.tag = element_text(size = 60, face = "bold")),tag_levels = "a")
ggsave(paste0("/","p5.pdf"), plot_combined, width = 12, height = 16, units = "in")
ggsave(paste0("/","p5.png"), plot_combined, width = 12, height = 16, units = "in")



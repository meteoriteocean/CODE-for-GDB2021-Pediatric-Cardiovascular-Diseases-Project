rm(list=ls())

df <- readRDS(file = here("data_processed/", "df_f2_ICN_20_21.RDS"))
iso3 <- read.csv(here("/","iso3.csv"))


fill_in<-"NC-CVD\nIncidence % Change\n(2020-2021)"
file_name <- "IN Rate Change 2020-2021 plus"

processed_data <- df %>%
  filter(metric_name %in% c("Rate")) %>%
  group_by(year, location_name, sex_name, age_name) %>%
  summarise(estimated_population = val, .groups="drop")

age_clean <- function(val){
  val <- val %>% 
    str_replace(pattern = " to ", replacement = "_") %>% 
    str_replace(pattern = " |-", replacement = "_") %>% 
    str_replace(pattern = "<", replacement = "Under_") %>% 
    str_replace(pattern = "Under_5", replacement = "0_4")
}

pop_groups <- processed_data %>%
  filter() %>%
  mutate(age_name = map_chr(age_name,age_clean))%>%
  group_by(year, location_name, sex_name, age_name) %>%
  summarise(est_val = estimated_population, .groups="drop")

dict <- c("0_4_years" =1 ,"10_14 years" = 3,"15_19 years"=4,"5_9 years"=2,"Under_20_years"=0)
pop_groups <- pop_groups %>%
  mutate(age_group = recode(age_name,!!!dict))%>%
  arrange(year,desc(location_name),desc(sex_name),age_group)

pop_groups_iso3 <- merge(x= pop_groups,y= iso3,by.x="location_name",by.y="Location.Name")
setdiff(unique(pop_groups$location_name),unique(iso3$Location.Name))
setdiff(unique(iso3$Location.Name),unique(pop_groups$location_name))
Pop_num_1 <- pop_groups_iso3 %>%
  filter(year %in% c(2021), sex_name %in% "Both",age_group %in% "0") %>%
  mutate(Pop = est_val ) %>%
  dplyr::select(ISO.3.Code,year, Pop)

Pop_num_2 <- pop_groups_iso3 %>%
  filter(year %in% c(2020), sex_name %in% "Both",age_group %in% "0") %>%
  mutate(Pop = est_val ) %>%
  dplyr::select(ISO.3.Code,year, Pop)

Pop_num <- inner_join(Pop_num_1, Pop_num_2, by = "ISO.3.Code") %>%
  mutate(Pop = ((Pop.x / Pop.y)-1) * 100) %>%
  dplyr::select(ISO.3.Code, Pop)

world_map <- ne_countries(scale = "medium", returnclass = "sf")


world_map <- world_map %>%
  right_join(Pop_num, by = c("iso_a3_eh" = "ISO.3.Code")) %>%
  dplyr::select(geounit, Pop,iso_n3,iso_a3_eh, wb_a3, continent, subregion, region_wb, geometry)


my_colors <- c(
  "#b22643",
  "#eb3237",
  "#f46a4d",
  "#f8995f",
  "#fec87e",
  "#fbe997",
  "#dff2ee",
  "#9dd4e8",
  "#1c97c1",
  "#0a599c"
)

min(Pop_num$Pop, na.rm = TRUE)
max(Pop_num$Pop, na.rm = TRUE)
#######################################

breaks <- c(-15,-5,-1,-0.5,0,0.5,1,2,3,5,15)

interval_labels <- sapply(2:length(breaks), function(i) {
  paste(sprintf("%.1f%%", breaks[i-1]), "to", sprintf("%.1f%%", breaks[i]))
})

color_match <- setNames(rev(my_colors),interval_labels)

world_map$pop_rate_buckets <- cut(
  world_map$Pop,
  breaks = breaks,
  labels = interval_labels,
  include.lowest = TRUE
)

Pop_num$pop_rate_buckets <- cut(
  Pop_num$Pop,
  breaks = breaks,
  labels = interval_labels,
  include.lowest = TRUE
)

world_map$pop_rate_bucket <- factor(
  world_map$pop_rate_buckets,
  levels = rev(interval_labels)
)

Pop_num$pop_rate_bucket <- factor(
  Pop_num$pop_rate_buckets,
  levels = rev(interval_labels)
)

color_scale <- scale_fill_manual(
  values = color_match[world_map$pop_rate_buckets],
  name = fill_in 
)


interval_counts <- Pop_num %>%
  group_by(pop_rate_bucket) %>%
  summarise(count = n())





bar_plot <- ggplot(interval_counts, aes(x = pop_rate_bucket, y = count, fill = pop_rate_bucket)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = color_match, name = fill_in) +
  scale_x_discrete(limits = rev(levels(interval_counts$pop_rate_bucket))) +
  geom_vline(xintercept = 4.5, linetype = "dashed", color = "black", size = 1) +
  theme_minimal() +
  labs(x = "Interval", y = "Number of Countries") +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1)
  )
bar_plot
world_map_plot <- ggplot(world_map) +
  geom_sf(aes(fill = factor(pop_rate_bucket)), color = "white", size = 0.1) +
  color_scale +
  theme_minimal()+
  labs(fill = fill_in)+
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left"
  )

subregions_coord <- list(`Caribbean and central America` = list(xlim = c(-93.647, -58.975), ylim = c(6.621, 27.645)),
                         `Persian Gulf`= list(xlim = c(45.132, 54.317), ylim = c(18.416, 31.391)),
                         `Balkan Peninsula` = list(xlim = c(12.30, 32.08), ylim = c(34.52, 52.75)),
                         `Southeast Asia` = list(xlim = c(97.4, 119.751), ylim = c(-9.276, 9.276)),
                         `West Africa` = list(xlim = c(-17.886, -6.987), ylim = c(6.752, 16.847)),
                         `Eastern Mediterranean` = list(xlim = c(30.322, 38.321), ylim = c(29.114, 34.958)),
                         `Northern Europe` = list(xlim = c(2.769, 27.422), ylim = c(47.990, 59.153)))

subregions_shp <- lapply(names(subregions_coord), function(region) {
  bbox_params <- subregions_coord[[region]]
  bbox <- st_bbox(c(xmin = bbox_params[[1]][1],
                    xmax = bbox_params[[1]][2],
                    ymin = bbox_params[[2]][1],
                    ymax = bbox_params[[2]][2]), crs = 4326)
  
  st_crop(world_map, bbox)
})


map_subregions <- list()
for (region in names(subregions_coord)) {
  bbox_params <- subregions_coord[[region]]
  bbox <- st_bbox(c(xmin = bbox_params[[1]][1], xmax = bbox_params[[1]][2],
                    ymin = bbox_params[[2]][1], ymax = bbox_params[[2]][2]), crs = 4326)
  subregion_map <- st_crop(world_map, bbox)
  
  map_subregions[[region]] <- ggplot(subregion_map) +
    geom_sf(aes(fill = factor(pop_rate_bucket)), color = "white", size = 0.1) +
    scale_fill_manual(values = color_match) +
    labs(caption = region)+
    
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank()
      ,plot.background = element_rect(colour = "black", size = 1)
    )
}


p1 <- map_subregions[[1]]
p2 <- map_subregions[[2]]
p3 <- map_subregions[[3]]
p4 <- map_subregions[[4]]
p5 <- map_subregions[[5]]
p6 <- map_subregions[[6]]
p7 <- map_subregions[[7]]

p14 <- grid.arrange(p1, p2, p3, p4,p5,p6,p7, ncol = 7, widths = c(2.02, 0.97, 1.13, 1.65,1.4,1.12*1.4,1.7))


final_layout1 <- grid.arrange(world_map_plot, p14, nrow = 2, heights = c(1.5, 0.4))


final_plot <- grid.arrange(final_layout1, bar_plot, ncol = 2, widths = c(0.7, 0.2))

saveRDS(final_plot,file = here("/data_processed/", "fmap20_21.RDS"))
ggsave(paste0("pics/mappics/",file_name,".pdf"), final_plot, width = 15, height = 8, units = "in")
ggsave(paste0("pics/mappics/",file_name,".png"), final_plot, width = 15, height = 8, units = "in")


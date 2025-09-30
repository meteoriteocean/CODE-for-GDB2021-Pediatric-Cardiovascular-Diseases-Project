rm(list=ls())

df <- readRDS(file = here("/data_processed/", "df_f2_ICN_1992.RDS"))
iso3 <- read.csv(here("/","iso3.csv"))

fill_in <- "NC-CVD\nIncidence (per 100,000 population per year)\n(2021)"
file_name <- "IN Rate 2021"

processed_data <- df %>%
  filter(metric_name %in% c("Rate"), year == 2021) %>%
  group_by(location_name, sex_name, age_name) %>%
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
  group_by(location_name, sex_name, age_name) %>%
  summarise(est_val = estimated_population, .groups="drop")

dict <- c("0_4_years" =1 ,"10_14 years" = 3,"15_19 years"=4,"5_9 years"=2,"Under_20_years"=0)
pop_groups <- pop_groups %>%
  mutate(age_group = recode(age_name,!!!dict))%>%
  arrange(desc(location_name),desc(sex_name),age_group)

pop_groups_iso3 <- merge(x= pop_groups,y= iso3,by.x="location_name",by.y="Location.Name")

Pop_num <- pop_groups_iso3 %>%
  filter(sex_name %in% "Both",age_group %in% "0") %>%
  mutate(Pop = est_val) %>%
  dplyr::select(ISO.3.Code, Pop)

world_map <- ne_countries(scale = "medium", returnclass = "sf")

world_map <- world_map %>%
  right_join(Pop_num, by = c("iso_a3_eh" = "ISO.3.Code")) %>%
  dplyr::select(geounit, Pop, iso_n3, iso_a3_eh, wb_a3, continent, subregion, region_wb, geometry)

my_colors <- c(
  "#053061",
  "#0a599c",
  "#1c97c1",
  "#9dd4e8",
  "#dff2ee",
  "#fbe997",
  "#fec87e",
  "#f8995f",
  "#f46a4d",
  "#eb3237"
)

pop_min <- min(world_map$Pop, na.rm = TRUE)
pop_max <- max(world_map$Pop, na.rm = TRUE)
color_scale <- scale_fill_gradientn(
  colors = my_colors,
  name = fill_in,
  na.value = "gray90",
  limits = c(pop_min, pop_max)
)

world_map_plot <- ggplot(world_map) +
  geom_sf(aes(fill = Pop), color = "white", size = 0.1) +
  color_scale +
  theme_minimal() +
  labs(fill = fill_in) +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.justification = "left"
  )

subregions_coord <- list(
  `Caribbean and central America` = list(xlim = c(-93.647, -58.975), ylim = c(6.621, 27.645)),
  `Persian Gulf`= list(xlim = c(45.132, 54.317), ylim = c(18.416, 31.391)),
  `Balkan Peninsula` = list(xlim = c(12.30, 32.08), ylim = c(34.52, 52.75)),
  `Southeast Asia` = list(xlim = c(97.4, 119.751), ylim = c(-9.276, 9.276)),
  `West Africa` = list(xlim = c(-17.886, -6.987), ylim = c(6.752, 16.847)),
  `Eastern Mediterranean` = list(xlim = c(30.322, 38.321), ylim = c(29.114, 34.958)),
  `Northern Europe` = list(xlim = c(2.769, 27.422), ylim = c(47.990, 59.153))
)

map_subregions <- list()
for (region in names(subregions_coord)) {
  bbox_params <- subregions_coord[[region]]
  bbox <- st_bbox(c(xmin = bbox_params[[1]][1], xmax = bbox_params[[1]][2],
                    ymin = bbox_params[[2]][1], ymax = bbox_params[[2]][2]), crs = 4326)
  subregion_map <- st_crop(world_map, bbox)
  
  map_subregions[[region]] <- ggplot(subregion_map) +
    geom_sf(aes(fill = Pop), color = "white", size = 0.1) +
    color_scale +
    labs(caption = region) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      plot.background = element_rect(colour = "black", size = 1)
    )
}

p1 <- map_subregions[[1]]
p2 <- map_subregions[[2]]
p3 <- map_subregions[[3]]
p4 <- map_subregions[[4]]
p5 <- map_subregions[[5]]
p6 <- map_subregions[[6]]
p7 <- map_subregions[[7]]

p14 <- grid.arrange(p1, p2, p3, p4, p5, p6, p7, ncol = 7, widths = c(2.02, 0.97, 1.13, 1.65, 1.4, 1.12*1.4, 1.7))

final_layout <- grid.arrange(world_map_plot, p14, nrow = 2, heights = c(1.5, 0.4))
saveRDS(final_layout,file = here("/data_processed/", "f2mapA.RDS"))
csv_data <- df %>%
  filter(metric_name %in% c("Rate"), year == 2021)%>%
  left_join(iso3,by=c("location_name"= "Location.Name"))
write.csv(csv_data,file = here("/", "f2mapA.csv"))
ggsave(paste0("/", file_name, ".pdf"), final_layout, width = 10, height = 7, units = "in")
ggsave(paste0("/", file_name, ".png"), final_layout, width = 10, height = 7, units = "in")

rm(list=ls())
Pop_data_dplyr::selected <-readRDS(file = here("/data_processed/", "Pop_data.RDS"))
df_inci_apc <- readRDS(file = here("/data_processed/apc", "df_inci_apc_cvd.RDS"))
age_replace <- function(df){
  age_modify <- function(val){
    val <- val %>% 
      str_replace(pattern = " to ", replacement = "_") %>% 
      str_replace(pattern = " |-", replacement = "_") %>% 
      str_replace(pattern = "<", replacement = "Under_") %>% 
      str_replace(pattern = "Under_5", replacement = "0_4") %>% 
      str_replace(pattern = "_years", replacement = "") %>% 
      str_replace(pattern = "years", replacement = "")
  }
  
  df <- df %>% 
    mutate(age_name = map_chr(age_name, age_modify))
}
inci_apc_dplyr::selected <- age_replace(df_inci_apc)
saveRDS(inci_apc_dplyr::selected,file = here("/data_processed/apc", "df_inci_apc_cvd_dplyr::selected.RDS"))

#prevalence
rm(list=ls())
Pop_data_dplyr::selected <-readRDS(file = here("/data_processed/", "Pop_data.RDS"))
df_pre_apc <- readRDS(file = here("/data_processed/apc", "df_pre_apc_cvd.RDS"))
age_replace <- function(df){
  age_modify <- function(val){
    val <- val %>% 
      str_replace(pattern = " to ", replacement = "_") %>% 
      str_replace(pattern = " |-", replacement = "_") %>% 
      str_replace(pattern = "<", replacement = "Under_") %>% 
      str_replace(pattern = "Under_5", replacement = "0_4") %>% 
      str_replace(pattern = "_years", replacement = "") %>% 
      str_replace(pattern = "years", replacement = "")
  }
  df <- df %>% 
    mutate(age_name = map_chr(age_name, age_modify))
}
pre_apc_dplyr::selected <- age_replace(df_pre_apc)
saveRDS(pre_apc_dplyr::selected,file = here("/data_processed/apc", "df_pre_apc_cvd_dplyr::selected.RDS"))
rm(list=ls())
Pop_data_dplyr::selected <-readRDS(file = here("/data_processed/", "Pop_data.RDS"))
df_deaths_apc <- readRDS(file = here("/data_processed/apc", "df_deaths_apc_cvd.RDS"))
age_replace <- function(df){
  age_modify <- function(val){
    val <- val %>% 
      str_replace(pattern = " to ", replacement = "_") %>% 
      str_replace(pattern = " |-", replacement = "_") %>% 
      str_replace(pattern = "<", replacement = "Under_") %>% 
      str_replace(pattern = "Under_5", replacement = "0_4") %>% 
      str_replace(pattern = "_years", replacement = "") %>% 
      str_replace(pattern = "years", replacement = "")
  }
  df <- df %>% 
    mutate(age_name = map_chr(age_name, age_modify))
}
deaths_apc_dplyr::selected <- age_replace(df_deaths_apc)
saveRDS(deaths_apc_dplyr::selected,file = here("/data_processed/apc", "df_deaths_apc_cvd_dplyr::selected.RDS"))

rm(list=ls())
Pop_data_dplyr::selected <-readRDS(file = here("/data_processed/", "Pop_data.RDS"))
df_DALYs_apc <- readRDS(file = here("/data_processed/apc/", "df_DALYs_apc_cvd.RDS"))
age_replace <- function(df){
  age_modify <- function(val){
    val <- val %>% 
      str_replace(pattern = " to ", replacement = "_") %>% 
      str_replace(pattern = " |-", replacement = "_") %>% 
      str_replace(pattern = "<", replacement = "Under_") %>% 
      str_replace(pattern = "Under_5", replacement = "0_4") %>% 
      str_replace(pattern = "_years", replacement = "") %>% 
      str_replace(pattern = "years", replacement = "")
  }
  
  df <- df %>% 
    mutate(age_name = map_chr(age_name, age_modify))
}
DALYs_apc_dplyr::selected <- age_replace(df_DALYs_apc)
saveRDS(DALYs_apc_dplyr::selected,file = here("data_processed/apc", "df_DALYs_apc_cvd_dplyr::selected.RDS"))

rm(list=ls())
Pop_data_dplyr::selected <-readRDS(file = here("/data_processed/", "Pop_data.RDS"))
df_pre_apc <- readRDS(file = here("/data_processed/apc", "df_pre_apc_cha.RDS"))
age_replace <- function(df){
  age_modify <- function(val){
    val <- val %>% 
      str_replace(pattern = " to ", replacement = "_") %>% 
      str_replace(pattern = " |-", replacement = "_") %>% 
      str_replace(pattern = "<", replacement = "Under_") %>% 
      str_replace(pattern = "Under_5", replacement = "0_4") %>% 
      str_replace(pattern = "_years", replacement = "") %>% 
      str_replace(pattern = "years", replacement = "")
  }
  
  df <- df %>% 
    mutate(age_name = map_chr(age_name, age_modify))
}
pre_apc_dplyr::selected <- age_replace(df_pre_apc)
saveRDS(pre_apc_dplyr::selected,file = here("/data_processed/apc", "df_pre_apc_cha_dplyr::selected.RDS"))

rm(list=ls())
Pop_data_dplyr::selected <-readRDS(file = here("/data_processed/", "Pop_data.RDS"))
df_deaths_apc <- readRDS(file = here("/data_processed/apc", "df_deaths_apc_cha.RDS"))
age_replace <- function(df){
  age_modify <- function(val){
    val <- val %>% 
      str_replace(pattern = " to ", replacement = "_") %>% 
      str_replace(pattern = " |-", replacement = "_") %>% 
      str_replace(pattern = "<", replacement = "Under_") %>% 
      str_replace(pattern = "Under_5", replacement = "0_4") %>% 
      str_replace(pattern = "_years", replacement = "") %>% 
      str_replace(pattern = "years", replacement = "")
  }
  
  df <- df %>% 
    mutate(age_name = map_chr(age_name, age_modify))
}
deaths_apc_dplyr::selected <- age_replace(df_deaths_apc)
saveRDS(deaths_apc_dplyr::selected,file = here("/data_processed/apc", "df_deaths_apc_cha_dplyr::selected.RDS"))

rm(list=ls())
Pop_data_dplyr::selected <-readRDS(file = here("/data_processed/", "Pop_data.RDS"))
df_DALYs_apc <- readRDS(file = here("/data_processed/apc/", "df_DALYs_apc_cha.RDS"))
age_replace <- function(df){
  age_modify <- function(val){
    val <- val %>% 
      str_replace(pattern = " to ", replacement = "_") %>% 
      str_replace(pattern = " |-", replacement = "_") %>% 
      str_replace(pattern = "<", replacement = "Under_") %>% 
      str_replace(pattern = "Under_5", replacement = "0_4") %>% 
      str_replace(pattern = "_years", replacement = "") %>% 
      str_replace(pattern = "years", replacement = "")
  }
  df <- df %>% 
    mutate(age_name = map_chr(age_name, age_modify))
}
DALYs_apc_dplyr::selected <- age_replace(df_DALYs_apc)
saveRDS(DALYs_apc_dplyr::selected,file = here("/data_processed/apc", "df_DALYs_apc_cha_dplyr::selected.RDS"))

rm(list=ls())
CVD_inci_data <- readRDS(file = here("/data_processed/apc", "df_inci_apc_cvd_dplyr::selected.RDS"))
Pop_data <- readRDS(file = here("/data_processed/", "Pop_data.RDS"))
year_groups <- c("1994", "1999", "2004", "2009", "2014", "2019")
period_match <- setNames(c("1992_1996", "1997_2001", "2002_2005", "2007_2011", "2012_2016", "2017_2021"),
                         year_groups)
CVD_inci_data$age_name <- gsub(" ", "", CVD_inci_data$age_name)
Pop_data$age_name <- gsub(" ", "", Pop_data$age_name)
CVD_data_processed <- CVD_inci_data %>%
  mutate(period0 = ((year-2)%/%5)*5+2) %>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("val", "year"), \(x) mean(x, na.rm = TRUE)), .groups = 'drop')%>%
  mutate(year = as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(death_count = val) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, death_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))
CVD_data_processed <- CVD_data_processed %>%
  dplyr::filter(year %in% year_groups)
Pop_data_processed <- Pop_data %>%
  filter(location_name%in%c("Global","High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"))%>%
  mutate(period0 = ((year-2)%/%5)*5+2)%>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("estimated_pop","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year= as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(pop_count = estimated_pop) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, pop_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))%>%
  dplyr::filter(year %in% year_groups)
APC_data <- CVD_data_processed %>%
  left_join(Pop_data_processed, by = c("year" = "year", "period" = "period", 
                                       "sex_name" = "sex_name", "age_name" = "age_name", 
                                       "location_id" = "location_id")) %>% 
  arrange(year) %>% 
  dplyr::select(-location_name.y) %>% 
  rename(location_name = location_name.x)
saveRDS(APC_data, file = here("/data_processed/apc", "APC-data_inci_cvd.RDS"))
age_order <- c("0_4", "5_9", "10_14", "15_19")
APC_data$age_name <- factor(APC_data$age_name, levels = age_order, ordered = TRUE)
APC_data <- APC_data[order(APC_data$age_name), ] 

APC_data_build <- function(APC_data){
  location_ID = unique(APC_data$location_id) 
  sex = unique(APC_data$sex_name)
  APC_data_list = list()
  period_adjust <- function(df){
    period_names <- unique(pull(df, var = period))
    df_list <- list()
    for (i in seq_along(period_names)){
      df_list[[i]] <- df %>% dplyr::filter(period == period_names[i]) 
    }
    return(df_list)
  }
  for(i in seq_along(location_ID)){
    name = paste0('APC', sep = '_', location_ID[i])
    tmp = list()
    for(j in seq_along(sex)){
      tmp_sub = list()
      name_sub = paste0(name, sep = '_', sex[j])
      tmp_sub[['table']] = APC_data %>% 
        dplyr::filter(location_id == location_ID[i] & sex_name == sex[j]) %>% 
        dplyr::select(period, age_name, death_count, pop_count)
      tmp_sub[['table']] <- bind_cols(period_adjust(tmp_sub[['table']])) %>% 
        as.data.frame() %>% 
        dplyr::select(death_count...3, pop_count...4,
               death_count...7, pop_count...8,
               death_count...11, pop_count...12,
               death_count...15, pop_count...16,
               death_count...19, pop_count...20,
               death_count...23, pop_count...24) %>% 
        as.matrix()
      tmp_sub[['title']] <- name_sub
      tmp_sub[['description']] <- name_sub
      tmp_sub[['startYear']] <- 1992
      tmp_sub[['startAge']] <- 0
      tmp_sub[['interval']] <- 5
      tmp_sub[['reference']] <- c(17.5, 1994.5, 1977)
      tmp[[name_sub]] <- tmp_sub
    }
    APC_data_list[[name]] <- tmp
  }
  return(APC_data_list)
}
APC_data_cleaned <- APC_data_build(APC_data)
saveRDS(APC_data_cleaned, file = here("/data_processed/apc", "APC-data-prep_inci_cvd.RDS"))

rm(list=ls())
CVD_pre_data <- readRDS(file = here("/data_processed/apc", "df_pre_apc_cvd_dplyr::selected.RDS"))
Pop_data <- readRDS(file = here("/data_processed/", "Pop_data.RDS"))
year_groups <- c("1994", "1999", "2004", "2009", "2014", "2019")
period_match <- setNames(c("1992_1996", "1997_2001", "2002_2005", "2007_2011", "2012_2016", "2017_2021"),
                         year_groups)
CVD_pre_data$age_name <- gsub(" ", "", CVD_pre_data$age_name)
Pop_data$age_name <- gsub(" ", "", Pop_data$age_name)
CVD_data_processed <- CVD_pre_data %>%
  mutate(period0 = ((year-2)%/%5)*5+2) %>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("val","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year = as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(death_count = val) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, death_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))
CVD_data_processed <- CVD_data_processed %>%
  dplyr::filter(year %in% year_groups)
Pop_data_processed <- Pop_data %>%
  filter(location_name%in%c("Global","High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"))%>%
  mutate(period0 = ((year-2)%/%5)*5+2)%>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("estimated_pop","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year= as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(pop_count = estimated_pop) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, pop_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))%>%
  dplyr::filter(year %in% year_groups)
APC_data <- CVD_data_processed %>%
  left_join(Pop_data_processed, by = c("year" = "year", "period" = "period", 
                                       "sex_name" = "sex_name", "age_name" = "age_name", 
                                       "location_id" = "location_id")) %>% 
  arrange(year) %>% 
  dplyr::select(-location_name.y) %>% 
  rename(location_name = location_name.x)
saveRDS(APC_data, file = here("/data_processed/apc", "APC-data_pre_cvd.RDS"))
age_order <- c("0_4", "5_9", "10_14", "15_19")
APC_data$age_name <- factor(APC_data$age_name, levels = age_order, ordered = TRUE)
APC_data <- APC_data[order(APC_data$age_name), ] 

APC_data_build <- function(APC_data){
  location_ID = unique(APC_data$location_id)  
  sex = unique(APC_data$sex_name)
  APC_data_list = list()
  period_adjust <- function(df){
    period_names <- unique(pull(df, var = period))
    df_list <- list()
    for (i in seq_along(period_names)){
      df_list[[i]] <- df %>% dplyr::filter(period == period_names[i]) 
    }
    return(df_list)
  }
  for(i in seq_along(location_ID)){
    name = paste0('APC', sep = '_', location_ID[i])
    tmp = list()
    for(j in seq_along(sex)){
      tmp_sub = list()
      name_sub = paste0(name, sep = '_', sex[j])
      tmp_sub[['table']] = APC_data %>% 
        dplyr::filter(location_id == location_ID[i] & sex_name == sex[j]) %>% 
        dplyr::select(period, age_name, death_count, pop_count)
      tmp_sub[['table']] <- bind_cols(period_adjust(tmp_sub[['table']])) %>% 
        as.data.frame() %>% 
        dplyr::select(death_count...3, pop_count...4,
               death_count...7, pop_count...8,
               death_count...11, pop_count...12,
               death_count...15, pop_count...16,
               death_count...19, pop_count...20,
               death_count...23, pop_count...24) %>% 
        as.matrix()
      tmp_sub[['title']] <- name_sub
      tmp_sub[['description']] <- name_sub
      tmp_sub[['startYear']] <- 1992
      tmp_sub[['startAge']] <- 0
      tmp_sub[['interval']] <- 5
      tmp_sub[['reference']] <- c(17.5, 1994.5, 1977)
      tmp[[name_sub]] <- tmp_sub
    }
    APC_data_list[[name]] <- tmp
  }
  return(APC_data_list)
}
APC_data_cleaned <- APC_data_build(APC_data)
saveRDS(APC_data_cleaned, file = here("/data_processed/apc", "APC-data-prep_pre_cvd.RDS"))

rm(list=ls())
CVD_deaths_data <- readRDS(file = here("/data_processed/apc", "df_deaths_apc_cvd_dplyr::selected.RDS"))
Pop_data <- readRDS(file = here("/data_processed/", "Pop_data.RDS"))
year_groups <- c("1994", "1999", "2004", "2009", "2014", "2019")
period_match <- setNames(c("1992_1996", "1997_2001", "2002_2005", "2007_2011", "2012_2016", "2017_2021"),
                         year_groups)
CVD_deaths_data$age_name <- gsub(" ", "", CVD_deaths_data$age_name)
Pop_data$age_name <- gsub(" ", "", Pop_data$age_name)
CVD_data_processed <- CVD_deaths_data %>%
  mutate(period0 = ((year-2)%/%5)*5+2) %>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("val","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year = as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(death_count = val) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, death_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))
CVD_data_processed <- CVD_data_processed %>%
  dplyr::filter(year %in% year_groups)
Pop_data_processed <- Pop_data %>%
  filter(location_name%in%c("Global","High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"))%>%
  mutate(period0 = ((year-2)%/%5)*5+2)%>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("estimated_pop","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year= as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(pop_count = estimated_pop) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, pop_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))%>%
  dplyr::filter(year %in% year_groups)
APC_data <- CVD_data_processed %>%
  left_join(Pop_data_processed, by = c("year" = "year", "period" = "period", 
                                       "sex_name" = "sex_name", "age_name" = "age_name", 
                                       "location_id" = "location_id")) %>% 
  arrange(year) %>% 
  dplyr::select(-location_name.y) %>% 
  rename(location_name = location_name.x)
saveRDS(APC_data, file = here("/data_processed/apc", "APC-data_deaths_cvd.RDS"))
age_order <- c("0_4", "5_9", "10_14", "15_19")
APC_data$age_name <- factor(APC_data$age_name, levels = age_order, ordered = TRUE)
APC_data <- APC_data[order(APC_data$age_name), ] 
APC_data_build <- function(APC_data){
  location_ID = unique(APC_data$location_id) 
  sex = unique(APC_data$sex_name)
  APC_data_list = list()
  period_adjust <- function(df){
    period_names <- unique(pull(df, var = period))
    df_list <- list()
    for (i in seq_along(period_names)){
      df_list[[i]] <- df %>% dplyr::filter(period == period_names[i]) 
    }
    return(df_list)
  }
  for(i in seq_along(location_ID)){
    name = paste0('APC', sep = '_', location_ID[i])
    tmp = list()
    for(j in seq_along(sex)){
      tmp_sub = list()
      name_sub = paste0(name, sep = '_', sex[j])
      tmp_sub[['table']] = APC_data %>% 
        dplyr::filter(location_id == location_ID[i] & sex_name == sex[j]) %>% 
        dplyr::select(period, age_name, death_count, pop_count)
      tmp_sub[['table']] <- bind_cols(period_adjust(tmp_sub[['table']])) %>% 
        as.data.frame() %>% 
        dplyr::select(death_count...3, pop_count...4,
               death_count...7, pop_count...8,
               death_count...11, pop_count...12,
               death_count...15, pop_count...16,
               death_count...19, pop_count...20,
               death_count...23, pop_count...24) %>% 
        as.matrix()
      tmp_sub[['title']] <- name_sub
      tmp_sub[['description']] <- name_sub
      tmp_sub[['startYear']] <- 1992
      tmp_sub[['startAge']] <- 0
      tmp_sub[['interval']] <- 5
      tmp_sub[['reference']] <- c(17.5, 1994.5, 1977) 
      tmp[[name_sub]] <- tmp_sub
    }
    APC_data_list[[name]] <- tmp
  }
  return(APC_data_list)
}
APC_data_cleaned <- APC_data_build(APC_data)
saveRDS(APC_data_cleaned, file = here("/data_processed/apc", "APC-data-prep_deaths_cvd.RDS"))

rm(list=ls())

CVD_DALYs_data <- readRDS(file = here("/data_processed/apc", "df_DALYs_apc_cvd_dplyr::selected.RDS"))
Pop_data <- readRDS(file = here("/data_processed/", "Pop_data.RDS"))
year_groups <- c("1994", "1999", "2004", "2009", "2014", "2019")
period_match <- setNames(c("1992_1996", "1997_2001", "2002_2005", "2007_2011", "2012_2016", "2017_2021"),
                         year_groups)
CVD_DALYs_data$age_name <- gsub(" ", "", CVD_DALYs_data$age_name)
Pop_data$age_name <- gsub(" ", "", Pop_data$age_name)
CVD_data_processed <- CVD_DALYs_data %>%
  mutate(period0 = ((year-2)%/%5)*5+2) %>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("val","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year = as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(death_count = val) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, death_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))
CVD_data_processed <- CVD_data_processed %>%
  dplyr::filter(year %in% year_groups)
Pop_data_processed <- Pop_data %>%
  filter(location_name%in%c("Global","High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"))%>%
  mutate(period0 = ((year-2)%/%5)*5+2)%>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("estimated_pop","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year= as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(pop_count = estimated_pop) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, pop_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))%>%
  dplyr::filter(year %in% year_groups)
APC_data <- CVD_data_processed %>%
  left_join(Pop_data_processed, by = c("year" = "year", "period" = "period", 
                                       "sex_name" = "sex_name", "age_name" = "age_name", 
                                       "location_id" = "location_id")) %>% 
  arrange(year) %>% 
  dplyr::select(-location_name.y) %>% 
  rename(location_name = location_name.x)
saveRDS(APC_data, file = here("/data_processed/apc", "APC-data_DALYs_cvd.RDS"))
age_order <- c("0_4", "5_9", "10_14", "15_19")
APC_data$age_name <- factor(APC_data$age_name, levels = age_order, ordered = TRUE)
APC_data <- APC_data[order(APC_data$age_name), ] 
APC_data_build <- function(APC_data){
  location_ID = unique(APC_data$location_id) 
  sex = unique(APC_data$sex_name)
  APC_data_list = list()
  period_adjust <- function(df){
    period_names <- unique(pull(df, var = period))
    df_list <- list()
    for (i in seq_along(period_names)){
      df_list[[i]] <- df %>% dplyr::filter(period == period_names[i]) 
    }
    return(df_list)
  }
  for(i in seq_along(location_ID)){
    name = paste0('APC', sep = '_', location_ID[i])
    tmp = list()
    for(j in seq_along(sex)){
      tmp_sub = list()
      name_sub = paste0(name, sep = '_', sex[j])
      tmp_sub[['table']] = APC_data %>% 
        dplyr::filter(location_id == location_ID[i] & sex_name == sex[j]) %>% 
        dplyr::select(period, age_name, death_count, pop_count)
      tmp_sub[['table']] <- bind_cols(period_adjust(tmp_sub[['table']])) %>% 
        as.data.frame() %>% 
        dplyr::select(death_count...3, pop_count...4,
               death_count...7, pop_count...8,
               death_count...11, pop_count...12,
               death_count...15, pop_count...16,
               death_count...19, pop_count...20,
               death_count...23, pop_count...24) %>% 
        as.matrix()
      tmp_sub[['title']] <- name_sub
      tmp_sub[['description']] <- name_sub
      tmp_sub[['startYear']] <- 1992
      tmp_sub[['startAge']] <- 0
      tmp_sub[['interval']] <- 5
      tmp_sub[['reference']] <- c(17.5, 1994.5, 1977) 
      tmp[[name_sub]] <- tmp_sub
    }
    APC_data_list[[name]] <- tmp
  }
  
  return(APC_data_list)
}
APC_data_cleaned <- APC_data_build(APC_data)
saveRDS(APC_data_cleaned, file = here("/data_processed/apc", "APC-data-prep_DALYs_cvd.RDS"))

rm(list=ls())
CVD_pre_data <- readRDS(file = here("/data_processed/apc", "df_pre_apc_cha_dplyr::selected.RDS"))
Pop_data <- readRDS(file = here("/data_processed/", "Pop_data.RDS"))
year_groups <- c("1994", "1999", "2004", "2009", "2014", "2019")
period_match <- setNames(c("1992_1996", "1997_2001", "2002_2005", "2007_2011", "2012_2016", "2017_2021"),
                         year_groups)
CVD_pre_data$age_name <- gsub(" ", "", CVD_pre_data$age_name)
Pop_data$age_name <- gsub(" ", "", Pop_data$age_name)
CVD_data_processed <- CVD_pre_data %>%
  mutate(period0 = ((year-2)%/%5)*5+2) %>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("val","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year = as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(death_count = val) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, death_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))
CVD_data_processed <- CVD_data_processed %>%
  dplyr::filter(year %in% year_groups)
Pop_data_processed <- Pop_data %>%
  filter(location_name%in%c("Global","High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"))%>%
  mutate(period0 = ((year-2)%/%5)*5+2)%>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("estimated_pop","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year= as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(pop_count = estimated_pop) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, pop_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))%>%
  dplyr::filter(year %in% year_groups)
APC_data <- CVD_data_processed %>%
  left_join(Pop_data_processed, by = c("year" = "year", "period" = "period", 
                                       "sex_name" = "sex_name", "age_name" = "age_name", 
                                       "location_id" = "location_id")) %>% 
  arrange(year) %>% 
  dplyr::select(-location_name.y) %>% 
  rename(location_name = location_name.x)
saveRDS(APC_data, file = here("/data_processed/apc", "APC-data_pre_cha.RDS"))
age_order <- c("0_4", "5_9", "10_14", "15_19")
APC_data$age_name <- factor(APC_data$age_name, levels = age_order, ordered = TRUE)
APC_data <- APC_data[order(APC_data$age_name), ] 
APC_data_build <- function(APC_data){
  location_ID = unique(APC_data$location_id) 
  sex = unique(APC_data$sex_name)
  APC_data_list = list()
  period_adjust <- function(df){
    period_names <- unique(pull(df, var = period))
    df_list <- list()
    for (i in seq_along(period_names)){
      df_list[[i]] <- df %>% dplyr::filter(period == period_names[i]) 
    }
    return(df_list)
  }
  for(i in seq_along(location_ID)){
    name = paste0('APC', sep = '_', location_ID[i])
    tmp = list()
    for(j in seq_along(sex)){
      tmp_sub = list()
      name_sub = paste0(name, sep = '_', sex[j])
      tmp_sub[['table']] = APC_data %>% 
        dplyr::filter(location_id == location_ID[i] & sex_name == sex[j]) %>% 
        dplyr::select(period, age_name, death_count, pop_count)
      tmp_sub[['table']] <- bind_cols(period_adjust(tmp_sub[['table']])) %>% 
        as.data.frame() %>% 
        dplyr::select(death_count...3, pop_count...4,
               death_count...7, pop_count...8,
               death_count...11, pop_count...12,
               death_count...15, pop_count...16,
               death_count...19, pop_count...20,
               death_count...23, pop_count...24) %>% 
        as.matrix()
      tmp_sub[['title']] <- name_sub
      tmp_sub[['description']] <- name_sub
      tmp_sub[['startYear']] <- 1992
      tmp_sub[['startAge']] <- 0
      tmp_sub[['interval']] <- 5
      tmp_sub[['reference']] <- c(17.5, 1994.5, 1977) 
      tmp[[name_sub]] <- tmp_sub
    }
    APC_data_list[[name]] <- tmp
  }
  
  return(APC_data_list)
}
APC_data_cleaned <- APC_data_build(APC_data)
saveRDS(APC_data_cleaned, file = here("/data_processed/apc", "APC-data-prep_pre_cha.RDS"))

rm(list=ls())
CVD_deaths_data <- readRDS(file = here("/data_processed/apc", "df_deaths_apc_cha_dplyr::selected.RDS"))
Pop_data <- readRDS(file = here("/data_processed/", "Pop_data.RDS"))
year_groups <- c("1994", "1999", "2004", "2009", "2014", "2019")
period_match <- setNames(c("1992_1996", "1997_2001", "2002_2005", "2007_2011", "2012_2016", "2017_2021"),
                         year_groups)
CVD_deaths_data$age_name <- gsub(" ", "", CVD_deaths_data$age_name)
Pop_data$age_name <- gsub(" ", "", Pop_data$age_name)
CVD_data_processed <- CVD_deaths_data %>%
  mutate(period0 = ((year-2)%/%5)*5+2) %>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("val","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year = as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(death_count = val) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, death_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))
CVD_data_processed <- CVD_data_processed %>%
  dplyr::filter(year %in% year_groups)
Pop_data_processed <- Pop_data %>%
  filter(location_name%in%c("Global","High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"))%>%
  mutate(period0 = ((year-2)%/%5)*5+2)%>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("estimated_pop","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year= as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(pop_count = estimated_pop) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, pop_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))%>%
  dplyr::filter(year %in% year_groups)
APC_data <- CVD_data_processed %>%
  left_join(Pop_data_processed, by = c("year" = "year", "period" = "period", 
                                       "sex_name" = "sex_name", "age_name" = "age_name", 
                                       "location_id" = "location_id")) %>% 
  arrange(year) %>% 
  dplyr::select(-location_name.y) %>% 
  rename(location_name = location_name.x)
saveRDS(APC_data, file = here("/data_processed/apc", "APC-data_deaths_cha.RDS"))
age_order <- c("0_4", "5_9", "10_14", "15_19")
APC_data$age_name <- factor(APC_data$age_name, levels = age_order, ordered = TRUE)
APC_data <- APC_data[order(APC_data$age_name), ] 
APC_data_build <- function(APC_data){
  location_ID = unique(APC_data$location_id) 
  sex = unique(APC_data$sex_name)
  APC_data_list = list()
  period_adjust <- function(df){
    period_names <- unique(pull(df, var = period))
    df_list <- list()
    for (i in seq_along(period_names)){
      df_list[[i]] <- df %>% dplyr::filter(period == period_names[i]) 
    }
    return(df_list)
  }
  for(i in seq_along(location_ID)){
    name = paste0('APC', sep = '_', location_ID[i])
    tmp = list()
    for(j in seq_along(sex)){
      tmp_sub = list()
      name_sub = paste0(name, sep = '_', sex[j])
      tmp_sub[['table']] = APC_data %>% 
        dplyr::filter(location_id == location_ID[i] & sex_name == sex[j]) %>% 
        dplyr::select(period, age_name, death_count, pop_count)
      tmp_sub[['table']] <- bind_cols(period_adjust(tmp_sub[['table']])) %>% 
        as.data.frame() %>% 
        dplyr::select(death_count...3, pop_count...4,
               death_count...7, pop_count...8,
               death_count...11, pop_count...12,
               death_count...15, pop_count...16,
               death_count...19, pop_count...20,
               death_count...23, pop_count...24) %>% 
        as.matrix()
      tmp_sub[['title']] <- name_sub
      tmp_sub[['description']] <- name_sub
      tmp_sub[['startYear']] <- 1992
      tmp_sub[['startAge']] <- 0
      tmp_sub[['interval']] <- 5
      tmp_sub[['reference']] <- c(17.5, 1994.5, 1977) 
      tmp[[name_sub]] <- tmp_sub
    }
    APC_data_list[[name]] <- tmp
  }
  return(APC_data_list)
}
APC_data_cleaned <- APC_data_build(APC_data)
saveRDS(APC_data_cleaned, file = here("/data_processed/apc", "APC-data-prep_deaths_cha.RDS"))
rm(list=ls())
CVD_DALYs_data <- readRDS(file = here("/data_processed/apc", "df_DALYs_apc_cha_dplyr::selected.RDS"))
Pop_data <- readRDS(file = here("/data_processed/", "Pop_data.RDS"))
year_groups <- c("1994", "1999", "2004", "2009", "2014", "2019")
period_match <- setNames(c("1992_1996", "1997_2001", "2002_2005", "2007_2011", "2012_2016", "2017_2021"),
                         year_groups)
CVD_DALYs_data$age_name <- gsub(" ", "", CVD_DALYs_data$age_name)
Pop_data$age_name <- gsub(" ", "", Pop_data$age_name)
CVD_data_processed <- CVD_DALYs_data %>%
  mutate(period0 = ((year-2)%/%5)*5+2) %>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("val","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year = as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(death_count = val) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, death_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))
CVD_data_processed <- CVD_data_processed %>%
  dplyr::filter(year %in% year_groups)
Pop_data_processed <- Pop_data %>%
  filter(location_name%in%c("Global","High-middle SDI","Low-middle SDI","High SDI","Low SDI","Middle SDI"))%>%
  mutate(period0 = ((year-2)%/%5)*5+2)%>%
  group_by(location_id,location_name,sex_name,age_name,period0)%>%
  summarise(across(c("estimated_pop","year"),mean,na.rm=TRUE),.groups = 'drop')%>%
  mutate(year= as.character(year)) %>%
  mutate(period = map_chr(year, function(x) period_match[x])) %>%
  rename(pop_count = estimated_pop) %>% 
  dplyr::select(location_id, location_name, year, period, sex_name, age_name, pop_count) %>% 
  dplyr::filter(age_name %in% c("0_4","5_9","10_14", "15_19"))%>%
  dplyr::filter(year %in% year_groups)
APC_data <- CVD_data_processed %>%
  left_join(Pop_data_processed, by = c("year" = "year", "period" = "period", 
                                       "sex_name" = "sex_name", "age_name" = "age_name", 
                                       "location_id" = "location_id")) %>% 
  arrange(year) %>% 
  dplyr::select(-location_name.y) %>% 
  rename(location_name = location_name.x)
saveRDS(APC_data, file = here("/data_processed/apc", "APC-data_DALYs_cha.RDS"))
age_order <- c("0_4", "5_9", "10_14", "15_19")
APC_data$age_name <- factor(APC_data$age_name, levels = age_order, ordered = TRUE)
APC_data <- APC_data[order(APC_data$age_name), ] 
APC_data_build <- function(APC_data){
  location_ID = unique(APC_data$location_id) 
  sex = unique(APC_data$sex_name)
  APC_data_list = list()
  period_adjust <- function(df){
    period_names <- unique(pull(df, var = period))
    df_list <- list()
    for (i in seq_along(period_names)){
      df_list[[i]] <- df %>% dplyr::filter(period == period_names[i]) 
    }
    return(df_list)
  }
  for(i in seq_along(location_ID)){
    name = paste0('APC', sep = '_', location_ID[i])
    tmp = list()
    for(j in seq_along(sex)){
      tmp_sub = list()
      name_sub = paste0(name, sep = '_', sex[j])
      tmp_sub[['table']] = APC_data %>% 
        dplyr::filter(location_id == location_ID[i] & sex_name == sex[j]) %>% 
        dplyr::select(period, age_name, death_count, pop_count)
      tmp_sub[['table']] <- bind_cols(period_adjust(tmp_sub[['table']])) %>% 
        as.data.frame() %>% 
        dplyr::select(death_count...3, pop_count...4,
               death_count...7, pop_count...8,
               death_count...11, pop_count...12,
               death_count...15, pop_count...16,
               death_count...19, pop_count...20,
               death_count...23, pop_count...24) %>% 
        as.matrix()
      tmp_sub[['title']] <- name_sub
      tmp_sub[['description']] <- name_sub
      tmp_sub[['startYear']] <- 1992
      tmp_sub[['startAge']] <- 0
      tmp_sub[['interval']] <- 5
      tmp_sub[['reference']] <- c(17.5, 1994.5, 1977) 
      tmp[[name_sub]] <- tmp_sub
    }
    APC_data_list[[name]] <- tmp
  }
  return(APC_data_list)
}
APC_data_cleaned <- APC_data_build(APC_data)
saveRDS(APC_data_cleaned, file = here("/data_processed/apc", "APC-data-prep_DALYs_cha.RDS"))

rm(list=ls())
source("/apcWrapper.R")
GBD_location <- read_csv(here("/", "location_id_2.csv")) 
APC_data <- readRDS(file = here("/data_processed/apc", "APC-data-prep_inci_cvd.RDS"))
APC_analysis <- function(data){
  result = list()
  for (i in seq_along(data)){
    result[[i]] = list()
    for(j in seq_along(data[[i]])){
      result[[i]][[j]] <- calculate(toJSON(data[[i]][[j]])) 
    }
  }
  return(result)
}
APC_results <- APC_analysis(APC_data)
names(APC_results) <- names(APC_data)
APC_dataset_generate <- function(APC_data, item){
  APC_data_extract <- function(data, item){
    df_new <- list()
    for (i in seq_along(data)){
      if(is.list(data[[i]][[item]])){
        s <- strsplit(data[[i]][["info"]],split = "_")
        df_new[[i]] <- data[[i]][[item]][["table"]] %>% 
          as.data.frame() %>% 
          mutate(sex = s[[1]][[3]])} 
      else df_new[[i]] <- data.frame(info = data[[i]][[item]])
    }
    df_new <- bind_rows(df_new)
  }
  export = list()
  for(i in seq_along(APC_data)){
    infos = strsplit(APC_results[[i]][[1]][["info"]], split = "_")
    export[[i]] = APC_data_extract(APC_data[[i]], item) %>% 
      mutate(region = infos[[1]][[2]])
  }
  export <- bind_rows(export) 
}
item_names <- names(APC_results[[1]][[1]])[1:14]
APC_results_processed <- list()
for(i in seq_along(item_names)){
  item = item_names[i]
  APC_results_processed[[item]] <- APC_dataset_generate(APC_results, item)
}
FittedRates <- data.frame(Age = NA,
                          Fitted.rate = NA,
                          CILo = NA,
                          CIHi = NA,
                          Period = NA,
                          Sex = NA,
                          region = NA)
FittedRates <- FittedRates[-1,]
periods <- unique(APC_results_processed$PeriodRR$Period)
ages <- unique(APC_results_processed$LocalDrifts$Age)
for (i in seq_along(APC_results)){
  for (j in seq_along(APC_results[[i]])){
    info = strsplit(APC_results[[i]][[j]][["info"]], split = "_") 
    for (k in seq(1:6)){
      df_new = data.frame(Age = ages,
                          Fitted.rate = APC_results[[i]][[j]][['FittedRates']][["rate"]][, k],
                          CILo = APC_results[[i]][[j]][['FittedRates']][["CILo"]][, k],
                          CIHi = APC_results[[i]][[j]][['FittedRates']][["CIHi"]][, k],
                          Period = periods[k],
                          Sex = info[[1]][3],
                          region = info[[1]][2])
      FittedRates = rbind(FittedRates, df_new) 
    }
  }
}
FittedRates$region <- as.numeric(as.character(FittedRates$region))
unique(FittedRates$region)
unique(APC_results_processed$info$region)
APC_results_processed[["FittedRates"]] <- FittedRates
GBD_location_hierachy <- read_csv(here("/", "location_id_2.csv")) 
GBD_location_iso3 <- read_csv(here("/", "iso3.csv")) 
GBD_location_hierachy <- GBD_location_hierachy%>%
  dplyr::select(location_id,location_name)
regions <- APC_results_processed$info %>%
  mutate(location_id = as.numeric(map_chr(info, function(x) str_remove_all(string = x, pattern = "APC_|_Female|_Male|_Both")))) %>%
  dplyr::select(region, location_id) %>% 
  distinct() %>% 
  left_join(GBD_location_hierachy, by = c("location_id"  = "location_id")) %>%
  left_join(GBD_location_iso3,by = c("location_name" = "Location.Name"))
GnSDI_id <- read_csv(here("/","sdi.csv"))
GnSDI_id <- GnSDI_id%>%
  dplyr::select(location_id,location_name)
regions <-left_join(regions, GnSDI_id, by = c("location_id" = "location_id")) %>%
  mutate(location_name = coalesce(location_name.x, location_name.y)) %>%
  dplyr::select(-location_name.x, -location_name.y)
regions$region <- as.character(regions$region)
APC_results_processed[[15]]$region <- as.character(APC_results_processed[[15]]$region)
for(i in seq_along(APC_results_processed)){
  APC_results_processed[[i]] <- APC_results_processed[[i]] %>% 
    left_join(regions, by = c("region" = "region"))
}
saveRDS(APC_results_processed, file = here("/data_processed/apc", "APC-results_inci_cvd.RDS"))
rm(list=ls())
source("/apcWrapper.R")
GBD_location <- read_csv(here("/", "location_id_2.csv")) 
APC_data <- readRDS(file = here("/data_processed/apc", "APC-data-prep_pre_cvd.RDS"))
APC_analysis <- function(data){
  result = list()
  for (i in seq_along(data)){
    result[[i]] = list()
    for(j in seq_along(data[[i]])){
      result[[i]][[j]] <- calculate(toJSON(data[[i]][[j]])) 
    }
  }
  return(result)
}
APC_results <- APC_analysis(APC_data)
names(APC_results) <- names(APC_data)
APC_dataset_generate <- function(APC_data, item){
  APC_data_extract <- function(data, item){
    df_new <- list()
    for (i in seq_along(data)){
      if(is.list(data[[i]][[item]])){
        s <- strsplit(data[[i]][["info"]],split = "_")
        df_new[[i]] <- data[[i]][[item]][["table"]] %>% 
          as.data.frame() %>% 
          mutate(sex = s[[1]][[3]])} 
      else df_new[[i]] <- data.frame(info = data[[i]][[item]])
    }
    df_new <- bind_rows(df_new)
  }
  export = list()
  for(i in seq_along(APC_data)){
    infos = strsplit(APC_results[[i]][[1]][["info"]], split = "_")
    export[[i]] = APC_data_extract(APC_data[[i]], item) %>% 
      mutate(region = infos[[1]][[2]])
  }
  export <- bind_rows(export) 
}



item_names <- names(APC_results[[1]][[1]])[1:14]
APC_results_processed <- list()
for(i in seq_along(item_names)){
  item = item_names[i]
  APC_results_processed[[item]] <- APC_dataset_generate(APC_results, item)
}


FittedRates <- data.frame(Age = NA,
                          Fitted.rate = NA,
                          CILo = NA,
                          CIHi = NA,
                          Period = NA,
                          Sex = NA,
                          region = NA)

FittedRates <- FittedRates[-1,]

periods <- unique(APC_results_processed$PeriodRR$Period)
ages <- unique(APC_results_processed$LocalDrifts$Age)

for (i in seq_along(APC_results)){
  for (j in seq_along(APC_results[[i]])){
    info = strsplit(APC_results[[i]][[j]][["info"]], split = "_") 
    for (k in seq(1:6)){
      df_new = data.frame(Age = ages,
                          Fitted.rate = APC_results[[i]][[j]][['FittedRates']][["rate"]][, k],
                          CILo = APC_results[[i]][[j]][['FittedRates']][["CILo"]][, k],
                          CIHi = APC_results[[i]][[j]][['FittedRates']][["CIHi"]][, k],
                          Period = periods[k],
                          Sex = info[[1]][3],
                          region = info[[1]][2])
      FittedRates = rbind(FittedRates, df_new) 
    }
  }
}

FittedRates$region <- as.numeric(as.character(FittedRates$region))
unique(FittedRates$region)
unique(APC_results_processed$info$region)

APC_results_processed[["FittedRates"]] <- FittedRates


GBD_location_hierachy <- read_csv(here("/", "location_id_2.csv")) 
GBD_location_iso3 <- read_csv(here("/", "iso3.csv")) 

GBD_location_hierachy <- GBD_location_hierachy%>%
  dplyr::select(location_id,location_name)


regions <- APC_results_processed$info %>%
  mutate(location_id = as.numeric(map_chr(info, function(x) str_remove_all(string = x, pattern = "APC_|_Female|_Male|_Both")))) %>%
  dplyr::select(region, location_id) %>% 
  distinct() %>% 
  left_join(GBD_location_hierachy, by = c("location_id"  = "location_id")) %>%
  left_join(GBD_location_iso3,by = c("location_name" = "Location.Name"))

GnSDI_id <- read_csv(here("/","sdi.csv"))
GnSDI_id <- GnSDI_id%>%
  dplyr::select(location_id,location_name)
regions <-left_join(regions, GnSDI_id, by = c("location_id" = "location_id")) %>%
  mutate(location_name = coalesce(location_name.x, location_name.y)) %>%
  dplyr::select(-location_name.x, -location_name.y)
regions$region <- as.character(regions$region)
APC_results_processed[[15]]$region <- as.character(APC_results_processed[[15]]$region)

for(i in seq_along(APC_results_processed)){
  APC_results_processed[[i]] <- APC_results_processed[[i]] %>% 
    left_join(regions, by = c("region" = "region"))
}
saveRDS(APC_results_processed, file = here("/data_processed/apc", "APC-results_pre_cvd.RDS"))

rm(list=ls())
source("/apcWrapper.R")
GBD_location <- read_csv(here("/", "location_id_2.csv")) 
APC_data <- readRDS(file = here("/data_processed/apc", "APC-data-prep_deaths_cvd.RDS"))

APC_analysis <- function(data){
  result = list()
  for (i in seq_along(data)){
    result[[i]] = list()
    for(j in seq_along(data[[i]])){
      result[[i]][[j]] <- calculate(toJSON(data[[i]][[j]])) 
    }
  }
  return(result)
}

APC_results <- APC_analysis(APC_data)

names(APC_results) <- names(APC_data)

APC_dataset_generate <- function(APC_data, item){
  
  
  APC_data_extract <- function(data, item){
    df_new <- list()
    for (i in seq_along(data)){
      if(is.list(data[[i]][[item]])){
        s <- strsplit(data[[i]][["info"]],split = "_")
        df_new[[i]] <- data[[i]][[item]][["table"]] %>% 
          as.data.frame() %>% 
          mutate(sex = s[[1]][[3]])} 
      else df_new[[i]] <- data.frame(info = data[[i]][[item]])
    }
    df_new <- bind_rows(df_new)
  }
  
 
  export = list()
  for(i in seq_along(APC_data)){
    infos = strsplit(APC_results[[i]][[1]][["info"]], split = "_")
    export[[i]] = APC_data_extract(APC_data[[i]], item) %>% 
      mutate(region = infos[[1]][[2]])
  }
  
  
  export <- bind_rows(export) 
}




item_names <- names(APC_results[[1]][[1]])[1:14]
APC_results_processed <- list()
for(i in seq_along(item_names)){
  item = item_names[i]
  APC_results_processed[[item]] <- APC_dataset_generate(APC_results, item)
}


FittedRates <- data.frame(Age = NA,
                          Fitted.rate = NA,
                          CILo = NA,
                          CIHi = NA,
                          Period = NA,
                          Sex = NA,
                          region = NA)

FittedRates <- FittedRates[-1,]

periods <- unique(APC_results_processed$PeriodRR$Period)
ages <- unique(APC_results_processed$LocalDrifts$Age)

for (i in seq_along(APC_results)){
  for (j in seq_along(APC_results[[i]])){
    info = strsplit(APC_results[[i]][[j]][["info"]], split = "_") 
    for (k in seq(1:6)){
      df_new = data.frame(Age = ages,
                          Fitted.rate = APC_results[[i]][[j]][['FittedRates']][["rate"]][, k],
                          CILo = APC_results[[i]][[j]][['FittedRates']][["CILo"]][, k],
                          CIHi = APC_results[[i]][[j]][['FittedRates']][["CIHi"]][, k],
                          Period = periods[k],
                          Sex = info[[1]][3],
                          region = info[[1]][2])
      FittedRates = rbind(FittedRates, df_new) 
    }
  }
}

FittedRates$region <- as.numeric(as.character(FittedRates$region))
unique(FittedRates$region)
unique(APC_results_processed$info$region)

APC_results_processed[["FittedRates"]] <- FittedRates


GBD_location_hierachy <- read_csv(here("/", "location_id_2.csv")) 
GBD_location_iso3 <- read_csv(here("/", "iso3.csv")) 

GBD_location_hierachy <- GBD_location_hierachy%>%
  dplyr::select(location_id,location_name)


regions <- APC_results_processed$info %>%
  mutate(location_id = as.numeric(map_chr(info, function(x) str_remove_all(string = x, pattern = "APC_|_Female|_Male|_Both")))) %>%
  dplyr::select(region, location_id) %>% 
  distinct() %>% 
  left_join(GBD_location_hierachy, by = c("location_id"  = "location_id")) %>%
  left_join(GBD_location_iso3,by = c("location_name" = "Location.Name"))

GnSDI_id <- read_csv(here("/","sdi.csv"))
GnSDI_id <- GnSDI_id%>%
  dplyr::select(location_id,location_name)
regions <-left_join(regions, GnSDI_id, by = c("location_id" = "location_id")) %>%
  mutate(location_name = coalesce(location_name.x, location_name.y)) %>%
  dplyr::select(-location_name.x, -location_name.y)
regions$region <- as.character(regions$region)
APC_results_processed[[15]]$region <- as.character(APC_results_processed[[15]]$region)

for(i in seq_along(APC_results_processed)){
  APC_results_processed[[i]] <- APC_results_processed[[i]] %>% 
    left_join(regions, by = c("region" = "region"))
}
saveRDS(APC_results_processed, file = here("/data_processed/apc", "APC-results_deaths_cvd.RDS"))

rm(list=ls())
source("/apcWrapper.R")
GBD_location <- read_csv(here("/", "location_id_2.csv")) 
APC_data <- readRDS(file = here("/data_processed/apc", "APC-data-prep_DALYs_cvd.RDS"))

APC_analysis <- function(data){
  result = list()
  for (i in seq_along(data)){
    result[[i]] = list()
    for(j in seq_along(data[[i]])){
      result[[i]][[j]] <- calculate(toJSON(data[[i]][[j]])) 
    }
  }
  return(result)
}
APC_results <- APC_analysis(APC_data)
names(APC_results) <- names(APC_data)
APC_dataset_generate <- function(APC_data, item){
  
  
  APC_data_extract <- function(data, item){
    df_new <- list()
    for (i in seq_along(data)){
      if(is.list(data[[i]][[item]])){
        s <- strsplit(data[[i]][["info"]],split = "_")
        df_new[[i]] <- data[[i]][[item]][["table"]] %>% 
          as.data.frame() %>% 
          mutate(sex = s[[1]][[3]])} 
      else df_new[[i]] <- data.frame(info = data[[i]][[item]])
    }
    df_new <- bind_rows(df_new)
  }
  
 
  export = list()
  for(i in seq_along(APC_data)){
    infos = strsplit(APC_results[[i]][[1]][["info"]], split = "_")
    export[[i]] = APC_data_extract(APC_data[[i]], item) %>% 
      mutate(region = infos[[1]][[2]])
  }
  
  
  export <- bind_rows(export) 
}




item_names <- names(APC_results[[1]][[1]])[1:14]
APC_results_processed <- list()
for(i in seq_along(item_names)){
  item = item_names[i]
  APC_results_processed[[item]] <- APC_dataset_generate(APC_results, item)
}


FittedRates <- data.frame(Age = NA,
                          Fitted.rate = NA,
                          CILo = NA,
                          CIHi = NA,
                          Period = NA,
                          Sex = NA,
                          region = NA)

FittedRates <- FittedRates[-1,]

periods <- unique(APC_results_processed$PeriodRR$Period)
ages <- unique(APC_results_processed$LocalDrifts$Age)

for (i in seq_along(APC_results)){
  for (j in seq_along(APC_results[[i]])){
    info = strsplit(APC_results[[i]][[j]][["info"]], split = "_") 
    for (k in seq(1:6)){
      df_new = data.frame(Age = ages,
                          Fitted.rate = APC_results[[i]][[j]][['FittedRates']][["rate"]][, k],
                          CILo = APC_results[[i]][[j]][['FittedRates']][["CILo"]][, k],
                          CIHi = APC_results[[i]][[j]][['FittedRates']][["CIHi"]][, k],
                          Period = periods[k],
                          Sex = info[[1]][3],
                          region = info[[1]][2])
      FittedRates = rbind(FittedRates, df_new) 
    }
  }
}

FittedRates$region <- as.numeric(as.character(FittedRates$region))
unique(FittedRates$region)
unique(APC_results_processed$info$region)

APC_results_processed[["FittedRates"]] <- FittedRates


GBD_location_hierachy <- read_csv(here("/", "location_id_2.csv")) 
GBD_location_iso3 <- read_csv(here("/", "iso3.csv")) 

GBD_location_hierachy <- GBD_location_hierachy%>%
  dplyr::select(location_id,location_name)


regions <- APC_results_processed$info %>%
  mutate(location_id = as.numeric(map_chr(info, function(x) str_remove_all(string = x, pattern = "APC_|_Female|_Male|_Both")))) %>%
  dplyr::select(region, location_id) %>% 
  distinct() %>% 
  left_join(GBD_location_hierachy, by = c("location_id"  = "location_id")) %>%
  left_join(GBD_location_iso3,by = c("location_name" = "Location.Name"))

GnSDI_id <- read_csv(here("/","sdi.csv"))
GnSDI_id <- GnSDI_id%>%
  dplyr::select(location_id,location_name)
regions <-left_join(regions, GnSDI_id, by = c("location_id" = "location_id")) %>%
  mutate(location_name = coalesce(location_name.x, location_name.y)) %>%
  dplyr::select(-location_name.x, -location_name.y)
regions$region <- as.character(regions$region)
APC_results_processed[[15]]$region <- as.character(APC_results_processed[[15]]$region)

for(i in seq_along(APC_results_processed)){
  APC_results_processed[[i]] <- APC_results_processed[[i]] %>% 
    left_join(regions, by = c("region" = "region"))
}
saveRDS(APC_results_processed, file = here("/data_processed/apc", "APC-results_DALYs_cvd.RDS"))

rm(list=ls())
source("/apcWrapper.R")
GBD_location <- read_csv(here("/", "location_id_2.csv")) 
APC_data <- readRDS(file = here("/data_processed/apc", "APC-data-prep_pre_cha.RDS"))

APC_analysis <- function(data){
  result = list()
  for (i in seq_along(data)){
    result[[i]] = list()
    for(j in seq_along(data[[i]])){
      result[[i]][[j]] <- calculate(toJSON(data[[i]][[j]])) 
    }
  }
  return(result)
}

APC_results <- APC_analysis(APC_data)

names(APC_results) <- names(APC_data)

APC_dataset_generate <- function(APC_data, item){
  
  
  APC_data_extract <- function(data, item){
    df_new <- list()
    for (i in seq_along(data)){
      if(is.list(data[[i]][[item]])){
        s <- strsplit(data[[i]][["info"]],split = "_")
        df_new[[i]] <- data[[i]][[item]][["table"]] %>% 
          as.data.frame() %>% 
          mutate(sex = s[[1]][[3]])} 
      else df_new[[i]] <- data.frame(info = data[[i]][[item]])
    }
    df_new <- bind_rows(df_new)
  }
  
  export = list()
  for(i in seq_along(APC_data)){
    infos = strsplit(APC_results[[i]][[1]][["info"]], split = "_")
    export[[i]] = APC_data_extract(APC_data[[i]], item) %>% 
      mutate(region = infos[[1]][[2]])
  }
  
  
  export <- bind_rows(export) 
}

item_names <- names(APC_results[[1]][[1]])[1:14]
APC_results_processed <- list()
for(i in seq_along(item_names)){
  item = item_names[i]
  APC_results_processed[[item]] <- APC_dataset_generate(APC_results, item)
}


FittedRates <- data.frame(Age = NA,
                          Fitted.rate = NA,
                          CILo = NA,
                          CIHi = NA,
                          Period = NA,
                          Sex = NA,
                          region = NA)

FittedRates <- FittedRates[-1,]

periods <- unique(APC_results_processed$PeriodRR$Period)
ages <- unique(APC_results_processed$LocalDrifts$Age)

for (i in seq_along(APC_results)){
  for (j in seq_along(APC_results[[i]])){
    info = strsplit(APC_results[[i]][[j]][["info"]], split = "_") 
    for (k in seq(1:6)){
      df_new = data.frame(Age = ages,
                          Fitted.rate = APC_results[[i]][[j]][['FittedRates']][["rate"]][, k],
                          CILo = APC_results[[i]][[j]][['FittedRates']][["CILo"]][, k],
                          CIHi = APC_results[[i]][[j]][['FittedRates']][["CIHi"]][, k],
                          Period = periods[k],
                          Sex = info[[1]][3],
                          region = info[[1]][2])
      FittedRates = rbind(FittedRates, df_new) 
    }
  }
}

FittedRates$region <- as.numeric(as.character(FittedRates$region))
unique(FittedRates$region)
unique(APC_results_processed$info$region)

APC_results_processed[["FittedRates"]] <- FittedRates

GBD_location_hierachy <- read_csv(here("/", "location_id_2.csv")) 
GBD_location_iso3 <- read_csv(here("/", "iso3.csv")) 

GBD_location_hierachy <- GBD_location_hierachy%>%
  dplyr::select(location_id,location_name)

regions <- APC_results_processed$info %>%
  mutate(location_id = as.numeric(map_chr(info, function(x) str_remove_all(string = x, pattern = "APC_|_Female|_Male|_Both")))) %>%
  dplyr::select(region, location_id) %>% 
  distinct() %>% 
  left_join(GBD_location_hierachy, by = c("location_id"  = "location_id")) %>%
  left_join(GBD_location_iso3,by = c("location_name" = "Location.Name"))

GnSDI_id <- read_csv(here("/","sdi.csv"))
GnSDI_id <- GnSDI_id%>%
  dplyr::select(location_id,location_name)
regions <-left_join(regions, GnSDI_id, by = c("location_id" = "location_id")) %>%
  mutate(location_name = coalesce(location_name.x, location_name.y)) %>%
  dplyr::select(-location_name.x, -location_name.y)
regions$region <- as.character(regions$region)
APC_results_processed[[15]]$region <- as.character(APC_results_processed[[15]]$region)

for(i in seq_along(APC_results_processed)){
  APC_results_processed[[i]] <- APC_results_processed[[i]] %>% 
    left_join(regions, by = c("region" = "region"))
}
saveRDS(APC_results_processed, file = here("/data_processed/apc", "APC-results_pre_cha.RDS"))

rm(list=ls())
source("/apcWrapper.R")
GBD_location <- read_csv(here("/", "location_id_2.csv")) 
APC_data <- readRDS(file = here("/data_processed/apc", "APC-data-prep_deaths_cha.RDS"))
APC_analysis <- function(data){
  result = list()
  for (i in seq_along(data)){
    result[[i]] = list()
    for(j in seq_along(data[[i]])){
      result[[i]][[j]] <- calculate(toJSON(data[[i]][[j]])) 
    }
  }
  return(result)
}

APC_results <- APC_analysis(APC_data)

names(APC_results) <- names(APC_data)

APC_dataset_generate <- function(APC_data, item){
  
  
  APC_data_extract <- function(data, item){
    df_new <- list()
    for (i in seq_along(data)){
      if(is.list(data[[i]][[item]])){
        s <- strsplit(data[[i]][["info"]],split = "_")
        df_new[[i]] <- data[[i]][[item]][["table"]] %>% 
          as.data.frame() %>% 
          mutate(sex = s[[1]][[3]])} 
      else df_new[[i]] <- data.frame(info = data[[i]][[item]])
    }
    df_new <- bind_rows(df_new)
  }
  export = list()
  for(i in seq_along(APC_data)){
    infos = strsplit(APC_results[[i]][[1]][["info"]], split = "_")
    export[[i]] = APC_data_extract(APC_data[[i]], item) %>% 
      mutate(region = infos[[1]][[2]])
  }
  export <- bind_rows(export) 
}

item_names <- names(APC_results[[1]][[1]])[1:14]
APC_results_processed <- list()
for(i in seq_along(item_names)){
  item = item_names[i]
  APC_results_processed[[item]] <- APC_dataset_generate(APC_results, item)
}

FittedRates <- data.frame(Age = NA,
                          Fitted.rate = NA,
                          CILo = NA,
                          CIHi = NA,
                          Period = NA,
                          Sex = NA,
                          region = NA)

FittedRates <- FittedRates[-1,]
periods <- unique(APC_results_processed$PeriodRR$Period)
ages <- unique(APC_results_processed$LocalDrifts$Age)

for (i in seq_along(APC_results)){
  for (j in seq_along(APC_results[[i]])){
    info = strsplit(APC_results[[i]][[j]][["info"]], split = "_") 
    for (k in seq(1:6)){
      df_new = data.frame(Age = ages,
                          Fitted.rate = APC_results[[i]][[j]][['FittedRates']][["rate"]][, k],
                          CILo = APC_results[[i]][[j]][['FittedRates']][["CILo"]][, k],
                          CIHi = APC_results[[i]][[j]][['FittedRates']][["CIHi"]][, k],
                          Period = periods[k],
                          Sex = info[[1]][3],
                          region = info[[1]][2])
      FittedRates = rbind(FittedRates, df_new) 
    }
  }
}

FittedRates$region <- as.numeric(as.character(FittedRates$region))
unique(FittedRates$region)
unique(APC_results_processed$info$region)

APC_results_processed[["FittedRates"]] <- FittedRates


GBD_location_hierachy <- read_csv(here("/", "location_id_2.csv")) 
GBD_location_iso3 <- read_csv(here("/", "iso3.csv")) 

GBD_location_hierachy <- GBD_location_hierachy%>%
  dplyr::select(location_id,location_name)


regions <- APC_results_processed$info %>%
  mutate(location_id = as.numeric(map_chr(info, function(x) str_remove_all(string = x, pattern = "APC_|_Female|_Male|_Both")))) %>%
  dplyr::select(region, location_id) %>% 
  distinct() %>% 
  left_join(GBD_location_hierachy, by = c("location_id"  = "location_id")) %>%
  left_join(GBD_location_iso3,by = c("location_name" = "Location.Name"))

GnSDI_id <- read_csv(here("/","sdi.csv"))
GnSDI_id <- GnSDI_id%>%
  dplyr::select(location_id,location_name)
regions <-left_join(regions, GnSDI_id, by = c("location_id" = "location_id")) %>%
  mutate(location_name = coalesce(location_name.x, location_name.y)) %>%
  dplyr::select(-location_name.x, -location_name.y)
regions$region <- as.character(regions$region)
APC_results_processed[[15]]$region <- as.character(APC_results_processed[[15]]$region)

for(i in seq_along(APC_results_processed)){
  APC_results_processed[[i]] <- APC_results_processed[[i]] %>% 
    left_join(regions, by = c("region" = "region"))
}
saveRDS(APC_results_processed, file = here("/data_processed/apc", "APC-results_deaths_cha.RDS"))

rm(list=ls())
source("/apcWrapper.R")
GBD_location <- read_csv(here("/", "location_id_2.csv")) 
APC_data <- readRDS(file = here("/data_processed/apc", "APC-data-prep_DALYs_cha.RDS"))

APC_analysis <- function(data){
  result = list()
  for (i in seq_along(data)){
    result[[i]] = list()
    for(j in seq_along(data[[i]])){
      result[[i]][[j]] <- calculate(toJSON(data[[i]][[j]])) 
    }
  }
  return(result)
}

APC_results <- APC_analysis(APC_data)

names(APC_results) <- names(APC_data)

APC_dataset_generate <- function(APC_data, item){
  
  
  APC_data_extract <- function(data, item){
    df_new <- list()
    for (i in seq_along(data)){
      if(is.list(data[[i]][[item]])){
        s <- strsplit(data[[i]][["info"]],split = "_")
        df_new[[i]] <- data[[i]][[item]][["table"]] %>% 
          as.data.frame() %>% 
          mutate(sex = s[[1]][[3]])} 
      else df_new[[i]] <- data.frame(info = data[[i]][[item]])
    }
    df_new <- bind_rows(df_new)
  }
  
 
  export = list()
  for(i in seq_along(APC_data)){
    infos = strsplit(APC_results[[i]][[1]][["info"]], split = "_")
    export[[i]] = APC_data_extract(APC_data[[i]], item) %>% 
      mutate(region = infos[[1]][[2]])
  }
  
  
  export <- bind_rows(export) 
}

item_names <- names(APC_results[[1]][[1]])[1:14]
APC_results_processed <- list()
for(i in seq_along(item_names)){
  item = item_names[i]
  APC_results_processed[[item]] <- APC_dataset_generate(APC_results, item)
}

FittedRates <- data.frame(Age = NA,
                          Fitted.rate = NA,
                          CILo = NA,
                          CIHi = NA,
                          Period = NA,
                          Sex = NA,
                          region = NA)

FittedRates <- FittedRates[-1,]

periods <- unique(APC_results_processed$PeriodRR$Period)
ages <- unique(APC_results_processed$LocalDrifts$Age)

for (i in seq_along(APC_results)){
  for (j in seq_along(APC_results[[i]])){
    info = strsplit(APC_results[[i]][[j]][["info"]], split = "_") 
    for (k in seq(1:6)){
      df_new = data.frame(Age = ages,
                          Fitted.rate = APC_results[[i]][[j]][['FittedRates']][["rate"]][, k],
                          CILo = APC_results[[i]][[j]][['FittedRates']][["CILo"]][, k],
                          CIHi = APC_results[[i]][[j]][['FittedRates']][["CIHi"]][, k],
                          Period = periods[k],
                          Sex = info[[1]][3],
                          region = info[[1]][2])
      FittedRates = rbind(FittedRates, df_new) 
    }
  }
}

FittedRates$region <- as.numeric(as.character(FittedRates$region))
unique(FittedRates$region)
unique(APC_results_processed$info$region)

APC_results_processed[["FittedRates"]] <- FittedRates


GBD_location_hierachy <- read_csv(here("/", "location_id_2.csv")) 
GBD_location_iso3 <- read_csv(here("/", "iso3.csv")) 

GBD_location_hierachy <- GBD_location_hierachy%>%
  dplyr::select(location_id,location_name)


regions <- APC_results_processed$info %>%
  mutate(location_id = as.numeric(map_chr(info, function(x) str_remove_all(string = x, pattern = "APC_|_Female|_Male|_Both")))) %>%
  dplyr::select(region, location_id) %>% 
  distinct() %>% 
  left_join(GBD_location_hierachy, by = c("location_id"  = "location_id")) %>%
  left_join(GBD_location_iso3,by = c("location_name" = "Location.Name"))

GnSDI_id <- read_csv(here("/","sdi.csv"))
GnSDI_id <- GnSDI_id%>%
  dplyr::select(location_id,location_name)
regions <-left_join(regions, GnSDI_id, by = c("location_id" = "location_id")) %>%
  mutate(location_name = coalesce(location_name.x, location_name.y)) %>%
  dplyr::select(-location_name.x, -location_name.y)
regions$region <- as.character(regions$region)
APC_results_processed[[15]]$region <- as.character(APC_results_processed[[15]]$region)

for(i in seq_along(APC_results_processed)){
  APC_results_processed[[i]] <- APC_results_processed[[i]] %>% 
    left_join(regions, by = c("region" = "region"))
}
saveRDS(APC_results_processed, file = here("/data_processed/apc", "APC-results_DALYs_cha.RDS"))

rm(list=ls())

APC_cvd_inci <-readRDS(file = here("/data_processed/apc", "APC-results_inci_cvd.RDS"))
APC_cvd_pre <-readRDS(file = here("/data_processed/apc", "APC-results_pre_cvd.RDS"))
APC_cvd_deaths <-readRDS(file = here("/data_processed/apc", "APC-results_deaths_cvd.RDS"))
APC_cvd_DALYs <-readRDS(file = here("/data_processed/apc", "APC-results_DALYs_cvd.RDS"))

APC_cha_pre <-readRDS(file = here("/data_processed/apc", "APC-results_pre_cha.RDS"))
APC_cha_deaths <-readRDS(file = here("/data_processed/apc", "APC-results_deaths_cha.RDS"))
APC_cha_DALYs <-readRDS(file = here("/data_processed/apc", "APC-results_DALYs_cha.RDS"))


measure_fit<-function(df,measure,cause){
  new_df<-list()
  new_df$LocalDrifts<-df$LocalDrift%>%
    mutate(measure_name = measure,cause_name = cause)
  new_df$LongAge<-df$LongAge%>%
    mutate(measure_name =measure,cause_name = cause)
  new_df$PeriodRR<-df$PeriodRR%>%
    mutate(measure_name =measure,cause_name = cause)
  new_df$CohortRR<-df$CohortRR%>%
    mutate(measure_name =measure,cause_name = cause)
  return(new_df)
}
APC_cvd<-list()
APC_cvd$inci<-measure_fit(APC_cvd_inci,"Incidence","NC-CVD")
APC_cvd$pre<-measure_fit(APC_cvd_pre,"Prevalence","NC-CVD")
APC_cvd$deaths<-measure_fit(APC_cvd_deaths,"Deaths","NC-CVD")
APC_cvd$DALYs<-measure_fit(APC_cvd_DALYs,"DALYs","NC-CVD")
APC_cvd$age<-rbind(APC_cvd$inci$LongAge,APC_cvd$pre$LongAge,APC_cvd$deaths$LongAge,APC_cvd$DALYs$LongAge)
APC_cvd$per<-rbind(APC_cvd$inci$PeriodRR,APC_cvd$pre$PeriodRR,APC_cvd$deaths$PeriodRR,APC_cvd$DALYs$PeriodRR)
APC_cvd$coh<-rbind(APC_cvd$inci$CohortRR,APC_cvd$pre$CohortRR,APC_cvd$deaths$CohortRR,APC_cvd$DALYs$CohortRR)
APC_cvd$loc<-rbind(APC_cvd$inci$LocalDrift,APC_cvd$pre$LocalDrift,APC_cvd$deaths$LocalDrift,APC_cvd$DALYs$LocalDrift)

APC_cha<-list()
APC_cha$pre<-measure_fit(APC_cha_pre,"Prevalence","CHD")
APC_cha$deaths<-measure_fit(APC_cha_deaths,"Deaths","CHD")
APC_cha$DALYs<-measure_fit(APC_cha_DALYs,"DALYs","CHD")
APC_cha$age<-rbind(APC_cha$pre$LongAge,APC_cha$deaths$LongAge,APC_cha$DALYs$LongAge)
APC_cha$per<-rbind(APC_cha$pre$PeriodRR,APC_cha$deaths$PeriodRR,APC_cha$DALYs$PeriodRR)
APC_cha$coh<-rbind(APC_cha$pre$CohortRR,APC_cha$deaths$CohortRR,APC_cha$DALYs$CohortRR)
APC_cha$loc<-rbind(APC_cha$pre$LocalDrift,APC_cha$deaths$LocalDrift,APC_cha$DALYs$LocalDrift)

APC<-list()
APC$age<-rbind(APC_cha$age,APC_cvd$age)
APC$per<-rbind(APC_cha$per,APC_cvd$per)
APC$coh<-rbind(APC_cha$coh,APC_cvd$coh)
APC$loc<-rbind(APC_cha$loc,APC_cvd$loc)

cb_st<-list()
cb_st$measure_name<-c("Incidence","Prevalence","Deaths","DALYs","Prevalence","Deaths","DALYs")
cb_st$cause_name<-c("NC-CVD","NC-CVD","NC-CVD","NC-CVD","CHD","CHD","CHD")


all_plot<-function(combinedstatue){
  plot_list<-list()
  for (i in seq_along(combinedstatue$measure_name)) {

    measure = combinedstatue$measure_name[[i]]
    cause = combinedstatue$cause_name[[i]]
    longage_data <- APC$age %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    PeriodRR_data <- APC$per %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    CohortRR_data <- APC$coh %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    LocalDrifts_data <- APC$loc %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    color_cause<-ifelse(cause=="CHD","#ef2a21","#01539d")
    
    LocalDrifts_plot <- LocalDrifts_data %>%
      ggplot(aes(x = Age, y = `Percent per Year`)) +
      geom_line(color = color_cause, size = 1) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
      labs(title = "Local Drifts", x = "Age", y = "Percent per Year") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
   
    longage_plot <- longage_data %>%
      ggplot(aes(x = Age, y = Rate)) +
      geom_line(color = color_cause, size = 1) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      labs(title = "Longitudinal Age Effect", x = "Age", y = "Rate") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
    
    PeriodRR_plot <- PeriodRR_data %>%
      ggplot(aes(x = Period, y = `Rate Ratio`)) +
      geom_line(color = color_cause, size = 1) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
      labs(title = "Period Rate Ratio", x = "Period", y = "Rate Ratio") +
      
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
    
    CohortRR_plot <- CohortRR_data %>%
      ggplot(aes(x = Cohort, y = `Rate Ratio`)) +
      geom_line(color = color_cause, size = 1) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
      labs(title = "Cohort Rate Ratio", x = "Cohort", y = "Rate Ratio") +
     
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
    
    title_plot <- ggplot() + 
      theme_void() +
      annotate("text", x = 0, y = 0, label = paste0(cause," ",measure), size = 8, fontface = "bold", angle = 90) +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    
    plot_row <- grid.arrange(title_plot, LocalDrifts_plot, longage_plot, PeriodRR_plot, CohortRR_plot, ncol = 5, widths = c(0.3, 1, 1, 1, 1))
    
    
    plot_list[[i]] <- plot_row
  }
  return(plot_list)
}

final_plot<-all_plot(cb_st)
label_grob <- grobTree(
  textGrob("A", x = 0, y = 1, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("B", x = 0, y = 0.857, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("C", x = 0, y = 0.714, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("D", x = 0, y = 0.571, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("E", x = 0, y = 0.429, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("F", x = 0, y = 0.286, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("G", x = 0, y = 0.143, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold"))
)
combined_plot <- grid.arrange(grobs = final_plot, ncol = 1, left = label_grob)
ggsave(here("/pics", "APC-combined.png"),
       plot = combined_plot, 
       limitsize = FALSE,
       units = "cm", width = 45, height = 59, dpi = 300)


cb_stx<-list()
cb_stx$measure_name<-c("Incidence","Prevalence","Prevalence","Deaths","Deaths","DALYs","DALYs")
cb_stx$cause_name<-c("NC-CVD","NC-CVD","CHD","NC-CVD","CHD","NC-CVD","CHD")

final_plotx<-all_plot(cb_stx)
label_grob <- grobTree(
  textGrob("A", x = 0, y = 1, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("B", x = 0, y = 0.857, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("C", x = 0, y = 0.714, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("D", x = 0, y = 0.571, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("E", x = 0, y = 0.429, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("F", x = 0, y = 0.286, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("G", x = 0, y = 0.143, hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold"))
)
combined_plotx <- grid.arrange(grobs = final_plotx, ncol = 1, left = label_grob)
ggsave(here("/pics", "APC-combinedx.png"),
       plot = combined_plotx, 
       limitsize = FALSE,
       units = "cm", width = 45, height = 59, dpi = 300)


all_plot_3x<-function(combinedstatue){
  plot_list<-list()
  for (i in seq_along(combinedstatue$measure_name)) {

    measure = combinedstatue$measure_name[[i]]
    cause = combinedstatue$cause_name[[i]]
    longage_data <- APC$age %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    PeriodRR_data <- APC$per %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    CohortRR_data <- APC$coh %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    LocalDrifts_data <- APC$loc %>% filter(location_name=="Global",measure_name == measure, cause_name == cause,sex =="Both")
    linestyle<-ifelse(cause=="CHD","longdash","solid")
    color_cause<-case_when(
      measure == "Incidence" ~ "#fcaf69",
      measure == "Prevalence" ~ "#288358",
      measure == "Deaths" ~"#3b54a6",
      measure == "DALYs" ~ "#b61642"
    )
    loc_limits<-APC$loc%>%filter(location_name=="Global",measure_name == measure,sex =="Both")
    loc_max<-max(0,max(loc_limits$CIHi)*1.1)
    loc_low<-min(0,min(loc_limits$CILo)*1.1)
    
    LocalDrifts_plot <- LocalDrifts_data %>%
      ggplot(aes(x = Age, y = `Percent per Year`)) +
      geom_line(color = color_cause, size = 1,linetype=linestyle) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "black", size = 0.8) +
      scale_y_continuous(labels = comma, limits = c(loc_low, loc_max)) +
      labs(title = "Local Drifts", x = "Age", y = "Percent per Year") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
    age_limits<-APC$age%>%filter(location_name=="Global",measure_name == measure,sex =="Both")
    age_max<-max(0,max(age_limits$CIHi)*1.1)
    age_low<-min(0,min(age_limits$CILo)*1.1)
   
    longage_plot <- longage_data %>%
      ggplot(aes(x = Age, y = Rate)) +
      geom_line(color = color_cause, size = 1,linetype=linestyle) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      scale_y_continuous(labels = comma, limits = c(age_low, age_max)) +
      labs(title = "Longitudinal Age Effect", x = "Age", y = "Rate") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
    per_limits<-APC$per%>%filter(location_name=="Global",measure_name == measure,sex =="Both")
    per_max<-max(1,max(per_limits$CIHi)*1.1)
    per_low<-min(1,min(per_limits$CILo)*0.9)
    
    PeriodRR_plot <- PeriodRR_data %>%
      ggplot(aes(x = Period, y = `Rate Ratio`)) +
      geom_line(color = color_cause, size = 1,linetype=linestyle) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
      scale_y_continuous(labels = comma, limits = c(per_low, per_max)) +
      labs(title = "Period Rate Ratio", x = "Period", y = "Rate Ratio") +
      
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
    coh_limits<-APC$coh%>%filter(location_name=="Global",measure_name == measure,sex =="Both")
    coh_max<-max(1,max(coh_limits$CIHi)*1.1)
    coh_low<-min(1,min(coh_limits$CILo)*0.9)
    
    CohortRR_plot <- CohortRR_data %>%
      ggplot(aes(x = Cohort, y = `Rate Ratio`)) +
      geom_line(color = color_cause, size = 1,linetype=linestyle) +
      geom_ribbon(aes(ymin = CILo, ymax = CIHi), alpha = 0.3, fill = color_cause) +
      geom_point(shape = 15, size = 2, color = color_cause) +
      geom_hline(yintercept = 1, linetype = "dashed", color = "black", size = 0.8) +
      scale_y_continuous(labels = comma, limits = c(coh_low, coh_max)) +
      labs(title = "Cohort Rate Ratio", x = "Cohort", y = "Rate Ratio") +
     
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5),
            panel.grid.major = element_line(color = "gray", linetype = "dashed"),
            panel.grid.minor = element_line(color = "lightgray", linetype = "dotted"))
    
    
    title_plot <- ggplot() + 
      theme_void() +
      annotate("text", x = 0, y = 0, label = paste0(cause," ",measure), size = 8, fontface = "bold", angle = 90) +
      theme(plot.background = element_blank(),
            panel.background = element_blank(),
            panel.grid = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank())
    
    
    plot_row <- grid.arrange(LocalDrifts_plot, longage_plot, PeriodRR_plot, CohortRR_plot, ncol = 4, widths = c(1, 1, 1, 1))
    
    
    plot_list[[i]] <- plot_row
  }
  return(plot_list)
}

cb_stx<-list()
cb_stx$measure_name<-c("Incidence","Prevalence","Prevalence","Deaths","Deaths","DALYs","DALYs")
cb_stx$cause_name<-c("NC-CVD","NC-CVD","CHD","NC-CVD","CHD","NC-CVD","CHD")
yline <- c(1,0.861,0.718,0.575,0.433,0.290,0.147)
xline <- c(0.003,0.253,0.503,0.753)
final_plot3x<-all_plot_3x(cb_stx)
label_grob <- grobTree(
  textGrob("a(i)", x = xline[[1]], y = yline[[1]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("b(i)", x = xline[[1]], y = yline[[2]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("c(i)", x = xline[[1]], y = yline[[3]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("d(i)", x = xline[[1]], y = yline[[4]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("e(i)", x = xline[[1]], y = yline[[5]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("f(i)", x = xline[[1]], y = yline[[6]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("g(i)", x = xline[[1]], y = yline[[7]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("a(ii)", x = xline[[2]], y = yline[[1]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("b(ii)", x = xline[[2]], y = yline[[2]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("c(ii)", x = xline[[2]], y = yline[[3]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("d(ii)", x = xline[[2]], y = yline[[4]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("e(ii)", x = xline[[2]], y = yline[[5]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("f(ii)", x = xline[[2]], y = yline[[6]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("g(ii)", x = xline[[2]], y = yline[[7]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("a(iii)", x = xline[[3]], y = yline[[1]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("b(iii)", x = xline[[3]], y = yline[[2]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("c(iii)", x = xline[[3]], y = yline[[3]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("d(iii)", x = xline[[3]], y = yline[[4]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("e(iii)", x = xline[[3]], y = yline[[5]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("f(iii)", x = xline[[3]], y = yline[[6]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("g(iii)", x = xline[[3]], y = yline[[7]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("a(iv)", x = xline[[4]], y = yline[[1]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("b(iv)", x = xline[[4]], y = yline[[2]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("c(iv)", x = xline[[4]], y = yline[[3]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("d(iv)", x = xline[[4]], y = yline[[4]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("e(iv)", x = xline[[4]], y = yline[[5]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("f(iv)", x = xline[[4]], y = yline[[6]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold")),
  textGrob("g(iv)", x = xline[[4]], y = yline[[7]], hjust = -0.2, vjust = 1.5, gp = gpar(fontsize = 18, fontface = "bold"))
)
g <- gridExtra::arrangeGrob(grobs = final_plot3x, ncol = 1)
combined_plot3x <- grobTree(g, label_grob)

ggsave(here("/", "figure 3.pdf"),
       plot = combined_plot3x, 
       limitsize = FALSE,
       units = "cm", width = 42, height = 59, dpi = 1200)
ggsave(here("/", "figure 3.png"),
       plot = combined_plot3x, 
       limitsize = FALSE,
       units = "cm", width = 42, height = 59, dpi = 1200)
write.csv(APC$age,file=here("/pics data", "APC age.csv"))
write.csv(APC$per,file=here("/pics data", "APC per.csv"))
write.csv(APC$coh,file=here("/pics data", "APC coh.csv"))
write.csv(APC$loc,file=here("/pics data", "APC loc.csv"))



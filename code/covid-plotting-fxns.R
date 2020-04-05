###############################
## Code for plotting
###############################
save_cty_data <- function(data_path, case_date = NULL){
  cty_data <- get_cty_data(data_path, case_date)
  save("cty_data", file = str_replace(string = data_path, pattern = "sim", replacement = "county-summary"))
}
get_cty_data <- function(data_path, 
                         case_date = NULL){
  require(usmap)
  
  load(data_path)
  epi_probs <- get_epidemic_prob_by_d(trials = sims, 
                                      prev_threshold = 10, 
                                      cum_threshold = 500, 
                                      max_detect = 50)
  
  ## Read in case data and subset to correct date
  ## Defaults to using the most recent case data
  cty_case_data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
  if(is.null(case_date)){
    cty_case_data <- cty_case_data %>% 
      arrange(desc(date)) %>% 
      group_by(fips) %>% 
      slice(1) %>% 
      ungroup()
  } else {
    cty_case_data <- cty_case_data %>% 
      filter(date == case_date)
  }
  
  ## Read in population data
  pop_data <- read_csv("raw_data/county_pop_2019.csv")
  
  ## Read in county shapefile data and attach all county information
  usmap::us_map(regions = "counties")  %>% 
    as_tibble() %>% 
    distinct(fips, full) %>% 
    inner_join(pop_data %>% 
                 mutate(fips = str_pad(as.character(fips), 5, side = "left", pad = "0")), by = "fips") %>% 
    rename(population = POPESTIMATE2019,
           county = CTYNAME) %>% 
    left_join(cty_case_data %>% 
                select(fips, date, cases, deaths), by = "fips") %>% 
    mutate(cases = ifelse(is.na(cases), 0, cases),
           epi_prob = epi_probs$prob_epidemic[cases+1]) %>% 
    mutate(epi_prob = ifelse(is.na(epi_prob), 1, epi_prob)) %>% 
    rename(state = full)
}

plot_county_risk <- function(county_data, state = NULL){
  shp_file <- usmap::us_map(regions = "counties")
  if(!is.null(state)){
    shp_file <- shp_file %>% filter(full == state)
  }
  shp_file %>% 
    left_join(county_data, by = "fips") %>% 
    ggplot(aes(x = x, y = y)) +
    geom_polygon(aes(group = group, fill = epi_prob), color = "black", size = 0.1) +
    scale_fill_gradient(low = "gainsboro", high = "dark red", name = "Probability")+
    scale_x_continuous("", breaks = NULL) + scale_y_continuous("", breaks = NULL)+
    #labs(title = paste0("Epidemic Probability per county, Cum. Case by ", case_date, ", R0=",r0)) +
    theme(panel.background = element_rect(color = "white", fill = "white"),
          legend.title=element_text(size=8),
          legend.text=element_text(size=6))
  
}

get_summary_stats <- function(county_df, state_stats = "Texas"){
  county_df %>% 
    summarize(frac_us_population = sum(population[which(epi_prob>.5)])/sum(population),
              frac_us_counties = sum(epi_prob>0.5)/n(),
              frac_state_population = sum(population[which(epi_prob>.5 & state == state_stats)]) /
                sum(population[which(state == state_stats)]),
              frac_state_counties = sum(epi_prob>0.5 & state == state_stats)/sum(state == state_stats),
              state = state_stats)
}

get_all_summary_data <- function(folder_path){
  summary_files <- list.files(path = folder_path, full.names = T)
  summary_files[grepl(pattern = "county-summary", summary_files)] %>% 
    map( function(path){load(path); return(cty_data);}) %>% 
    map(get_summary_stats, state_stats = "Texas") %>% 
    bind_rows() %>% 
    mutate(path = summary_files[grepl(pattern = "county-summary", summary_files)]) %>% 
    separate(col = path, into = c("junk", "junk2", "r_not", "detection_probability", "importation_rate", "num_reps"), sep = "_") %>% 
    select(-junk, -junk2)
}

plot_county_summary_sensitivity <- function(df){
  df %>% 
    select(-frac_state_counties, - frac_state_population) %>% 
    gather(key, value, frac_us_population, frac_us_counties) %>% 
    mutate(key = ifelse(key == 'frac_us_population', "US Population", "US Counties")) %>% 
    ggplot(aes(detection_probability, value, color = as.factor(r_not), group = r_not, shape=as.factor(r_not)))+
    geom_line(size=1) + 
    geom_point(size=2) +
    scale_y_continuous(labels = scales::percent, limits = c(0,1))+
    facet_wrap(~key) +
    background_grid(major = 'xy')+
    labs(color  = expression(R[0]), shape=expression(R[0]), linetype=expression(R[0]))+
    xlab("Detection Probability")+
    ylab("Percent")+
    scale_color_manual(values=c("#999999", "grey39", "#000000"))+
    theme_bw(base_size = 10)
}

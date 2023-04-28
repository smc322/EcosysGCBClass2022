
## Function to combine datasets and create cQ graphs


cQ_graph <- function(chemData, chemVar, name_units, disData) {
  
  disData.1 <- disData |>
    select(local_site, date, discharge_rate) 
  
  chemData.1 <- chemData |>
    mutate(chemVar = as.numeric(chemVar)) |>
    filter(!is.na(chemVar)) |>
    select(local_site, date, chemVar) 
  
  chem_dis <- inner_join(disData.1, chemData.1)
  
  
  ggplot(chem_dis) +
    geom_point(aes(log(discharge_rate), log(chemVar))) +
    geom_smooth(method = "loess", aes(log(discharge_rate), log(chemVar)), se = FALSE) +
    theme_light() +
    labs(x = "ln(Q)",
         y = name_units)
  
}

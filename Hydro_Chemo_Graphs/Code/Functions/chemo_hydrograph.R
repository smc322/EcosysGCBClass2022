
## Function to create the hydrograph and chemograph

chemo_hydrograph <- function(chemData, chemVar, name_units, disData) {
  
  
  chemData.1 <- chemData |>
    mutate(chemVar = as.numeric(chemVar)) |>
    filter(!is.na(chemVar))
  
  
  mindate <- min(chemData.1$date)
  maxdate <- max(chemData.1$date)
  
  a <- ggplot(chemData.1) +
    geom_line(aes(date, chemVar)) +
    theme_light() +
    scale_x_date(limits = c(mindate, maxdate)) +
    labs(x = "",
         y = name_units)
  
  
  
  b <- ggplot(disData) +
    geom_line(aes(date, discharge_rate)) +
    theme_light() +
    labs(x = "",
         y = "Streamflow" ~ (m ^ 3 ~ s ^ -1)) +
    scale_x_date(limits = c(mindate, maxdate))
  
  
  library(cowplot)
  plot_grid(a, b, align = "v", ncol = 1)
  
}

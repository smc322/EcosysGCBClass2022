
## Function to create the hydrograph and chemograph


chemo_hydrograph <- function(chemData, chemVar, name_units, disData) {
  
  chemData.1 <- chemData |>
    mutate(chemVar = as.numeric(chemVar)) |>
    filter(!is.na(chemVar))|>
    filter(chemVar != Inf)
  
  
  mindate <- min(chemData.1$date)
  maxdate <- max(chemData.1$date)
  
  coef <- mean(as.numeric(chemData.1$chemVar), na.rm = TRUE) / mean(disData$discharge_rate, na.rm = TRUE)
  
  
  ggplot() +
    geom_line(disData, mapping = aes(date, discharge_rate * coef), color = "#9A9391") +
    geom_line(chemData.1 |> filter(!is.na(chemVar)), mapping = aes(date, chemVar), color = "red3") +
    theme_light() +
    scale_x_date(limits = c(mindate, maxdate)) +
    labs(x = "") +
    scale_y_continuous(
      # first axis
      name = name_units,
      
      # second axis 
      sec.axis = sec_axis(~./coef, name = "Streamflow" ~ (m ^ 3 ~ s ^ -1))
    )  +
    theme(axis.title.y.right = element_text(color = "#9A9391"),
          axis.title.y = element_text(color = "red3"),
          axis.line.y.left = element_line(color = "red3")) 
  
}


################################################################################
#### Code to create hydro/chemographs that are stacked 


#
# chemo_hydrograph <- function(chemData, chemVar, name_units, disData) {
#   
#   
#   chemData.1 <- chemData |>
#     mutate(chemVar = as.numeric(chemVar)) |>
#     filter(!is.na(chemVar))
#   
#   
#   mindate <- min(chemData.1$date)
#   maxdate <- max(chemData.1$date)
#   
#   a <- ggplot(chemData.1) +
#     geom_line(aes(date, chemVar)) +
#     theme_light() +
#     scale_x_date(limits = c(mindate, maxdate)) +
#     labs(x = "",
#          y = name_units)
#   
#   
#   
#   b <- ggplot(disData) +
#     geom_line(aes(date, discharge_rate)) +
#     theme_light() +
#     labs(x = "",
#          y = "Streamflow" ~ (m ^ 3 ~ s ^ -1)) +
#     scale_x_date(limits = c(mindate, maxdate))
#   
#   
#   library(cowplot)
#   plot_grid(a, b, align = "v", ncol = 1)
#   
# }


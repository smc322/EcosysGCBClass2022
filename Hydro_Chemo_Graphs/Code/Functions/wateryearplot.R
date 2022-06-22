
# function to plot all years on one water year plot
library(dataRetrieval)

wateryearplot <- function(chemData, chemVar, name_units) {
  
  chemData.1 <- chemData |>
    mutate(chemVar = as.numeric(chemVar)) |>
    filter(!is.na(chemVar))|>
    filter(chemVar != Inf) |>
    rename(Date = date) |> # must be named "Date" to work with the dataRetrieval function
     #seq along dates starting with the beginning of your water year
    mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                                "-", month(Date), "-", day(Date))))
  
  chemData.2 <- addWaterYear(chemData.1)
  
  ggplot(chemData.2) +
    geom_line(aes(CDate, chemVar, group = waterYear, color = waterYear)) +
    geom_point(aes(CDate, chemVar, group = waterYear, color = waterYear)) +
    stat_smooth(method = "loess", aes(CDate, chemVar), color = "red4", se = FALSE) +
    theme_light() +
    scale_color_viridis_c() +
    scale_x_date(date_labels = "%b %d") +
    labs(x = "") +
    scale_y_continuous(
      name = name_units
    ) 
  
}

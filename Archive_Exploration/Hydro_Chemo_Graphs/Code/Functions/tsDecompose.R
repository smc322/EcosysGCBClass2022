
library(patchwork)
library(anomalize)
library(ggpubr)

## function to decompose timeseries data

tsDecompose <- function(df, variable, a_title, filename) {
  
  df1 <- df |>
    mutate(var = as.numeric(variable))  |>
    filter(!is.na(var)) 
  
  df2 <- df1 %>%
    as_tibble() %>%
    time_decompose(var)
  
  plot_decomp <- function(data, var, a_title) {
    ggplot(data) +
      geom_line(aes(date, var), color = "red4") +
      labs(x = "", y = "", title = a_title) +
      theme_light()  +
      geom_vline(xintercept = seq.Date(from = as.Date('1980-01-01'), to = as.Date('2020-01-01'),by = 'year'),
                 linetype=2, alpha = 0.4) 
  }
  
  obs <- plot_decomp(df2, df2$observed, "Average Quarterly Value")
  trend <- plot_decomp(df2, df2$trend, "Trend")
  szn <- plot_decomp(df2, df2$season, "Seasonal")
  rem <- plot_decomp(df2, df2$remainder, "Remainder")
  
  
  plot <- (obs) / (trend) / (szn) / (rem) +
    plot_annotation(title = a_title)
  ggsave(paste("Hydro_Chemo_Graphs/Plots/", filename, ".png", sep = ""), height = 12, width = 12, units = "in", dpi = 500)
  
  return(df2)
}
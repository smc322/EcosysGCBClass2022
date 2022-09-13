library(tidyverse)
library(lubridate)
library(dataRetrieval)


# Loch Vale streamflow and nutreint data
lochvale <- read.csv("Hydro_Chemo_Graphs/Data/USGS_lochvaleoutlet.csv") |>
  select(-X, -X.1) |>
  mutate(Date = as.Date(Date)) |>
  rename(date = Date) |>
  filter(year(date) > 1984) # only keep years when we had nutrient and streamflwo


# Make HCG of weekly average values 

lochvale_weekly <- lochvale |>
  mutate(x = round((day(date)/5))*5,
         x = ifelse(x == 0, 1, x), 
         date2 = paste(year(date), month(date), x, sep = "-")) |>
  mutate(Date = as.Date(date2)) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
  mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date)))) |>
  mutate(decade = ifelse(year(date) <= 1990, 1, NA),
         decade = ifelse(between(year(date), 1990, 2000), "1990-1999", decade),
         decade = ifelse(year(date) >= 2000, "2000-2019", decade)) |>
  mutate(decade = as.factor(decade)) |>
  group_by(CDate, decade) |>
  mutate(ave_weekly_nitrate = mean(Nitrate_mgl, na.rm = TRUE),
         ave_weekly_dis = mean(discharge_rate, na.rm = TRUE)) |>
  ungroup() |>
  addWaterYear() |>
  select(CDate, decade, ave_weekly_nitrate, ave_weekly_dis) |>
  distinct() |>
  mutate(date = CDate) |>
  rename(discharge_rate = ave_weekly_dis)

  
  #coef <- mean(as.numeric(lochvale_weekly$ave_weekly_nitrate), na.rm = TRUE) / mean(lochvale_weekly$discharge_rate, na.rm = TRUE)
  coef <- 0.05
  
  ggplot(lochvale_weekly |> filter(decade != "1")) +
    geom_line(aes(date, discharge_rate * coef, linetype = decade), color = "#336a98") +
    geom_line(aes(date, ave_weekly_nitrate, linetype = decade), color = "red4") +
    theme_classic() +
    #scale_x_date(limits = c(mindate, maxdate)) +
    labs(x = "",
         caption = "Figure 3. Average weekly nitrate concentrations (red lines) and average weekly streamflow (blue lines). Data 
represented by the solid lines were collected in 1990-1999 and data represented by thedotted lines were 
collected in 2000-2019.") +
    scale_y_continuous(
      # first axis
      name = "Average Weekly Nitrate"~(mg~L^-1),
      
      # second axis 
      sec.axis = sec_axis(~./coef, name = "Average Weekly Streamflow" ~ (m ^ 3 ~ s ^ -1))
    )  +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0),
          axis.title.y.right = element_text(color = "#336a98"),
          axis.title.y = element_text(color = "red4"),
          #axis.line.y.left = element_line(color = "red4"),
          legend.title = element_blank())



ggsave("HCG_average_weekly_decadal.png", width = 6.5, height = 4.5, dpi=500)



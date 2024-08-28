# Script to create HCG plots for Loch Vale Case Study ####
# timeseries broken into 3 periods via Palmer Drought Severity Index 

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)

nitr <- '#90C24C'
hydro <- '#4D6BBC'  
    
  # Loch Vale streamflow and nutreint data
  lochvale <- read.csv("Data/USGS_lochvaleoutlet.csv") |>
    select(-X) |>
    mutate(Date = as.Date(date)) |>
    select(-date) |>
    rename(nitrate_mgL = Nitrate_mgL) |>
    mutate(site_code = 'LochVale') |>
    # add seasons to the dataframe (based on snow depth data)
    mutate(mon = month(Date)) |>
    mutate(season = case_when(mon %in% c(12,1,2,3,4) ~ "Winter",
                              mon %in% c(5,6)  ~ "Snowmelt runoff",
                              mon %in% c(7,8,9,10,11) ~ "Summer")) |>
    mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer')))  |>
    mutate(decade = ifelse(year(Date) <= 1990, 1, NA),
           # BASED ON MCMC changepoint analysis in PDSI_fig.R :) -- the time periods should be split like this!!
           decade = ifelse(between(year(Date), 1990, 2000), "1990-2000", decade),
           decade = ifelse(between(year(Date), 2001, 2007), "2001-2007 (Drought)", decade),
           decade = ifelse(between(year(Date), 2008, 2019), "2008-2019", decade)) |>
    mutate(decade = as.factor(decade))
  
  # format data into weekly averages, per decade and organized by water year ####
  
  lochvale_weekly <- lochvale |>
    mutate(x = round((day(Date)/5))*5,
           x = ifelse(x == 0, 1, x), 
           date2 = paste(year(Date), month(Date), x, sep = "-")) |>
    rename(Date_orig = Date) |>
    mutate(Date = as.Date(date2)) |># must be named "Date" to work with the dataRetrieval function
    #seq along dates starting with the beginning of your water year
    mutate(date2 = ifelse(is.na(Date), paste0(year(Date_orig), '-03-01'), date2)) |>
    mutate(Date = as.Date(date2)) |>
    mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                                "-", month(Date), "-", day(Date)))) |>
    mutate(decade = ifelse(year(Date_orig) <= 1990, 1, NA),
           # BASED ON MCMC changepoint analysis in PDSI_fig.R :) -- the time periods should be split like this!!
           decade = ifelse(between(year(Date_orig), 1990, 2000), "1990-2000", decade),
           decade = ifelse(between(year(Date_orig), 2001, 2007), "2001-2007 (Drought)", decade),
           decade = ifelse(between(year(Date_orig), 2008, 2019), "2008-2019", decade)) |>
    mutate(decade = as.factor(decade)) |>
    group_by(CDate, decade) |>
    mutate(ave_weekly_nitrate = mean(nitrate_mgL, na.rm = TRUE),
           ave_weekly_dis = mean(discharge_Ls, na.rm = TRUE)) |>
    ungroup() |>
    addWaterYear() |>
    select(CDate, decade, ave_weekly_nitrate, ave_weekly_dis, season) |>
    distinct() |>
    mutate(date = CDate) |>
    rename(discharge_rate = ave_weekly_dis) # renamed to work with plot code
  
  
  #timeseries ####
  
  coef <- mean(as.numeric(lochvale_weekly$ave_weekly_nitrate), na.rm = TRUE) / mean(lochvale_weekly$discharge_rate, na.rm = TRUE)
  
  
  p1 <- ggplot(lochvale_weekly |> filter(decade != "1")) +
    geom_line(aes(date, discharge_rate * coef, linetype = decade), color = hydro) +
    geom_line(aes(date, ave_weekly_nitrate, linetype = decade), color = nitr) +
    theme_classic() +
    labs (x = '') +
    scale_y_continuous(
      # first axis
      name = "Average Weekly Nitrate "~(mg~L^-1),
      
      # second axis 
      sec.axis = sec_axis(~./coef, name = "Average Weekly Streamflow "~(L~s^-1))
    )  +
    
    guides(linetype = guide_legend(override.aes = list(color = "black"))) +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0),
          axis.title.y.right = element_text(color = hydro),
          axis.title.y = element_text(color = nitr, vjust = -2),
          #axis.line.y.left = element_line(color = "red4"),
          legend.title = element_blank()) +
    scale_x_date(labels = date_format('%b')) +
    scale_linetype_manual(values = c(3, 2, 1)) +
    geom_vline(xintercept= c(as.numeric(as.Date("1900-12-01")), as.numeric(as.Date('1901-05-01')), as.numeric(as.Date('1901-07-01'))),
               linetype=4, colour="grey") +
    theme(plot.title = element_text(face = 'bold', size = rel(0.5),
                                    hjust = 0.5),
           
          axis.text = element_text(size = 9),
          axis.title = element_text(size =9)) +
    theme(legend.position = 'none')
  
  p1
  
  # cumulative sum figure ####
  massload <- lochvale_weekly |> 
    filter(decade != "1") |>
    arrange(decade, date) |>
    group_by(decade) |>
    mutate(timestep = as.numeric(difftime(date, lag(date)), units="secs")) |> #timestep in seconds
    mutate(masskg = (ave_weekly_nitrate * discharge_rate * timestep)/1000000) |>
    mutate(cum_sum = cumsum(ifelse(is.na(masskg), 0, masskg))) |>
    ungroup()
  
  
  p2 <- ggplot(massload) +
    geom_line(aes(date, cum_sum, linetype = decade)) +
    #geom_line(aes(date, ave_weekly_nitrate, linetype = decade), color = nitr) +
    theme_classic() +
    labs(x = '', y = 'Cumulative sum nitrate (kg)') +
    guides(linetype = guide_legend(override.aes = list(color = "black"))) +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0),
          legend.title = element_blank())+
    #legend.position = c(0.1,0.9)) +
    scale_x_date(labels = date_format('%b')) +
    scale_linetype_manual(values = c(3, 2, 1)) +
    geom_vline(xintercept= c(as.numeric(as.Date("1900-11-01")), as.numeric(as.Date('1901-04-01')), as.numeric(as.Date('1901-07-01'))),
               linetype=4, colour="grey") +
    theme(plot.title = element_text(face = 'bold',  size = rel(0.5),
                                    hjust = 0.5),
           
          axis.text = element_text(size = 9),
          axis.title = element_text(size =9))  +
    theme(legend.position = 'none')
  p2
  
  
  # cQ figure ####
  slopes <- lochvale_weekly |>
    filter(decade != "1") |>
    group_by(decade, season) |>
    do({
      mod = lm(log10(ave_weekly_nitrate) ~ log10(discharge_rate), data = .)
      data.frame(Intercept = coef(mod)[1],
                 Slope = coef(mod)[2],
                 SE = as.numeric((coef(summary(mod))[, "Std. Error"])[2]),
                 CI.up = confint(mod, 'log10(discharge_rate)', level=0.95)[2],
                 CI.down = confint(mod, 'log10(discharge_rate)', level=0.95)[1]) 
    })

  
  p3 <- ggplot(lochvale_weekly |> filter(decade != '1',
                                         !is.na(log10(discharge_rate)),
                                         !is.na(log10(ave_weekly_nitrate))), aes(log10(discharge_rate), log10(ave_weekly_nitrate))) +
    geom_point(aes(shape = decade, fill = season), alpha = 0.25) +
    scale_shape_manual('', values = c(21,22,24)) +
    geom_smooth(method = "lm", se = FALSE, aes(color = season, linetype = decade)) +
    scale_linetype_manual('', values = c(3,2,1)) + 
    guides(linetype = guide_legend(override.aes = list(color = "black"))) +
    scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
    scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
    theme_classic() +
    labs(y = 'log10 nitrate',
         x = 'log10 streamflow')  +
    theme(plot.title = element_text(face = 'bold',  size = rel(0.5),
                                    hjust = 0.5),
           
          axis.text = element_text(size = 9),
          axis.title = element_text(size =9)) +
    guides(fill = 'none') + 
    theme(legend.position = 'none') 
  
p3
  

  
  
  
  p4 <- ggplot(slopes) +
    labs(x = "cQ slope", y = "") +
    annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
    annotate("text", label = 'chemostatic', x = 0, y = 0.2, size = 2,color = "black") +
    annotate("text", label = 'mobilization', x = 0.3, y = 0.2, size = 2,color = "black") +
    annotate("text", label = 'dilution', x = -0.2, y = 0.2, size = 2,color = "black") +
    geom_point(mapping = aes(Slope, season, shape = decade, fill = season, color = season),
                size = 2.5, alpha = 0.7, position = position_dodge(width=0.5)) +
    # geom_errorbarh(mapping = aes(Slope, season, xmin=Slope-SE, xmax=Slope+SE, color = season, group =decade),
    #                height = 0.2, position=position_dodge(width=0.5)) +
    geom_errorbarh(mapping = aes(Slope, season, xmin=CI.down, xmax=CI.up, color = season, group =decade),
                   height = 0.2, position=position_dodge(width=0.5)) +
    scale_y_discrete(limits=rev) + # flip y axis order for continuity with other plots
    theme_classic() +
    scale_shape_manual('', values = c(21,22,24)) +
    scale_fill_manual('', values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
    scale_color_manual('', values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
    # guides(fill = 'none') + 
    #theme(legend.position = 'none') +
    theme(plot.title = element_text(face = 'bold', size = rel(0.5),
                                    hjust = 0.5),
           
          axis.text = element_text(size = 9),
          axis.title = element_text(size =9),
          axis.text.y = element_blank()) +
    theme(legend.position = 'none')
p4
  
# Flow duration curves ####

## text below, code, and instructions from: https://vt-hydroinformatics.github.io/Quarto_Book/09-Flow_Duration_Curves.html#plot-a-flow-duration-curve-using-the-probabilities

## Exceedence probability (P), Probability a flow is equaled or exceeded: P=100*[M/(n+1)]
## M = Ranked position of the flow n = total number of observations in data record

# Here’s a description of what we will do: > Pass our Qdat data to mutate and create a new column that is equal to the ranks of the discharge column. > Then pass that result to mutate again and create another column equal exceedence probability (P) * 100, which will give us %.

#Flow is negative in rank() to make 
#high flows ranked low (#1)
Qdat <- lochvale |>
  filter(decade != 1) |>
  group_by(decade, season) |>
  mutate(rank = rank(-discharge_Ls)) |>
  mutate(P = 100 * (rank / (length(discharge_Ls) + 1))) |>
  ungroup()

## Now construct the following plot: A line with P on the x axis and flow on the y axis. Name the x axis “% Time flow equaled or exceeded” and log the y axis.

p5 <- Qdat |> ggplot(aes(x = P, y = discharge_Ls, color = season, linetype =decade))+
  geom_line()+
  scale_y_log10()+
  xlab("% Time flow equalled or exceeded")+
  ylab("Streamflow"~(L~s^-1)) +
  scale_color_manual('', values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  scale_linetype_manual('',values = c(3, 2, 1)) +
  theme_classic() +
  theme(plot.title = element_text(face = 'bold',  size = rel(0.5),
                                  hjust = 0.5),
         
        axis.text = element_text(size = 9),
        axis.title = element_text(size =9)) +
  theme(legend.position = 'none')

p5

# Format multi-panel figure ####

layout = '
AAABBB
CCDDEE '


(p1+p2+p3+p4+p5) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(guides = 'collect', design = layout)

ggsave("Figures/LochVale/HCG_average_weekly_BREAKPOINT.png", width = 8, height = 6, dpi=1200)




# look closer at winter 2008-2019 ####
lochvale_lateperiod <- lochvale |>
  filter(year(Date) > 2007) 


#mon %in% c(11,12,1,2,3) # winter months
library(plotly)
ggplotly(ggplot(lochvale_lateperiod) +
           geom_line(aes(Date, discharge_Ls)))

ggplotly(ggplot(lochvale) +
           geom_line(aes(Date, nitrate_mgL)))

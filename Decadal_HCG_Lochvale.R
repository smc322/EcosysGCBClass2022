library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)

nitr <- '#90C24C'
hydro <- '#4D6BBC'  

# Loch Vale streamflow and nutreint data
lochvale <- read.csv("Hydro_Chemo_Graphs/Data/USGS_lochvaleoutlet.csv") |>
  select(-X, -X.1) |>
  mutate(Date = as.Date(Date)) |>
  rename(date = Date) |>
  filter(year(date) > 1984) # only keep years when we had nutrient and streamflow


# format data into weekly averages, per decade and organized by water year ####

lochvale_weekly <- lochvale |>
  mutate(x = round((day(date)/5))*5,
         x = ifelse(x == 0, 1, x), 
         date2 = paste(year(date), month(date), x, sep = "-")) |>
    mutate(Date = as.Date(date2)) |># must be named "Date" to work with the dataRetrieval function
  #seq along dates starting with the beginning of your water year
    mutate(date2 = ifelse(is.na(Date), paste0(year(date), '-03-01'), date2)) |>
    mutate(Date = as.Date(date2)) |>
    mutate(CDate=as.Date(paste0(ifelse(month(Date) < 10, "1901", "1900"),
                              "-", month(Date), "-", day(Date)))) |>
  mutate(decade = ifelse(year(date) <= 1990, 1, NA),
         decade = ifelse(between(year(date), 1990, 2000), "1990-1999", decade),
         decade = ifelse(between(year(date), 2000, 2009), "2000-2009", decade),
         decade = ifelse(between(year(date), 2010, 2019), "2010-2019", decade)) |>
  mutate(decade = as.factor(decade)) |>
  mutate(mon = month(date)) |> #and add seasons to the dataframe
  mutate(season = case_when(mon %in% c(10,11,12) ~ "Oct-Dec",
                            mon %in% c(1,2,3) ~ "Jan-Mar",
                            mon %in% c(4,5,6)  ~ "Apr-Jun",
                            mon %in% c(7,8,9) ~ "Jul-Sep")) |>
  mutate(season = factor(season, levels = c('Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep'))) |>
  group_by(CDate, decade) |>
  mutate(ave_weekly_nitrate = mean(Nitrate_mgl, na.rm = TRUE),
         ave_weekly_dis = mean(discharge_rate, na.rm = TRUE)) |>
  ungroup() |>
  addWaterYear() |>
  select(CDate, decade, ave_weekly_nitrate, ave_weekly_dis, season) |>
  distinct() |>
  mutate(date = CDate) |>
  rename(discharge_rate = ave_weekly_dis) # renamed to work with pre-written plot code


#timeseries ####
  
  #coef <- mean(as.numeric(lochvale_weekly$ave_weekly_nitrate), na.rm = TRUE) / mean(lochvale_weekly$discharge_rate, na.rm = TRUE)
  coef <- 0.05
  
p1 <- ggplot(lochvale_weekly |> filter(decade != "1")) +
    geom_line(aes(date, discharge_rate * coef, linetype = decade), color = hydro) +
    geom_line(aes(date, ave_weekly_nitrate, linetype = decade), color = nitr) +
    theme_classic() +
  labs (x = '') +
#     labs(x = "",
#          caption = "Figure 3. Average weekly nitrate concentrations (red lines) and average weekly streamflow (blue lines) at the 
# Loch Vale outlet. Data represented by the dotted lines were collected in 1990-1999, data represented by the dashed 
# lines were collected between 2000-2009, and data represented by the solid lines were collected in 2010-2019.") +
    scale_y_continuous(
      # first axis
      name = "Average Weekly Nitrate "~(mg~L^-1),
      
      # second axis 
      sec.axis = sec_axis(~./coef, name = "Average Weekly Streamflow "~(m^3~s^-1))
    )  +
    
    guides(linetype = guide_legend(override.aes = list(color = "black"))) +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0),
          axis.title.y.right = element_text(color = hydro),
          axis.title.y = element_text(color = nitr, vjust = -2),
          #axis.line.y.left = element_line(color = "red4"),
          legend.title = element_blank(),
          legend.position = 'none') +
    scale_x_date(labels = date_format('%b')) +
    scale_linetype_manual(values = c(3, 2, 1)) +
    geom_vline(xintercept= c(as.numeric(as.Date("1900-10-01")), as.numeric(as.Date('1901-01-01')), as.numeric(as.Date('1901-04-01')), as.numeric(as.Date('1901-07-01')), as.numeric(as.Date('1901-09-30'))),
                linetype=4, colour="grey") +
    theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))



# cumulative sum figure ####
massload <- lochvale_weekly |> 
  filter(decade != "1") |>
  arrange(decade, date) |>
  group_by(decade) |>
  mutate(timestep = as.numeric(difftime(date, lag(date)), units="secs")) |> #timestep in seconds
  mutate(massMg = (ave_weekly_nitrate * discharge_rate * timestep)/1000000) |>
  mutate(cum_sum = cumsum(ifelse(is.na(massMg), 0, massMg))) |>
  ungroup()


p2 <- ggplot(massload) +
  geom_line(aes(date, cum_sum, linetype = decade)) +
  #geom_line(aes(date, ave_weekly_nitrate, linetype = decade), color = nitr) +
  theme_classic() +
  labs(x = '', y = 'Cumulative sum nitrate (Mg)') +
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0),
        legend.title = element_blank())+
        #legend.position = c(0.1,0.8)) +
  scale_x_date(labels = date_format('%b')) +
  scale_linetype_manual(values = c(3, 2, 1)) +
  geom_vline(xintercept= c(as.numeric(as.Date("1900-10-01")), as.numeric(as.Date('1901-01-01')), as.numeric(as.Date('1901-04-01')), as.numeric(as.Date('1901-07-01')), as.numeric(as.Date('1901-09-30'))),
             linetype=4, colour="grey") +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))



# cQ figure ####
slopes <- lochvale_weekly |>
  filter(decade != "1") |>
  group_by(decade, season) |>
  do({
    mod = lm(log10(ave_weekly_nitrate) ~ log10(discharge_rate), data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2]) 
  })

  
p3 <- ggplot(lochvale_weekly |> filter(decade != '1',
                                            !is.na(log10(discharge_rate)),
                                            !is.na(log10(ave_weekly_nitrate))), aes(log10(discharge_rate), log10(ave_weekly_nitrate))) +
  geom_point(aes(shape = decade, fill = season), alpha = 0.25) +
  scale_shape_manual('', values = c(21,22,24)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = season, linetype = decade)) +
  scale_linetype_manual('', values = c(3,2,1)) + 
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  scale_fill_manual('',#labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                    #                   values = palette_OkabeIto[1:4]) +
                    values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
  scale_color_manual('',#labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                     #                   values = palette_OkabeIto[1:4]) +
                     values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
  theme_classic() +
  labs(y = 'log10 nitrate'~(mg~L^-1),
       x = 'log10 streamflow'~(m^3~s^-1))  +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8)) +
  guides(fill = 'none') + 
  theme(legend.position = 'none') 


p4 <- ggplot() +
  labs(x = "cQ slope", y = "") +
  annotate("rect", xmin = -0.05, xmax = 0.05, ymin = 0, ymax = Inf, alpha = 0.2, color = "grey") +
  annotate("text", label = 'chemostatic', x = 0, y = 0.2, size = 2,color = "black") +
  annotate("text", label = 'mobilization', x = 0.2, y = 0.2, size = 2,color = "black") +
  annotate("text", label = 'dilution', x = -0.2, y = 0.2, size = 2,color = "black") +
  geom_jitter(slopes, mapping = aes(Slope, season, shape = decade, fill = season, color = season), 
              width = 0, height = 0.2, size = 2.5, alpha = 0.7) +
  scale_y_discrete(limits=rev) + # flip y axis order for continuity with other plots
  theme_classic() +
  scale_shape_manual('', values = c(21,22,24)) +
  scale_fill_manual('', values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
  scale_color_manual('', values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
 # guides(fill = 'none') + 
  #theme(legend.position = 'none') +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))



(p1 |p2)/(p3|p4) +
  plot_annotation(tag_levels = 'a', tag_suffix = ')')
ggsave("HCG_average_weekly_decadal.png", width = 6.5, height = 4.5, dpi=500)




# Take a look at the final decade, especially the mobilization in January-March and dilution October-December ####
a <- ggplot() +
  geom_line(lochvale |> 
              filter(year(date)>=1990) |>
              mutate(mon = month(date)) |> #and add seasons to the dataframe
              mutate(season = case_when(mon %in% c(10,11,12) ~ "Oct-Dec",
                                        mon %in% c(1,2,3) ~ "Jan-Mar",
                                        mon %in% c(4,5,6)  ~ "Apr-Jun",
                                        mon %in% c(7,8,9) ~ "Jul-Sep")) |>
              mutate(season = factor(season, levels = c('Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep'))), mapping = aes(date, nitrate_umol, color = season, group = 1)) +
  geom_vline(xintercept= c(as.numeric(as.Date("2000-01-01")), as.numeric(as.Date('2010-01-01')))) +
  theme_classic()


b <- ggplot() +
  geom_line(lochvale |> 
              filter(year(date)>=1990) |>
              mutate(mon = month(date)) |> #and add seasons to the dataframe
              mutate(season = case_when(mon %in% c(10,11,12) ~ "Oct-Dec",
                                        mon %in% c(1,2,3) ~ "Jan-Mar",
                                        mon %in% c(4,5,6)  ~ "Apr-Jun",
                                        mon %in% c(7,8,9) ~ "Jul-Sep")) |>
              mutate(season = factor(season, levels = c('Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep'))), mapping = aes(date, discharge_rate, color = season, group = 1)) +
  geom_vline(xintercept= c(as.numeric(as.Date("2000-01-01")), as.numeric(as.Date('2010-01-01')))) +
  theme_classic()





ggplot(lochvale_weekly |> filter(decade == '2010-2019',
                                 !is.na(log10(discharge_rate)),
                                 !is.na(log10(ave_weekly_nitrate))), aes(log10(discharge_rate), log10(ave_weekly_nitrate))) +
  geom_point(aes(shape = decade, fill = season), alpha = 0.25) +
  scale_shape_manual('', values = c(21,22,24)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = season, linetype = decade)) +
  scale_linetype_manual('', values = c(3,2,1)) + 
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  scale_fill_manual('',#labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                    #                   values = palette_OkabeIto[1:4]) +
                    values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
  scale_color_manual('',#labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                     #                   values = palette_OkabeIto[1:4]) +
                     values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
  theme_classic() +
  labs(y = 'log10 nitrate'~(mg~L^-1),
       x = 'log10 streamflow'~(m^3~s^-1))  +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8)) +
  guides(fill = 'none') + 
  theme(legend.position = 'none') 


c <- ggplot(lochvale_weekly |> filter(decade != '1',
                                 season == 'Oct-Dec',
                                 !is.na(log10(discharge_rate)),
                                 !is.na(log10(ave_weekly_nitrate))), aes(log10(discharge_rate), log10(ave_weekly_nitrate))) +
  geom_point(aes(shape = decade, fill = date), alpha = 0.25, size = 3) +
  scale_shape_manual('', values = c(21,22,24)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = decade, linetype = decade)) +
  scale_linetype_manual('', values = c(3,2,1)) + 
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  scale_fill_viridis_c(trans = 'date') +
  scale_color_manual('',#labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                     #                   values = palette_OkabeIto[1:4]) +
                     values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
  theme_classic() +
  labs(y = 'log10 nitrate'~(mg~L^-1),
       x = 'log10 streamflow'~(m^3~s^-1))  +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))


d<- ggplot(lochvale_weekly |> filter(decade != '1',
                                 season == 'Jan-Mar',
                                 !is.na(log10(discharge_rate)),
                                 !is.na(log10(ave_weekly_nitrate))), aes(log10(discharge_rate), log10(ave_weekly_nitrate))) +
  geom_point(aes(shape = decade, fill = date), alpha = 0.25, size = 3) +
  scale_shape_manual('', values = c(21,22,24)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = decade, linetype = decade)) +
  scale_linetype_manual('', values = c(3,2,1)) + 
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  scale_fill_viridis_c(trans = 'date') +
  scale_color_manual('',#labels = c("Jan-Mar", "Apr-Jun", "Jul-Sep","Oct-Dec"),
                     #                   values = palette_OkabeIto[1:4]) +
                     values = c("#8EA42E","#7EA8C4","#EFD15E","#93796B")) +
  theme_classic() +
  labs(y = 'log10 nitrate'~(mg~L^-1),
       x = 'log10 streamflow'~(m^3~s^-1))  +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))


library(ggpubr)

e <- ggplot(lochvale_weekly |> filter(decade != '1',
                                 season == 'Jan-Mar',
                                 !is.na(log10(discharge_rate)),
                                 !is.na(log10(ave_weekly_nitrate))), aes(decade, ave_weekly_nitrate)) +
  geom_boxplot() +
  labs(title = 'Jan-Mar (mobilization in late decade)') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('1990-1999','2000-2009'), c('1990-1999','2010-2019'), c('2000-2009','2010-2019')))

f <- ggplot(lochvale_weekly |> filter(decade != '1',
                                 season == 'Jan-Mar',
                                 !is.na(log10(discharge_rate)),
                                 !is.na(log10(ave_weekly_nitrate))), aes(decade, discharge_rate)) +
  geom_boxplot() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('1990-1999','2000-2009'), c('1990-1999','2010-2019'), c('2000-2009','2010-2019')))

g <- ggplot(lochvale_weekly |> filter(decade != '1',
                                      season == 'Oct-Dec',
                                      !is.na(log10(discharge_rate)),
                                      !is.na(log10(ave_weekly_nitrate))), aes(decade, ave_weekly_nitrate)) +
  geom_boxplot() +
  labs(title = 'Oct-Dec (dilution in late decade)') +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('1990-1999','2000-2009'), c('1990-1999','2010-2019'), c('2000-2009','2010-2019')))

h <- ggplot(lochvale_weekly |> filter(decade != '1',
                                      season == 'Oct-Dec',
                                      !is.na(log10(discharge_rate)),
                                      !is.na(log10(ave_weekly_nitrate))), aes(decade, discharge_rate)) +
  geom_boxplot() +
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.1))) +
  stat_compare_means(fontface='bold',label = 'p.signif',comparisons = list(c('1990-1999','2000-2009'), c('1990-1999','2010-2019'), c('2000-2009','2010-2019')))


a/b

c/d

(e | g)/(f | h)

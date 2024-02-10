
library(tidyverse)
library(macrosheds)

library(tidyverse)
library(lubridate)
library(dataRetrieval)
library(scales)
library(patchwork)

nitr <- '#90C24C'
hydro <- '#4D6BBC' 


?ms_download_core_data
#ms_download_core_data(my_ms_dir, domains='hjandrews')

# list available Macrosheds R package functions
help(package = macrosheds)

#ms_vars <- ms_load_variables()


?ms_load_sites
site_data <- ms_load_sites() |>
  filter(site_code == 'GSWS08')

?ms_load_product
my_ms_dir <- "./data/macrosheds"
my_chem <- ms_load_product(
  my_ms_dir,
  prodname = "stream_chemistry",
  filter_vars = "NO3_N", # units == mg/L
  site_codes = c("GSWS02", "GSWS08"),
  sort_result = TRUE,
  warn = TRUE
)


my_q <- ms_load_product(
  my_ms_dir,
  prodname = "discharge", # units == L/s
  site_codes = c("GSWS02", "GSWS08"),
  sort_result = TRUE,
  warn = TRUE
)


# flux calc using macrosheds
my_flux <- ms_calc_flux(
  chemistry = my_chem,
  q = my_q,
  q_type = 'discharge'
)



my_chem_2 <- my_chem |>
  drop_na(val) |>
  rename(Nitrate_mgl = val,
         date = datetime)  |>
  select(-var, -ms_status, -ms_interp)

my_q_2 <- my_q |>
  rename(discharge_rate = val,
         date=datetime) |>
  select(-var, -ms_status, -ms_interp, -val_err)


andrews <- left_join(my_chem_2, my_q_2)

andrews02 <- andrews |>
  filter(site_code == 'GSWS02')

andrews08 <- andrews |>
  filter(site_code == 'GSWS08')

# format data into weekly averages, per decade and organized by water year ####

andrews02_weekly <- andrews02 |>
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
         # BASED ON MCMC changepoint analysis in PDSI_fig.R :) -- the time periods should be split like this!!
         decade = ifelse(between(year(date), 1990, 2000), "1990-2000", decade),
         decade = ifelse(between(year(date), 2001, 2007), "2001-2007 (Drought)", decade),
         decade = ifelse(between(year(date), 2008, 2019), "2008-2019", decade)) |>
  mutate(decade = as.factor(decade)) |>
  mutate(mon = month(date)) |> #and add seasons to the dataframe
  # mutate(season = case_when(mon %in% c(10,11,12) ~ "Oct-Dec",
  #                           mon %in% c(1,2,3) ~ "Jan-Mar",
  #                           mon %in% c(4,5,6)  ~ "Apr-Jun",
  #                           mon %in% c(7,8,9) ~ "Jul-Sep")) |>
  # mutate(season = factor(season, levels = c('Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep'))) |>
  mutate(season = case_when(mon %in% c(11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9,10) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  group_by(CDate, decade, site_code) |>
  mutate(ave_weekly_nitrate = mean(Nitrate_mgl, na.rm = TRUE),
         ave_weekly_dis = mean(discharge_rate, na.rm = TRUE)) |>
  ungroup() |>
  addWaterYear() |>
  select(CDate, decade, site_code, ave_weekly_nitrate, ave_weekly_dis, season) |>
  distinct() |>
  mutate(date = CDate) |>
  rename(discharge_rate = ave_weekly_dis) # renamed to work with pre-written plot code


andrews08_weekly <- andrews08 |>
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
         # BASED ON MCMC changepoint analysis in PDSI_fig.R :) -- the time periods should be split like this!!
         decade = ifelse(between(year(date), 1990, 2000), "1990-2000", decade),
         decade = ifelse(between(year(date), 2001, 2007), "2001-2007 (Drought)", decade),
         decade = ifelse(between(year(date), 2008, 2019), "2008-2019", decade)) |>
  mutate(decade = as.factor(decade)) |>
  mutate(mon = month(date)) |> #and add seasons to the dataframe
  # mutate(season = case_when(mon %in% c(10,11,12) ~ "Oct-Dec",
  #                           mon %in% c(1,2,3) ~ "Jan-Mar",
  #                           mon %in% c(4,5,6)  ~ "Apr-Jun",
  #                           mon %in% c(7,8,9) ~ "Jul-Sep")) |>
  # mutate(season = factor(season, levels = c('Oct-Dec','Jan-Mar','Apr-Jun','Jul-Sep'))) |>
  mutate(season = case_when(mon %in% c(11,12,1,2,3) ~ "Winter",
                            mon %in% c(4,5,6)  ~ "Snowmelt runoff",
                            mon %in% c(7,8,9,10) ~ "Summer")) |>
  mutate(season = factor(season, levels = c('Winter','Snowmelt runoff','Summer'))) |>
  group_by(CDate, decade, site_code) |>
  mutate(ave_weekly_nitrate = mean(Nitrate_mgl, na.rm = TRUE),
         ave_weekly_dis = mean(discharge_rate, na.rm = TRUE)) |>
  ungroup() |>
  addWaterYear() |>
  select(CDate, decade, site_code, ave_weekly_nitrate, ave_weekly_dis, season) |>
  distinct() |>
  mutate(date = CDate) |>
  rename(discharge_rate = ave_weekly_dis) # renamed to work with pre-written plot code


#timeseries ####

coef02 <- mean(as.numeric(andrews02_weekly$ave_weekly_nitrate), na.rm = TRUE) / mean(andrews02_weekly$discharge_rate, na.rm = TRUE)

coef08 <- mean(as.numeric(andrews08_weekly$ave_weekly_nitrate), na.rm = TRUE) / mean(andrews08_weekly$discharge_rate, na.rm = TRUE)


p1_site2 <- ggplot(andrews02_weekly |> filter(decade != "1",
                                            site_code == 'GSWS02')) +
  geom_line(aes(date, discharge_rate * coef02, linetype = decade), color = hydro) +
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
    sec.axis = sec_axis(~./coef02, name = "Average Weekly Streamflow "~(m^3~s^-1))
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
  geom_vline(xintercept= c(as.numeric(as.Date("1900-11-01")), as.numeric(as.Date('1901-04-01')), as.numeric(as.Date('1901-07-01'))),
             linetype=4, colour="grey") +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))


p1_site8 <- ggplot(andrews08_weekly |> filter(decade != "1",
                                            site_code == 'GSWS08')) +
  geom_line(aes(date, discharge_rate * coef08, linetype = decade), color = hydro) +
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
    sec.axis = sec_axis(~./coef08, name = "Average Weekly Streamflow "~(m^3~s^-1))
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
  geom_vline(xintercept= c(as.numeric(as.Date("1900-11-01")), as.numeric(as.Date('1901-04-01')), as.numeric(as.Date('1901-07-01'))),
             linetype=4, colour="grey") +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))


p1_site2
p1_site8


# cumulative sum figure ####
massload02 <- andrews02_weekly |> 
  filter(decade != "1") |>
  arrange(decade, date, site_code) |>
  group_by(decade, site_code) |>
  mutate(timestep = as.numeric(difftime(date, lag(date)), units="secs")) |> #timestep in seconds
  mutate(massMg = (ave_weekly_nitrate * discharge_rate * timestep)/1000000) |>
  mutate(cum_sum = cumsum(ifelse(is.na(massMg), 0, massMg))) |>
  ungroup()

massload08 <- andrews08_weekly |> 
  filter(decade != "1") |>
  arrange(decade, date, site_code) |>
  group_by(decade, site_code) |>
  mutate(timestep = as.numeric(difftime(date, lag(date)), units="secs")) |> #timestep in seconds
  mutate(massMg = (ave_weekly_nitrate * discharge_rate * timestep)/1000000) |>
  mutate(cum_sum = cumsum(ifelse(is.na(massMg), 0, massMg))) |>
  ungroup()


p2_site2 <- ggplot(massload02 |> filter(site_code == 'GSWS02')) +
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
  geom_vline(xintercept= c(as.numeric(as.Date("1900-11-01")), as.numeric(as.Date('1901-04-01')), as.numeric(as.Date('1901-07-01'))),
             linetype=4, colour="grey") +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))


p2_site8 <- ggplot(massload08 |> filter(site_code == 'GSWS08')) +
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
  geom_vline(xintercept= c(as.numeric(as.Date("1900-11-01")), as.numeric(as.Date('1901-04-01')), as.numeric(as.Date('1901-07-01'))),
             linetype=4, colour="grey") +
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8))


# cQ figure ####
slopes02 <- andrews02_weekly |>
  filter(decade != "1") |>
  group_by(decade, season, site_code) |>
  do({
    mod = lm(log10(ave_weekly_nitrate) ~ log10(discharge_rate), data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2],
               SE = as.numeric((coef(summary(mod))[, "Std. Error"])[2]),
               CI.up = confint(mod, 'log10(discharge_rate)', level=0.95)[2],
               CI.down = confint(mod, 'log10(discharge_rate)', level=0.95)[1]) 
  })

slopes08 <- andrews08_weekly |>
  filter(decade != "1") |>
  group_by(decade, season, site_code) |>
  do({
    mod = lm(log10(ave_weekly_nitrate) ~ log10(discharge_rate), data = .)
    data.frame(Intercept = coef(mod)[1],
               Slope = coef(mod)[2],
               SE = as.numeric((coef(summary(mod))[, "Std. Error"])[2]),
               CI.up = confint(mod, 'log10(discharge_rate)', level=0.95)[2],
               CI.down = confint(mod, 'log10(discharge_rate)', level=0.95)[1]) 
  })


p3_site2 <- ggplot(andrews02_weekly |> filter(decade != '1',
                                       !is.na(log10(discharge_rate)),
                                       !is.na(log10(ave_weekly_nitrate)),
                                       site_code == 'GSWS02'), aes(log10(discharge_rate), log10(ave_weekly_nitrate))) +
  geom_point(aes(shape = decade, fill = season), alpha = 0.25) +
  scale_shape_manual('', values = c(21,22,24)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = season, linetype = decade)) +
  scale_linetype_manual('', values = c(3,2,1)) + 
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
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


p3_site8 <- ggplot(andrews08_weekly |> filter(decade != '1',
                                            !is.na(log10(discharge_rate)),
                                            !is.na(log10(ave_weekly_nitrate)),
                                            site_code == 'GSWS08'), aes(log10(discharge_rate), log10(ave_weekly_nitrate))) +
  geom_point(aes(shape = decade, fill = season), alpha = 0.25) +
  scale_shape_manual('', values = c(21,22,24)) +
  geom_smooth(method = "lm", se = FALSE, aes(color = season, linetype = decade)) +
  scale_linetype_manual('', values = c(3,2,1)) + 
  guides(linetype = guide_legend(override.aes = list(color = "black"))) +
  scale_fill_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
  scale_color_manual('',values = c("#7EA8C4","#EFD15E","#E6A0C4")) +
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




p4_site2 <- ggplot(slopes02 |>
               filter(site_code == 'GSWS02')) +
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
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8),
        axis.text.y = element_blank())


p4_site8 <- ggplot(slopes08 |> filter(site_code == 'GSWS08')) +
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
  theme(plot.title = element_text(face = 'bold', family = 'serif', size = rel(0.5),
                                  hjust = 0.5),
        text = element_text(family = 'serif'),
        axis.text = element_text(size = 8),
        axis.title = element_text(size =8),
        axis.text.y = element_blank())


layout = '
AB
CD'


p1_site2+p2_site2+p3_site2+p4_site2 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(guides = 'collect', design = layout)


p1_site8+p2_site8+p3_site8+p4_site8 +
  plot_annotation(tag_levels = 'a', tag_suffix = ')') +
  plot_layout(guides = 'collect', design = layout)


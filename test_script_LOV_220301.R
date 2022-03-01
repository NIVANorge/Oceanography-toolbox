
library (lubridate)
library (dplyr)
library(tidyr)     
library(akima) # for interpolations
library(ggplot2)
library(readxl)
library(mgcv)     # for gam() in plots
library(patchwork) # for arranging plots in panels
library(tidyverse)
library(scales)

library(RColorBrewer)
# specific functions needed for the contour plots

# NB! this source file contains the function (used by okokyst_plot function here), and needs to be saved in the same folder as this script

source("okokyst_plot_functions_LOV.R")# new version in 2021

# Use a self-defined color scale  
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

#dataset
df <- read.delim ("Datasett/OKOKYST_data_cleaned_Ns_2.txt")
df$Date <- as.Date(df$Date, "%d.%m.%Y")




#find out where in the dataset there is high extreme values
extreme_values <- df %>% filter((TOTN > 800) | (SIO2 > 2000) | (TOTP > 100) | (NH4 > 400))
#remove the extreme values from dataset
df <- df %>% filter(((TOTN < 800)  %>% replace_na(TRUE)) , ((SIO2  < 2000) %>% replace_na(TRUE)) , ((TOTP < 100)%>% replace_na(TRUE)) , ((NH4 < 400)%>% replace_na(TRUE)))

#select years
df <- subset(df, Date >= "2017-01-01" &  Date <= "2021-11-30")

# For water chemistry variables with discrete depths (0-30m), NB! you need to replace with the column names in the file you use!  remove NA values
df_chem <- df %>%
  select(-Depth2)  %>%
  #filter (Depth1 == 0 | Depth1 == 5| Depth1 == 10 | Depth1 == 20 | Depth1 == 30) %>%
  rename (Depth = Depth1) 

VT16 <- df_chem %>%
  filter(StationId == "VT16")

#TOTP
quantile (VT16$TOTP, na.rm = T)

# med punkter

grenser <- c(0,60)
VT16_TOTP <- okokyst_plot(data = VT16, varname = "TOTP", ctd_variable = FALSE,
                          colored_points = TRUE, limits = grenser, binwidth = 20, max_timediff = 25, ylabel = F, xlabel = F) +
  scale_fill_gradientn("TOTP", colours = jet.colors(16), limits=grenser, breaks = c(10,20,30,40,50), oob=squish) +
  scale_x_datetime(date_breaks = "2 month", date_labels =  "%b %y") +
  theme(axis.text.x=element_text(angle=60, hjust=1), plot.title = element_text(hjust = 0.5))

VT16_TOTP

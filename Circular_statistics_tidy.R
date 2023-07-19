# This is an adaptation of Callum's script for the circular plots with tidyverse syntax
# this syntax prevents you from using for loops making the code shorter
##### load the packages #####
library(tidyverse)
library(readxl)
library(circular)
library(CircMLE)
library(ggridges)
library(REdaS)

##### load data #####
# Clears work space turns figure windows off
rm(list = ls())
graphics.off()

# open data
path <- 'C:\\data\\juan\\MEGAsync\\data\\behavioural_data\\selective_attention'
file <- 'res_sa.xlsx'

data <- tibble()
for (page in excel_sheets(paste(path,file,sep= '\\'))){
    data <- bind_rows(data,read_excel(paste(path,file,sep= '\\'),sheet= page))
}

# change the sex column from numbers to actual categories
# add a column for positive and negative runs
data <- data %>%
    mutate(sex= ifelse(sex== 1, 'male', 'female'), runtrue= ifelse(is.na(rundir), 0, 1),
           protocol= as.factor(protocol))

# get rid of the na and tell R that the data is circular
# my data is in degrees but you can also work in radians or hours
# modulo is specified in radians independently of the units
data <- data %>%
    filter(!is.na(run_digi_rot) & !is.na(run_fictrac_rot)) %>%
    mutate(run_digi_rot_rad= deg2rad(run_digi_rot),
           run_fictrac_rot_rad= deg2rad(run_fictrac_rot)) %>%
    mutate(run_digi_rot_rad= circular(run_digi_rot_rad, units= 'radians', modulo= '2pi'),
           run_fictrac_rot_rad= circular(run_fictrac_rot_rad, units= 'radians', modulo= '2pi'))

# make a new data frame with the mean, sd and mean resultant length (rho) for each protocol
mdata <- data %>%
    group_by(protocol) %>%
    summarise(avg= mean.circular(run_fictrac_rot_rad), dev= sd.circular(run_fictrac_rot_rad),
              mrl= rho.circular(run_fictrac_rot_rad)) %>%
    mutate(dev= circular(dev, units= 'radians', modulo= '2pi'),
           mrl= circular(mrl, units= 'radians', modulo= '2pi')) %>%
    mutate(lb= avg-dev, ub= avg+dev) %>%
    mutate(lb= ifelse(lb>0, lb, 0), ub= ifelse(ub<2*pi, ub, 2*pi)) # for plotting purposes only


##### plot data #####
# The first step plots all the running directions along the outside of the circle
# (circle perimeter= 1) and colors the points based on treatment    
p <- ggplot(data= fmdata, aes(x= mrl, y= avg)) +
geom_point(data= fdata, aes(x= 1.15, y= run_fictrac_rot_rad, color= protocol), pch= 16,
               alpha= 0.75, size= 3, position= position_jitter(width= 0.1))

# then, creates a line segment that connects the center of the circle with the mean running
# directions     
p <- p+ geom_segment(aes(x= 0, y= avg, xend= mrl , yend= avg, color= protocol), linewidth= 1)
    
# add the error bars
p <- p+ geom_linerange(aes(x= mrl, ymin= lb, ymax= ub, color= protocol), size= 1)

# show the mean running directions, colored by protocol
p <- p+ geom_point(aes(fill = protocol),color = 'black',pch = 21,  stroke = 0.35, size = 3)

# format the axis, legend, etc
p <- p+ theme_bw()+ theme(panel.grid.major= element_blank(), panel.grid.minor= element_blank(),
                          legend.position= "none", text= element_text(size=14))

# make the plot circular
p <- p+ coord_polar(theta= 'y', start= rad(90), direction= 1, clip= 'off')

# Adds a line at 1 that becomes the outside of the circle
p <- p+ geom_vline(xintercept = 1, color = "black", size = 0.5)

# Adds the degree values you want to include around the perimeter
p <- p+ scale_y_continuous(limits=c(0, 2*pi), breaks= c(0, 90, 180, 270),
                               labels= c(0,90,180,270))

    
# makes the plot even more pretty
p <- p+ theme(axis.title.y=element_blank(), axis.text.y=element_blank(), 
              axis.ticks.y=element_blank(), axis.title.x=element_blank(), 
              axis.text.x=element_blank())

# make panels for each protocol
p <- p+ facet_wrap(~protocol, ncol= 4)

# remove panel labels and add a legend
p <- p+ theme(strip.text= element_blank())

p

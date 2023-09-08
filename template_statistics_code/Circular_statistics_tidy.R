# This is an adaptation of Callum's script for the circular plots with tidyverse syntax
# this syntax prevents you from using for loops making the code shorter
##### load the packages #####
library(tidyverse)
library(readxl)
library(ggridges)
library(circular)
library(REdaS)
library(brms)
library(bpnreg)
library(easystats)

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
    mutate(sex= ifelse(sex== 1, 'male', 'female'), runtrue= ifelse(is.na(rundir), 0, 1))

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
              mrl= rho.circular(run_fictrac_rot_rad), int_contrast= mean(int_contrast)) %>%
    mutate(dev= circular(dev, units= 'radians', modulo= '2pi'),
           mrl= circular(mrl, units= 'radians', modulo= '2pi')) %>%
    mutate(lb= avg-dev, ub= avg+dev) %>%
    mutate(lb= ifelse(lb>0, lb, 0), ub= ifelse(ub<2*pi, ub, 2*pi)) # for plotting purposes only
# 
# 
# ##### plot data #####
# # set color palette
palette <- c('#4527a0', '#283593', '#1565c0', '#0277bd', '#00838f', '#00695c', '#2e7d32','#558b2f')
# 
# # The first step plots all the running directions along the outside of the circle
# # (circle perimeter= 1) and colors the points based on treatment    
# p <- ggplot(data= mdata, aes(x= mrl, y= avg)) +
# geom_point(data= data, aes(x= 1.15, y= run_fictrac_rot_rad, color= protocol), pch= 16,
#                alpha= 0.75, size= 3, position= position_jitter(width= 0.1))
# 
# # then, creates a line segment that connects the center of the circle with the mean running
# # directions     
# p <- p+ geom_segment(aes(x= 0, y= avg, xend= mrl , yend= avg, color= protocol), linewidth= 1)
#     
# # add the error bars
# p <- p+ geom_linerange(aes(x= mrl, ymin= lb, ymax= ub, color= protocol), linewidth= 1)
# 
# # show the mean running directions, colored by protocol
# p <- p+ geom_point(aes(fill = protocol),color = 'black',pch= 21,  stroke= 0.35, size= 3)
# 
# # format the axis, legend, etc
# p <- p+ theme_bw()+ theme(panel.grid.major= element_blank(), panel.grid.minor= element_blank(),
#                           legend.position= "none", text= element_text(size=14))
# 
# # make the plot circular
# p <- p+ coord_polar(theta= 'y', start= rad(90), direction= 1, clip= 'off')
# 
# # Adds a line at 1 that becomes the outside of the circle
# p <- p+ geom_vline(xintercept = 1, color = "black", linewidth= 0.5)
# 
# # Adds the degree values you want to include around the perimeter
# p <- p+ scale_y_continuous(limits=c(0, 2*pi), breaks= c(0, 90, 180, 270),
#                                labels= c(0,90,180,270))
# 
#     
# # makes the plot even more pretty
# p <- p+ theme(axis.title.y=element_blank(), axis.text.y=element_blank(), 
#               axis.ticks.y=element_blank(), axis.title.x=element_blank(), 
#               axis.text.x=element_blank())
# 
# # make panels for each protocol
# p <- p+ facet_wrap(~protocol, ncol= 4)
# 
# # remove panel labels and add a legend
# p <- p+ theme(strip.text= element_blank())
# 
# # change the color palette
# p <- p+ scale_color_manual(values= palette) + scale_fill_manual(values= palette)
# 
# p


##### plot for single stimulus conditions #####
sdata <- data %>%
    filter(protocol== 1 | protocol== 8) %>%
    mutate(int_contrast= factor(int_contrast, levels= c(0, -0.95)))

smdata <- mdata %>%
    filter(protocol== 1 | protocol== 8) %>%
    mutate(int_contrast= factor(int_contrast, levels= c(0, -0.95)))

spalette <- c('#4527a0','#558b2f')

# plot
fig2c <- ggplot(data= smdata, aes(x= mrl, y= avg)) +
    geom_point(data= sdata, aes(x= 1.15, y= run_fictrac_rot_rad, color= int_contrast), pch= 16,
              alpha= 0.75, size= 1, position= position_jitter(width= 0.1)) +
    geom_segment(aes(x= 0, y= avg, xend= mrl , yend= avg, color= int_contrast), linewidth= 1) +
    geom_linerange(aes(x= mrl, ymin= lb, ymax= ub, color= int_contrast), linewidth= 1) +
    geom_point(aes(fill = int_contrast),color = 'black',pch = 21,  stroke = 0.35, size = 2) +
    theme_bw()+
    theme(panel.grid.major= element_blank(), panel.grid.minor= element_blank(),
                       legend.position= "bottom", text= element_text(size= 8)) +
     coord_polar(theta= 'y', start= rad(90), direction= 1, clip= 'off') +
     geom_vline(xintercept = 1, color = "black", linewidth= 0.5) +
     scale_y_continuous(limits=c(0, 2*pi), breaks= c(0, 90, 180, 270),
                        labels= c(0,90,180,270)) +
     theme(axis.title.y=element_blank(), axis.text.y=element_blank(), 
           axis.ticks.y=element_blank(), axis.title.x=element_blank(), 
           axis.text.x=element_blank()) +
     facet_wrap(~int_contrast, ncol= 2) +
     theme(strip.text= element_blank(), panel.border= element_blank()) +
     scale_color_manual(values= spalette, name= 'luminance contrast\npolarisation contrast',
                        labels= c('  0\n-0.95', '-0.95\n  0')) +
     scale_fill_manual(values= spalette, name= 'luminance contrast\npolarisation contrast',
                       labels= c('  0\n-0.95', '-0.95\n  0'))

ggsave('C:\\Users\\22905562\\Desktop\\figure2c.pdf', plot= fig2c, width= 7.09, height= 2.36,
       dpi= 300)

# do the stats
# keep only those columns of interest for bayesian analysis
bsdata <- subset(sdata, select= c(crab, protocol, run_fictrac_rot_rad)) %>%
  mutate(protocol= as.factor(protocol))


mod0 <- bpnr(pred.I= run_fictrac_rot_rad~ (1|crab), data= bsdata,
             its= 10000, burn= 1000, n.lag= 3, seed= 105)
mod1 <- bpnr(pred.I= run_fictrac_rot_rad~ protocol+ (1|crab), data= bsdata,
             its= 10000, burn= 1000, n.lag= 3, seed= 106)

run_single <- bsdata %>%
  group_by(protocol) %>%
  summarise(rad2deg(mean(run_fictrac_rot_rad)), rad2deg(sd(run_fictrac_rot_rad)))

##### plot for dual loom only #####
ddata <- data %>%
    filter(protocol!= 1 & protocol!= 8) %>%
    mutate(int_contrast= factor(int_contrast, levels= c(-0.03125, -0.0625, -0.125, -0.25, -0.5,
                                                        -0.95)))

dmdata <- mdata %>%
    filter(protocol!= 1 & protocol!= 8) %>%
    mutate(avg_deg= rad2deg(avg), dev_deg=rad2deg(dev),
           int_contrast= factor(int_contrast, levels= c(-0.03125, -0.0625, -0.125, -0.25, -0.5,
                                                        -0.95)))

# plot
ggplot(data= dmdata, aes(x= mrl, y= avg)) +
    geom_point(data= ddata, aes(x= 1.15, y= run_fictrac_rot_rad, color= protocol), pch= 16,
               alpha= 0.75, size= 3, position= position_jitter(width= 0.1)) +
    geom_segment(aes(x= 0, y= avg, xend= mrl , yend= avg, color= protocol), linewidth= 1) +
    geom_linerange(aes(x= mrl, ymin= lb, ymax= ub, color= protocol), linewidth= 1) +
    geom_point(aes(fill = protocol),color = 'black',pch = 21,  stroke = 0.35, size = 3) +
    theme_bw()+ theme(panel.grid.major= element_blank(), panel.grid.minor= element_blank(),
                      legend.position= "none", text= element_text(size=14)) +
    coord_polar(theta= 'y', start= rad(90), direction= 1, clip= 'off') +
    geom_vline(xintercept = 1, color = "black", linewidth= 0.5) +
    scale_y_continuous(limits=c(0, 2*pi), breaks= c(0, 90, 180, 270),
                       labels= c(0,90,180,270)) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), 
          axis.ticks.y=element_blank(), axis.title.x=element_blank(), 
          axis.text.x=element_blank()) +
    facet_wrap(~protocol, ncol= 3) +
    theme(strip.text= element_blank()) +
    scale_color_manual(values= palette[2:7]) +
  scale_fill_manual(values= palette[2:7])

fig3c <- ggplot(data= dmdata, aes(x= mrl, y= avg)) +
    geom_point(data= ddata, aes(x= 1.15, y= run_fictrac_rot_rad, color= int_contrast), pch= 16,
               alpha= 0.75, size= 1, position= position_jitter(width= 0.1)) +
    geom_segment(aes(x= 0, y= avg, xend= mrl , yend= avg, color= int_contrast), linewidth= 1) +
    geom_linerange(aes(x= mrl, ymin= lb, ymax= ub, color= int_contrast), linewidth= 1) +
    geom_point(aes(fill = int_contrast),color = 'black',pch = 21,  stroke = 0.35, size = 2) +
    theme_bw()+
    theme(panel.grid.major= element_blank(), panel.grid.minor= element_blank(),
          legend.position= "right", text= element_text(size= 8)) +
    coord_polar(theta= 'y', start= rad(90), direction= 1, clip= 'off') +
    geom_vline(xintercept = 1, color = "black", linewidth= 0.5) +
    scale_y_continuous(limits=c(0, 2*pi), breaks= c(0, 90, 180, 270),
                       labels= c(0,90,180,270)) +
    theme(axis.title.y=element_blank(), axis.text.y=element_blank(), 
          axis.ticks.y=element_blank(), axis.title.x=element_blank(), 
          axis.text.x=element_blank()) +
    facet_wrap(~int_contrast, ncol= 3) +
    theme(strip.text= element_blank(), panel.border= element_blank()) +
    scale_color_manual(values= palette[2:7], name= 'luminance contrast',
                       labels= c('-0.03', '-0.06', '-0.13', '-0.25', '-0.5', '-0.95')) +
    scale_fill_manual(values= palette[2:7], name= 'luminance contrast',
                      labels= c('-0.03', '-0.06', '-0.13', '-0.25', '-0.5', '-0.95'))

ggsave('C:\\Users\\22905562\\Desktop\\figure3c.pdf', plot= fig3c, width= 7.09, height= 2.36,
       dpi= 300)

# do the stats
bddata <- ddata %>%
  subset(select= c(crab, protocol, run_fictrac_rot_rad)) %>%
  mutate(protocol= as.factor(protocol))

mod0 <- bpnr(pred.I= run_fictrac_rot_rad~ (1|crab), data= bddata,
             its= 10000, burn= 1000, n.lag= 3, seed= 105)
mod1 <- bpnr(pred.I= run_fictrac_rot_rad~ protocol+ (1|crab), data= bddata,
             its= 10000, burn= 1000, n.lag= 3, seed= 106)

run_double <- bddata %>%
  group_by(protocol) %>%
  summarise(rad2deg(mean(run_fictrac_rot_rad)), rad2deg(sd(run_fictrac_rot_rad)))

# ##### stats for all data #####
# # Performs a Rayleigh test of uniformity, assessing the significance of the mean
# # resultant length
 ray_test <- rayleigh.test(ddata$run_fictrac_rot_rad)
# # Performs the Watson-Wheeler test (or Mardia-Watson-Wheeler, or uniform score)
# # for homogeneity on two or more samples of circular data. Non-parametric
# wheel_test <- watson.wheeler.test(data$run_fictrac_rot_rad, data$protocol)
# # Performs the Watson-Williams test for homogeneity of means between several
# # samples of circular data. Parametric and so has some assumptions. 
# # Check these out in the package docs. 
# wat_test <- watson.williams.test(ddata$run_fictrac_rot_rad, ddata$protocol)
# 
# # mod0 <- bpnr(pred.I= run_fictrac_rot_rad~ (1|crab), data= bdata,
# #              its= 10000, burn= 1000, n.lag= 3, seed= 101)
# # mod1 <- bpnr(pred.I= run_fictrac_rot_rad~ protocol+ (1|crab), data= bdata,
# #              its= 10000, burn= 1000, n.lag= 3, seed= 102)
# # 
# # windows()
# # traceplot(mod0, parameter = 'beta1')
# # traceplot(mod1, parameter = 'beta2')
# # # trace plots look consistently variable. Bayesian stats are ok
# # # extract model coefficients
# # mod1_coef <- coef_circ(mod1, type = "categorical", units = "radians")
# # mod1_coef <- as.data.frame(mod1_coef$Means)



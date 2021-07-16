## Code to process thermal images from hummingbirds
## Field data Jun-Jul 2021
## Data collected by: Anusha Shankar, Emily Blackwell
## Code started by: Anusha Shankar


library(here)
library(ggplot2)
library(stringr)
library(reshape2)
library(gridExtra)
library(dplyr) ## for renaming columns etc.
library(plyr)

here <- here::here()
ir_dat <- read.csv(here::here("IR", "IR_data.csv"))

## General functions
## Generic plot theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())

## Defining axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))

## Processing time
ir_dat$Time <- str_pad(ir_dat$Time, width=4, side="left", pad="0")

ir_dat$Hour <- substr(ir_dat$Time, 1, 2)
ir_dat$Minute <- substr(ir_dat$Time, 3, 4)

ir_dat$Hour[ir_dat$Hour==24] <- "00"


ir_dat$DateFormat <- as.POSIXct(paste(paste(ir_dat$Year, ir_dat$Month, ir_dat$Day, sep = "-"), 
                                         paste(str_pad(ir_dat$Hour, width=2, side="left", pad="0"), 
                                               str_pad(ir_dat$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                                   format='%Y-%m-%d %H:%M')

## Melting to make plotting all temp measurements together easier
m.ir_dat <- melt(ir_dat, id.vars = c("BirdID", "DateFormat"), measure.vars = c("Ts_max", "Teye", "Tamb"))
m.ir_dat <- dplyr::rename(m.ir_dat, Measure = variable, Temp = value)
m.ir_dat$Measure <- plyr::revalue(as.factor(m.ir_dat$Measure), c(Ts_max = "Surface Temp", Teye= "Eye Temp", Tamb = "Ambient Temp"))


ggplot(ir_dat, aes(DateFormat, Ts_max)) + geom_point(aes(col=Tamb)) + my_theme #+ facet_wrap(~BirdID, scales = "free_x")


#### Plots ####
bird1 <- ggplot(ir_dat[ir_dat$BirdID=="CAAN01",], aes(DateFormat, Ts_max)) + 
  geom_point(aes(col=BirdID)) + my_theme +
  facet_grid(BirdID~., scales = "free") + ylim(0,40) +
  theme(axis.text.x = element_text(angle=90, size = 10), axis.title.x = element_blank(),
        legend.position = "none")

bird2 <- ggplot(ir_dat[ir_dat$BirdID=="CAAN02",], aes(DateFormat, Ts_max)) + 
  geom_point(aes(col=BirdID)) + my_theme +
  facet_grid(BirdID~., scales = "free") + ylim(0,40) +
  theme(axis.text.x = element_text(angle=90, size = 10), axis.title.x = element_blank(),
        legend.position = "none")

bird3 <- ggplot(ir_dat[ir_dat$BirdID=="CAAN03",], aes(DateFormat, Ts_max)) + 
  geom_point(aes(col=BirdID)) + my_theme +
  facet_grid(BirdID~., scales = "free") + ylim(0,40) +
  theme(axis.text.x = element_text(angle=90, size = 10),
        legend.position = "none")

grid.arrange(bird1, bird2, bird3, ncol=1, nrow=3)

ggplot(ir_dat[ir_dat$BirdID=="CAAN02",], aes(DateFormat, Ts_max)) +  my_theme +
  geom_point(aes(col=BirdID)) + geom_line(aes(group=BirdID)) + geom_line(aes(y=Teye)) + 
  geom_line(aes(y=Tamb), linetype="dashed") + theme(legend.position = "none")


ggplot(m.ir_dat[m.ir_dat$BirdID != "CAAN01",], aes(DateFormat, Temp)) +  my_theme +
  geom_point(aes(col=Measure)) + geom_line(aes(group=Measure, col=Measure)) +
  facet_grid(.~BirdID, scales = "free") + xlab("Time") + ylab(Temp.lab) +
  scale_color_manual(values=c("magenta", "maroon", "grey30")) +
  theme(legend.key.height = unit(3, 'lines'))


ggplot(m.ir_dat[m.ir_dat$BirdID=="CAAN02",], aes(DateFormat, Temp)) +  my_theme_blank +
  geom_point(aes(col=Measure)) + geom_line(aes(group=Measure, col=Measure)) +
  #facet_grid(.~BirdID, scales = "free") + 
  xlab("Time") + ylab(Temp.lab) +
  scale_color_manual(values=c("magenta", "maroon", "grey30")) +
  theme(legend.key.height = unit(3, 'lines'))
  theme(legend.position = "none")
  
  
ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateFormat, Ts_max)) + 
    geom_point(aes(col=BirdID)) + my_theme +
    facet_grid(BirdID~., scales = "free") + ylim(0,40) +
    theme(axis.text.x = element_text(angle=90, size = 10),
          legend.position = "none")

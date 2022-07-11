## Code to process thermal images from hummingbirds
## Field data Jun-Jul 2022
## Data collected by: Emily Blackwell, Shenni Liang, Sophia Wolfe, Santi Tabares Erices, Anusha Shankar
## Code started by: Anusha Shankar


library(here)
library(ggplot2)
library(stringr)
library(reshape2)
library(gridExtra)
library(dplyr) ## for renaming columns etc.
library(plyr)

#here <- here::here()
ir_dat <- read.csv(here::here("IR", "IR_data_2022.csv"))
#Temps <- read.csv(here::here("IR", "Thermocouple_Temps.csv"))

## General functions
## Generic plot theme
my_theme <- theme_classic(base_size = 20) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())

my_colors <- c("#23988aff", "#440558ff", "#9ed93aff")

## Defining axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))
AmbTemp.lab <- expression(atop(paste("Ambient Temperature (", degree,"C)")))
SurfTemp.lab <- expression(atop(paste("Max Surface Temperature (", degree,"C)")))


## Label bird ID with run number if applicable
ir_dat$BirdID[!is.na(ir_dat$Run)] <- paste0(ir_dat$BirdID[!is.na(ir_dat$Run)], "_", ir_dat$Run[!is.na(ir_dat$Run)])
#ir_dat <- ir_dat[ir_dat$Run=="Y",]
##Only include values where eye region is clearly visible
ir_dat <- ir_dat[ir_dat$Reliable=="Y",]
ir_dat <- ir_dat[!is.na(ir_dat$Time),]

## Processing time
ir_dat$Time <- str_pad(ir_dat$Time, width=4, side="left", pad="0")

ir_dat$Hour <- substr(ir_dat$Time, 1, 2)
ir_dat$Minute <- substr(ir_dat$Time, 3, 4)

ir_dat$Hour[ir_dat$Hour==24] <- "00"
ir_dat$Day[ir_dat$Hour<19] <- ir_dat$Day[ir_dat$Hour<19]+1

ir_dat$DateFormat <- as.POSIXct(paste(paste(ir_dat$Year, ir_dat$Month, ir_dat$Day, sep = "-"), 
                                         paste(str_pad(ir_dat$Hour, width=2, side="left", pad="0"), 
                                               str_pad(ir_dat$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                                   format='%Y-%m-%d %H:%M')

ir_dat$DateLubri <- lubridate::ymd_hms(ir_dat$DateFormat)
ir_dat <- dplyr::arrange(ir_dat, DateLubri)

ir_dat$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                    paste(str_pad(ir_dat$Hour, width=2, side="left", pad="0"), 
                                          str_pad(ir_dat$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                              format='%Y-%m-%d %H:%M')
ir_dat$SameDate[ir_dat$Hour<19] <- ir_dat$SameDate[ir_dat$Hour<19]+86400
ir_dat$SameDate <- lubridate::ymd_hms(ir_dat$SameDate)

## Make sure temperature columns are numeric
ir_dat$Ts_max <- as.numeric(ir_dat$Ts_max)
ir_dat$Teye <- as.numeric(ir_dat$Teye)
ir_dat$Tamb <- as.numeric(ir_dat$Tamb)
ir_dat$Tc_thermocouple <- as.numeric(ir_dat$Tc_thermocouple)

## 01 - Normo
## 02 - Deep Torpor
## 03 - Deep Torpor
## 04 - Deep Torpor
## 05 - Deep Torpor
## 06 - Normo
## 07 - Transition
## 08 - Normo
## 09 - Normo
## 10 - Normo
## 11 - Transition
## 12 - Transition

ir_dat$BirdID <- as.factor(ir_dat$BirdID)
# ir_dat$Category <- NA
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[1]] <- "Normothermic"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[2]] <- "DeepTorpor"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[3]] <- "DeepTorpor"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[4]] <- "DeepTorpor"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[5]] <- "DeepTorpor"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[6]] <- "Normothermic"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[7]] <- "Transition"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[8]] <- "Normothermic"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[9]] <- "Normothermic"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[10]] <- "Normothermic"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[11]] <- "Transition"
# ir_dat$Category[ir_dat$BirdID==levels(ir_dat$BirdID)[12]] <- "Transition"

# ir_dat$Category <- factor(ir_dat$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))

## Add Tc and Ta column from thermocouple data into ir_dat
Temps$DateLubri <- lubridate::ymd_hms(Temps$DateLubri)
agg_ir <- merge(ir_dat, Temps, by="DateLubri", all.x=TRUE)
agg_ir$AmbientTemp_C <- as.numeric(agg_ir$AmbientTemp_C)
agg_ir$ChamberTemp_C <- as.numeric(agg_ir$ChamberTemp_C)


## Melting to make plotting all temp measurements together easier
m.ir_dat <- melt(ir_dat, id.vars = c("BirdID", "DateFormat"), measure.vars = c("Ts_max", "Teye", "Tamb"))
m.ir_dat <- dplyr::rename(m.ir_dat, Measure = variable, Temp = value)
m.ir_dat$Measure <- plyr::revalue(as.factor(m.ir_dat$Measure), c(Ts_max = "Surface Temp", Teye= "Eye Temp", Tamb = "Ambient Temp"))
m.ir_dat$Temp <- as.numeric(m.ir_dat$Temp)

ggplot(ir_dat, aes(DateFormat, Ts_max)) + geom_point() + #geom_point(aes(col=Tamb)) + scale_color_manual() +
  my_theme + facet_wrap(~BirdID, scales = "free_x")

#### Plots ####
bird1 <- ggplot(ir_dat[ir_dat$BirdID=="BCHU02",], aes(DateFormat, Ts_max)) + 
  geom_point(aes(col=BirdID)) + my_theme +
  geom_line(aes(col=BirdID)) +
  #facet_grid(BirdID~., scales = "free") + ylim(0,40) +
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

ggplot(ir_dat[ir_dat$BirdID=="BTMG01_1",], aes(DateFormat, Ts_max)) +  my_theme +
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

  
ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateLubri, Ts_max)) + 
    geom_point(aes(col=BirdID), size=3) + my_theme +
  geom_line(aes(y=Tamb), linetype="dashed") + 
    facet_grid(BirdID~., scales = "free") + ylim(0,40) +
    theme(axis.text.x = element_text(size = 20),
          legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)

## All individuals
ggplot(ir_dat, aes(DateFormat, Ts_max)) + 
  #geom_point(aes(col=Category), size=3) + 
  geom_point() + my_theme +
  geom_line(aes(y=Tamb), linetype="dashed") + 
  #scale_color_manual(values = my_colors) +
  facet_wrap(~BirdID, scales = "free") +
  ylim(0,40) +
  theme(axis.text.x = element_text(size = 20),
        legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)


## Standardize axes by using the same date for all individuals
ggplot(agg_ir, aes(SameDate, Ts_max)) + 
  geom_point(aes(col=Category), size=3) + my_theme +
  geom_line(aes(y=AmbientTemp_C), linetype="dashed") + 
  scale_color_manual(values = my_colors) +
  facet_wrap(~BirdID, scales = "free") +
  scale_x_datetime(limits = ymd_hms(c("2021-07-23 21:10:00", "2021-07-24 02:45:00"))) +
  ylim(0,40) +
  theme(axis.text.x = element_text(size = 20),
        legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)

## Standardize x (time) axis by using the same date for all individuals
ggplot(agg_ir, aes(SameDate, Ts_max)) + 
  geom_point(aes(col=Category), size=3) + my_theme +
  geom_line(aes(y=Tamb), col='grey30', linetype="dotted", size=1.5) + 
  geom_line(aes(y=ChamberTemp_C), col='grey50', linetype="dashed", size=1.5) + 
  scale_color_manual(values = my_colors) +
  facet_wrap(~BirdID, scales = "free") +
  scale_x_datetime(limits = ymd_hms(c("2021-07-23 21:10:00", "2021-07-24 02:45:00"))) +
  ylim(0,40) +
  theme(axis.text.x = element_text(size = 20), legend.key.height = unit(3, "line")) + 
  xlab("Time of night") + ylab(SurfTemp.lab)


## Just the ones we have MR data for
ggplot(agg_ir[agg_ir$BirdID==c("CAAN01", "CAAN02", "CAAN04", "CAAN07", "CAAN08", "CAAN09"),], aes(SameDate, Ts_max)) + 
  geom_point(aes(col=Category), size=3) + my_theme +
  geom_line(aes(y=Tamb), col='grey30', linetype="dotted", size=1.5) + 
  geom_line(aes(y=ChamberTemp_C), col='grey50', linetype="dashed", size=1.5) + 
  scale_color_manual(values = my_colors) +
  facet_wrap(~BirdID, scales = "free") +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-23 21:10:00", "2021-07-24 02:45:00"))) +
  ylim(0,40) +
  theme(axis.text.x = element_text(size = 20), legend.key.height = unit(3, "line")) + 
  xlab("Time of night") + ylab(SurfTemp.lab)


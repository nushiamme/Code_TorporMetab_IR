## Code to process thermal images from hummingbirds
## Field data Jun-Jul 2022
## Data collected by: Emily Blackwell, Shenni Liang, Sophia Wolfe, Santi
## Tabares ## Erices, Anusha Shankar
## Code started by: Anusha Shankar

## Import library for data analysis and plotting
library(here)
library(ggplot2)
library(stringr)
library(reshape2)
library(gridExtra)
library(dplyr) ## for renaming columns etc.
library(plyr)

## Read in Oregon 2022 data
ir_dat_caan <- read.csv(here::here("IR", "IR_data_CAAN2022.csv"))
## Read in SWRS 2022 data
ir_dat_swrs <- read.csv(here::here("IR", "IR_data_SWRS2022.csv"))
## List the interesting columns
interest_cols <- c("BirdID", "Run", "Day", "Month", "Year", "Time", "Ts_max",
                   "Teye", "Tamb", "Tc_thermocouple", "Reliable", "Category2")
## Subset the interesting columns from both dataframe
ir_dat_caan <- ir_dat_caan[interest_cols]
ir_dat_swrs <- ir_dat_swrs[interest_cols]
## concatenate the SWRS and Oregon data 
## (when we only want to look at either swrs or caan data, we can rename the dataframe)
ir_dat <- rbind(ir_dat_swrs, ir_dat_caan)

## Generic plot theme
my_theme <- theme_classic(base_size = 20) + theme(panel.border = element_rect(colour = "black", fill = NA))

my_theme_blank <- theme_classic(base_size = 30) +
theme(axis.title.y = element_text(vjust = 2), panel.border = element_blank())

my_colors <- c("#23988aff", "#440558ff", "#9ed93aff")

my_colors_four <- c("#23988aff", "#440558ff", "#9ed93aff", "darkgoldenrod2") #NTDA to match AZ birds

my_colors_five <- c("#23988aff", "#F38BA8", "#440558ff",  "#9ed93aff", "darkgoldenrod2") #NSTDA

names(my_colors_five) <- levels(c("Normothermic", "ShallowTorpor", "Transition", "DeepTorpor", "Arousal"))

colScale <- scale_colour_manual(name = "Category2", values = my_colors_five) # nolint

## Defining axis labels
Temp.lab <- expression(atop(paste("Temperature (", degree, "C)"))) # nolint
AmbTemp.lab <- expression(atop(paste("Ambient Temperature (", degree, "C)"))) # nolint
SurfTemp.lab <- expression(atop(paste("Max Surface Temperature (", degree, "C)"))) # nolint

## Label bird ID with run number if applicable
ir_dat$BirdID[!is.na(ir_dat$Run)] <- paste0(ir_dat$BirdID[!is.na(ir_dat$Run)], "_", ir_dat$Run[!is.na(ir_dat$Run)])

## Only include values where eye region is clearly visible
## sl984: I changed this from !"N" to "Y" since some values are NA
ir_dat <- ir_dat[ir_dat$Reliable == "Y", ]
ir_dat <- ir_dat[!is.na(ir_dat$Time), ]

## Processing time
ir_dat$Time <- str_pad(ir_dat$Time, width = 4, side = "left", pad = "0")
ir_dat$Hour <- substr(ir_dat$Time, 1, 2)
ir_dat$Minute <- substr(ir_dat$Time, 3, 4)
## Update the date for dawn data
ir_dat$Hour[ir_dat$Hour == 24] <- "00"
ir_dat$Day[ir_dat$Hour < 19] <- ir_dat$Day[ir_dat$Hour < 19] + 1
## Concatenate Time info
ir_dat$DateFormat <- as.POSIXct(paste(paste(ir_dat$Year, ir_dat$Month, ir_dat$Day, sep = "-"),
                                      paste(str_pad(ir_dat$Hour, width = 2, side = "left", pad = "0"),
                str_pad(ir_dat$Minute, width = 2, side = "left", pad = "0"), "00", sep = ":"), sep = " "),
                format = "%Y-%m-%d %H:%M")

ir_dat$DateLubri <- lubridate::ymd_hms(ir_dat$DateFormat)
## Order rows of ir_dat w/ Time value
ir_dat <- dplyr::arrange(ir_dat, DateLubri)

## Set everything to the same date
ir_dat$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"),
                                    paste(str_pad(ir_dat$Hour, width = 2, side = "left", pad = "0"),
              str_pad(ir_dat$Minute, width = 2, side = "left", pad = "0"), "00", sep = ":"), sep = " "),
              format = "%Y-%m-%d %H:%M")
## There are 86,400 seconds in a day
ir_dat$SameDate[ir_dat$Hour < 19] <- ir_dat$SameDate[ir_dat$Hour < 19] + 86400
ir_dat$SameDate <- lubridate::ymd_hms(ir_dat$SameDate)

## Make sure temperature columns are numeric
ir_dat$Ts_max <- as.numeric(ir_dat$Ts_max)
ir_dat$Teye <- as.numeric(ir_dat$Teye)
ir_dat$Tamb <- as.numeric(ir_dat$Tamb)
# 1 has NA Tamb value
ir_dat$Tc_thermocouple <- as.numeric(ir_dat$Tc_thermocouple)
# [1]    1    2    3  104  401  582  596  597  599  600  618  619  677  844 1007 1036 1254 1255 1256 1263
# [21] 1312 1316 1361 1393 1394 1395 1587 1635 1636 1637 has NA Tc_thermocouple value

ir_dat$Category2 <- as.factor(ir_dat$Category2)
## Replace letter with complete state name
levels(ir_dat$Category2) <- list(Normothermic = "N", Transition = "T", DeepTorpor = "D", Arousal = "A",
                                 ShallowTorpor = "S")

ir_dat$Category2 <- ordered(ir_dat$Category2, levels = c("Normothermic", "ShallowTorpor", "Transition",
                                                         "DeepTorpor", "Arousal"))

## Make the BirdIDs factor
ir_dat$BirdID <- as.factor(ir_dat$BirdID)

## Melt to make plotting all temp measurements together easier
m.ir_dat <- melt(ir_dat, id.vars = c("BirdID", "DateFormat"), measure.vars = c("Ts_max", "Teye", "Tamb")) # nolint
m.ir_dat <- dplyr::rename(m.ir_dat, Measure = variable, Temp = value) # nolint
m.ir_dat$Measure <- plyr::revalue(as.factor(m.ir_dat$Measure), # nolint
c(Ts_max = "Surface Temp", Teye= "Eye Temp", Tamb = "Ambient Temp")) # nolint
m.ir_dat$Temp <- as.numeric(m.ir_dat$Temp) # nolint

## Rplot01
ggplot(ir_dat[!is.na(ir_dat$Ts_max), ], aes(DateFormat, Ts_max)) + geom_point(aes(col = Category2)) +
  geom_line() +
  my_theme + facet_wrap(~BirdID, scale = "free_x") + colScale + 
  theme(axis.text.x = element_text(size =11), 
        legend.key.size = unit(1.0, 'cm')) + 
  labs(x = "Time")

#### Plots ####
bird1 <- ggplot(ir_dat[ir_dat$BirdID == "BCHU02_1",], aes(DateFormat, Ts_max)) +
  geom_point(aes(col = BirdID)) + my_theme +
  geom_line(aes(col = BirdID)) +
  # facet_grid(BirdID ~ ., scales = "free") + ylim(0, 40) +
  theme(axis.text.x = element_text(angle = 90, size = 10), axis.title.x = element_blank(),
        legend.position = "none") + labs(x = "Time")

## Rplot02
ggplot(ir_dat[ir_dat$BirdID %in% c("BCHU03_1", "RIHU11_1"), ], aes(DateFormat, Ts_max)) +
  geom_point(aes(col = BirdID)) + my_theme +
  geom_line(aes(col = BirdID)) +
  facet_grid(BirdID ~ ., scales = "free") + ylim(0, 40) +
  theme(axis.text.x = element_text(angle = 90, size = 10), axis.title.x = element_blank(),
        legend.position = "none")

bird2 <- ggplot(ir_dat[ir_dat$BirdID == "CAAN02", ], aes(DateFormat, Ts_max)) +
  geom_point(aes(col = BirdID)) + my_theme +
  # facet_grid(BirdID ~ ., scales = "free") + ylim(0, 40) +
  theme(axis.text.x = element_text(angle = 90, size = 10), axis.title.x = element_blank(),
        legend.position = "none")

bird3 <- ggplot(ir_dat[ir_dat$BirdID == "CAAN03", ], aes(DateFormat, Ts_max)) +
  geom_point(aes(col = BirdID)) + my_theme +
  facet_grid(BirdID ~ ., scales = "free") + ylim(0, 40) +
  theme(axis.text.x = element_text(angle = 90, size = 10),
        legend.position = "none")

## Rplot03
grid.arrange(bird1, bird2, bird3, ncol = 1, nrow = 3)

## Rplot04
ggplot(ir_dat[ir_dat$BirdID == "BTMG01_1", ], aes(DateFormat, Ts_max)) +  my_theme +
  geom_point(aes(col = BirdID)) + geom_line(aes(group = BirdID)) + geom_line(aes(y = Teye)) +
  geom_line(aes(y = Tamb), linetype = "dashed") + theme(legend.position = "none")

## Rplot05
ggplot(m.ir_dat[m.ir_dat$BirdID != "CAAN01", ], aes(DateFormat, Temp)) +  my_theme +
  geom_point(aes(col = Measure)) + geom_line(aes(group = Measure, col = Measure)) +
  facet_grid(BirdID ~ ., scales = "free") + xlab("Time") + ylab(Temp.lab) +
  scale_color_manual(values = c("magenta", "maroon", "grey30")) +
  theme(legend.key.height = unit(3, "lines"))

## Rplot06
ggplot(m.ir_dat[m.ir_dat$BirdID == "CAAN02", ], aes(DateFormat, Temp)) +  my_theme_blank +
  geom_point(aes(col = Measure)) + geom_line(aes(group = Measure, col = Measure)) +
  facet_grid(. ~ BirdID, scales = "free") +
  xlab("Time") + ylab(Temp.lab) +
  scale_color_manual(values = c("magenta", "maroon", "grey30")) +
  theme(legend.key.height = unit(3, "lines"))

## Rplot07
ggplot(ir_dat[ir_dat$BirdID == "CAAN08", ], aes(DateLubri, Ts_max)) +
    geom_point(aes(col = BirdID), size = 3) + my_theme +
  geom_line(aes(y = Tamb), linetype = "dashed") +
    facet_grid(BirdID ~ ., scales = "free") + ylim(0, 40) +
    theme(axis.text.x = element_text(size = 20),
          legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)

## Rplot08
## All individuals
ggplot(ir_dat, aes(DateFormat, Ts_max)) +
  #geom_point(aes(col=Category), size=3) +
  geom_point() + my_theme +
  geom_line(aes(y = Tamb), linetype = "dashed") +
  #scale_color_manual(values = my_colors) +
  facet_wrap(~BirdID, scales = "free") +
  ylim(0, 40) +
  theme(axis.text.x = element_text(size = 10),
        legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)

## Rplot09
## Standardize axes by using the same date for all individuals
ggplot(ir_dat, aes(SameDate, Ts_max)) +
  geom_point(aes(col = Category2), size = 1) + my_theme +
  geom_line(aes(y = Tamb), linetype = "dashed") +
  scale_color_manual(values = my_colors_five) +
  facet_wrap(~BirdID, scales = "free") +
  scale_x_datetime(limits = lubridate::ymd_hms(c("2021-07-23 21:10:00", "2021-07-24 02:45:00"))) +
  ylim(0, 40) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 8),
        legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)

## Rplot10
## Standardize x (time) axis by using the same date for all individuals
ggplot(ir_dat, aes(SameDate, Ts_max)) +
  geom_point(aes(col = Category2), size = 1) + my_theme +
  geom_line(aes(y = Tamb), col = "grey30", linetype = "dotted", linewidth = 1.5) +
  geom_line(aes(y = Tamb), col = "grey50", linetype = "dashed", linewidth = 1.5) +
  scale_color_manual(values = my_colors_five) +
  facet_wrap(~BirdID, scales = "free") +
  scale_x_datetime(limits = lubridate::ymd_hms(c("2021-07-23 21:10:00", "2021-07-24 02:45:00"))) +
  ylim(0, 40) +
  theme(axis.text.x = element_text(size = 10), axis.text.y = element_text(size = 8), 
        legend.key.height = unit(3, "line")) +
  xlab("Time of night") + ylab(SurfTemp.lab)

## Rplot11
## Just the ones we have MR data for
ggplot(ir_dat[ir_dat$BirdID ==
c("CAAN01", "CAAN02", "CAAN04", "CAAN07", "CAAN08", "CAAN09"), ], aes(SameDate, Ts_max)) +
  geom_point(aes(col = Category2), size = 3) + my_theme +
  geom_line(aes(y = Tamb), col = "grey30", linetype = "dotted", linewidth = 1.5) +
  geom_line(aes(y = ChamberTemp_C), col = "grey50", linetype = "dashed", linewidth = 1.5) +
  scale_color_manual(values = my_colors_five) +
  facet_wrap(~BirdID, scales = "free") +
  #scale_x_datetime(limits = lubridate::ymd_hms(c("2021-07-23 21:10:00", "2021-07-24 02:45:00"))) +
  ylim(0, 40) +
  theme(axis.text.x = element_text(size = 20), legend.key.height = unit(3, "line")) +
  xlab("Time of night") + ylab(SurfTemp.lab)

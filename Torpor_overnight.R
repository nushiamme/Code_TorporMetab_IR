## Putting all respirometry points on a plot together for nighttime MR measurements
## Code author: Anusha Shankar
## Data collected by Emily Blackwell, Anusha Shankar
## Collaboration with Don Powers

## Reading in packages
library(here)
library(ggplot2)
library(dplyr) ## To sum EE over one minute intervals, and for IR for renaming columns etc.

## For just MR data
library(caTools)
library(png)
library(gganimate)
library(janitor) ## To make a row into colnames
library(lubridate)
library(hms) ## To use as_hms function to extract just hms from a Lubridate DateTime var

##For IR data
library(stringr)
library(reshape2)
library(gridExtra)
library(plyr)


## Read in files
#MRsumm_1min <- read.csv(here::here("MR_summary_1min_EE_Tc.csv"))
#caan02 <- read.csv(here("MR", "CAAN02_0623_WholeNight_Analyzed.csv"))
paths <- dir(here::here("MR", "Multiple"), pattern = ".csv$")
names(paths) <- basename(paths)



## For IR
ir_dat <- read.csv(here::here("IR", "IR_data.csv"))
categories <- read.csv(here::here("IR", "Category_Entry.csv"))
categories$BirdID <- as.factor(categories$BirdID)

# Temps <- read.csv(here::here("IR", "Thermocouple_Temps.csv"))


## General theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 20) + 
  theme(panel.border = element_rect(color='black', size=0.5, fill=NA))

my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())

Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))
Tc_short.lab <- expression(atop(paste("Ta (", degree,"C)")))
SurfTemp_short.lab <- expression(atop(paste("Ts (", degree,"C)")))
SurfTemp.lab <- expression(atop(paste("Maximum Surface Temperature (", degree,"C)")))

# ##Fixed color scale for categories
# my_colors <- c("#23988aff", "#F38BA8", "#9ed93aff") #"#440558ff"
# names(my_colors) <- levels(c("Normothermic", "Transition", "DeepTorpor"))
# colScale <- scale_colour_manual(name = "Category", values = my_colors)

#### Ignore this section if reading in MRsumm_1min data frame ####

ThermFiles <- lapply(here::here("MR", "Multiple", paths), read.csv, header=F)


# ### Creating a summary data frame of 
# # Can also create automatic lists of summaries: lapply(ThermFiles_na_omit[[i]], summary)
# Thermsumm <- data.frame(matrix(NA, nrow=length(ThermFiles), ncol=9))
# names(Thermsumm) <- c("BirdID", "File", "Day", "Month", "Year", "Time_hours", "VO2_ml_min", "AmbientTemp_C", "ChamberTemp_C")
# Thermsumm$File <- noquote(names(paths))

ThermDat <- do.call(rbind.data.frame, ThermFiles)

ThermDat <- ThermDat %>%
  row_to_names(row_number = 1)
ThermDat <- ThermDat[ThermDat$BirdID != "BirdID",]
ThermDat$VO2_ml_min <- as.numeric(ThermDat$VO2_ml_min)
ThermDat <- ThermDat[complete.cases(ThermDat[,"VO2_ml_min"]),]

## Formatting date and time
ThermDat$StartDateFormat <- as.POSIXct(paste(paste(ThermDat$Year, ThermDat$Month, ThermDat$Day, sep = "-"), "00:00:00", sep = " "),
                                     format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")


ThermDat$DateTime <- ThermDat$StartDateFormat + (3600*as.numeric(ThermDat$Time_hours))
ThermDat$DateLubri <- lubridate::ymd_hms(ThermDat$DateTime)
ThermDat <- dplyr::arrange(ThermDat, DateLubri)

## Merging categories csv and ThermDat data frame to make sure transition times and categories are identified
ThermDat$BirdID <- as.factor(ThermDat$BirdID)
ThermDat <- merge(ThermDat, categories, "BirdID")
ThermDat$Category <- factor(ThermDat$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))


# ##Fixed color scale for categories
my_colors <- c("#23988aff", "#F38BA8", "#9ed93aff") #"#440558ff"
names(my_colors) <- levels(ThermDat$Category)
colScale <- scale_colour_manual(name = "Category", values = my_colors)

#categ_func(ThermDat)



## First 2 hours get an RER of 21.16, next hours get RER of 19.67
## Lighton equation: 16 + 5.164*RER	
## So for RER of 1 (carbs), 16 + 5.164*1 = 21.16
## For RER of 0.71 (protein/fat), 16 + 5.164*0.71 = 19.67
## TAKES A MINUTE or two TO RUN
MRsumm <- data.frame()
for(n in unique(ThermDat$BirdID)) {
  dat1 <- ThermDat[ThermDat$BirdID==n,]
  for(i in 1:length(dat1$VO2_ml_min)) {
    if(dat1$DateLubri[i] < (min(dat1$DateLubri)+7200)) { ## 2 hours in seconds is 2*60*60 = 7200
      dat1$EE_J[i] <- dat1$VO2_ml_min[i]*21.16/1000
    } else {
      dat1$EE_J[i] <- dat1$VO2_ml_min[i]*19.67/1000
    }
  }
  MRsumm <- rbind(MRsumm, dat1)
}

MRsumm_safe <- MRsumm


head(MRsumm)
MRsumm$ChamberTemp_C <- as.numeric(MRsumm$ChamberTemp_C)
MRsumm$AmbientTemp_C <- as.numeric(MRsumm$AmbientTemp_C)

MRsumm$TransitionTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                          paste(str_pad(substr(MRsumm$Time_transitionStart, 1, 2), width=2, side="left", pad="0"), 
                                                str_pad(substr(MRsumm$Time_transitionStart, 3, 4), width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                                    format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")

MRsumm$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                    as_hms(MRsumm$DateLubri), sep=" "),
                              format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
MRsumm$SameDate[MRsumm$Hour<19] <- MRsumm$SameDate[MRsumm$Hour<19]+86400
MRsumm$SameDate <- lubridate::ymd_hms(MRsumm$SameDate, tz = "America/Los_Angeles")


## Summarize by second
MRsumm_1sec <- as.data.frame(MRsumm %>%
                                 select(DateTime, BirdID, Category, TransitionTime, EE_J) %>%
                                 group_by(BirdID, TransitionTime, Category, DateLubri = cut(DateTime, breaks="1 sec")) %>%
                                 dplyr::summarize(EE_J = mean(EE_J)) %>%
                                 ungroup())

MRsumm_1sec$DateLubri <- lubridate::ymd_hms(MRsumm_1sec$DateLubri)
MRsumm_1sec$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                           as_hms(ymd_hms(MRsumm_1sec$DateLubri)), sep=" "),
                                     format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
MRsumm_1sec$SameDate[MRsumm_1sec$SameDate<"2021-07-23 19:00:00"] <- MRsumm_1sec$SameDate[MRsumm_1sec$SameDate<"2021-07-23 19:00:00"]+86400
MRsumm_1sec$DateLubri <- lubridate::ymd_hms(MRsumm_1sec$SameDate, tz = "America/Los_Angeles")

MRsumm_1sec$EE_J_sec <- MRsumm_1sec$EE_J*4

#mMRsumm_1sec <- merge(MRsumm_1sec, Categories)
#MRsumm_1sec$TransitionTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
 #                                           paste(str_pad(substr(MRsumm_1sec$Time_transitionStart, 1, 2), width=2, side="left", pad="0"), 
  #                                                str_pad(substr(MRsumm_1sec$Time_transitionStart, 3, 4), width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
   #                                   format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")


## Summarize by minute
MRsumm_1min_forMerge <- as.data.frame(MRsumm %>%
                                 select(DateTime, BirdID, Category, ChamberTemp_C, EE_J) %>%
                                 group_by(BirdID, Category, DateLubri = cut(DateTime, breaks="1 min")) %>%
                                 dplyr::summarize(across(c("EE_J", "ChamberTemp_C"), ~ mean(.x, na.rm = TRUE))) %>%
                                 ungroup())

EE_1min_forMerge <- as.data.frame(MRsumm %>%
                select(DateTime, BirdID, Category, EE_J) %>%
                group_by(BirdID, Category, DateLubri = cut(DateTime, breaks="1 min")) %>%
                dplyr::summarize(EE_J_min=sum(EE_J)) %>%
                ungroup())

Tc_1min <- as.data.frame(MRsumm %>%
                           select(DateTime, BirdID, Category, ChamberTemp_C) %>%
                           group_by(BirdID, Category, DateLubri = cut(DateTime, breaks="1 min")) %>%
                           dplyr::summarize(ChamberTemp_C=mean(ChamberTemp_C)) %>%
                           ungroup())

ir_forMerge <- as.data.frame(ir_dat %>%
                           select(DateTime, BirdID, Category, ChamberTemp_C) %>%
                           group_by(BirdID, Category, DateLubri = cut(DateTime, breaks="1 min")) %>%
                           dplyr::summarize(ChamberTemp_C=mean(ChamberTemp_C)) %>%
                           ungroup())

Merged_1min <- merge(EE_1min_forMerge, Tc_1min)

MRsumm_1min <- MRsumm_1min_forMerge
MRsumm_1min$DateLubri <- lubridate::ymd_hms(MRsumm_1min$DateLubri)
MRsumm_1min$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                      as_hms(ymd_hms(MRsumm_1min$DateLubri)), sep=" "),
                                format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
MRsumm_1min$SameDate[MRsumm_1min$SameDate<"2021-07-23 19:00:00"] <- MRsumm_1min$SameDate[MRsumm_1min$SameDate<"2021-07-23 19:00:00"]+86400
MRsumm_1min$DateLubri <- lubridate::ymd_hms(MRsumm_1min$SameDate, tz = "America/Los_Angeles")

MRsumm_1min$EE_J_min <- MRsumm_1min$EE_J*60*4

write.csv(x = MRsumm_1min, file = here::here("MR_summary_1min_EE_Tc.csv"))

# 
# vars_keep <- names(ThermDat) %in% c("DateLubri", "EE_J")
# ThermDat_sub <- ThermDat[vars_keep]
# 
# 
# vars_keep <- names(MRsumm) %in% c("BirdID", "DateLubri", "EE_J", "Category")
# MRsumm_sub <- MRsumm[vars_keep]

# ## Extracting Tc and Ta values for IR data
# Temps <- MRsumm %>% 
#   select(c(DateLubri, ChamberTemp_C, AmbientTemp_C))
# write.csv(Temps, "IR//Thermocouple_Temps.csv")


#### IR data ####
## Subset out only good runs
ir_dat <- ir_dat[ir_dat$Run=="Y",]
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
                                format='%Y-%m-%d %H:%M', tz = "America/Los_Angeles")

ir_dat$DateLubri <- lubridate::ymd_hms(ir_dat$DateFormat)
ir_dat <- dplyr::arrange(ir_dat, DateLubri)

ir_dat$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                    paste(str_pad(ir_dat$Hour, width=2, side="left", pad="0"), 
                                          str_pad(ir_dat$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                              format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")
ir_dat$SameDate[ir_dat$Hour<19] <- ir_dat$SameDate[ir_dat$Hour<19]+86400
ir_dat$SameDate <- lubridate::ymd_hms(ir_dat$SameDate, tz = "America/Los_Angeles")

## Fill in Categories
# ir_dat$Category <- NA
ir_dat$BirdID <- as.factor(ir_dat$BirdID)
ir_dat <- merge(ir_dat, categories, "BirdID")

ir_dat$Ts_max <- as.numeric(ir_dat$Ts_max)
ir_dat$Category <- factor(ir_dat$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))

ir_dat$TransiHour<- str_pad(substr(ir_dat$Time_transitionStart, 1, 2), width=2, side="left", pad="0")
ir_dat$TransiHour[ir_dat$TransiHour==24] <- "00"
ir_dat$TransiMin<- str_pad(substr(ir_dat$Time_transitionStart, 3, 4), width=2, side="left", pad="0")
ir_dat$TransitionTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                          paste(ir_dat$TransiHour, 
                                                ir_dat$TransiMin, "00", sep = ":"), sep=" "),
                                    format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")

#categ_func(ir_dat)

## Add Tc and Ta column from thermocouple data into ir_dat
# #Temps$DateLubri <- lubridate::ymd_hms(Temps$DateLubri, tz = "America/Los_Angeles")
## Average EE for every minute
MR_ToMerge_1min <- as.data.frame(MRsumm %>%
  group_by(DateLubri = cut(DateLubri, breaks="1 min"), BirdID) %>%
  dplyr::summarize(EE_J = sum(EE_J)) %>%
  ungroup())

IR_ToMerge <- as.data.frame(ir_dat %>%
                              group_by(DateLubri = cut(DateLubri, breaks="1 min"), BirdID) %>%
                              dplyr::summarize(Ts_max = mean(Ts_max)) %>%
                              ungroup())

agg_ir_mr <- merge(IR_ToMerge, MR_ToMerge_1min,  by=c("DateLubri", "BirdID"))
m.agg <- merge(agg_ir_mr, categories)
m.agg$Category <- factor(m.agg$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))

ggplot(m.agg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category)) + my_theme + facet_wrap(.~BirdID)  +
  colScale


ggplot(m.agg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category), size=2) + my_theme + #facet_wrap(.~BirdID)  +
  colScale


# agg_ir$AmbientTemp_C <- as.numeric(agg_ir$Ta)
# agg_ir$ChamberTemp_C <- as.numeric(agg_ir$Tc)


# ## Melting to make plotting all temp measurements together easier
# m.ir_dat <- melt(ir_dat, id.vars = c("BirdID", "DateFormat"), measure.vars = c("Ts_max", "Teye", "Tamb"))
# m.ir_dat <- dplyr::rename(m.ir_dat, Measure = variable, Temp = value)
# m.ir_dat$Measure <- plyr::revalue(as.factor(m.ir_dat$Measure), c(Ts_max = "Surface Temp", Teye= "Eye Temp", Tamb = "Ambient Temp"))

ggplot(ir_dat, aes(SameDate, Ts_max))

ir_dat$Tamb_chr <- as.character(ir_dat$Tamb) 
p <- ggplot(data=ir_dat, mapping=aes(SameDate, Ts_max)) + my_theme + facet_wrap(.~BirdID) +
  geom_point(aes(col=Category)) + geom_line(aes(group=BirdID, col=Category))

p

ggplot(ir_dat, aes(SameDate, Ts_max)) +
  geom_point(aes(col=Tamb)) + 
  my_theme + 
  facet_grid(Category~., scales = "free_x") +
  scale_color_gradient(low="blue", high="red") +
  geom_hline(yintercept = 33, linetype="dotted") + 
  ylab(SurfTemp_short.lab)


ggplot(ir_dat, aes(SameDate, Ts_max)) + geom_point(aes(col=Category)) + my_theme + facet_wrap(~BirdID, scales = "free_x") +
  geom_line(aes(SameDate, Tamb)) +
  colScale + geom_hline(yintercept = 30, linetype="dotted") + ylab(Temp.lab)

MRsumm_1min$BirdID <- as.factor(MRsumm_1min$BirdID)
MRsumm_1min$DateLubri <- lubridate::ymd_hms(MRsumm_1min$DateLubri)
MRsumm_1min$SameDate <- as.POSIXct(MRsumm_1min$SameDate)


# ## Making a "same date" column to align all times on x-axis
# MRsumm$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
#                                       as_hms(ymd_hms(MRsumm$DateLubri)), sep=" "),
#                               format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
# MRsumm$SameDate[MRsumm$SameDate<"2021-07-23 19:00:00"] <- MRsumm$SameDate[MRsumm$SameDate<"2021-07-23 19:00:00"]+86400
# MRsumm$DateLubri <- lubridate::ymd_hms(MRsumm$SameDate, tz = "America/Los_Angeles")
#summary(MRsumm$SameDate)

# agg.ThermDat <- aggregate(. ~ cut(ThermDat_sub$DateLubri, "1 min"), 
#                         ThermDat_sub[setdiff(names(ThermDat_sub), "date")], 
#                         sum)
# names(agg.ThermDat) <- c("Date", "VO2_ml_min")
# agg.ThermDat$Date <- as.Date(agg.ThermDat$Date, "%Y-%m-%d %H:%M:%S")

# ## Average VO2 for every second, rather than quarter second
# for(n in unique(MRsumm$BirdID)) {
#   dat1 <- ThermDat[MRsumm$BirdID==n,]
#   seq_avg <- seq(1, length(MRsumm$EE_J), 4)
#   EE_J_per_sec <- sapply(seq_avg, function(i) {mean(MRsumm$EE_J[i:(i+4)])})
#   EE_J_toPlot <- as.data.frame(cbind(EE_J_per_sec, seq(1,length(EE_J_per_sec),1)))
#   names(EE_J_toPlot) <- c("EE_J_per_sec", "SampleNo")
# }


# ## Average VO2 for every minute, rather than quarter second
# seq_avg_min <- seq(1, length(MRsumm$EE_J), 240)
# EE_J_per_min <- sapply(seq_avg, function(i) {mean(MRsumm$EE_J[i:(i+240)])})
# EE_J_min_toPlot <- as.data.frame(cbind(EE_J_per_sec, seq(1,length(EE_J_per_sec),1)))
# names(EE_J_min_toPlot) <- c("EE_J_per_min", "SampleNo")

# 
# # d2 <- data.frame(DateLubri = seq(as.POSIXct(min(MRsumm_sub$DateLubri)), as.POSIXct((max(MRsumm_sub$DateLubri))), 
# #                                   by="1 min"))
# MR_all<- MRsumm_sub %>%
#   mutate(DateLubri = floor_date(DateLubri, "1 minute")) %>%
#   group_by(DateLubri, BirdID) %>%
#   summarise(EE_J_min = mean(EE_J))
# 
# 
# 
# ThermDat_sub %>% 
#   mutate(interval = floor_date(DateLubri, unit="min")) %>% 
#   group_by(interval) %>% 
#   mutate(EE_J_min=mean(EE_J))  %>% 
#   select(interval,EE_J_min) 


## Plot
ggplot(EE_J_toPlot, aes(x=SampleNo, y=EE_J_per_sec)) +
  geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
  my_theme_blank + #colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_continuous(breaks= seq(0,9500,3600)) +
  #scale_y_continuous(breaks= seq(0,50,10)) +
  #xlab("Seconds") + 
  ylab("Energy expended (J) per second")


## Plot per min EE (J) values rather than per second
ggplot(EE_J_min_toPlot, aes(x=SampleNo, y=EE_J_per_min)) +
  geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
  my_theme_blank + #colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_continuous(breaks= seq(0,9500,3600)) +
  #scale_y_continuous(breaks= seq(0,50,10)) +
  #xlab("Seconds") + 
  ylab("Energy expended (J) per minute")

ggplot(ThermDat, aes(x=DateLubri, y=EE_J)) +
  geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
  my_theme + #colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")

### ALL THE BIRDS!!!
ggplot(MRsumm, aes(x=DateLubri, y=EE_J)) + facet_wrap(~BirdID, scales="free") +
  geom_point(alpha=0.8, col='grey90') + geom_smooth(aes(col=Category)) + 
  my_theme + scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")

## All birds, 5 min intervals
ggplot(MRsumm_1min, aes(x=DateLubri, y=EExp_J)) + facet_wrap(~BirdID, scales="free") +
  geom_point(alpha=0.8, col='grey90') + geom_smooth(aes(col=Category)) + 
  my_theme + scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")

### ALL THE BIRDS!!!
ggplot(NULL, aes(x=DateLubri, y=EE_J)) + facet_wrap(~BirdID, scales="free") +
  scale_y_continuous(name = "Max Ts",sec.axis=sec_axis(trans=~.*1000,name='MR (Watts)'))+
  geom_point(data=MRsumm, alpha=0.8, col='grey90') + geom_smooth(data=MRsumm, aes(col=Category)) + 
  geom_smooth(data=ir_dat[ir_dat$BirdID %in% MRsumm$BirdID,], aes(x=DateLubri, y=Ts_max)) +
  my_theme + scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")


### ALL THE BIRDS!!!
ggplot(NULL, aes(x=DateLubri, y=Ts_max)) + facet_wrap(~BirdID, scales="free") +
  scale_y_continuous(name = "Max Ts",sec.axis=sec_axis(trans=~.*2,name='MR (Watts*1000)'))+
  geom_smooth(data=ir_dat, aes(x=DateLubri, y=Ts_max)) +
  geom_point(data=MRsumm, alpha=0.8, col='grey90', aes(y=EE_J*500)) + 
  geom_smooth(data=MRsumm, aes(col=Category, y=EE_J*500)) +
  my_theme + colScale +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")


### ALL THE BIRDS, MR and IR, 1 min average for MR
ggplot(NULL, aes(x=SameDate, y=Ts_max)) + facet_wrap(~BirdID, scales="free") +
  scale_y_continuous(name = "Max Ts",limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  geom_smooth(data=ir_dat[!is.na(ir_dat$Ts_max),], aes(col=Category)) +
  geom_point(data=ir_dat[!is.na(ir_dat$Ts_max),], col="black", size=0.5) +
  geom_line(data=ir_dat[!is.na(ir_dat$Tamb),], aes(SameDate, y=Tamb), linetype="dotted", col="gray") +
  geom_point(data=MRsumm_1min, alpha=0.8, col='grey90', aes(y=EE_J_min*600)) + 
  geom_smooth(data=MRsumm_1min, aes(col=Category, y=EE_J_min*600)) +
  my_theme + scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=10),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")


### ALL THE BIRDS, minute averages
ggplot(MRsumm_sub, aes(x=DateLubri, y=EE_J)) + facet_wrap(~BirdID, scales="free") + 
  geom_point(alpha=0.8, col='grey90') + geom_smooth(aes(col=Category)) + 
  my_theme + scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")



## Loop to make stacked Ts - MR - Ta plots for all individuals
#plot_list <- list()

plot_data_list <- function (bird, data, data2) {
  subdataIR <- subset(data,data$BirdID == bird)
  subdataMR <- subset(data2,data2$BirdID == bird)
  minTime <- min(c(subdataIR$SameDate, subdataMR$SameDate))
  maxTime <- max(c(subdataIR$SameDate, subdataMR$SameDate))
  
  Ts_plot <- ggplot(subdataIR[subdataIR$BirdID==bird,], aes(SameDate, Ts_max)) + 
    geom_point(aes(col=Category), size=2) + my_theme2 +
    geom_line(aes(col=Category, group=BirdID)) +
    geom_hline(yintercept = 33, linetype="dotted") +
    scale_x_datetime(limits = c(minTime, maxTime)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(15,40)) +
    colScale + theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=10),
          axis.title.x = element_blank(), axis.text.x = element_blank()) + 
    xlab("Time of night") + ylab(SurfTemp_short.lab) + ggtitle(bird)
  
  ## Just MR, narrow y-axis
  mr_plot <- ggplot(subdataMR[subdataMR$BirdID==bird,], aes(x=SameDate, y=EE_J)) +
    geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
    my_theme2 + #colScale + 
    theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
          axis.text.y = element_text(size=10)) +
    ylim(-0.01,0.1) + geom_hline(yintercept = 0, linetype="dotted") +
    scale_x_datetime(limits = c(minTime, maxTime)) +
    ylab("MR (W) \n") + xlab("Time of night") 
  
  ## Just amb temp
  amb_plot <- ggplot(subdataMR[subdataMR$BirdID==bird,], aes(SameDate, as.numeric(ChamberTemp_C))) + 
    my_theme2 + 
    geom_line(linetype="dashed") + 
    #ylim(0,40) +
    scale_x_datetime(limits = c(minTime, maxTime)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 1),limits=c(13,28)) +
    theme(legend.position = "none",axis.text.x = element_text(size=10)) +
    xlab("Time of night") + ylab(Tc_short.lab)
  
  ## Stack all three
  #grid.arrange(Ts_plot, mr_plot, amb_plot, nrow=3)
  ggsave(arrangeGrob(Ts_plot, mr_plot, amb_plot, nrow=3),
         file=paste0("MR_IR_plot_",bird, ".png"), width = 7.26, height = 7.26, units = "in")
}

ir_sub <- ir_dat[ir_dat$BirdID %in% c(as.character(unique(MRsumm$BirdID))),]
ir_sub$BirdID <- droplevels(ir_sub$BirdID)
birds <- unique(MRsumm[,"BirdID"])
lapply(X = birds, FUN = plot_data_list, data = ir_sub, data2 = MRsumm)

### TO DOO ###
## Make subsetted dataset for just an hour before and after transition
# and color lines after transition by a different color than normo?
transitionTimeFunc <- function (bird, data, data2) {
  subdataIR <- subset(data,data$BirdID == bird)
  StartTime <- subdataIR$TransitionTime - 60*60
  EndTime <- subdataIR$TransitionTime + 60*60
  subdataIR <- subdataIR[subdataIR$SameDate >=StartTime & subdataIR$SameDate <= EndTime,]
  
  subdataMR <- subset(data2,data2$BirdID == bird)
  subdataMR <- subdataMR[subdataMR$SameDate >=StartTime & subdataMR$SameDate <= EndTime,]

  categ_col <- NA
  if (unique(subdataIR$Category)=="Transition") {
    categ_col <- "#F38BA8"
  } else if (unique(subdataIR$Category)=="DeepTorpor") {
    categ_col <- "#9ed93aff"
  }
  
  all_plot <- ggplot(NULL, aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID) +
    scale_y_continuous(name = SurfTemp_short.lab, limits=c(-0.1,43), sec.axis=sec_axis(trans=~./10,name='MR (W)'))+
    geom_line(data=subdataIR[!is.na(subdataIR$Ts_max),], aes(x=SameDate, y=Ts_max), col='blue',se = F, method="loess") +
    geom_point(data=subdataIR[!is.na(subdataIR$Ts_max),], aes(x=SameDate, y=Ts_max), col="blue", size=0.5) +
    geom_line(data=subdataIR[!is.na(subdataIR$Tamb),], aes(x=SameDate, y=Tamb), linetype="dotted", col="blue", alpha=0.8) +
    geom_point(data=subdataMR, alpha=0.8, col='grey90', aes(x=SameDate, y=EE_J_sec*100)) + 
    geom_line(data=subdataMR, aes(x=SameDate, y=EE_J_sec*100, col=Category)) +
    my_theme2 + colScale +
    geom_vline(xintercept = unique(subdataIR$TransitionTime)) +
    #scale_x_datetime(limits = c(StartTime, EndTime)) +
    theme(legend.position = "none",axis.text = element_text(size=15),
          strip.text = element_text(size = 15), plot.title = element_text(hjust=0.5),
          axis.line.y.left = element_line(colour = 'blue', size=0.5),
          axis.line.y.right = element_line(colour=categ_col, size=0.5),
          axis.title.y.left = element_text(colour = 'blue'),
          axis.title.y.right = element_text(colour=categ_col),
          axis.text.y.left = element_text(colour = 'blue'),
          #axis.text.y.right = element_text(colour = 'grey'),
          axis.text.y.right = element_text(colour=categ_col)) +
    xlab("Time of night") + ggtitle(bird)
  
  ggsave(all_plot,
         file=paste0("TransitionTime_",bird, ".tiff"), width = 7.26, height = 5, units = "in")
}

ir_sub <- ir_dat[ir_dat$BirdID %in% c(as.character(unique(MRsumm$BirdID))),]
ir_sub$BirdID <- droplevels(ir_sub$BirdID)
ir_sub <- ir_sub[!is.na(ir_sub$TransitionTime),]
MRsumm_sub <- MRsumm_1sec[!is.na(MRsumm_1sec$TransitionTime),]
birds <- unique(ir_sub[,"BirdID"])
lapply(X = birds, FUN = transitionTimeFunc, data = ir_sub, data2 = MRsumm_sub)

onebird <- "CAAN02"
subIR <- subset(ir_dat,ir_dat$BirdID == onebird)
StartTime <- unique(subIR$TransitionTime) - 60*60
EndTime <- unique(subIR$TransitionTime) + 60*60
subIR <- subIR[subIR$SameDate >=StartTime & subIR$SameDate <= EndTime,]

subMR <- subset(MRsumm_1sec,MRsumm_1sec$BirdID == onebird)
subMR <- subMR[subMR$SameDate >=StartTime & subMR$SameDate <= EndTime,]

categ_col <- NA
if(unique(subMR$Category)=="Normothermic") {
  categ_col <- "#23988aff"
} else if (unique(subMR$Category)=="Transition") {
  categ_col <- "#F38BA8"
} else if (unique(subMR$Category)=="DeepTorpor") {
  categ_col <- "#9ed93aff"
}

ggplot(NULL, aes(x=SameDate, y=EE_J_sec)) + #facet_wrap(~BirdID) +
  scale_y_continuous(name = 'MR (W)', sec.axis=sec_axis(trans=~.*100,name=SurfTemp_short.lab))+
  geom_point(data=subMR, alpha=0.8, col='grey90', aes(x=SameDate, y=EE_J_sec)) + 
  geom_line(data=subMR, aes(x=SameDate, y=EE_J_sec, col=Category)) +
  geom_smooth(data=subIR[!is.na(subIR$Ts_max),], aes(x=SameDate, y=Ts_max/100), col='blue',se =F) +
  geom_point(data=subIR[!is.na(subIR$Ts_max),], aes(x=SameDate, y=Ts_max/100), col="blue", size=0.5) +
  geom_line(data=subIR[!is.na(subIR$Tamb),], aes(x=SameDate, y=Tamb/100), linetype="dotted", col="blue", alpha=0.8) +
  my_theme2 + colScale +
  #scale_x_datetime(limits = c(StartTime, EndTime)) +
  theme(legend.position = "none",axis.text = element_text(size=15),
        strip.text = element_text(size = 15), plot.title = element_text(hjust=0.5),
        axis.line.y.right = element_line(colour = 'blue', size=0.5),
        axis.line.y.left = element_line(colour=categ_col, size=0.5),
        axis.title.y.right = element_text(colour = 'blue'),
        axis.title.y.left = element_text(colour=categ_col),
        axis.text.y.right = element_text(colour = 'blue'),
        #axis.text.y.right = element_text(colour = 'grey'),
        axis.text.y.left = element_text(colour=categ_col)) +
  xlab("Time of night") + ggtitle(onebird)

# ## Loop with MR averages over 5 min
# plot_data_list <- function (bird, data, data2) {
#   subdataIR <- subset(data,data$BirdID == bird)
#   subdataMR <- subset(data2,data2$BirdID == bird)
#   minTime <- min(c(subdataIR$SameDate, subdataMR$SameDate))
#   maxTime <- max(c(subdataIR$SameDate, subdataMR$SameDate))
#   
#   Ts_plot <- ggplot(subdataIR[subdataIR$BirdID==bird,], aes(SameDate, Ts_max)) + 
#     geom_point(aes(col=Category), size=2) + my_theme2 +
#     geom_line(aes(col=Category, group=BirdID)) +
#     geom_hline(yintercept = 33, linetype="dotted") +
#     scale_x_datetime(limits = c(minTime, maxTime)) +
#     scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(15,40)) +
#     colScale + theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=10),
#                      axis.title.x = element_blank(), axis.text.x = element_blank()) + 
#     xlab("Time of night") + ylab(SurfTemp_short.lab) + ggtitle(bird)
#   
#   ## Just MR, narrow y-axis
#   mr_plot <- ggplot(subdataMR[subdataMR$BirdID==bird,], aes(x=SameDate, y=EE_J)) +
#     geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
#     my_theme2 + #colScale + 
#     theme(axis.title.x = element_blank(), axis.text.x = element_blank(),
#           axis.text.y = element_text(size=10)) +
#     ylim(-0.01,0.1) + geom_hline(yintercept = 0, linetype="dotted") +
#     scale_x_datetime(limits = c(minTime, maxTime)) +
#     ylab("MR (J/5 min) \n") + xlab("Time of night") 
#   
#   ## Just amb temp
#   amb_plot <- ggplot(subdataMR[subdataMR$BirdID==bird,], aes(SameDate, as.numeric(ChamberTemp_C))) + 
#     my_theme2 + 
#     geom_line(linetype="dashed") + 
#     #ylim(0,40) +
#     scale_x_datetime(limits = c(minTime, maxTime)) +
#     scale_y_continuous(labels = scales::number_format(accuracy = 1),limits=c(13,28)) +
#     theme(legend.position = "none",axis.text.x = element_text(size=10)) +
#     xlab("Time of night") + ylab(Tc_short.lab)
#   
#   ## Stack all three
#   #grid.arrange(Ts_plot, mr_plot, amb_plot, nrow=3)
#   ggsave(arrangeGrob(Ts_plot, mr_plot, amb_plot, nrow=3),
#          file=paste0("1minMR_IR_plot_",bird, ".png"), width = 7.26, height = 7.26, units = "in")
# }
# 
# ir_sub <- ir_dat[ir_dat$BirdID %in% c(as.character(unique(MRsumm_1min$BirdID))),]
# ir_sub$BirdID <- droplevels(ir_sub$BirdID)
# birds <- unique(MRsumm_1min[,"BirdID"])
# lapply(X = birds, FUN = plot_data_list, data = ir_sub, data2 = MRsumm_1min)


## Trying to put MR and IR on same plot for all birds
plot_data_list <- function (bird, data, data2) {
  subdataIR <- subset(data,data$BirdID == bird)
  subdataMR <- subset(data2,data2$BirdID == bird)
  minTime <- min(c(subdataIR$SameDate, subdataMR$SameDate))
  maxTime <- max(c(subdataIR$SameDate, subdataMR$SameDate))
  categ_col <- NA
  if(unique(subdataMR$Category)=="Normothermic") {
    categ_col <- "#23988aff"
  } else if (unique(subdataMR$Category)=="Transition") {
    categ_col <- "#F38BA8"
  } else if (unique(subdataMR$Category)=="DeepTorpor") {
    categ_col <- "#9ed93aff"
  }
  all_plot <- ggplot(NULL, aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID) +
    scale_y_continuous(name = SurfTemp_short.lab,limits = c(-1,43), 
                       sec.axis=sec_axis(trans=~.*1,name='MR (J/min)'))+
    geom_line(data=subdataIR[!is.na(subdataIR$Ts_max),], 
              aes(x=SameDate, y=Ts_max), col='blue') + #,se = F
    geom_point(data=subdataIR[!is.na(subdataIR$Ts_max),], 
               aes(x=SameDate, y=Ts_max), col="blue", size=0.5) +
    geom_line(data=subdataIR[!is.na(subdataIR$Tamb),], 
              aes(SameDate, y=Tamb), linetype="dotted", col="blue", alpha=0.8) +
    #geom_point(data=subdataMR, alpha=0.8, col='grey90', aes(y=EE_J_min)) + 
    geom_line(data=subdataMR, aes(col=Category, y=EE_J_min)) +
    my_theme2 + colScale +
    geom_vline(xintercept = unique(subdataIR$TransitionTime), col="grey") +
    theme(legend.position = "none",axis.text = element_text(size=15),
          strip.text = element_text(size = 15), plot.title = element_text(hjust=0.5),
          axis.line.y.left = element_line(colour = 'blue', size=0.5),
          axis.line.y.right = element_line(colour=categ_col, size=0.5),
          axis.title.y.left = element_text(colour = 'blue'),
          axis.title.y.right = element_text(colour=categ_col),
          axis.text.y.left = element_text(colour = 'blue'),
          #axis.text.y.right = element_text(colour = 'grey'),
          axis.text.y.right = element_text(colour=categ_col)) +
    xlab("Time of night") + ggtitle(bird)
  
  ## Stack all three
  #grid.arrange(Ts_plot, mr_plot, amb_plot, nrow=3)
  ggsave(all_plot,
         file=paste0("Combined_MR_IR_plot_",bird, ".tiff"), width = 7.26, height = 5, units = "in")
}

ir_sub <- ir_dat[ir_dat$BirdID %in% c(as.character(unique(MRsumm_1min$BirdID))),]
ir_sub$BirdID <- droplevels(ir_sub$BirdID)
birds <- unique(MRsumm_1min[,"BirdID"])
lapply(X = birds, FUN = plot_data_list, data = ir_sub, data2 = MRsumm_1min)



## Just MR
## Trying to put MR and IR on same plot for all birds
plot_data_list <- function (bird, data, data2) {
  subdataMR <- subset(data,data$BirdID == bird)
  subdataIR <- subset(data2,data2$BirdID == bird)
  #minTime <- min(subdataMR$SameDate)
  #maxTime <- max(subdataMR$SameDate)
  categ_col <- NA
  if(unique(subdataMR$Category)=="Normothermic") {
    categ_col <- "#23988aff"
  } else if (unique(subdataMR$Category)=="Transition") {
    categ_col <- "#F38BA8"
  } else if (unique(subdataMR$Category)=="DeepTorpor") {
    categ_col <- "#9ed93aff"
  }
  all_plot <- ggplot(NULL, aes(x=SameDate, y=EE_J_min)) + #facet_wrap(~BirdID) +
    geom_point(data=subdataMR, alpha=0.8, col='grey90', aes(y=EE_J_min)) + 
    geom_line(data=subdataMR, aes(col=Category, y=EE_J_min)) +
    my_theme2 + colScale +
    geom_vline(xintercept = unique(subdataIR$TransitionTime), col="grey") +
    theme(legend.position = "none",axis.text = element_text(size=15),
          strip.text = element_text(size = 15), plot.title = element_text(hjust=0.5)) +
          ylab("Energy Expenditure (J/min)") +
    xlab("Time of night") + ggtitle(bird)
  
  ## Stack all three
  #grid.arrange(Ts_plot, mr_plot, amb_plot, nrow=3)
  ggsave(all_plot,
         file=paste0("MR_plot_",bird, ".tiff"), width = 7.26, height = 5, units = "in")
}

birds <- unique(MRsumm_1min[,"BirdID"])
lapply(X = birds, FUN = plot_data_list, data = MRsumm_1min, data2 = ir_sub)



### Just IR
## Just MR
## Trying to put MR and IR on same plot for all birds
plot_data_list <- function (bird, data, data2) {
  subdataMR <- subset(data,data$BirdID == bird)
  subdataIR <- subset(data2,data2$BirdID == bird)
  #minTime <- min(subdataMR$SameDate)
  #maxTime <- max(subdataMR$SameDate)
  categ_col <- NA
  if(unique(subdataMR$Category)=="Normothermic") {
    categ_col <- "#23988aff"
  } else if (unique(subdataMR$Category)=="Transition") {
    categ_col <- "#F38BA8"
  } else if (unique(subdataMR$Category)=="DeepTorpor") {
    categ_col <- "#9ed93aff"
  }
  all_plot <- ggplot(NULL, aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID) +
    geom_point(data=subdataIR, aes(col=Category), alpha=0.8, col='grey90') + 
    geom_line(data=subdataIR, aes(col=Category)) +
    my_theme2 + colScale +
    geom_vline(xintercept = unique(subdataIR$TransitionTime), col="grey") +
    theme(legend.position = "none",axis.text = element_text(size=15),
          strip.text = element_text(size = 15), plot.title = element_text(hjust=0.5)) +
    ylab("Energy Expenditure (J/min)") +
    xlab("Time of night") + ggtitle(bird)
  
  ## Stack all three
  #grid.arrange(Ts_plot, mr_plot, amb_plot, nrow=3)
  ggsave(all_plot,
         file=paste0("IR_plot_",bird, ".tiff"), width = 7.26, height = 5, units = "in")
}

birds <- unique(MRsumm_1min[,"BirdID"])
lapply(X = birds, FUN = plot_data_list, data = MRsumm_1min, data2 = ir_sub)


#  # Save plots to tiff. Makes a separate file for each plot.
# for (i in unique(MRsumm$BirdID)) {
#   file_name <- paste0("MR_IR_plot_", i, ".tiff")
#   tiff(file_name)
#   print(myplots[[i]])
#   dev.off()
# }

# # Another option: create pdf where each page is a separate plot.
# pdf("MR_IR_plots.pdf")
# for (i in 1:3) {
#   print(myplots[[i]])
# }
# dev.off()

onebird <- "CAAN02"
subIR <- subset(ir_dat,ir_dat$BirdID == onebird)
subMR <- subset(MRsumm_1min,MRsumm_1min$BirdID == onebird)
miniTime <- min(c(subIR$SameDate, subMR$SameDate))
maxiTime <- max(c(subIR$SameDate, subMR$SameDate))
categ_col <- NA
if(unique(subMR$Category)=="Normothermic") {
  categ_col <- "#23988aff"
} else if (unique(subMR$Category)=="Transition") {
  categ_col <- "#F38BA8"
} else if (unique(subMR$Category)=="DeepTorpor") {
  categ_col <- "#9ed93aff"
}


ggplot(NULL, aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID) +
  scale_y_continuous(name = SurfTemp_short.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  geom_smooth(data=subIR[!is.na(subIR$Ts_max),], aes(x=SameDate, y=Ts_max), col='blue',se = F) +
  geom_point(data=subIR[!is.na(subIR$Ts_max),], aes(x=SameDate, y=Ts_max), col="blue", size=0.5) +
  geom_line(data=subIR[!is.na(subIR$Tamb),], aes(SameDate, y=Tamb), linetype="dotted", col="blue", alpha=0.8) +
  geom_point(data=subMR, alpha=0.8, col='grey90', aes(y=EE_J_min*10)) + 
  geom_line(data=subMR, aes(col=Category, y=EE_J_min*10)) +
  my_theme2 + colScale +
  theme(legend.position = "none",axis.text = element_text(size=15),
        strip.text = element_text(size = 15), plot.title = element_text(hjust=0.5),
        axis.line.y.left = element_line(colour = 'blue', size=0.5),
        axis.line.y.right = element_line(colour=categ_col, size=0.5),
        axis.title.y.left = element_text(colour = 'blue'),
        axis.title.y.right = element_text(colour=categ_col),
        axis.text.y.left = element_text(colour = 'blue'),
        #axis.text.y.right = element_text(colour = 'grey'),
        axis.text.y.right = element_text(colour=categ_col)) +
  xlab("Time of night") + ggtitle(onebird)

### Just MR
ggplot(MRsumm_1sec[MRsumm_1sec$BirdID==onebird,], aes(x=DateLubri, y=EE_J)) +
  geom_point(alpha=0.8, col='grey90') + 
  geom_line(aes(col=Category)) +
  my_theme2 + colScale +
  theme(legend.position = "none",axis.text = element_text(size=15),
        strip.text = element_text(size = 15), plot.title = element_text(hjust=0.5),
        axis.line.y.left = element_line(colour = 'blue', size=0.5),
        axis.line.y.right = element_line(colour=categ_col, size=0.5),
        axis.title.y.left = element_text(colour = 'blue'),
        axis.title.y.right = element_text(colour=categ_col),
        axis.text.y.left = element_text(colour = 'blue'),
        #axis.text.y.right = element_text(colour = 'grey'),
        axis.text.y.right = element_text(colour=categ_col)) +
  xlab("Time of night") + ggtitle(onebird)


ggplot(ir_dat[ir_dat$BirdID==onebird,], aes(SameDate, Ts_max)) + 
  geom_point(aes(col=Category), size=2) + my_theme2 +
  geom_line(aes(col=Category, group=BirdID)) +
  geom_hline(yintercept = 33, linetype="dotted") +
  scale_x_datetime(limits = c(miniTime, maxiTime)) +
  scale_y_continuous(labels = scales::number_format(accuracy = 1),limits = c(15,40)) +
  colScale + theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=10)) + 
  xlab("Time of night") + ylab(SurfTemp_short.lab) + ggtitle(onebird)



## Plot
ggplot(EE_J_toPlot, aes(x=SampleNo, y=EE_J_per_sec)) +
  geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
  my_theme_blank + #colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_continuous(breaks= seq(0,9500,3600)) +
  #scale_y_continuous(breaks= seq(0,50,10)) +
  #xlab("Seconds") + 
  ylab("Energy expended (J) per second")

## Plot
ggplot(caan02, aes(x=DateTime, y=EE_J)) +
  geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
  my_theme_blank + #colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  # scale_x_date(breaks = function(x) seq.Date(from = min(x), 
  #                                            to = max(x), 
  #                                            by = "1 hour")) +
  # xlab("Seconds") + 
  ylab("Energy expended (J) per second")

# ## Just CAAN04
# ggplot(MRsumm[MRsumm$BirdID=="CAAN04",], aes(x=DateLubri, y=EE_J)) + #facet_wrap(~BirdID, scales="free") + 
#   geom_point(alpha=0.8, col='grey90') + geom_smooth(aes(col=Category)) + 
#   my_theme + 
#   scale_color_manual(values=my_colors) +
#   theme(axis.text.x = element_text(size=20),
#         legend.key.height=unit(3,"line"),
#         axis.line.x = element_line(colour = "grey50"),
#         axis.line.y = element_line(colour = "grey50")) +
#   #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
#   ylab("Energy expended (J) per second") + xlab("Time of night")

# #### From IR script, won't read here- temporary
# caan08_ir_plot <- ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateLubri, Ts_max)) + 
#   geom_point(aes(col=BirdID), size=3) + my_theme +
#   #scale_y_continuous(name = "Max Surf Temp", sec.axis = sec_axis( trans=~./100, name="EE (J)")) +
#   geom_line(aes(y=Tamb), linetype="dashed") + 
#   ylim(0,40) +
#   scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
#   theme(axis.text.x = element_text(size = 20),
#         legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)
# 
# ## Just MR, narrow y-axis
# caan08_mr_plot <- ggplot(MRsumm[MRsumm$BirdID=="CAAN08",], aes(x=DateLubri, y=EE_J*10)) +
#   geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
#   my_theme + #colScale + 
#   theme(axis.text.x = element_text(size=20),
#         legend.key.height=unit(3,"line"),
#         axis.line.x = element_line(colour = "grey50"),
#         axis.line.y = element_line(colour = "grey50")) +
#   scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
#   ylab("MR (J/s)") + xlab("Time of night")
# 
# 
# #### Just max Ts
# caan08_ir_Ts_plot <- ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateLubri, Ts_max)) + 
#   geom_point(aes(col=BirdID), size=3) + my_theme +
#   scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
#   theme(axis.text.x = element_text(size = 20),
#         legend.position = "none") + xlab("Time of night") + ylab("Max Ts")
# 
# ## Just amb temp
# caan08_ir_amb_plot <- ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateLubri, Ts_max)) + 
#   my_theme + 
#   geom_line(aes(y=Tamb), linetype="dashed") + 
#   #ylim(0,40) +
#   scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
#   theme(axis.text.x = element_text(size = 20),
#         legend.position = "none") + xlab("Time of night") + ylab("Ta")
# 
# ## Stack all three
# grid.arrange(caan08_ir_Ts_plot, caan08_mr_plot, caan08_ir_amb_plot, nrow=3)
# 


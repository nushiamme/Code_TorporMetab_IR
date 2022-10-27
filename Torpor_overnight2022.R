## Putting all respirometry points on a plot together for nighttime MR measurements
## Code author: Anusha Shankar
## Data collected by Emily Blackwell, Anusha Shankar
## Collaboration with Don Powers

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

## Packages for both
library(ggplot2)
library(dplyr) ## To sum EE over one minute intervals, and for IR for renaming columns etc.
library(here)
library(nlme) ## for gls model to compare them with lmer, and to account for autocorrelation
library(emmeans)
library(glue) ## kind of like paste, using it for time axis



## Read in files
#MRsumm_1min <- read.csv(here::here("MR_summary_1min_EE_Tc.csv"))
MRsumm <- read.csv(here::here("MR_summary_EE_Tc_AllAZbirds2022.csv"))
MRsumm$SameDate <- lubridate::ymd_hms(MRsumm$SameDate, tz = "America/Los_Angeles")
MRsumm$DateLubri <- lubridate::ymd_hms(MRsumm$DateLubri, tz = "America/Los_Angeles")
MRsumm$BirdID <- as.factor(MRsumm$BirdID)
#caan02 <- read.csv(here("MR", "CAAN02_0623_WholeNight_Analyzed.csv"))
paths <- dir(here::here("MR", "Multiple_AZ2022"), pattern = ".csv$")
names(paths) <- basename(paths)


## For IR
#ir_dat <- read.csv(here::here("IR", "IR_data_SWRS2022.csv"))
#categories <- read.csv(here::here("IR", "Category_Entry.csv"))
#categories$BirdID <- as.factor(categories$BirdID)

# Temps <- read.csv(here::here("IR", "Thermocouple_Temps.csv"))


## General theme
my_theme <- theme_classic(base_size = 30) + 
  theme(panel.border = element_rect(colour = "black", fill=NA))

my_theme2 <- theme_classic(base_size = 15) + 
  theme(panel.border = element_rect(color='black', size=0.5, fill=NA))

my_theme_doubleaxis <- theme_classic(base_size = 15) + 
  theme(axis.line.x = element_line(color='black', size=0.5), 
        axis.line.y.left = element_line(color='black', size=0.5), 
        axis.line.y.right = element_line(color="red"))

my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())

Temp.lab <- expression(atop(paste("Temperature (", degree,"C)")))
Tc_short.lab <- expression(atop(paste("Ta (", degree,"C)")))
SurfTemp_short.lab <- expression(atop(paste("Ts (", degree,"C)")))
SurfTemp.lab <- expression(atop(paste("Maximum Surface Temperature (", degree,"C)")))

##Fixed color scale for categories
#my_colors <- c("#F38BA8", "#9ed93aff", "#23988aff", "#440558ff") #ADNT
my_colors <- c("#23988aff", "#440558ff", "#9ed93aff", "darkgoldenrod2") #NTDA
my_colors_five <- c("#23988aff",  "#F38BA8", "#440558ff",  "#9ed93aff", "darkgoldenrod2") #NSTDA
my_colors_noArousal <- c("#23988aff",  "#F38BA8", "#440558ff",  "#9ed93aff") #NSTDA
my_colors_bchu01 <- c("#23988aff", "#440558ff",  "#9ed93aff") #NTD
#names(my_colors) <- c("Normothermic", "ShallowTorpor", "Transition", "DeepTorpor", "Arousal")
colScale <- scale_colour_manual(name = "Category", values = my_colors)
colScale_bchu01 <- scale_colour_manual(name = "Category", values = my_colors,
                                       labels=c("Normothermic", "Transition", "DeepTorpor"))

colScale_named <- scale_colour_manual(name = "Category", values = my_colors_five, 
                                labels=c("Normothermic", "ShallowTorpor", "Transition", "DeepTorpor", "Arousal"))
colScale_named_noShallow <- scale_colour_manual(name = "Category", values = my_colors, 
                                      labels=c("Normothermic", "Transition", "DeepTorpor", "Arousal"))
colScale_noArousal <- scale_colour_manual(name = "Category", values = my_colors_noArousal, 
                                          labels=c("Normothermic", "ShallowTorpor", "Transition", "DeepTorpor"))


#### Ignore this section if reading in MRsumm_1min data frame and/or MR_summ ####

ThermFiles <- lapply(here::here("MR", "Multiple_AZ2022", paths), read.csv, header=T)

for (i in 1:length(ThermFiles)) {
  #ThermFiles[[i]] <- ThermFiles[[i]] %>%
   # row_to_names(row_number = 1) %>%
    #clean_names()
  ThermFiles[[i]] <- ThermFiles[[i]][ThermFiles[[i]]$BirdID != "BirdID",]
  ThermFiles[[i]]$VO2_ml_min <- as.numeric(ThermFiles[[i]]$VO2_ml_min)
  ThermFiles[[i]] <- ThermFiles[[i]][complete.cases(ThermFiles[[i]][,"VO2_ml_min"]),]
  
  ThermFiles[[i]]$StartTime[ThermFiles[[i]]$StartTime==24] <- "00"
  ThermFiles[[i]]$StartTime <- as.numeric(ThermFiles[[i]]$StartTime)
  ThermFiles[[i]]$Day <- as.numeric(ThermFiles[[i]]$Day)
  ThermFiles[[i]]$Day[ThermFiles[[i]]$StartTime<19] <- ThermFiles[[i]]$Day[ThermFiles[[i]]$StartTime<19]+1
  
  ## Formatting date and time
  ThermFiles[[i]]$StartDateFormat <- as.POSIXct(paste(paste(ThermFiles[[i]]$Year, ThermFiles[[i]]$Month, ThermFiles[[i]]$Day, sep = "-"), 
                                               paste(str_pad(substr(ThermFiles[[i]]$StartTime, 1, 2), width=2, side="left", pad="0"), 
                                                     str_pad(substr(ThermFiles[[i]]$StartTime_min, 1, 2), width=2, side="left", pad="0"), "00", sep = ":"),
                                               sep = " "),
                                         format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
}
 
for (i in 1:length(ThermFiles)) {
  ThermFiles[[i]]$NewTime <- as.POSIXct(NA, tz="America/Los_Angeles")
  ThermFiles[[i]]$NewTime[1] <- as.POSIXct(ThermFiles[[i]]$StartDateFormat[1], tz="America/Los_Angeles")
  ThermFiles[[i]]$NewTime[2] <- .25 + ThermFiles[[i]]$NewTime[1]
  for (j in 3:nrow(ThermFiles[[i]])) {
    ThermFiles[[i]]$NewTime[j] <- .25 + ThermFiles[[i]]$NewTime[j-1]
  }
}




# ### Creating a summary data frame of 
# # Can also create automatic lists of summaries: lapply(ThermFiles_na_omit[[i]], summary)
# Thermsumm <- data.frame(matrix(NA, nrow=length(ThermFiles), ncol=9))
# names(Thermsumm) <- c("BirdID", "File", "Day", "Month", "Year", "Time_hours", "VO2_ml_min", "AmbientTemp_C", "ChamberTemp_C")
# Thermsumm$File <- noquote(names(paths))

# for(i in 1:length(ThermFiles)){
#   print (names(ThermFiles[[i]]))
# }

# Checking column names. Can change numbers to get different parts of data frame to check for consistency in naming
for(i in 1:length(ThermFiles)){
  print (ThermFiles[[i]][2,1])
}

ThermDat <- do.call(rbind.data.frame, ThermFiles)

# ThermDat <- ThermDat %>%
#   row_to_names(row_number = 1)
# ThermDat <- ThermDat[ThermDat$BirdID != "BirdID",]
# ThermDat$VO2_ml_min <- as.numeric(ThermDat$VO2_ml_min)
# ThermDat <- ThermDat[complete.cases(ThermDat[,"VO2_ml_min"]),]
# 
# ThermDat$StartTime[ThermDat$StartTime==24] <- "00"
# ThermDat$StartTime <- as.numeric(ThermDat$StartTime)
# ThermDat$Day <- as.numeric(ThermDat$Day)
# ThermDat$Day[ThermDat$StartTime<19] <- ThermDat$Day[ThermDat$StartTime<19]+1
# 
# 
# ## Formatting date and time
# ThermDat$StartDateFormat <- as.POSIXct(paste(paste(ThermDat$Year, ThermDat$Month, ThermDat$Day, sep = "-"), 
#                                              paste(str_pad(substr(ThermDat$StartTime, 1, 2), width=2, side="left", pad="0"), 
#                                                    str_pad(substr(ThermDat$StartTime_min, 1, 2), width=2, side="left", pad="0"), "00", sep = ":"),
#                                              sep = " "),
#                                      format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
# 

# ThermDat$NewTime <- as.POSIXct(NA)
# ThermDat$NewTime[1] <- as.POSIXct(ThermDat$StartDateFormat[1])
# for (i in 1:nrow(ThermDat)) {
#   ThermDat$NewTime[i] <- .25 + ThermDat$NewTime[i-1]
# }


# ThermDat$NewTime <- as.POSIXct(NA)
# ThermDat$NewTime[1] <- as.POSIXct(ThermDat$StartDateFormat[1])
# ThermDat <- ThermDat %>%
#   mutate(NewTime2 = .25 + lag(NewTime))

# ThermDat %>%
#   mutate(NewTime2 = 25 + lag(NewTime))
# 
# ThermDat$StartDateFormat2 <- ThermDat$StartDateFormat
# setDT(ThermDat)[, StartDateFormat2 := 25 + shift(StartDateFormat2, fill = 0)]
# ThermDat$DateTime <- 25 + shift(ThermDat$StartDateFormat, 1, type = "lag")
ThermDat$DateLubri <- lubridate::ymd_hms(ThermDat$NewTime)
ThermDat <- dplyr::arrange(ThermDat, DateLubri)

## Merging categories csv and ThermDat data frame to make sure transition times and categories are identified
ThermDat$BirdID <- as.factor(ThermDat$BirdID)
# ThermDat <- merge(ThermDat, categories, "BirdID")
# ThermDat$Category <- factor(ThermDat$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))


# ##Fixed color scale for categories
# my_colors <- c("#23988aff", "#440558ff", "#9ed93aff") #"#F38BA8"
# names(my_colors) <- levels(ThermDat$Category)
# colScale <- scale_colour_manual(name = "Category", values = my_colors)

#categ_func(ThermDat)



## First 2 hours get an RER of 21.16, next hours get RER of 19.67
## Lighton equation: 16 + 5.164*RER	
## So for RER of 1 (carbs), 16 + 5.164*1 = 21.16
## For RER of 0.71 (protein/fat), 16 + 5.164*0.71 = 19.67
## TAKES A FEW MINUTES TO RUN
# AVOID RUNNING IF UNNECESSARY
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
  #print(head(dat1))
  MRsumm <- rbind(MRsumm, dat1)
}

MRsumm_safe <- MRsumm

MRsumm <- MRsumm_safe


head(MRsumm)
MRsumm$ChamberTemp_C <- as.numeric(MRsumm$ChamberTemp_C)
MRsumm$AmbientTemp_C <- as.numeric(MRsumm$AmbientTemp_C)

NightID <- c(paste0(unique(MRsumm$BirdID), "_", "1"), "BTMG01_2")
#levels(MRsumm$BirdID)[length(levels(MRsumm$BirdID))+1] <- "BTMG01_1"
#levels(MRsumm$BirdID)[length(levels(MRsumm$BirdID))+1] <- "BTMG01_2"
levels(MRsumm$BirdID) <- c(levels(MRsumm$BirdID), NightID)
MRsumm$BirdID[MRsumm$BirdID=="BTMG01" & MRsumm$Day<11] <- "BTMG01_1"
MRsumm$BirdID[MRsumm$BirdID=="BTMG01" & MRsumm$Day>10] <- "BTMG01_2"
MRsumm$BirdID[!(MRsumm$BirdID %in% c("BTMG01_1", "BTMG01_2"))] <- paste0(MRsumm$BirdID[!(MRsumm$BirdID %in% c("BTMG01_1", "BTMG01_2"))], "_", "1")

# #MRsumm$TransitionTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
#                                           paste(str_pad(substr(MRsumm$Time_transitionStart, 1, 2), width=2, side="left", pad="0"), 
#                                                 str_pad(substr(MRsumm$Time_transitionStart, 3, 4), width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
#                                     format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")

MRsumm$SameDate <- as.POSIXct(paste(paste("2022", "7", "23", sep = "-"), 
                                    as_hms(MRsumm$DateLubri), sep=" "),
                              format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
MRsumm$SameDate[hour(MRsumm$DateLubri)<19] <- MRsumm$SameDate[hour(MRsumm$DateLubri)<19]+86400
MRsumm$SameDate <- lubridate::ymd_hms(MRsumm$SameDate, tz = "America/Los_Angeles")


write.csv(x = MRsumm, file = here::here("MR_summary_EE_Tc_AllAZbirds2022.csv"))

## Summarize by second
# MRsumm_1sec <- as.data.frame(MRsumm %>%
#                                  select(DateTime, BirdID, Category, TransitionTime, EE_J) %>%
#                                  group_by(BirdID, TransitionTime, Category, DateLubri = cut(DateTime, breaks="1 sec")) %>%
#                                  dplyr::summarize(EE_J = mean(EE_J)) %>%
#                                  ungroup())
# 
# MRsumm_1sec$DateLubri <- lubridate::ymd_hms(MRsumm_1sec$DateLubri)
# MRsumm_1sec$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
#                                            as_hms(ymd_hms(MRsumm_1sec$DateLubri)), sep=" "),
#                                      format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
# MRsumm_1sec$SameDate[MRsumm_1sec$SameDate<"2021-07-23 19:00:00"] <- MRsumm_1sec$SameDate[MRsumm_1sec$SameDate<"2021-07-23 19:00:00"]+86400
# MRsumm_1sec$DateLubri <- lubridate::ymd_hms(MRsumm_1sec$SameDate, tz = "America/Los_Angeles")
# 
# MRsumm_1sec$EE_J_sec <- MRsumm_1sec$EE_J*4

#mMRsumm_1sec <- merge(MRsumm_1sec, Categories)
#MRsumm_1sec$TransitionTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
 #                                           paste(str_pad(substr(MRsumm_1sec$Time_transitionStart, 1, 2), width=2, side="left", pad="0"), 
  #                                                str_pad(substr(MRsumm_1sec$Time_transitionStart, 3, 4), width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
   #                                   format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")


## Summarize by minute
# MRsumm_1min_forMerge <- as.data.frame(MRsumm %>%
#                                  select(DateLubri, BirdID, Category, ChamberTemp_C, EE_J) %>%
#                                  group_by(BirdID, Category, Date_min = cut(DateLubri, breaks="1 min")) %>%
#                                  dplyr::summarize(across(c("EE_J", "ChamberTemp_C"), ~ mean(.x, na.rm = TRUE))) %>%
#                                  ungroup())

# EE_1min_forMerge <- as.data.frame(MRsumm %>%
#                                         select(SameDate, EE_J) %>%
#                                         group_by(SameDate = cut(SameDate, breaks="1 min")) %>%
#                                         dplyr::summarize(EE_J_min = sum(EE_J, na.rm = TRUE)) %>%
#                                         ungroup())
# EE_1min_forMerge$SameDate <- lubridate::ymd_hms(EE_1min_forMerge$SameDate, tz = "America/Los_Angeles")
# 
# EE_1min_forMerge <- dplyr::arrange(EE_1min_forMerge, SameDate)

# EE_1min_forMerge <- as.data.frame(MRsumm %>%
#                 select(DateTime, BirdID, Category, EE_J) %>%
#                 group_by(BirdID, Category, DateLubri = cut(DateTime, breaks="1 min")) %>%
#                 dplyr::summarize(EE_J_min=sum(EE_J)) %>%
#                 ungroup())

# Tc_1min <- as.data.frame(MRsumm %>%
#                            select(DateTime, BirdID, Category, ChamberTemp_C) %>%
#                            group_by(BirdID, Category, DateLubri = cut(DateTime, breaks="1 min")) %>%
#                            dplyr::summarize(ChamberTemp_C=mean(ChamberTemp_C)) %>%
#                            ungroup())
# 
# ir_forMerge <- as.data.frame(ir_dat %>%
#                            select(DateTime, BirdID, Category, ChamberTemp_C) %>%
#                            group_by(BirdID, Category, DateLubri = cut(DateTime, breaks="1 min")) %>%
#                            dplyr::summarize(ChamberTemp_C=mean(ChamberTemp_C)) %>%
#                            ungroup())
# 
# Merged_1min <- merge(EE_1min_forMerge, Tc_1min)
# 
# MRsumm_1min <- MRsumm_1min_forMerge
# MRsumm_1min$DateLubri <- lubridate::ymd_hms(MRsumm_1min$DateLubri)
# MRsumm_1min$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
#                                       as_hms(ymd_hms(MRsumm_1min$DateLubri)), sep=" "),
#                                 format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
# MRsumm_1min$SameDate[MRsumm_1min$SameDate<"2022-07-23 19:00:00"] <- MRsumm_1min$SameDate[MRsumm_1min$SameDate<"2022-07-23 19:00:00"]+86400
# MRsumm_1min$DateLubri <- lubridate::ymd_hms(MRsumm_1min$SameDate, tz = "America/Los_Angeles")
# 
# MRsumm_1min$EE_J_min <- MRsumm_1min$EE_J*60*4
# 
# write.csv(x = MRsumm_1min, file = here::here("MR_summary_1min_EE_Tc.csv"))

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
## For IR - reading in - ignore if already done
ir_dat <- read.csv(here::here("IR", "IR_data_SWRS2022.csv"))
## Subset out only good runs
#ir_dat <- ir_dat[ir_dat$Run=="Y",]
##Only include values where eye region is clearly visible
ir_dat <- ir_dat[!(ir_dat$ReliableTry=="N"),]
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

ir_dat$SameDate <- as.POSIXct(paste(paste("2022", "7", "23", sep = "-"), 
                                    paste(str_pad(ir_dat$Hour, width=2, side="left", pad="0"), 
                                          str_pad(ir_dat$Minute, width=2, side="left", pad="0"), "00", sep = ":"), sep=" "),
                              format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")

ir_dat$SameDate[ir_dat$Hour<19] <- ir_dat$SameDate[ir_dat$Hour<19]+86400
ir_dat$SameDate <- lubridate::ymd_hms(ir_dat$SameDate, tz = "America/Los_Angeles")
ir_dat$Tamb <- as.numeric(ir_dat$Tamb)
ir_dat$Teye <- as.numeric(ir_dat$Teye)


## Fill in Categories
#ir_dat$Category <- NA
#ir_dat$BirdID <- as.factor(ir_dat$BirdID)
#ir_dat <- merge(ir_dat, categories, "BirdID")

ir_dat$NightID <- as.factor(ir_dat$NightID)

ir_dat$Ts_max <- as.numeric(ir_dat$Ts_max)
ir_dat$Category <- as.factor(ir_dat$Category) #, levels=c("Normothermic", "Transition", "DeepTorpor", "Arousal"))

ir_dat$Category <- recode_factor(ir_dat$Category, Normothermic = "N", 
                                 ShallowTorpor = "S", Transition = "T", DeepTorpor = "D", Arousal = "A")
#ir_dat$Category <- factor(ir_dat$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))

rihu07 <- ir_dat[ir_dat$BirdID=="RIHU07",]
bchu02 <- ir_dat[ir_dat$BirdID=="BCHU02",]
rihu10 <- ir_dat[ir_dat$BirdID=="RIHU10",]
btmg01 <- ir_dat[ir_dat$NightID=="BTMG01_1",]

# ir_dat$TransiHour<- str_pad(substr(ir_dat$Time_TransitionStart, 1, 2), width=2, side="left", pad="0")
# ir_dat$TransiHour[ir_dat$TransiHour==24] <- "00"
# ir_dat$TransiMin<- str_pad(substr(ir_dat$Time_TransitionStart, 3, 4), width=2, side="left", pad="0")
# ir_dat$TransitionTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
#                                           paste(ir_dat$TransiHour, 
#                                                 ir_dat$TransiMin, "00", sep = ":"), sep=" "),
#                                     format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")
# 
#categ_func(ir_dat)

## Add Tc and Ta column from thermocouple data into ir_dat
# #Temps$DateLubri <- lubridate::ymd_hms(Temps$DateLubri, tz = "America/Los_Angeles")
## Average EE for every minute
MR_ToMerge_1min <- as.data.frame(MRsumm %>%
  group_by(SameDate = cut(SameDate, breaks="1 min"), BirdID) %>%
  dplyr::summarize(EE_J = sum(EE_J)) %>%
  ungroup())
MR_ToMerge_1min$SameDate <- lubridate::ymd_hms(MR_ToMerge_1min$SameDate, tz = "America/Los_Angeles")

MR_ToMerge_5min <- as.data.frame(MRsumm %>%
                                   group_by(SameDate = cut(SameDate, breaks="5 min"), BirdID) %>%
                                   dplyr::summarize(EE_J = sum(EE_J)) %>%
                                   ungroup())
MR_ToMerge_5min$SameDate <- lubridate::ymd_hms(MR_ToMerge_5min$SameDate, tz = "America/Los_Angeles")

rihu07$Ts_max <- as.numeric(rihu07$Ts_max)
IR_ToMerge <- as.data.frame(ir_dat %>%
                              group_by(SameDate = cut(SameDate, breaks="1 min"), NightID, Category, Tamb) %>%
                              dplyr::summarize(Ts_max = mean(Ts_max)) %>%
                              ungroup())
IR_ToMerge$SameDate <- lubridate::ymd_hms(IR_ToMerge$SameDate, tz = "America/Los_Angeles")

agg_ir_mr <- merge(MR_ToMerge_1min, IR_ToMerge, by.y =c("SameDate", "BirdID"), by.x= c("SameDate", "NightID"))
agg_ir_mr$NightID <- as.factor(agg_ir_mr$NightID)
library(scales)

## Excluding BTMG01_1 to do the scaling
agg_ir_mr <- agg_ir_mr[agg_ir_mr$NightID!="BTMG01_1",]
agg_ir_mr$EE_scaled <- rescale(agg_ir_mr$EE_J, from = c(0, 10.06), to = c(0, 43))


#agg_ir_mr$BirdID <- as.factor(agg_ir_mr$BirdID)
# m.agg <- merge(agg_ir_mr, categories)
# m.agg$Category <- factor(m.agg$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))
# 
# m.agg$TransiHour<- str_pad(substr(m.agg$Time_TransitionStart, 1, 2), width=2, side="left", pad="0")
# m.agg$TransiHour[m.agg$TransiHour==24] <- "00"
# m.agg$TransiMin<- str_pad(substr(m.agg$Time_TransitionStart, 3, 4), width=2, side="left", pad="0")
# m.agg$TransitionTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
#                                           paste(m.agg$TransiHour, 
#                                                 m.agg$TransiMin, "00", sep = ":"), sep=" "),
#                                     format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")
# m.agg$TransitionTime[!is.na(m.agg$TransiHour) & m.agg$TransiHour<19] <- m.agg$TransitionTime[!is.na(m.agg$TransiHour) & m.agg$TransiHour<19]+86400
# 
# 
# m.agg$TorporHour<- str_pad(m.agg$Time_DeepTorporStart, width=4, side="left", pad="0")
# m.agg$TorporHour<- substr(m.agg$TorporHour, 1, 2)
# m.agg$TorporHour[m.agg$TorporHour==24] <- "00"
# m.agg$TorporMin<- str_pad(substr(m.agg$Time_DeepTorporStart, 3, 4), width=2, side="left", pad="0")
# m.agg$DeepTorporTime <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
#                                          paste(m.agg$TorporHour, 
#                                                m.agg$TorporMin, "00", sep = ":"), sep=" "),
#                                    format='%Y-%m-%d %H:%M', tz="America/Los_Angeles")
# m.agg$DeepTorporTime[!is.na(m.agg$TorporHour) & m.agg$TorporHour<19] <- m.agg$DeepTorporTime[!is.na(m.agg$TorporHour) & m.agg$TorporHour<19]+86400
# 
# ggplot(m.agg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category)) + my_theme + facet_wrap(.~BirdID)  +
#   colScale
#library(scales)
# ir_rihu <- 


ggplot(data=MR_ToMerge_1min[MR_ToMerge_1min$BirdID=="RIHU10",], aes(SameDate, EE_J)) + geom_point(col="black") + 
  my_theme + #geom_point(data=rihu07, aes(SameDate, Ts_max), col='red') +
  theme(axis.text.x = element_text(size=10, angle = 90, vjust=0.5))

grid.arrange(ir_rihu, mr_rihu, nrow=2)
# scale_x_continuous(
  #   labels = function(x) glue("{x}:00")
  # )
  #scale_x_date(labels = date_format("%H"))#+ #facet_wrap(.~BirdID)  +


## Plots for Emily's Murdoch poster
birds_IRplot <- c("BCHU01_1", "BCHU02_1", "BCHU03_1", "BTMG01_01", "BTMG01_2", "BTMG03_1", "RIHU01_1", "RIHU09_1", "RIHU10_1")

## 9 birds IR plot
ggplot(data=ir_dat[ir_dat$NightID %in% c(birds_IRplot, "BTMG01_1") & !is.na(ir_dat$Ts_max),], aes(SameDate, Ts_max)) + facet_wrap(.~NightID) +
  my_theme +
  geom_line(aes(y=Tamb), linetype="dotted", col="grey30", size=1.5) +
  geom_point(aes(SameDate, Ts_max, col=Category), size=3) + #scale_color_viridis_d() +
  colScale_named + xlab("Time of Night") + ylab(SurfTemp.lab) +
  theme(axis.text.x = element_text(size=20), legend.key.height =unit(3,"line"))

ggplot(data=ir_dat[ir_dat$NightID=="BTMG01_1" & !is.na(ir_dat$Ts_max),], aes(SameDate, Ts_max)) + facet_wrap(.~NightID) +
  my_theme +
  geom_line(aes(y=Tamb), linetype="dotted", col="grey30", size=1.5) +
  geom_point(aes(SameDate, Ts_max, col=Category), size=3) + #scale_color_viridis_d() +
  colScale + xlab("Time of Night") + ylab(SurfTemp.lab) +
  theme(axis.text.x = element_text(size=20))

## Good individual IR BCHU plots
ggplot(data=ir_dat[ir_dat$BirdID=="BCHU01",], aes(SameDate, Ts_max)) + #geom_point(data=MR_ToMerge_1min, aes(SameDate, EE_J), col="black") + 
  my_theme +
  geom_line(aes(y=Tamb), linetype="dotted", col="grey30", size=1.5) +
  geom_point(aes(SameDate, Ts_max, col=Category), size=3) +
  xlab("Time of Night") + ylab(SurfTemp.lab) +
  colScale_bchu01 + theme(axis.text.x = element_text(size=20))

ggplot(data=ir_dat[ir_dat$BirdID=="BCHU02",], aes(SameDate, Ts_max)) + #geom_point(data=MR_ToMerge_1min, aes(SameDate, EE_J), col="black") + 
  my_theme +
  geom_line(aes(y=Tamb), linetype="dotted", col="grey30", size=1.5) +
  geom_point(aes(SameDate, Ts_max, col=Category), size=3) +
  xlab("Time of Night") + ylab(SurfTemp.lab) +
  colScale_named_noShallow + theme(axis.text.x = element_text(size=20))

ggplot(data=ir_dat[ir_dat$BirdID=="BCHU03",], aes(SameDate, Ts_max)) + #geom_point(data=MR_ToMerge_1min, aes(SameDate, EE_J), col="black") + 
  my_theme +
  geom_line(aes(y=Tamb), linetype="dotted", col="grey30", size=1.5) +
  geom_point(aes(SameDate, Ts_max, col=Category), size=3) +
  xlab("Time of Night") + ylab(SurfTemp.lab) +
  colScale_named_noShallow + theme(axis.text.x = element_text(size=20))

## IR plus MR for RIHU10, CAAN29, BTMG01_1
### Latest 10/21 - try BCHU03, RIHU09, RIHU02
ggplot(data=ir_dat[ir_dat$NightID=="BTMG01_1",], aes(x=SameDate, y=Ts_max)) + facet_grid(.~BirdID) +
  geom_point(aes(x=SameDate, y=Ts_max, col=Category), size=2) +
  geom_line(aes(y=Tamb, group=NightID), linetype="dotted", col="grey20", size=1) +
  my_theme + colScale_named + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night") +  ylab(SurfTemp.lab)

ggplot(MR_ToMerge_1min[MR_ToMerge_1min$BirdID=="BTMG01_1",], aes(x=SameDate, y=EE_J)) + facet_wrap(~BirdID, scales="free") +
  geom_point(alpha=0.8, col='grey10') + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*600)) +
  my_theme + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")

ggplot(MR_ToMerge_1min[MR_ToMerge_1min$BirdID=="BTMG01_2",], aes(x=SameDate, y=EE_J)) + facet_wrap(~BirdID, scales="free") +
  geom_point(alpha=0.8, col='grey10') + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*600)) +
  my_theme + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")


ggplot(MR_ToMerge_1min[MR_ToMerge_1min$BirdID=="RIHU10_1",], aes(x=SameDate, y=EE_J)) + facet_wrap(~BirdID, scales="free") +
  geom_point(alpha=0.8, col='black') + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*600)) +
  my_theme + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")

## Emily poster
## Using "scaled EE_J" = EE_scaled column in agg_ir_mr because that is scaled to the range of IR values (-1 to 43)
### RIHU10, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="RIHU10_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./4,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(aes(SameDate, y=Tamb), linetype="dotted", col="black", size=1) +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_noArousal + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")

### BTMG01_2, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="BTMG01_2" & !is.na(agg_ir_mr$Ts_max),], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./4,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(aes(SameDate, y=Tamb), linetype="dotted", col="black", size=1) +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_named_noShallow + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")

### BTMG02_1, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="BTMG02_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./4,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(aes(SameDate, y=Tamb), linetype="dotted", col="black", size=1) +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_named_noShallow + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")

### BCHU03_1, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="BCHU03_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./4,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(aes(SameDate, y=Tamb), linetype="dotted", col="black", size=1) +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_named_noShallow + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")

### RIHU09_1, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="RIHU09_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./4,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(aes(SameDate, y=Tamb), linetype="dotted", col="black", size=1) +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_named_noShallow + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")

### RIHU02_1, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="RIHU02_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./4,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(aes(SameDate, y=Tamb), linetype="dotted", col="black", size=1) +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_named_noShallow + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")

### RIHU03_1, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="RIHU03_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./4,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(aes(SameDate, y=Tamb), linetype="dotted", col="black", size=1) +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_named_noShallow + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")


### All indivs, MR and IR, 1 min average for MR
ggplot(agg_ir_mr[!is.na(agg_ir_mr$Category),], aes(x=SameDate, y=Ts_max)) + facet_wrap(~NightID) +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./3.33,name='MR (J/min)'))+
  geom_line(aes(group=NightID), alpha=0.5) +
  geom_point(aes(col=Category), size=3) +
  geom_line(data=agg_ir_mr[!is.na(agg_ir_mr$Tamb),], aes(SameDate, y=Tamb), linetype="dotted", col="black") +
  geom_point(alpha=0.8, col='red', size=2, aes(y=EE_scaled)) + 
  geom_line(aes(y=EE_scaled), col="red", alpha=0.5) +
  my_theme + colScale_named + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")




## All birds, IR plus MR
ggplot(agg_ir_mr[agg_ir_mr$NightID=="BCHU03_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(.~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./5,name='MR (J/min)'))+
  geom_line(aes(group="NightID")) +
  geom_point(aes(x=SameDate, y=Ts_max, col=Category), size=4) +
  #geom_line(data=IR_ToMerge[!is.na(IR_ToMerge$Tamb),], aes(SameDate, y=Tamb), linetype="dotted", col="gray") +
  geom_point(aes(x=SameDate, y=EE_J*5), alpha=0.8, col='red', size=3) + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*600)) +
  my_theme + colScale_named_noShallow + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night") + ylab(SurfTemp.lab)

ggplot(agg_ir_mr[agg_ir_mr$NightID =="RIHU09_1",], aes(x=SameDate, y=Ts_max)) + facet_wrap(.~NightID, scales="free") +
  scale_y_continuous(name = SurfTemp.lab,limits = c(-1,43), sec.axis=sec_axis(trans=~./5,name='MR (J/min)'))+
  geom_line(aes(group="NightID")) +
  geom_point(aes(x=SameDate, y=Ts_max, col=Category), size=4) +
  #geom_line(data=IR_ToMerge[!is.na(IR_ToMerge$Tamb),], aes(SameDate, y=Tamb), linetype="dotted", col="gray") +
  geom_point(data=agg_ir_mr[agg_ir_mr$EE_J < 3 & agg_ir_mr$NightID=="RIHU09_1",], aes(x=SameDate, y=EE_J*5), alpha=0.8, col='red', size=3) + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*600)) +
  my_theme + colScale_named + #scale_color_manual(values=my_colors) +
  theme(axis.text = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50"), axis.line.y.right = element_line(color="red"),
        axis.text.y.right = element_text(color="red"), axis.title.y.right = element_text(color="red")) +
  xlab("Time of night")


### AZ - ALL THE BIRDS - MR
ggplot(NULL, aes(x=SameDate, y=EE_J)) + facet_wrap(~BirdID) +
  #scale_y_continuous(name = "Max Ts",sec.axis=sec_axis(trans=~.*1000,name='MR (Watts)'))+
  geom_point(data=MRsumm, alpha=0.8, col='grey90') + #geom_smooth(data=MRsumm) + 
  #geom_smooth(data=ir_dat, aes(x=SameDate, y=Ts_max, col=Category)) + #[ir_dat$BirdID %in% MRsumm$BirdID,]
  my_theme2 + scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")




ggplot(NULL, aes(SameDate, Ts_max)) + geom_point(data=ir_dat, aes(SameDate, Ts_max), col="red", size=3) + 
  geom_point(data=agg_ir_mr, aes(SameDate, y=EE_J), col="black") + my_theme + facet_wrap(.~BirdID)  +   #colScale + 
  xlab("Energy expenditure (J/min)") + ylab(SurfTemp.lab) + scale_alpha_manual(values = c(0.8,0.6,0.8)) +
  theme(axis.text.x = element_text(size=20, angle = 90, vjust=0.5))
  # stat_smooth(aes(col=Category), method = "lm", formula = y ~ x + I(x^2), size = 1)

MR_ToMerge_1min <- dplyr::arrange(MR_ToMerge_1min, DateLubri)
  
ggplot(MR_ToMerge_1min, aes(SameDate, EE_J)) + geom_point(col="red", size=3) + facet_grid(.~BirdID) +
    my_theme
  

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



ggplot(MR_ToMerge_1min, aes(DateLubri, EE_J)) + geom_point()


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

m.agg$DateLubri <- lubridate::ymd_hms(m.agg$DateLubri, tz = "America/Los_Angeles")
m.agg$SameDate <- as.POSIXct(paste(paste("2021", "7", "23", sep = "-"), 
                                    as_hms(m.agg$DateLubri), sep=" "),
                              format='%Y-%m-%d %H:%M:%S', tz="America/Los_Angeles")
m.agg$SameDate[m.agg$SameDate < '2021-07-23 19:00:00'] <- m.agg$SameDate[m.agg$SameDate < '2021-07-23 19:00:00']+86400
m.agg$SameDate <- lubridate::ymd_hms(m.agg$SameDate, tz = "America/Los_Angeles")
m.agg$RowCateg <- NA
agg <- data.frame(matrix(ncol=ncol(m.agg),nrow=0))
colnames(agg) <- colnames(m.agg)
### Assigning row-specific categories
RowCategFunc <- function (bird, data) {
  subdata <- subset(data,data$BirdID == bird)
  TransiTime <- unique(subdata$TransitionTime)
  TorporTime <- unique(subdata$DeepTorporTime)
  
  if(is.na(TransiTime) & is.na(TorporTime)) {
    subdata$RowCateg <- "Normothermic"
  } else if(!is.na(TransiTime) & is.na(TorporTime)) {
    subdata$RowCateg[subdata$SameDate < TransiTime] <- "Normothermic"
    subdata$RowCateg[subdata$SameDate >= TransiTime] <- "Transition"
  } else {
    subdata$RowCateg[subdata$SameDate < TransiTime] <- "Normothermic"
    subdata$RowCateg[subdata$SameDate >= TransiTime & subdata$SameDate < TorporTime] <- "Transition"
    subdata$RowCateg[subdata$SameDate >= TorporTime] <- "DeepTorpor"
  }
  rbind(agg, subdata)
}

birds <- unique(m.agg[,"BirdID"])
aggd <- lapply(X = birds, FUN = RowCategFunc, data = m.agg)
AllDatCateg <- do.call(rbind.data.frame, aggd)

m.agg %>% 
  group_by(BirdID) %>% 
  dplyr::summarise(EE_kJ_Night = sum(EE_J)/1000)

## Changing "RowCateg" to "Category" and "Category" to "CategBird"
AllDatCateg <- dplyr::rename(AllDatCateg, CategBird = Category, Category = RowCateg)
AllDatCateg$Category <- factor(AllDatCateg$Category, levels=c("Normothermic", "Transition", "DeepTorpor"))

write.csv(x = AllDatCateg, file = here::here("MR_IR_Merged_EEJpermin.csv"))


ggplot(AllDatCateg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category)) + my_theme + facet_wrap(.~BirdID)  +
  colScale

## No geom_smooth
ggplot(AllDatCateg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category, alpha=Category), size=3) +
  my_theme + #facet_wrap(.~BirdID)  +
  colScale + xlab("Energy expenditure (J/min)") + 
  ylab(SurfTemp.lab) + scale_alpha_manual(values = c(0.6,0.6,0.8))

##With geom_smooth, all lm
ggplot(AllDatCateg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category, alpha=Category), size=3) +
  my_theme + #facet_wrap(.~BirdID)  +
  colScale + xlab("Energy expenditure (J/min)") + 
  ylab(SurfTemp.lab) + scale_alpha_manual(values = c(0.6,0.6,0.8)) + 
  stat_smooth(aes(col=Category), method = "lm")

## With geom_smooth quadratic
ggplot(AllDatCateg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category, alpha=Category), size=3) +
  my_theme + #facet_wrap(.~BirdID)  +
  colScale + xlab("Energy expenditure (J/min)") + 
  ylab(SurfTemp.lab) + scale_alpha_manual(values = c(0.6,0.6,0.8)) + 
  stat_smooth(aes(col=Category), method = "lm", formula = y ~ x + I(x^2), size = 1)

## With geom_smooth, some quadratic and some lm
ggplot(AllDatCateg, aes(EE_J, Ts_max)) + geom_point(aes(col=Category, alpha=Category), size=3.5) +
  my_theme + #facet_wrap(.~BirdID)  +
  colScale + xlab("Energy expenditure (J/min)") + 
  ylab(SurfTemp.lab) + scale_alpha_manual(values = c(0.6,0.6,0.8)) + 
  stat_smooth(data=AllDatCateg[AllDatCateg$Category %in% c("Normothermic", "DeepTorpor"),], 
              aes(col=Category), method = "lm", alpha=0.2, size=1) +
  stat_smooth(data=AllDatCateg[AllDatCateg$Category=="Transition",], 
              aes(col=Category), method = "lm", formula = y ~ x + I(x^2), alpha=0.2, size=1) +
  theme(legend.key.height = unit(3,"line"))

ggplot(AllDatCateg, aes(EE_J, Ts_max)) + geom_point(aes(col=RowCateg, alpha=RowCateg), size=3) + 
  my_theme + #facet_wrap(.~BirdID)  +
  colScale + xlab("Energy expenditure (J/min)") + 
  ylab(SurfTemp.lab) + scale_alpha_manual(values = c(0.8,0.6,0.8)) 


mod_test_ID <- nlme::lme(data=na.exclude(AllDatCateg), fixed=Ts_max ~ 
                       EE_J + 
                       Category,
                       random= ~1|BirdID/Category, 
                     correlation=corAR1(form=~1|BirdID/Category))

mod_test <- nlme::lme(data=na.exclude(AllDatCateg), fixed=Ts_max ~ 
                        EE_J + 
                        Category,
                      random= ~1|Category)

acf(resid(mod_test), plot=F)
summary(mod_test, correlation=T)
coef(mod_test)
intervals(mod_test)
acf(resid(mod_test))
em <- emmeans(mod_test,  ~Category)
em
plot(residuals(mod_test),type="b")
abline(h=0,lty=3)
summary(mod_test)$tTable


mod_test <- glm(data=AllDatCateg, formula = Ts_max ~ EE_J+Category)

acf(resid(mod_test), plot=F)
summary(mod_test)
coef(mod_test)
intervals(mod_test)
acf(resid(mod_test))
em <- emmeans(mod_test,  ~Category)
em
plot(residuals(mod_test),type="b")
abline(h=0,lty=3)
summary(mod_test)$tTable


## Trying lm's with plyr -  3 separate lm's, one per category
# Break up data frame by Category, then fit the specified model to each piece and
# return a list
models <- dlply(AllDatCateg, "Category", function(df) 
  lm(Ts_max ~ EE_J + EE_J2, data = df))


AllDatCateg$EE_J2 <- 0
AllDatCateg$EE_J2[AllDatCateg$Category=="Transition"] <- (AllDatCateg$EE_J[AllDatCateg$Category=="Transition"])^2

lm(Ts_max ~ EE_J + EE_J2, data = AllDatCateg[AllDatCateg$Category=="Transition",])

# Apply coef to each model and return a data frame
ldply(models, coef)

# Print the summary of each model
l_ply(models, summary, .print = TRUE)

tab_model(models)

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
ggplot(NULL, aes(x=SameDate, y=EE_J)) + facet_wrap(~BirdID) +
  scale_y_continuous(name = "Max Ts",sec.axis=sec_axis(trans=~.*1000,name='MR (Watts)'))+
  geom_point(data=MRsumm, alpha=0.8, col='grey90') + #geom_smooth(data=MRsumm) + 
  geom_smooth(data=ir_dat, aes(x=SameDate, y=Ts_max, col=Category)) + #[ir_dat$BirdID %in% MRsumm$BirdID,]
  my_theme + scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")

### AZ - ALL THE BIRDS - MR
ggplot(NULL, aes(x=SameDate, y=EE_J)) + facet_wrap(~BirdID) +
  #scale_y_continuous(name = "Max Ts",sec.axis=sec_axis(trans=~.*1000,name='MR (Watts)'))+
  geom_point(data=MRsumm, alpha=0.8, col='grey90') + #geom_smooth(data=MRsumm) + 
  #geom_smooth(data=ir_dat, aes(x=SameDate, y=Ts_max, col=Category)) + #[ir_dat$BirdID %in% MRsumm$BirdID,]
  my_theme2 + scale_color_manual(values=my_colors) +
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

### ALL THE BIRDS, MR and IR, 1 min average for MR
ggplot(NULL, aes(x=SameDate, y=Ts_max)) + facet_wrap(~BirdID, scales="free") +
  scale_y_continuous(name = "Max Ts",limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  geom_smooth(data=ir_dat[!is.na(ir_dat$Ts_max),]) +
  geom_point(data=ir_dat[!is.na(ir_dat$Ts_max),], col="black", size=2) +
  geom_line(data=ir_dat[!is.na(ir_dat$Tamb),], aes(SameDate, y=Tamb), linetype="dotted", col="gray") +
  geom_point(data=MR_ToMerge_1min, alpha=0.8, col='grey90', aes(y=EE_J*600)) + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*600)) +
  my_theme + #scale_color_manual(values=my_colors) +
  # theme(axis.text = element_text(size=10),
  #       legend.key.height=unit(3,"line"),
  #       axis.line.x = element_line(colour = "grey50"),
  #       axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")

### RIHU07 both MR and IR
temp_rihu07 <- ggplot(ir_dat[!is.na(ir_dat$Ts_max) & ir_dat$BirdID=="RIHU07",], 
       aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID, scales="free") +
  #scale_y_continuous(name = "Max Ts",limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  #geom_smooth() +
  geom_point(col="black", size=2) +
  geom_line(aes(SameDate, y=Tamb), linetype="dashed", col="black", size=1) +
  #geom_point(data=MR_ToMerge_1min, alpha=0.8, col='grey30', aes(y=EE_J*60)) + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*60)) +
  my_theme + #scale_color_manual(values=my_colors) +
  # theme(axis.text = element_text(size=10),
  #       legend.key.height=unit(3,"line"),
  #       axis.line.x = element_line(colour = "grey50"),
  #       axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")

metab_rihu07 <- ggplot(MR_ToMerge_1min, aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID, scales="free") +
  #scale_y_continuous(name = "Max Ts",limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  #geom_smooth() +
  #geom_point(col="black", size=2) +
  #geom_line(aes(SameDate, y=Tamb), linetype="dashed", col="black", size=1) +
  geom_point(alpha=0.8, col='grey30', aes(y=EE_J*60)) + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*60)) +
  my_theme + #scale_color_manual(values=my_colors) +
  # theme(axis.text = element_text(size=10),
  #       legend.key.height=unit(3,"line"),
  #       axis.line.x = element_line(colour = "grey50"),
  #       axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")

grid.arrange(temp_rihu07, mr_rihu, ncol=1)

temp_rihu10 <- ggplot(ir_dat[!is.na(ir_dat$Ts_max) & ir_dat$BirdID=="RIHU10",], 
                      aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID, scales="free") +
  #scale_y_continuous(name = "Max Ts",limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  #geom_smooth() +
  geom_point(col="black", size=2) +
  geom_line(aes(SameDate, y=Tamb), linetype="dashed", col="black", size=1) +
  #geom_point(data=MR_ToMerge_1min, alpha=0.8, col='grey30', aes(y=EE_J*60)) + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*60)) +
  my_theme + #scale_color_manual(values=my_colors) +
  geom_hline(yintercept = c(18,29.5)) +
  # theme(axis.text = element_text(size=10),
  #       legend.key.height=unit(3,"line"),
  #       axis.line.x = element_line(colour = "grey50"),
  #       axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")
temp_rihu10

temp_btmg01 <- ggplot(ir_dat[!is.na(ir_dat$Ts_max) & ir_dat$BirdID=="BTMG01" & ir_dat$Run==2,], 
                      aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID, scales="free") +
  #scale_y_continuous(name = "Max Ts",limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  #geom_smooth() +
  geom_point(col="black", size=2) +
  geom_line(aes(SameDate, y=Tamb), linetype="dashed", col="black", size=1) +
  #geom_point(data=MR_ToMerge_1min, alpha=0.8, col='grey30', aes(y=EE_J*60)) + 
  #geom_smooth(data=MR_ToMerge_1min, aes(y=EE_J*60)) +
  my_theme + #scale_color_manual(values=my_colors) +
  geom_hline(yintercept = c(18,29.5)) +
  # theme(axis.text = element_text(size=10),
  #       legend.key.height=unit(3,"line"),
  #       axis.line.x = element_line(colour = "grey50"),
  #       axis.line.y = element_line(colour = "grey50")) +
  xlab("Time of night")
temp_btmg01


### RIHU07, MR and IR, 1 min average for MR
ggplot(NULL, aes(x=SameDate, y=Ts_max)) + #facet_wrap(~BirdID, scales="free") +
  scale_y_continuous(name = "Max Ts",limits = c(-1,43), sec.axis=sec_axis(trans=~./10,name='MR (J/min)'))+
  #geom_smooth(data=agg_ir_mr, aes(Ts_max)) +
  geom_point(data=agg_ir_mr, col="black", size=3) +
  #geom_line(data=agg_ir_mr, aes(DateLubri, y=Tamb), linetype="dotted", col="gray") +
  geom_point(data=agg_ir_mr, alpha=0.8, col='grey90', aes(y=EE_J*600)) + 
  geom_smooth(data=agg_ir_mr, aes(y=EE_J*600)) +
  my_theme + #scale_color_manual(values=my_colors) +
  # theme(axis.text = element_text(size=10),
  #       legend.key.height=unit(3,"line"),
  #       axis.line.x = element_line(colour = "grey50"),
  #       axis.line.y = element_line(colour = "grey50")) +
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


## All birds just IR
ggplot(ir_dat, aes(x=SameDate, y=Ts_max)) + facet_wrap(~BirdID) + 
  geom_point() + #geom_smooth(aes(col=Category)) + 
  geom_line(aes(y=Tamb), linetype="dashed") + my_theme + #scale_color_manual(values=my_colors) +
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


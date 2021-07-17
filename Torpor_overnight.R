##Putting all respirometry points on a plot together for nighttime MR measurements

## Reading in packages
library(here)
library(animation)
#library(requireR)
#library(magick)
library(caTools)
library(png)
#library(gapminder)
library(ggplot2)
#devtools::install_github("dgrtwo/gganimate")
library(gganimate)
library(dplyr) ## To sum EE over one minute intervals
library(janitor) ## To make a row into colnames
library(lubridate)

## Read in files
caan02 <- read.csv(here("MR", "CAAN02_0623_WholeNight_Analyzed.csv"))
paths <- dir(here::here("MR", "Multiple"), pattern = ".csv$")
names(paths) <- basename(paths)

## General theme
my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())

my_colors <- c("#23988aff", "#440558ff", "#9ed93aff")


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

ThermDat[ThermDat$BirdID=="CAAN01",1]



ThermDat$StartDateFormat <- as.POSIXct(paste(paste(ThermDat$Year, ThermDat$Month, ThermDat$Day, sep = "-"), "00:00:00", sep = " "),
                                     format='%Y-%m-%d %H:%M:%S')


ThermDat$DateTime <- ThermDat$StartDateFormat + (3600*as.numeric(ThermDat$Time_hours))

# ## Trying out adding Time_hours column to start time
# min(ThermDat$StartDateFormat) + (3600*21.66063)
# 
# aggregate(. ~ cut(ThermDat$DateTime, "1 min"), 
#           ThermDat[setdiff(names(ThermDat), "DateTime")], 
#           sum)
# 

ThermDat$DateLubri <- lubridate::ymd_hms(ThermDat$DateTime)
ThermDat <- dplyr::arrange(ThermDat, DateLubri)


## 01 - Normo
## 02 - Deep Torpor
## 04 - Deep Torpor
## 07 - Transition
## 08 - Normo
## 09 - Normo

ThermDat$BirdID <- as.factor(ThermDat$BirdID)
ThermDat$Category <- NA
ThermDat$Category[ThermDat$BirdID==levels(ThermDat$BirdID)[1]] <- "Normothermic"
ThermDat$Category[ThermDat$BirdID==levels(ThermDat$BirdID)[2]] <- "DeepTorpor"
ThermDat$Category[ThermDat$BirdID==levels(ThermDat$BirdID)[3]] <- "DeepTorpor"
ThermDat$Category[ThermDat$BirdID==levels(ThermDat$BirdID)[4]] <- "Transition"
ThermDat$Category[ThermDat$BirdID==levels(ThermDat$BirdID)[5]] <- "Normothermic"
ThermDat$Category[ThermDat$BirdID==levels(ThermDat$BirdID)[6]] <- "Normothermic"

ThermDat$Category <- factor(ThermDat$Category, levels=c("Normothermic", "Transition", "Deep Torpor"))


## First 2 hours get an RER of 21.16, next hours get RER of 19.67
## Lighton equation: 16 + 5.164*RER	
## So for RER of 1 (carbs), 16 + 5.164*1 = 21.16
## For RER of 0.71 (protein/fat), 16 + 5.164*0.71 = 19.67
## TAKES A MINUTE or two TO RUN
Tempsumm <- data.frame()
for(n in unique(ThermDat$BirdID)) {
  dat1 <- ThermDat[ThermDat$BirdID==n,]
  for(i in 1:length(dat1$VO2_ml_min)) {
    if(dat1$DateLubri[i] < (min(dat1$DateLubri[i])+7200)) { ## 2 hours in seconds is 2*60*60 = 7200
      dat1$EE_J[i] <- dat1$VO2_ml_min[i]*21.16/1000
    } else {
      dat1$EE_J[i] <- dat1$VO2_ml_min[i]*19.67/1000
    }
  }
  Tempsumm <- rbind(Tempsumm, dat1)
}

head(Tempsumm)

vars_keep <- names(ThermDat) %in% c("DateLubri", "EE_J")
ThermDat_sub <- ThermDat[vars_keep]


vars_keep <- names(Tempsumm) %in% c("DateLubri", "EE_J")
Tempsumm_sub <- Tempsumm[vars_keep]


# agg.ThermDat <- aggregate(. ~ cut(ThermDat_sub$DateLubri, "1 min"), 
#                         ThermDat_sub[setdiff(names(ThermDat_sub), "date")], 
#                         sum)
# names(agg.ThermDat) <- c("Date", "VO2_ml_min")
# agg.ThermDat$Date <- as.Date(agg.ThermDat$Date, "%Y-%m-%d %H:%M:%S")

## Average VO2 for every second, rather than quarter second
for(n in unique(ThermDat$BirdID)) {
  dat1 <- ThermDat[ThermDat$BirdID==n,]
  seq_avg <- seq(1, length(ThermDat$EE_J), 4)
  EE_J_per_sec <- sapply(seq_avg, function(i) {mean(ThermDat$EE_J[i:(i+4)])})
  EE_J_toPlot <- as.data.frame(cbind(EE_J_per_sec, seq(1,length(EE_J_per_sec),1)))
  names(EE_J_toPlot) <- c("EE_J_per_sec", "SampleNo")
}


## Average VO2 for every minute, rather than quarter second
seq_avg_min <- seq(1, length(ThermDat$EE_J), 240)
EE_J_per_min <- sapply(seq_avg, function(i) {mean(ThermDat$EE_J[i:(i+240)])})
EE_J_min_toPlot <- as.data.frame(cbind(EE_J_per_sec, seq(1,length(EE_J_per_sec),1)))
names(EE_J_min_toPlot) <- c("EE_J_per_min", "SampleNo")

## Average EE for every minute
ThermDat_sub %>%
  group_by(DateLubri = cut(DateLubri, breaks="1 min")) %>%
  summarize(EE_J = mean(EE_J))

d2 <- data.frame(DateLubri = seq(as.POSIXct(min(ThermDat$DateLubri)), as.POSIXct((max(ThermDat$DateLubri))), 
                                  by="1 min"))
ThermDat_sub %>%
  mutate(DateLubri = floor_date(DateLubri, "1 minute")) %>%
  group_by(DateLubri) %>%
  summarise(EE_J_min = mean(EE_J)) %>%
  right_join(d2, by = DateLubri)



ThermDat_sub %>% 
  mutate(interval = floor_date(DateLubri, unit="min")) %>% 
  group_by(interval) %>% 
  mutate(EE_J_min=mean(EE_J))  %>% 
  select(interval,EE_J_min) 



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
ggplot(Tempsumm, aes(x=DateLubri, y=EE_J)) + facet_wrap(~BirdID, scales="free") + 
  geom_point(alpha=0.8, col='grey90') + geom_smooth(aes(col=Category)) + 
  my_theme + 
  scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")


## Just CAAN04
ggplot(Tempsumm[Tempsumm$BirdID=="CAAN04",], aes(x=DateLubri, y=EE_J)) + #facet_wrap(~BirdID, scales="free") + 
  geom_point(alpha=0.8, col='grey90') + geom_smooth(aes(col=Category)) + 
  my_theme + 
  scale_color_manual(values=my_colors) +
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  #scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J) per second") + xlab("Time of night")



caan08_mr_plot <- ggplot(ThermDat, aes(x=DateLubri, y=EE_J*10)) +
  geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
  my_theme + #colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  ylab("Energy expended (J*10) per second") + xlab("Time of night")

#### From IR script, won't read here- temporary
caan08_ir_plot <- ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateLubri, Ts_max)) + 
  geom_point(aes(col=BirdID), size=3) + my_theme +
  scale_y_continuous(name = "Max Surf Temp", sec.axis = sec_axis( trans=~./100, name="EE (J)")) +
  geom_line(aes(y=Tamb), linetype="dashed") + 
  ylim(0,40) +
  scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  theme(axis.text.x = element_text(size = 20),
        legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)

#### From IR script, won't read here- temporary
caan08_ir_Ts_plot <- ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateLubri, Ts_max)) + 
  geom_point(aes(col=BirdID), size=3) + my_theme +
  scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  theme(axis.text.x = element_text(size = 20),
        legend.position = "none") + xlab("Time of night") + ylab(SurfTemp.lab)

caan08_ir_amb_plot <- ggplot(ir_dat[ir_dat$BirdID=="CAAN08",], aes(DateLubri, Ts_max)) + 
  my_theme + 
  geom_line(aes(y=Tamb), linetype="dashed") + 
  #ylim(0,40) +
  scale_x_datetime(limits = ymd_hms(c("2021-07-07 21:00:00", "2021-07-08 03:00:00"))) +
  theme(axis.text.x = element_text(size = 20),
        legend.position = "none") + xlab("Time of night") + ylab(AmbTemp.lab)

grid.arrange(caan08_ir_Ts_plot, caan08_ir_amb_plot, caan08_mr_plot, nrow=3)


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




# gcb_0720$EE_per_second_J <- gcb_0720$EE_J/60
ggplot(NULL, aes(x=SampleNo, y=EE_per_second_J, col=Category)) +
  geom_path(data=gcb_0720[gcb_0720$SampleNo<20000 & gcb_0720$Category=="Normothermy",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$SampleNo>30000 & gcb_0720$Category=="Normothermy",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$Category=="Torpor",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$Category=="Rewarming",], size=1.25) +
  my_theme_blank + colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  scale_x_continuous(breaks= seq(0,36000,3600), labels = seq(0,10,1)) +
  #scale_y_continuous(breaks= seq(0,50,10)) +
  xlab("Hours") + 
  ylab("Energy expended (J) per second")

################## Animation ##############
setwd("C:/Users/nushi/Dropbox/Hummingbird energetics/Tables_for_paper")

gcb_0720 <- read.csv("E14_0720_GCB_no_bsln_Rewarm.csv")

my_theme <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_rect(colour = "black", fill=NA))

my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())

gcb_0720$Category <- factor(gcb_0720$Category, levels=c("N","T", "R"))
torCol <- c("white", "black", "red", "purple")
names(torCol) <- levels(gcb_0720$Category)
colScale <- scale_colour_manual(name = "Category", values = torCol)


# Make a ggplot, but add frame=year: one image per year
#gpminder_data <- gapminder::gapminder

for (tslot in unique(gcb_0720$TimeSlot)){
  p_temp <- subset(gcb_0720, TimeSlot==tslot)
  gcb_gif_temp <- ggplot(p_temp, aes(Time2, EE_J, frame = Time_chunks, col=Category)) +
    geom_path(aes(cumulative=T)) +
    my_theme_blank + colScale + 
    theme(axis.text.x = element_text(angle=30, hjust=1, size=20),
          legend.key.height=unit(3,"line"),
          axis.line.x = element_line(colour = "grey50"),
          axis.line.y = element_line(colour = "grey50")) +
    ylim(0,50) + xlab("Time (seconds)") + ylab("Energy expenditure (J)")
  gganimate(gcb_gif_temp, paste("gcb0720_",tslot,".gif"), interval=0.05, 
            ani.width=1500, ani.height=800)
}

gcb_gif_1 <- ggplot(gcb_0720[gcb_0720$TimeSlot==c(5,6),], aes(Time2, EE_J, frame = BirdID, col=Category)) +
  geom_path(aes(cumulative=T)) + my_theme_blank + theme(axis.text.x = element_text(angle=60, hjust=1, size=20),
                                                        axis.line.x = element_line(colour = "grey50"),
                                                        axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)

gcb_gif_5 <- ggplot(gcb_0720[gcb_0720$TimeSlot==5,], aes(Time2, EE_J, frame = Time_chunks, col=Category)) +
  geom_path(aes(cumulative=T)) + my_theme_blank + theme(axis.text.x = element_text(angle=60, hjust=1, size=20),
                                                        axis.line.x = element_line(colour = "grey50"),
                                                        axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)

## Animate torpor entry
gcb_gif_5_6 <- ggplot(gcb_0720[gcb_0720$TimeSlot==c(5,6),], aes(Time_hour, EE_J, frame = Time_chunks, col=Category)) +
  geom_path() + my_theme_blank + theme(axis.text.x = element_text(angle=60, hjust=1, size=20),
                                                        axis.line.x = element_line(colour = "grey50"),
                                                        axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)

## Plotting torpor entry
gcb_gif_5_6 <- ggplot(gcb_0720[gcb_0720$TimeSlot==c(5,6),], aes(SampleNo, EE_J, col=Category)) +
  geom_path() + my_theme_blank + theme(axis.text.x = element_text(angle=30, hjust=1, size=20),
                                       axis.line.x = element_line(colour = "grey50"),
                                       axis.line.y = element_line(colour = "grey50")) +
  scale_color_manual(values = c("black", "red")) + xlab("Time") #+ xlim(0,4060)
gcb_gif_5_6 ## plot still graph

# Make the animation!
gganimate(gcb_gif_5_6, interval=0.075, ani.width=1500, ani.height=800)

# Save it to Gif
gganimate(gcf_gif_5_6, "C:\Users\ANUSHA\Dropbox\Hummingbird energetics\Tables_for_paper\\gcb_0720_5_6.gif")

gcb_gif_total <- ggplot(gcb_0720, aes(SampleNo, EE_J, frame = BirdID, col=Category)) +
  geom_path(aes(cumulative=T)) +
  my_theme_blank + colScale + 
  theme(axis.text.x = element_text(angle=30, hjust=1, size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  ylim(0,50) + xlab("Time (seconds)") + ylab("Energy expenditure (J)")
gganimate(gcb_gif_total, "gcb_0720_total.gif", interval=0.05, ani.width=1200, ani.height=600)


## Whole night for Torpor paper 
gcb_0720$Category <- factor(gcb_0720$Category, levels=c("B", "N", "R", "T"), 
                            labels=c("B", "Normothermy", "Rewarming", "Torpor"))
ggplot(NULL, aes(x=SampleNo, y=EE_J, col=Category)) +
  geom_path(data=gcb_0720[gcb_0720$SampleNo<20000 & gcb_0720$Category=="Normothermy",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$SampleNo>30000 & gcb_0720$Category=="Normothermy",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$Category=="Torpor",], size=1.25) +
  geom_path(data=gcb_0720[gcb_0720$Category=="Rewarming",], size=1.25) +
  my_theme_blank + colScale + 
  theme(axis.text.x = element_text(angle=30, hjust=1, size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  scale_x_continuous(breaks= seq(0,36000,3000)) +
  #geom_hline(yintercept=seq(0,50,2)) +
  #ylim(0,50) + 
  xlab("Time (seconds)") + ylab("Energy expenditure (J)")


#im.convert("*.png", output = "bm-animation1.gif")

#image <- readPNG(system.file("*.png"))
#write.gif(delay = 3, image = "*.png", filename = "example.gif")

frames = 50

saveGIF(for(i in 1:frames){
  # creating a name for each plot file with leading zeros
  if (i < 10) {name = paste('000',i,'plot.png',sep='')}
  if (i < 100 && i >= 10) {name = paste('00',i,'plot.png', sep='')}
  if (i >= 100) {name = paste('0', i,'plot.png', sep='')}
  x = seq(0, i, 1)
  f.3 = dbinom(x, size = i, prob=.3)
  f.7 = dbinom(x, size = i, prob=.7)
  #saves the plot as a .png file in the working directory
  png(name)
  plot(x, f.3, type='h', xlim = c(0,frames), ylim = c(0,.7), ylab ='probability',   
       main = paste('Binomial density with n = ', i), col = 'red')
  
  lines(x,f.7,type='h',col='blue')
  text(45, .6, 'p = .3', col='red')
  text(45, .6, 'p = .7', col='blue', pos=1)
  dev.off()
  },
  movie.name = "animation.gif", img.name = "Rplot", im.convert = "magick", 
  cmd.fun = system, clean = TRUE, extra.opts = ""
)

saveGIF({
  for (i in 1:10) plot(runif(10), ylim = 0:1)
})

setwd(".\\gif_files")
shell(' "C:\\Program Files\\ImageMagick-7.0.7-Q16\\magick.exe" magick -delay 80 *.png example_1.gif')

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

## Read in file
caan02 <- read.csv(here("MR", "CAAN02_0623_WholeNight_Analyzed.csv"))

## General theme
my_theme_blank <- theme_classic(base_size = 30) + 
  theme(axis.title.y = element_text(vjust = 2),
        panel.border = element_blank())


# ## Assign colors and name categories appropriately
# gcb_0720$Category <- factor(gcb_0720$Category, levels=c("B", "N", "R", "T"), 
#                             labels=c("B", "Normothermy", "Rewarming", "Torpor"))
# torCol <- c("white", "black", "red", "purple")
# names(torCol) <- levels(gcb_0720$Category)
# colScale <- scale_colour_manual(name = "Category", values = torCol)

## Average VO2 for every second, rather than quarter second
seq_avg <- seq(1, length(caan02$EE_J), 4)
EE_J_per_sec <- sapply(seq_avg, function(i) {mean(caan02$EE_J[i:(i+4)])})
EE_J_toPlot <- as.data.frame(cbind(EE_J_per_sec, seq(1,length(EE_J_per_sec),1)))
names(EE_J_toPlot) <- c("EE_J_per_sec", "SampleNo")

## Plot
ggplot(EE_J_toPlot, aes(x=SampleNo, y=EE_J_per_sec)) +
  geom_point(alpha=0.8, col='grey90') + geom_smooth() + 
  my_theme_blank + #colScale + 
  theme(axis.text.x = element_text(size=20),
        legend.key.height=unit(3,"line"),
        axis.line.x = element_line(colour = "grey50"),
        axis.line.y = element_line(colour = "grey50")) +
  scale_x_continuous(breaks= seq(0,9500,3600)) +
  #scale_y_continuous(breaks= seq(0,50,10)) +
  xlab("Seconds") + 
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

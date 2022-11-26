###### Paper 1 - Point to survey comparisons for EIA/GES ######
require(ggplot2)
require(chron)
require(plyr)
require(dplyr)
require(rlang)
require(readr)
require(rpart)
require(rpart.plot)
require(factoextra)
require(Hmisc)
require(caret)
require(tidyverse)
require(reshape2)
require(ggpubr)
require(gapminder)

################ FISH ###########################################
## FLOWBEC 2015 ####
# Processing datasets ####
Meygen15_fish_38 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen2015_38_new.csv", header=TRUE)
Meygen15_fish_120 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen2015_120_new.csv", header=TRUE)
Meygen15_fish_200 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen2015_200_new.csv", header=TRUE)


Meygen15_fish_38_120 <- inner_join(Meygen15_fish_38, Meygen15_fish_120, by=c("Time_M","Date_M"))
dim(Meygen15_fish_38_120)
write.csv(Meygen15_fish_38_120,"C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen15_fish_38_120.csv", row.names = FALSE)
#edit it
Meygen15_fish_38_120 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen15_fish_38_120.csv", header=TRUE)
View(Meygen15_fish_38_120)
dim(Meygen15_fish_38_120)

Meygen15_fish_38_120_200 <- inner_join(Meygen15_fish_38_120, Meygen15_fish_200, by=c("Time_M","Date_M"))
dim(Meygen15_fish_38_120_200)
write.csv(Meygen15_fish_38_120_200,"C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen15_fish_38_120_200.csv", row.names = FALSE)
# edit it
Meygen15_fish_38_120_200 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen15_fish_38_120_200.csv", header=TRUE)
dim(Meygen15_fish_38_120_200)

Meygen15_Allfreqs <- Meygen15_fish_38_120_200

View(Meygen15_Allfreqs)
dim(Meygen15_Allfreqs)

View(Meygen15_Allfreqs)
names(Meygen15_Allfreqs)
## We want
# "Date_M","Time_M", "Lat_M_38", "Lon_M_38", "Region_class_38"
# "Sv_mean_38", "NASC_38", "Sv_max_38", "Sv_min_38",
# "Sv_mean_120", "NASC_120", "Sv_max_120", "Sv_min_120",
# "Sv_mean_200", "NASC_200", "Sv_max_200", "Sv_min_200"
# "Height_mean_38","Height_mean_120","Height_mean_200",
# "fish_depth","Depth_mean_120","Depth_mean_200"
# "Corrected_MVBS_38","Corrected_MVBS_120","Corrected_MVBS_200"
#
# 14,15,16,17,
# 5, 6, 7, 8,
# 41, 42, 43, 44,
# 75, 76, 77, 78,
# 109, 110, 111, 112,
# 9, 45, 79, 113,
# 10, 46, 80, 114,
# 24, 58, 92, 126,

# cutlist <- c(14, 15, 16, 17, 5, 6, 7, 8, 41, 42, 43, 44, 75, 76, 77, 78, 109, 110, 111, 112, 9, 45, 79, 113, 10, 46, 80, 114, 24, 58, 92, 126)
cutlist <- c("Date_M","Time_M", "Region_class_38",
             "Sv_mean_38", "NASC_38", "Sv_max_38", "Sv_min_38",
             "Sv_mean_120", "NASC_120", "Sv_max_120", "Sv_min_120", 
             "Sv_mean_200", "NASC_200", "Sv_max_200", "Sv_min_200", 
             "Height_mean_38","Height_mean_120","Height_mean_200","fish_depth","Depth_mean_120","Depth_mean_200",
             "Corrected_MVBS_38","Corrected_MVBS_120","Corrected_MVBS_200")

Meygen15_Allfreqs_cut <- Meygen15_Allfreqs[c(cutlist),]
dim(Meygen15_Allfreqs_cut)

# Data wrangling #####
Meygen15_fish <- Meygen15_Allfreqs
Meygen15_fish$Region_class.x <- factor(Meygen15_fish$Region_class.x)
levels(Meygen15_fish$Region_class.x)
summary(Meygen15_fish$Region_class.x)
Meygen15_fish <- Meygen15_fish[Meygen15_fish$Region_class.x == " /"Fish/"",]

View(Meygen15_Allfreqs_cut)
dim(Meygen15_Allfreqs_cut)
dim(Meygen15_fish)

Meygen15_fish <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Meygen 2015/Outputs/Meygen15_onlyfish_cheat.csv", header=TRUE)

write.csv(Meygen15_fish,"C:/Users/james/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen15_fish.csv", row.names = FALSE)


# DAY/NIGHT ####
View(Meygen15_fish)
dim(Meygen15_fish)

str(Meygen15_fish)
Meygen15_fish$Time_M <- as.character(Meygen15_fish$Time_M)
Meygen15_fish$Time_M <- trimws(Meygen15_fish$Time_M, "l")

## Day / Night
dates<-as.POSIXct(paste(Meygen15_fish$Date_M, Meygen15_fish$Time_M,sep=" "), format="%Y%m%d %H:%M:%S", tz="UTC")
Meygen15_fish$hour <- hours(dates)

# Meygen15_fish$Timing<- NA
# Meygen15_fish$Timing<- "Day"
# Meygen15_fish$Timing<- "Night"

Meygen15_fish$Timing <- NA
for(i in 1:length(Meygen15_fish$Timing)){
  if(Meygen15_fish$hour[i] > 18 | Meygen15_fish$hour[i] < 9){
    Meygen15_fish$Timing[i] <- "Night"
  } else{
    Meygen15_fish$Timing[i] <- "Day"
  }
}

View(Meygen15_fish)
Meygen15_fish$Timing <- factor(Meygen15_fish$Timing)
summary(Meygen15_fish$Timing)

# Depth bins ####
max(Meygen15_fish$Depth_mean) # 36.5
Meygen15_fish$Depth_bin <- NA
for(i in 1:length(Meygen15_fish$Depth_mean )){
  Meygen15_fish$Depth_bin[i] <- ifelse(Meygen15_fish$Depth_mean [i] > 0 && Meygen15_fish$Depth_mean [i] < 2, 
                                       Meygen15_fish$Depth_bin[i] <- "38-40", 
                                       ifelse(Meygen15_fish$Depth_mean [i] >= 2 && Meygen15_fish$Depth_mean [i] < 4, 
                                              Meygen15_fish$Depth_bin[i] <- "36-38", 
                                              ifelse(Meygen15_fish$Depth_mean [i] >= 4 && Meygen15_fish$Depth_mean [i] < 6, 
                                                     Meygen15_fish$Depth_bin[i] <- "34-36",
                                                     ifelse(Meygen15_fish$Depth_mean [i] >= 6 && Meygen15_fish$Depth_mean [i] < 8, 
                                                            Meygen15_fish$Depth_bin[i] <- "32-34", 
                                                            ifelse(Meygen15_fish$Depth_mean [i] >= 8 && Meygen15_fish$Depth_mean [i] < 10, 
                                                                   Meygen15_fish$Depth_bin[i] <- "30-32",
                                                                   ifelse(Meygen15_fish$Depth_mean [i] >= 10 && Meygen15_fish$Depth_mean [i] < 12, 
                                                                          Meygen15_fish$Depth_bin[i] <- "28-30",
                                                                          ifelse(Meygen15_fish$Depth_mean [i] >= 12 && Meygen15_fish$Depth_mean [i] < 14, 
                                                                                 Meygen15_fish$Depth_bin[i] <- "26-28",
                                                                                 ifelse(Meygen15_fish$Depth_mean [i] >= 14 && Meygen15_fish$Depth_mean [i] < 16, 
                                                                                        Meygen15_fish$Depth_bin[i] <- "24-26", 
                                                                                        ifelse(Meygen15_fish$Depth_mean [i] >= 16 && Meygen15_fish$Depth_mean [i] < 18, 
                                                                                               Meygen15_fish$Depth_bin[i] <- "22-24",
                                                                                               ifelse(Meygen15_fish$Depth_mean [i] >= 18 && Meygen15_fish$Depth_mean [i] < 20, 
                                                                                                      Meygen15_fish$Depth_bin[i] <- "20-22",
                                                                                                      ifelse(Meygen15_fish$Depth_mean [i] >= 20 && Meygen15_fish$Depth_mean [i] < 22, 
                                                                                                             Meygen15_fish$Depth_bin[i] <- "18-20",
                                                                                                             ifelse(Meygen15_fish$Depth_mean [i] >= 22 && Meygen15_fish$Depth_mean [i] < 24, 
                                                                                                                    Meygen15_fish$Depth_bin[i] <- "16-18",
                                                                                                                    ifelse(Meygen15_fish$Depth_mean [i] >= 24 && Meygen15_fish$Depth_mean [i] < 26, 
                                                                                                                           Meygen15_fish$Depth_bin[i] <- "14-16",
                                                                                                                           ifelse(Meygen15_fish$Depth_mean [i] >= 26 && Meygen15_fish$Depth_mean [i] < 28, 
                                                                                                                                  Meygen15_fish$Depth_bin[i] <- "12-14",
                                                                                                                                  ifelse(Meygen15_fish$Depth_mean [i] >= 28 && Meygen15_fish$Depth_mean [i] < 30, 
                                                                                                                                         Meygen15_fish$Depth_bin[i] <- "10-12",
                                                                                                                                         ifelse(Meygen15_fish$Depth_mean [i] >= 30 && Meygen15_fish$Depth_mean [i] < 32, 
                                                                                                                                                Meygen15_fish$Depth_bin[i] <- "8-10",
                                                                                                                                                ifelse(Meygen15_fish$Depth_mean [i] >= 32 && Meygen15_fish$Depth_mean [i] < 34, 
                                                                                                                                                       Meygen15_fish$Depth_bin[i] <- "6-8",
                                                                                                                                                       ifelse(Meygen15_fish$Depth_mean [i] >= 34 && Meygen15_fish$Depth_mean [i] < 36, 
                                                                                                                                                              Meygen15_fish$Depth_bin[i] <- "4-6",
                                                                                                                                                              ifelse(Meygen15_fish$Depth_mean [i] >= 36 && Meygen15_fish$Depth_mean [i] < 38, 
                                                                                                                                                                     Meygen15_fish$Depth_bin[i] <- "2-4",
                                                                                                                                                                     ifelse(Meygen15_fish$Depth_mean [i] >= 38 && Meygen15_fish$Depth_mean [i] < 40, 
                                                                                                                                                                            Meygen15_fish$Depth_bin[i] <- "0-2",
                                                                                                                                                                            Meygen15_fish$Depth_bin[i] <- NA))))))))))))))))))))
}
Meygen15_fish$Depth_bin <- as.factor(Meygen15_fish$Depth_bin)
Meygen15_fish$Depth_bin_RL <- reorder(Meygen15_fish$Depth_bin, Meygen15_fish$Depth_mean)

View(Meygen15_fish)
dim(Meygen15_fish)


# Tides - Flood/Ebb #####
# not currently required however would use FVCOM outputs for the period to give velocity differences
# and use that as a proxy for tidal phase (Shaun Frasers work did similar). done manually previous to R input
MeygenRandomfile <- read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/Outputs/Meygen15_fish_random.csv", header=TRUE)

Meygen15_fish$tides<- factor(MeygenRandomfile$tides)

# Split datasets #####
## cant remember what this was
# MG15_34_38 <- Meygen15_fish[Meygen15_fish$Depth_bin== "34-36",]
# MG15_34_36 <-subset(Meygen15_fish, Depth_bin== "34-36")
# dim(MG15_34_38)
# 
# Meygen15_n <- dim(Meygen15_fish)[1]

Meygen15_fish.n <- Meygen15_fish[Meygen15_fish$Timing == "Night",]
dim(Meygen15_fish.n) # 275 106
Meygen15_fish.d <- Meygen15_fish[Meygen15_fish$Timing == "Day",]
dim(Meygen15_fish.d) #727 106


# Stats #####
Meanfishdepth_n <- mean(MeygenFish_n$Depth_mean_38, na.rm=TRUE)
# 29.95971
Meanfishdepth_d <- mean(MeygenFish_d$Depth_mean_38, na.rm=TRUE)
# 30.395

wilcox.test(Depth_mean_120~Timing, data= Meygen15_fish, exact = FALSE)
mean(MeygenFish_d$Depth_mean_120)
mean(MeygenFish_n$Depth_mean_120)

M15top10 <- Meygen15_fish[Meygen15_fish$Depth_mean_38 > 26,]
dim(M15top10)

# Graphing it #####
x_axis_cats_FLOWBEC <- c("32-34", "30-32" , "28-30" , "26-28" , "24-26" , "22-24", 
                                  "20-22" , "18-20" , "16-18",  "14-16" , "12-14",
                                  "10-12" , "8-10" ,    "6-8",    "4-6" ,   "2-4")

x_axis_cats_FLOWBEC_cut <- c("32-34", "30-32" , "28-30" , "26-28" , "24-26" , "22-24", 
                                      "20-22" , "18-20" , "16-18",  "14-16" , "12-14",
                                     "10-12" , "8-10" ,    "6-8",    "4-6" ,   "2-4")

# All fish data
Meygen15_depth_all <- ggplot(Meygen15_fish, aes(x=Depth_bin_RL))
Meygen15_depth_all +
  labs(x="Depth bin", y= "Count", title = "FLOWBEC 2015") +
  geom_bar(aes(fill = Timing), position = position_stack(reverse = TRUE))+
  scale_x_discrete(limits = x_axis_cats_FLOWBEC, drop=FALSE) +
  ylim(0,225)+
  coord_flip()

# All fish data - flood-ebb day-night
# Day
Meygen15.d <- ggplot(MeygenFish_d, aes(x=Depth_bin_RL))
Meygen15.d +
  labs(x=" ", y= "Count", title = "FLOWBEC 2015 Day", fill="Tidal state") +
  geom_bar(aes(fill = tides))+ theme(text=element_text(size=13))+
  scale_x_discrete(limits = x_axis_cats_FLOWBEC, labels = x_axis_cats_FLOWBEC_cut, drop=FALSE) +
  ylim(0,225)+
  coord_flip()

# Night
Meygen15.n <- ggplot(MeygenFish_n, aes(x=Depth_bin_RL))
Meygen15.n +
  labs(x="Depth Bin", y= "Count", title = "FLOWBEC 2015 Night", fill="Tidal state") +
  geom_bar(aes(fill = tides))+ theme(text=element_text(size=13))+
  scale_x_discrete(limits = x_axis_cats_FLOWBEC, labels = x_axis_cats_FLOWBEC_cut, drop=FALSE) +
  scale_y_reverse()+
  ylim(225,0)+
  coord_flip()

############### Scotia 2016 ######################################
# Dataset ####
# dataset came from Ana and she has already done equivalent of inner joins
Scotia16_fish_38 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2016/EK60/Output/Echogram_classified_38.csv", header=TRUE)
dim(Scotia16_fish_38)
Scotia16_fish_120 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2016/EK60/Output/Echogram_classified_120.csv", header=TRUE)
dim(Scotia16_fish_120)
Scotia16_fish_200 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2016/EK60/Output/Echogram_classified_200.csv", header=TRUE)
dim(Scotia16_fish_200)

Scotia16_fish_38$Time

Scotia16_fish_38_120 <- inner_join(Scotia16_fish_38, Scotia16_fish_120, by=c("Time_M","Date_M"))
dim(Scotia16_fish_38_120)
Scotia16_fish_38_120_200 <- inner_join(Scotia16_fish_38_120, Scotia16_fish_200, by=c("Time_M","Date_M"))
dim(Scotia16_fish_38_120_200)

Scotia16_Allfreqs<- Scotia16_fish_38_120_200

View(Scotia16_Allfreqs)
summary(Scotia16_Allfreqs)

dim(Scotia16_Allfreqs)
names(Scotia16_Allfreqs)[1:51]
# We want
# "Date_M","Time_M", "Lat_M_38", "Lon_M_38", "Region_class_38"
# # "Sv_mean_38", "NASC_38", "Sv_max_38", "Sv_min_38",
# # "Sv_mean_120", "NASC_120", "Sv_max_120", "Sv_min_120",
# # "Sv_mean_200", "NASC_200", "Sv_max_200", "Sv_min_200"
# # "Height_mean_38","Height_mean_120","Height_mean_200",
# # "fish_depth","Depth_mean_120","Depth_mean_200"
# # "Corrected_MVBS_38","Corrected_MVBS_120","Corrected_MVBS_200"

cutlist <- c("Date_M_120","Time_M_120", "Region_class_120" ,"Region_class_120", "Lat_M_120", "Lon_M_120",
             "NASC_120", "Sv_mean_120", "Sv_max_120", "Sv_min_120", 
             "Sv_mean_200", "Sv_max_200", "Sv_min_200", 
             "Height_mean_38","Height_mean_120","Height_mean_200",
             "fish_depth","Depth_mean_120","Depth_mean_200",
             "Corrected_MVBS_38","Corrected_MVBS_120","Corrected_MVBS_200")
# 
Scotia16_Allfreqs_cut <- Scotia16_Allfreqs[c(cutlist),]
dim(Scotia16_Allfreqs_cut)

# Data wrangling #####
str(Scotia16_Allfreqs)
Scotia16_Allfreqs$Region_class

Scotia16_fish <- Scotia16_Allfreqs[Scotia16_Allfreqs$Region_class.x == " fish",]
dim(Scotia16_Allfreqs)
dim(Scotia16_fish)

View(Scotia16_fish)
Scotia2016timing <- cbind(Scotia16_fish$Date_M,Scotia16_fish$Time_M) # check the times on this list with EK60 output before publishing

# on inspection some time points are repeated for some reason...
Scotia16_fish <- Scotia16_fish[-c(12:14,56:58),]
dim(Scotia16_fish)


# Depth bins ####
Scotia16_fish

summary(Scotia16_fish$Depth_mean) # 68.5527
Scotia16_fish$Depth_mean <- Scotia16_fish$Depth_mean + 12 #Due to Scotia build detections start at 12metres depth
Scotia16_fish$Depth_bin <- NA
for(i in 1:length(Scotia16_fish$Depth_mean )){
  Scotia16_fish$Depth_bin[i] <- ifelse(Scotia16_fish$Depth_mean[i] > 0 && Scotia16_fish$Depth_mean [i] < 2, 
                                       Scotia16_fish$Depth_bin[i] <- "0-2", 
                                       ifelse(Scotia16_fish$Depth_mean [i] >= 2 && Scotia16_fish$Depth_mean [i] < 4, 
                                              Scotia16_fish$Depth_bin[i] <- "2-4", 
                                              ifelse(Scotia16_fish$Depth_mean [i] >= 4 && Scotia16_fish$Depth_mean [i] < 6, 
                                                     Scotia16_fish$Depth_bin[i] <- "4-6",
                                                     ifelse(Scotia16_fish$Depth_mean [i] >= 6 && Scotia16_fish$Depth_mean [i] < 8, 
                                                            Scotia16_fish$Depth_bin[i] <- "6-8", 
                                                            ifelse(Scotia16_fish$Depth_mean [i] >= 8 && Scotia16_fish$Depth_mean [i] < 10, 
                                                                   Scotia16_fish$Depth_bin[i] <- "8-10",
                                                                   ifelse(Scotia16_fish$Depth_mean [i] >= 10 && Scotia16_fish$Depth_mean [i] < 12, 
                                                                          Scotia16_fish$Depth_bin[i] <- "10-12",
                                                                          ifelse(Scotia16_fish$Depth_mean [i] >= 12 && Scotia16_fish$Depth_mean [i] < 14, 
                                                                                 Scotia16_fish$Depth_bin[i] <- "12-14",
                                                                                 ifelse(Scotia16_fish$Depth_mean [i] >= 14 && Scotia16_fish$Depth_mean [i] < 16, 
                                                                                        Scotia16_fish$Depth_bin[i] <- "14-16", 
                                                                                        ifelse(Scotia16_fish$Depth_mean [i] >= 16 && Scotia16_fish$Depth_mean [i] < 18, 
                                                                                               Scotia16_fish$Depth_bin[i] <- "16-18",
                                                                                               ifelse(Scotia16_fish$Depth_mean [i] >= 18 && Scotia16_fish$Depth_mean [i] < 20, 
                                                                                                      Scotia16_fish$Depth_bin[i] <- "18-20",
                                                                                                      ifelse(Scotia16_fish$Depth_mean [i] >= 20 && Scotia16_fish$Depth_mean [i] < 22, 
                                                                                                             Scotia16_fish$Depth_bin[i] <- "20-22",
                                                                                                             ifelse(Scotia16_fish$Depth_mean [i] >= 22 && Scotia16_fish$Depth_mean [i] < 24, 
                                                                                                                    Scotia16_fish$Depth_bin[i] <- "22-24",
                                                                                                                    ifelse(Scotia16_fish$Depth_mean [i] >= 24 && Scotia16_fish$Depth_mean [i] < 26, 
                                                                                                                           Scotia16_fish$Depth_bin[i] <- "24-26",
                                                                                                                           ifelse(Scotia16_fish$Depth_mean [i] >= 26 && Scotia16_fish$Depth_mean [i] < 28, 
                                                                                                                                  Scotia16_fish$Depth_bin[i] <- "26-28",
                                                                                                                                  ifelse(Scotia16_fish$Depth_mean [i] >= 28 && Scotia16_fish$Depth_mean [i] < 30, 
                                                                                                                                         Scotia16_fish$Depth_bin[i] <- "28-30",
                                                                                                                                         ifelse(Scotia16_fish$Depth_mean [i] >= 30 && Scotia16_fish$Depth_mean [i] < 32, 
                                                                                                                                                Scotia16_fish$Depth_bin[i] <- "30-32",
                                                                                                                                                ifelse(Scotia16_fish$Depth_mean [i] >= 32 && Scotia16_fish$Depth_mean [i] < 34, 
                                                                                                                                                       Scotia16_fish$Depth_bin[i] <- "32-34",
                                                                                                                                                       ifelse(Scotia16_fish$Depth_mean [i] >= 34 && Scotia16_fish$Depth_mean [i] < 36, 
                                                                                                                                                              Scotia16_fish$Depth_bin[i] <- "34-36",
                                                                                                                                                              ifelse(Scotia16_fish$Depth_mean [i] >= 36 && Scotia16_fish$Depth_mean [i] < 38, 
                                                                                                                                                                     Scotia16_fish$Depth_bin[i] <- "36-38",
                                                                                                                                                                     ifelse(Scotia16_fish$Depth_mean [i] >= 38 && Scotia16_fish$Depth_mean [i] < 40, 
                                                                                                                                                                            Scotia16_fish$Depth_bin[i] <- "38-40",
                                                                                                                                                                            ifelse(Scotia16_fish$Depth_mean [i] > 40 && Scotia16_fish$Depth_mean [i] < 42, 
                                                                                                                                                                                   Scotia16_fish$Depth_bin[i] <- "40-42", 
                                                                                                                                                                                   ifelse(Scotia16_fish$Depth_mean [i] >= 42 && Scotia16_fish$Depth_mean [i] < 44, 
                                                                                                                                                                                          Scotia16_fish$Depth_bin[i] <- "42-44", 
                                                                                                                                                                                          ifelse(Scotia16_fish$Depth_mean [i] >= 44 && Scotia16_fish$Depth_mean [i] < 46, 
                                                                                                                                                                                                 Scotia16_fish$Depth_bin[i] <- "44-46",
                                                                                                                                                                                                 ifelse(Scotia16_fish$Depth_mean [i] >= 46 && Scotia16_fish$Depth_mean [i] < 48, 
                                                                                                                                                                                                        Scotia16_fish$Depth_bin[i] <- "46-48", 
                                                                                                                                                                                                        ifelse(Scotia16_fish$Depth_mean [i] >= 48 && Scotia16_fish$Depth_mean [i] < 50, 
                                                                                                                                                                                                               Scotia16_fish$Depth_bin[i] <- "48-50",
                                                                                                                                                                                                               ifelse(Scotia16_fish$Depth_mean [i] >= 50 && Scotia16_fish$Depth_mean [i] < 52, 
                                                                                                                                                                                                                      Scotia16_fish$Depth_bin[i] <- "50-52",
                                                                                                                                                                                                                      ifelse(Scotia16_fish$Depth_mean [i] >= 52 && Scotia16_fish$Depth_mean [i] < 54, 
                                                                                                                                                                                                                             Scotia16_fish$Depth_bin[i] <- "52-54",
                                                                                                                                                                                                                             ifelse(Scotia16_fish$Depth_mean [i] >= 54 && Scotia16_fish$Depth_mean [i] < 56, 
                                                                                                                                                                                                                                    Scotia16_fish$Depth_bin[i] <- "54-56", 
                                                                                                                                                                                                                                    ifelse(Scotia16_fish$Depth_mean [i] >= 56 && Scotia16_fish$Depth_mean [i] < 58, 
                                                                                                                                                                                                                                           Scotia16_fish$Depth_bin[i] <- "56-58",
                                                                                                                                                                                                                                           ifelse(Scotia16_fish$Depth_mean [i] >= 58 && Scotia16_fish$Depth_mean [i] < 60, 
                                                                                                                                                                                                                                                  Scotia16_fish$Depth_bin[i] <- "58-60",
                                                                                                                                                                                                                                                  ifelse(Scotia16_fish$Depth_mean [i] >= 60 && Scotia16_fish$Depth_mean [i] < 62, 
                                                                                                                                                                                                                                                         Scotia16_fish$Depth_bin[i] <- "60-62",
                                                                                                                                                                                                                                                         ifelse(Scotia16_fish$Depth_mean [i] >= 62 && Scotia16_fish$Depth_mean [i] < 64, 
                                                                                                                                                                                                                                                                Scotia16_fish$Depth_bin[i] <- "62-64",
                                                                                                                                                                                                                                                                ifelse(Scotia16_fish$Depth_mean [i] >= 64 && Scotia16_fish$Depth_mean [i] < 66, 
                                                                                                                                                                                                                                                                       Scotia16_fish$Depth_bin[i] <- "64-66",
                                                                                                                                                                                                                                                                       ifelse(Scotia16_fish$Depth_mean [i] >= 66 && Scotia16_fish$Depth_mean [i] < 68, 
                                                                                                                                                                                                                                                                              Scotia16_fish$Depth_bin[i] <- "66-68",
                                                                                                                                                                                                                                                                              ifelse(Scotia16_fish$Depth_mean [i] >= 68 && Scotia16_fish$Depth_mean [i] < 70, 
                                                                                                                                                                                                                                                                                     Scotia16_fish$Depth_bin[i] <- "68-70",
                                                                                                                                                                                                                                                                                     ifelse(Scotia16_fish$Depth_mean [i] >= 70 && Scotia16_fish$Depth_mean [i] < 72, 
                                                                                                                                                                                                                                                                                            Scotia16_fish$Depth_bin[i] <- "70-72",
                                                                                                                                                                                                                                                                                            ifelse(Scotia16_fish$Depth_mean [i] >= 72 && Scotia16_fish$Depth_mean [i] < 74, 
                                                                                                                                                                                                                                                                                                   Scotia16_fish$Depth_bin[i] <- "72-74",
                                                                                                                                                                                                                                                                                                   ifelse(Scotia16_fish$Depth_mean [i] >= 74 && Scotia16_fish$Depth_mean [i] < 76, 
                                                                                                                                                                                                                                                                                                          Scotia16_fish$Depth_bin[i] <- "74-76",
                                                                                                                                                                                                                                                                                                          ifelse(Scotia16_fish$Depth_mean [i] >= 76 && Scotia16_fish$Depth_mean [i] < 78, 
                                                                                                                                                                                                                                                                                                                 Scotia16_fish$Depth_bin[i] <- "76-78",
                                                                                                                                                                                                                                                                                                                 ifelse(Scotia16_fish$Depth_mean [i] >= 78 && Scotia16_fish$Depth_mean [i] < 80, 
                                                                                                                                                                                                                                                                                                                        Scotia16_fish$Depth_bin[i] <- "78-80",
                                                                                                                                                                                                                                                                                                                        ifelse(Scotia16_fish$Depth_mean [i] >= 80 && Scotia16_fish$Depth_mean [i] < 82, 
                                                                                                                                                                                                                                                                                                                               Scotia16_fish$Depth_bin[i] <- "80-82",
                                                                                                                                                                                                                                                                                                                               ifelse(Scotia16_fish$Depth_mean [i] >= 82 && Scotia16_fish$Depth_mean [i] < 84, 
                                                                                                                                                                                                                                                                                                                                      Scotia16_fish$Depth_bin[i] <- "82-84",
                                                                                                                                                                                                                                                                                                                                      ifelse(Scotia16_fish$Depth_mean [i] >= 84 && Scotia16_fish$Depth_mean [i] < 86, 
                                                                                                                                                                                                                                                                                                                                             Scotia16_fish$Depth_bin[i] <- "84-86",
                                                                                                                                                                                                                                                                                                                                             ifelse(Scotia16_fish$Depth_mean [i] >= 86 && Scotia16_fish$Depth_mean [i] < 88, 
                                                                                                                                                                                                                                                                                                                                                    Scotia16_fish$Depth_bin[i] <- "86-88",
                                                                                                                                                                                                                                                                                                                                                    ifelse(Scotia16_fish$Depth_mean [i] >= 90 && Scotia16_fish$Depth_mean [i] < 92, 
                                                                                                                                                                                                                                                                                                                                                           Scotia16_fish$Depth_bin[i] <- "90-92",
                                                                                                                                                                                                                                                                                                                                                           ifelse(Scotia16_fish$Depth_mean [i] >= 92 && Scotia16_fish$Depth_mean [i] < 94, 
                                                                                                                                                                                                                                                                                                                                                                  Scotia16_fish$Depth_bin[i] <- "92-94",
                                                                                                                                                                                                                                                                                                                                                                  Scotia16_fish$Depth_bin[i] <- NA))))))))))))))))))))))))))))))))))))))))))))))
}
Scotia16_fish$Depth_bin <- as.factor(Scotia16_fish$Depth_bin)
Scotia16_fish$Depth_bin_RL <- reorder(Scotia16_fish$Depth_bin, Scotia16_fish$Depth_mean)

# Tides ####
View(Scotia16_fish)
dim(Scotia16_fish)
names(Scotia16_fish)
Scotia16_fish$tides <- NA

Scotia16_fish$tides[1:4] <- "ebb" 
Scotia16_fish$tides[5] <- "flood"
Scotia16_fish$tides[6:11] <- "ebb"
Scotia16_fish$tides[12:24] <-  "flood"
Scotia16_fish$tides[25:32] <- "ebb"
Scotia16_fish$tides[33:35] <- "flood"
Scotia16_fish$tides[36] <- "ebb"
Scotia16_fish$tides[37:45] <- "flood"
Scotia16_fish$tides[46:52] <- "ebb"
Scotia16_fish$tides[53:56] <- "flood"

Scotia16_fish$tides <- factor(Scotia16_fish$tides)


# Day/Night ####
str(Scotia16_fish)
Scotia16_fish$Time_M <- as.character(Scotia16_fish$Time_M)
Scotia16_fish$Time_M <- trimws(Scotia16_fish$Time_M, "l")
# 
## Day / Night
Scotia_dates<-as.POSIXct(paste(Scotia16_fish$Date_M, Scotia16_fish$Time_M,sep=" "), format="%Y%m%d %H:%M:%S", tz="UTC")
Scotia16_fish$hour <- hours(Scotia_dates)

Scotia16_fish$Timing <- NA
for(i in 1:length(Scotia16_fish$Timing)){
  if(Scotia16_fish$hour[i] > 18 | Scotia16_fish$hour[i] < 9){
    Scotia16_fish$Timing[i] <- "Night"
  } else{
    Scotia16_fish$Timing[i] <- "Day"
  }
}

Scotia16_fish$Timing <- factor(Scotia16_fish$Timing)
dim(Scotia16_fish)
## 
## View(Scotia16_fish)
Scotia16_fish.d <- Scotia16_fish[Scotia16_fish$Timing == "Day",]
Scotia16_fish.n <- Scotia16_fish[Scotia16_fish$Timing == "Night",]
dim(Scotia16_fish.d)
dim(Scotia16_fish.n)

# Stats NOT RAN #####
# MG15_34_38 <- Scotia16_fish[Scotia16_fish$Depth_bin== "34-36",]
# MG15_34_36 <-subset(Scotia16_fish, Depth_bin== "34-36")
# dim(MG15_34_36)

# sandeel E-F
Scotia16sandeel <- Scotia16_fish[Scotia16_fish$Region_class_38==Scotia16_fish$Region_class_38[223],]
dim(Scotia16sandeel)
View(Scotia16sandeel)
Scotia16sandeel$tides
wilcox.test(Depth_mean_120 ~ Timing, data= Scotia16sandeel, exact = FALSE)
wilcox.test(Depth_mean_120 ~ tides, data= Scotia16sandeel, exact = FALSE)

Scotia16sandeel.ebb <- Scotia16sandeel[Scotia16sandeel$tides == "ebb",]
dim(Scotia16sandeel.ebb)
View(Scotia16sandeel.ebb)
wilcox.test(Depth_mean_120 ~ Timing, data= Scotia16sandeel.ebb, exact = FALSE)

Scotia16sandeel.flood <- Scotia16sandeel[Scotia16sandeel$tides == "flood",]
dim(Scotia16sandeel.flood)
View(Scotia16sandeel.flood)
wilcox.test(Depth_mean_120 ~ Timing, data= Scotia16sandeel.flood, exact = FALSE)

# day night depth difference
wilcox.test(Depth_mean ~ Timing, data = Scotia16_fish, exact = FALSE)
mean(Scotia16_fish.d$Depth_mean)
mean(Scotia16_fish.n$Depth_mean)

###Import Scotiafish after internet issues ##
Scotia16_fish <-  read.csv("E:/Offline_saves/Chapter 2 - Rprocessing+analysis/R/Scotia16_fish.csv", header=TRUE)

# Crop to inner sound ####
## Subset Scotia data to inner sound only, visualised on arcGIS for speed and to check inner sound schools exist
Scotia16_fish_inner <-  Scotia16_fish[Scotia16_fish$Lat_M < 58.67,]
print(Scotia16_inner_count <- dim(Scotia16_fish_inner)[1])
summary(Scotia16_fish_inner$Depth_mean) # includes areas that are a bit too deep

Scotia16_fish_inner <-  Scotia16_fish_inner[Scotia16_fish_inner$Lon_M > -3.157199 & Scotia16_fish_inner$Lon_M  < -3.085736,]

summary(Scotia16_fish_inner$Depth_mean) # includes areas that are a bit too deep

print(Scotia16_inner_count <- dim(Scotia16_fish_inner)[1])
summary(Scotia16_fish_inner$Depth_mean) # includes areas that are a bit too deep
View(Scotia16_fish_inner)
dim(Scotia16_fish_inner)
summary(Scotia16_fish_inner$Timing)

Scotia16_fish_inner.d <- Scotia16_fish_inner[Scotia16_fish_inner$Timing == "Day",]
Scotia16_fish_inner.n <- Scotia16_fish_inner[Scotia16_fish_inner$Timing == "Night",]
Scotia16_fish_inner.d$tides <- factor(Scotia16_fish_inner.d$tides) 
Scotia16_fish_inner.n$tides <- factor(Scotia16_fish_inner.n$tides) 

# Graphing it #####
rev(levels(Scotia16_fish$Depth_bin_RL))
x_axis_cats_Scotia <- c("80-82", "78-80", "76-78", "74-76", "72-74", 
                        "70-72", "68-70", "66-68", "64-66", "62-64",
                        "60-62", "58-60", "56-58", "54-56", "52-54",
                        "50-52", "48-50", "46-48", "44-46", "42-44",
                        "40-42", "38-40", "36-38", "34-36", "32-34",
                        "30-32", "28-30", "26-28", "24-26", "22-24",
                        "20-22", "18-20", "16-18", "14-16", "12-14", 
                        "10-12", "8-10", "6-8", "4-6", "2-4")

x_axis_cats_Scotia_cut <- c("80-82", " ", " ", " ", " ", 
                            "70-72", " ", "", " ", " ",
                            "60-62", " ", " ", " ", " ",
                            "50-52", " ", " ", " ", " ",
                            "40-42", " ", " ", " ", " ",
                            "30-32", " ", " ", " ", " ",
                            "20-22", " ", " ", " ", " ", 
                            "10-12", " ", " ", " ", "2-4 ")


# All fish data
Scotia16_All <- ggplot(Scotia16_fish, aes(x=Depth_bin_RL))
Scotia16_All +
  labs(x=" ", y= "Count", title = "Scotia 2016") +
  geom_bar(aes(fill = Timing))+
  scale_x_discrete(limits = x_axis_cats_Scotia, drop=FALSE) +
  scale_y_reverse() +
  ylim(225,0)+
  coord_flip()

# All fish data - flood-ebb, day-night
# Day
Scotia16_d <- ggplot(Scotia16_fish.d, aes(x=Depth_bin_RL))
Scotia16_d +
  labs(x=" ", y= "Count", title = "Scotia 2016 Day", fill="Tidal state") +
  geom_bar(aes(fill = tides))+ 
  theme(text=element_text(size=13))+
  scale_x_discrete(limits = x_axis_cats_Scotia, labels = x_axis_cats_Scotia_cut, drop=FALSE) +
  ylim(0,75)+
  coord_flip()

# Night
Scotia16_n <- ggplot(Scotia16_fish.n, aes(x=Depth_bin_RL))
Scotia16_n +
  labs(x="Depth bin (m)", y= "Count", title = "Scotia 2016 Night", fill="Tidal state") +
  geom_bar(aes(fill = tides))+ 
  theme(text=element_text(size=13))+
  scale_x_discrete(limits = x_axis_cats_Scotia, labels = x_axis_cats_Scotia_cut, drop=FALSE) +
  scale_y_reverse()+
  ylim(75,0)+
  coord_flip()

#  Inner sound only
# x_axis_cats <- c("38-40" ,"36-38" , "36-24" , "32-34" , "30-32" , "28-30" , "26-28" , "24-26" , "22-24", 
#                  "20-22" , "18-20" , "16-18",  "14-16" , "12-14" , "10-12" , "8-10" ,  "6-8" ,   "4-6" , "2-4")
Scotia16_inner <- ggplot(Scotia16_fish_inner, aes(x=Depth_bin_RL))
Scotia16_inner +
  labs(x=" ", y= "Count", title = "Scotia 2016 Inner Sound") +
  geom_bar(aes(fill = tides))+
  scale_x_discrete(limits = x_axis_cats_Scotia, drop=FALSE) +
  scale_y_reverse() +
  ylim(75,0)+
  coord_flip()
# same graph code used to check ebb flood but only flood schools detected in inner sound 
#                                                       for 2018 so not graphed in paper
save.image()

# Full graph #####

ggarrange(Meygen15_depth_all, Scotia16_inner, 
          Meygen15.n, Meygen15.d,
          Scotia16_n, Scotia16_d, ncol = 2, nrow = 3)

Meygen15_depth_all
Meygen15.n
Meygen15.d
Scotia16_All
Scotia16_d
Scotia16_n
Scotia16_inner

?ggarrange

# Exports ####
write.csv(Meygen15_fish,"C:/Users/james/Desktop/Meygen15_fish.csv")
write.csv(Scotia18_fish,"C:/Users/james/Desktop/Scotia18_fish.csv")
write.csv(Scotia16_fish,"C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2016/EK60/Output/Scotia16_fish.csv")

############### Scotia 2018 #######################################
# Dataset ####
# dataset came from Ana and she has already done equivalent of inner joins
Scotia18_fishEV <- read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2018/fish_EK60_2018.csv", header = TRUE)
Scotia18_sandeelEV <- read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2018/sandeels_EK60_2018.csv", header = TRUE)
Scotia18_Allfreqs <- rbind(Scotia18_fishEV,Scotia18_sandeelEV)

View(Scotia18_Allfreqs)

dim(Scotia18_Allfreqs)
names(Scotia18_Allfreqs)[1:51]
# We want
# "Date_M","Time_M", "Lat_M_38", "Lon_M_38", "Region_class_38"
# # "Sv_mean_38", "NASC_38", "Sv_max_38", "Sv_min_38",
# # "Sv_mean_120", "NASC_120", "Sv_max_120", "Sv_min_120",
# # "Sv_mean_200", "NASC_200", "Sv_max_200", "Sv_min_200"
# # "Height_mean_38","Height_mean_120","Height_mean_200",
# # "fish_depth","Depth_mean_120","Depth_mean_200"
# # "Corrected_MVBS_38","Corrected_MVBS_120","Corrected_MVBS_200"

cutlist <- c("Date_M_120","Time_M_120", "Region_class_120" ,"Region_class_120", "Lat_M_120", "Lon_M_120",
              "NASC_120", "Sv_mean_120", "Sv_max_120", "Sv_min_120", 
              "Sv_mean_200", "Sv_max_200", "Sv_min_200", 
              "Height_mean_38","Height_mean_120","Height_mean_200",
              "fish_depth","Depth_mean_120","Depth_mean_200",
              "Corrected_MVBS_38","Corrected_MVBS_120","Corrected_MVBS_200")
# 
Scotia18_Allfreqs_cut <- Scotia18_Allfreqs[,1:51]
View(Scotia18_Allfreqs_cut)

# Data wrangling #####
Scotia18_fish <- Scotia18_Allfreqs_cut
View(Scotia18_fish)

# Depth bins ####
Scotia18_fish

summary(Scotia18_fish$Depth_mean_120) # 68.5527
Scotia18_fish$Depth_mean_120 <- Scotia18_fish$Depth_mean_120 + 12 #Due to Scotia build detections start at 12metres depth
Scotia18_fish$Depth_bin <- NA
for(i in 1:length(Scotia18_fish$Depth_mean_120)){
  Scotia18_fish$Depth_bin[i] <- ifelse(Scotia18_fish$Depth_mean_120[i] > 0 && Scotia18_fish$Depth_mean_120[i] < 2, 
                                             Scotia18_fish$Depth_bin[i] <- "0-2", 
                                             ifelse(Scotia18_fish$Depth_mean_120[i] >= 2 && Scotia18_fish$Depth_mean_120[i] < 4, 
                                                    Scotia18_fish$Depth_bin[i] <- "2-4", 
                                                    ifelse(Scotia18_fish$Depth_mean_120[i] >= 4 && Scotia18_fish$Depth_mean_120[i] < 6, 
                                                           Scotia18_fish$Depth_bin[i] <- "4-6",
                                                           ifelse(Scotia18_fish$Depth_mean_120[i] >= 6 && Scotia18_fish$Depth_mean_120[i] < 8, 
                                                                  Scotia18_fish$Depth_bin[i] <- "6-8", 
                                                                  ifelse(Scotia18_fish$Depth_mean_120[i] >= 8 && Scotia18_fish$Depth_mean_120[i] < 10, 
                                                                         Scotia18_fish$Depth_bin[i] <- "8-10",
                                                                         ifelse(Scotia18_fish$Depth_mean_120[i] >= 10 && Scotia18_fish$Depth_mean_120[i] < 12, 
                                                                                Scotia18_fish$Depth_bin[i] <- "10-12",
                                                                                ifelse(Scotia18_fish$Depth_mean_120[i] >= 12 && Scotia18_fish$Depth_mean_120[i] < 14, 
                                                                                       Scotia18_fish$Depth_bin[i] <- "12-14",
                                                                                       ifelse(Scotia18_fish$Depth_mean_120[i] >= 14 && Scotia18_fish$Depth_mean_120[i] < 16, 
                                                                                              Scotia18_fish$Depth_bin[i] <- "14-16", 
                                                                                              ifelse(Scotia18_fish$Depth_mean_120[i] >= 16 && Scotia18_fish$Depth_mean_120[i] < 18, 
                                                                                                     Scotia18_fish$Depth_bin[i] <- "16-18",
                                                                                                     ifelse(Scotia18_fish$Depth_mean_120[i] >= 18 && Scotia18_fish$Depth_mean_120[i] < 20, 
                                                                                                            Scotia18_fish$Depth_bin[i] <- "18-20",
                                                                                                            ifelse(Scotia18_fish$Depth_mean_120[i] >= 20 && Scotia18_fish$Depth_mean_120[i] < 22, 
                                                                                                                   Scotia18_fish$Depth_bin[i] <- "20-22",
                                                                                                                   ifelse(Scotia18_fish$Depth_mean_120[i] >= 22 && Scotia18_fish$Depth_mean_120[i] < 24, 
                                                                                                                          Scotia18_fish$Depth_bin[i] <- "22-24",
                                                                                                                          ifelse(Scotia18_fish$Depth_mean_120[i] >= 24 && Scotia18_fish$Depth_mean_120[i] < 26, 
                                                                                                                                 Scotia18_fish$Depth_bin[i] <- "24-26",
                                                                                                                                 ifelse(Scotia18_fish$Depth_mean_120[i] >= 26 && Scotia18_fish$Depth_mean_120[i] < 28, 
                                                                                                                                        Scotia18_fish$Depth_bin[i] <- "26-28",
                                                                                                                                        ifelse(Scotia18_fish$Depth_mean_120[i] >= 28 && Scotia18_fish$Depth_mean_120[i] < 30, 
                                                                                                                                               Scotia18_fish$Depth_bin[i] <- "28-30",
                                                                                                                                               ifelse(Scotia18_fish$Depth_mean_120[i] >= 30 && Scotia18_fish$Depth_mean_120[i] < 32, 
                                                                                                                                                      Scotia18_fish$Depth_bin[i] <- "30-32",
                                                                                                                                                      ifelse(Scotia18_fish$Depth_mean_120[i] >= 32 && Scotia18_fish$Depth_mean_120[i] < 34, 
                                                                                                                                                             Scotia18_fish$Depth_bin[i] <- "32-34",
                                                                                                                                                             ifelse(Scotia18_fish$Depth_mean_120[i] >= 34 && Scotia18_fish$Depth_mean_120[i] < 36, 
                                                                                                                                                                    Scotia18_fish$Depth_bin[i] <- "34-36",
                                                                                                                                                                    ifelse(Scotia18_fish$Depth_mean_120[i] >= 36 && Scotia18_fish$Depth_mean_120[i] < 38, 
                                                                                                                                                                           Scotia18_fish$Depth_bin[i] <- "36-38",
                                                                                                                                                                           ifelse(Scotia18_fish$Depth_mean_120[i] >= 38 && Scotia18_fish$Depth_mean_120[i] < 40, 
                                                                                                                                                                                  Scotia18_fish$Depth_bin[i] <- "38-40",
                                                                                                                                                                                  ifelse(Scotia18_fish$Depth_mean_120[i] > 40 && Scotia18_fish$Depth_mean_120[i] < 42, 
                                                                                                                                                                                         Scotia18_fish$Depth_bin[i] <- "40-42", 
                                                                                                                                                                                         ifelse(Scotia18_fish$Depth_mean_120[i] >= 42 && Scotia18_fish$Depth_mean_120[i] < 44, 
                                                                                                                                                                                                Scotia18_fish$Depth_bin[i] <- "42-44", 
                                                                                                                                                                                                ifelse(Scotia18_fish$Depth_mean_120[i] >= 44 && Scotia18_fish$Depth_mean_120[i] < 46, 
                                                                                                                                                                                                       Scotia18_fish$Depth_bin[i] <- "44-46",
                                                                                                                                                                                                       ifelse(Scotia18_fish$Depth_mean_120[i] >= 46 && Scotia18_fish$Depth_mean_120[i] < 48, 
                                                                                                                                                                                                              Scotia18_fish$Depth_bin[i] <- "46-48", 
                                                                                                                                                                                                              ifelse(Scotia18_fish$Depth_mean_120[i] >= 48 && Scotia18_fish$Depth_mean_120[i] < 50, 
                                                                                                                                                                                                                     Scotia18_fish$Depth_bin[i] <- "48-50",
                                                                                                                                                                                                                     ifelse(Scotia18_fish$Depth_mean_120[i] >= 50 && Scotia18_fish$Depth_mean_120[i] < 52, 
                                                                                                                                                                                                                            Scotia18_fish$Depth_bin[i] <- "50-52",
                                                                                                                                                                                                                            ifelse(Scotia18_fish$Depth_mean_120[i] >= 52 && Scotia18_fish$Depth_mean_120[i] < 54, 
                                                                                                                                                                                                                                   Scotia18_fish$Depth_bin[i] <- "52-54",
                                                                                                                                                                                                                                   ifelse(Scotia18_fish$Depth_mean_120[i] >= 54 && Scotia18_fish$Depth_mean_120[i] < 56, 
                                                                                                                                                                                                                                          Scotia18_fish$Depth_bin[i] <- "54-56", 
                                                                                                                                                                                                                                          ifelse(Scotia18_fish$Depth_mean_120[i] >= 56 && Scotia18_fish$Depth_mean_120[i] < 58, 
                                                                                                                                                                                                                                                 Scotia18_fish$Depth_bin[i] <- "56-58",
                                                                                                                                                                                                                                                 ifelse(Scotia18_fish$Depth_mean_120[i] >= 58 && Scotia18_fish$Depth_mean_120[i] < 60, 
                                                                                                                                                                                                                                                        Scotia18_fish$Depth_bin[i] <- "58-60",
                                                                                                                                                                                                                                                        ifelse(Scotia18_fish$Depth_mean_120[i] >= 60 && Scotia18_fish$Depth_mean_120[i] < 62, 
                                                                                                                                                                                                                                                               Scotia18_fish$Depth_bin[i] <- "60-62",
                                                                                                                                                                                                                                                               ifelse(Scotia18_fish$Depth_mean_120[i] >= 62 && Scotia18_fish$Depth_mean_120[i] < 64, 
                                                                                                                                                                                                                                                                      Scotia18_fish$Depth_bin[i] <- "62-64",
                                                                                                                                                                                                                                                                      ifelse(Scotia18_fish$Depth_mean_120[i] >= 64 && Scotia18_fish$Depth_mean_120[i] < 66, 
                                                                                                                                                                                                                                                                             Scotia18_fish$Depth_bin[i] <- "64-66",
                                                                                                                                                                                                                                                                             ifelse(Scotia18_fish$Depth_mean_120[i] >= 66 && Scotia18_fish$Depth_mean_120[i] < 68, 
                                                                                                                                                                                                                                                                                    Scotia18_fish$Depth_bin[i] <- "66-68",
                                                                                                                                                                                                                                                                                    ifelse(Scotia18_fish$Depth_mean_120[i] >= 68 && Scotia18_fish$Depth_mean_120[i] < 70, 
                                                                                                                                                                                                                                                                                           Scotia18_fish$Depth_bin[i] <- "68-70",
                                                                                                                                                                                                                                                                                           ifelse(Scotia18_fish$Depth_mean_120[i] >= 70 && Scotia18_fish$Depth_mean_120[i] < 72, 
                                                                                                                                                                                                                                                                                                  Scotia18_fish$Depth_bin[i] <- "70-72",
                                                                                                                                                                                                                                                                                                  ifelse(Scotia18_fish$Depth_mean_120[i] >= 72 && Scotia18_fish$Depth_mean_120[i] < 74, 
                                                                                                                                                                                                                                                                                                         Scotia18_fish$Depth_bin[i] <- "72-74",
                                                                                                                                                                                                                                                                                                         ifelse(Scotia18_fish$Depth_mean_120[i] >= 74 && Scotia18_fish$Depth_mean_120[i] < 76, 
                                                                                                                                                                                                                                                                                                                Scotia18_fish$Depth_bin[i] <- "74-76",
                                                                                                                                                                                                                                                                                                                ifelse(Scotia18_fish$Depth_mean_120[i] >= 76 && Scotia18_fish$Depth_mean_120[i] < 78, 
                                                                                                                                                                                                                                                                                                                       Scotia18_fish$Depth_bin[i] <- "76-78",
                                                                                                                                                                                                                                                                                                                       ifelse(Scotia18_fish$Depth_mean_120[i] >= 78 && Scotia18_fish$Depth_mean_120[i] < 80, 
                                                                                                                                                                                                                                                                                                                              Scotia18_fish$Depth_bin[i] <- "78-80",
                                                                                                                                                                                                                                                                                                                              ifelse(Scotia18_fish$Depth_mean_120[i] >= 80 && Scotia18_fish$Depth_mean_120[i] < 82, 
                                                                                                                                                                                                                                                                                                                                     Scotia18_fish$Depth_bin[i] <- "80-82",
                                                                                                                                                                                                                                                                                                                                     ifelse(Scotia18_fish$Depth_mean_120[i] >= 82 && Scotia18_fish$Depth_mean_120[i] < 84, 
                                                                                                                                                                                                                                                                                                                                            Scotia18_fish$Depth_bin[i] <- "82-84",
                                                                                                                                                                                                                                                                                                                                            ifelse(Scotia18_fish$Depth_mean_120[i] >= 84 && Scotia18_fish$Depth_mean_120[i] < 86, 
                                                                                                                                                                                                                                                                                                                                                   Scotia18_fish$Depth_bin[i] <- "84-86",
                                                                                                                                                                                                                                                                                                                                                   ifelse(Scotia18_fish$Depth_mean_120[i] >= 86 && Scotia18_fish$Depth_mean_120[i] < 88, 
                                                                                                                                                                                                                                                                                                                                                          Scotia18_fish$Depth_bin[i] <- "86-88",
                                                                                                                                                                                                                                                                                                                                                          ifelse(Scotia18_fish$Depth_mean_120[i] >= 90 && Scotia18_fish$Depth_mean_120[i] < 92, 
                                                                                                                                                                                                                                                                                                                                                                 Scotia18_fish$Depth_bin[i] <- "90-92",
                                                                                                                                                                                                                                                                                                                                                                 ifelse(Scotia18_fish$Depth_mean_120[i] >= 92 && Scotia18_fish$Depth_mean_120[i] < 94, 
                                                                                                                                                                                                                                                                                                                                                                        Scotia18_fish$Depth_bin[i] <- "92-94",
                                                                                                                                                                                  Scotia18_fish$Depth_bin[i] <- NA))))))))))))))))))))))))))))))))))))))))))))))
}
Scotia18_fish$Depth_bin <- as.factor(Scotia18_fish$Depth_bin)
Scotia18_fish$Depth_bin_RL <- reorder(Scotia18_fish$Depth_bin, Scotia18_fish$Depth_mean_120)

# Tides ####
View(Scotia18_fish)
dim(Scotia18_fish)
names(Scotia18_fish)
Scotia18_fish$tides <- NA

Scotia18_fish$tides[1:42] <- "ebb" 
Scotia18_fish$tides[43:65] <- "flood"
Scotia18_fish$tides[66:90] <- "ebb"
Scotia18_fish$tides[91:100] <-  "flood"
Scotia18_fish$tides[101:105] <- "ebb"
Scotia18_fish$tides[106:109] <- "flood"
Scotia18_fish$tides[110:123] <- "ebb"
Scotia18_fish$tides[124:132] <- "flood"
Scotia18_fish$tides[133:157] <- "ebb"
Scotia18_fish$tides[158:170] <- "flood"
Scotia18_fish$tides[171:172] <- "ebb"
Scotia18_fish$tides[173:211] <- "flood"
Scotia18_fish$tides[212:222] <- "ebb"     # start of sandeels section
Scotia18_fish$tides[223:226] <- "ebb"
Scotia18_fish$tides[227:231] <- "flood"
Scotia18_fish$tides[232]     <- "ebb"
Scotia18_fish$tides[233]     <- "flood"
Scotia18_fish$tides[234:236] <- "ebb"
Scotia18_fish$tides[237:245] <- "flood"
Scotia18_fish$tides[246:252] <- "ebb"
Scotia18_fish$tides[253:255] <- "flood"
Scotia18_fish$tides[256]     <- "ebb"
Scotia18_fish$tides[257:263] <- "flood"
Scotia18_fish$tides[264:269] <- "ebb"

Scotia18_fish$tides <- factor(Scotia18_fish$tides)

# Day/Night ####
# Dont need this as Anas dataset has hour column
str(Scotia18_fish)
Scotia18_fish$Time_M_120 <- as.character(Scotia18_fish$Time_M_120)
Scotia18_fish$Time_M_120 <- trimws(Scotia18_fish$Time_M_120, "l")
# 
## Day / Night
Scotia_dates<-as.POSIXct(paste(Scotia18_fish$Date_M, Scotia18_fish$Time_M,sep=" "), format="%Y%m%d %H:%M:%S", tz="UTC")
Scotia18_fish$hour <- hours(Scotia_dates)

Scotia18_fish$Timing <- NA
for(i in 1:length(Scotia18_fish$Timing)){
  if(Scotia18_fish$hour[i] > 18 | Scotia18_fish$hour[i] < 9){
    Scotia18_fish$Timing[i] <- "Night"
  } else{
    Scotia18_fish$Timing[i] <- "Day"
  }
}

Scotia18_fish$Timing <- factor(Scotia18_fish$Timing)

## 
## View(Scotia18_fish)
Scotia18_fish.d <- Scotia18_fish[Scotia18_fish$Timing == "Day",]
Scotia18_fish.n <- Scotia18_fish[Scotia18_fish$Timing == "Night",]

# Stats #####
# MG15_34_38 <- Scotia18_fish[Scotia18_fish$Depth_bin== "34-36",]
# MG15_34_36 <-subset(Scotia18_fish, Depth_bin== "34-36")
# dim(MG15_34_36)

# sandeel E-F
Scotia18sandeel <- Scotia18_fish[Scotia18_fish$Region_class_120==Scotia18_fish$Region_class_120[223],]
dim(Scotia18sandeel) 
View(Scotia18sandeel)
Scotia18sandeel$tides
wilcox.test(Depth_mean_120 ~ Timing, data= Scotia18sandeel, exact = FALSE)
wilcox.test(Depth_mean_120 ~ tides, data= Scotia18sandeel, exact = FALSE)

Scotia18sandeel.ebb <- Scotia18sandeel[Scotia18sandeel$tides == "ebb",]
dim(Scotia18sandeel.ebb)
View(Scotia18sandeel.ebb)
wilcox.test(Depth_mean_120 ~ Timing, data= Scotia18sandeel.ebb, exact = FALSE)

Scotia18sandeel.flood <- Scotia18sandeel[Scotia18sandeel$tides == "flood",]
dim(Scotia18sandeel.flood)
View(Scotia18sandeel.flood)
wilcox.test(Depth_mean_120 ~ Timing, data= Scotia18sandeel.flood, exact = FALSE)

# day night depth difference
wilcox.test(Depth_mean_120 ~ Timing, data = Scotia18_fish, exact = FALSE)
mean(Scotia18_fish.d$Depth_mean_120)
mean(Scotia18_fish.n$Depth_mean_120)


# Norht south #
Scotia18_fish_Northern <- Scotia18_fish[Scotia18_fish$Lat_S_120 > 58.682852,]
dim(Scotia18_fish_Northern)


# Crop to inner sound ####
## Subset Scotia data to inner sound only, visualised on arcGIS for speed and to check inner sound schools exist
Scotia18_fish_inner <-  Scotia18_fish[Scotia18_fish$Lat_M_120 < 58.67,]
print(Scotia18_inner_count <- dim(Scotia18_fish_inner)[1])
summary(Scotia18_fish_inner$Depth_mean_120) # includes areas that are a bit too deep

Scotia18_fish_inner <-  Scotia18_fish_inner[Scotia18_fish_inner$Lon_M_120 > -3.157199 & Scotia18_fish_inner$Lon_M_120  < -3.085736,]

summary(Scotia18_fish_inner$Depth_mean_120) # includes areas that are a bit too deep

print(Scotia18_inner_count <- dim(Scotia18_fish_inner)[1])
summary(Scotia18_fish_inner$Depth_mean_120) # includes areas that are a bit too deep
View(Scotia18_fish_inner)
dim(Scotia18_fish_inner)

Scotia18_fish_inner.d <- Scotia18_fish_inner[Scotia18_fish_inner$Timing == "Day",]
Scotia18_fish_inner.n <- Scotia18_fish_inner[Scotia18_fish_inner$Timing == "Night",]


# Graphing it #####
rev(levels(Scotia18_fish$Depth_bin_RL))
# x_axis_cats_Scotia <- c("80-82", "78-80", "76-78", "74-76", "72-74", "70-72", "68-70", "66-68","62-64","54-56","52-54","50-52",
#                         "48-50","46-48","44-46","42-44","40-42","38-40" ,"36-38" , "34-36" , "32-34" , "30-32" , "28-30" , "26-28" ,
#                        "24-26" , "22-24", "20-22" , "18-20" , "16-18",  "14-16" , "12-14" , "10-12" , "8-10" ,  "6-8" ,   "4-6" , "2-4")
?reorder
# All fish data
Scotia18_All <- ggplot(Scotia18_fish, aes(x=Depth_bin_RL))
Scotia18_All +
  labs(x=" ", y= "Count", title = "Scotia 2018") +
  geom_bar(aes(fill = Timing))+
  scale_x_discrete(limits = x_axis_cats_Scotia, drop=FALSE) +
  scale_y_reverse() +
  ylim(325,0)+
  coord_flip()

# All fish data - flood-ebb, day-night
# Day
Scotia18_d <- ggplot(Scotia18_fish.d, aes(x=Depth_bin_RL))
Scotia18_d +
  labs(x=" ", y= "Count", title = "Scotia 2018 Day") +
  geom_bar(aes(fill = tides))+ 
  theme(text=element_text(size=13))+
  scale_x_discrete(limits = x_axis_cats_Scotia, labels = x_axis_cats_Scotia_cut, drop=FALSE) +
  ylim(0,75)+
  coord_flip()

# Night
Scotia18_n <- ggplot(Scotia18_fish.n, aes(x=Depth_bin_RL))
Scotia18_n +
  labs(x=" ", y= "Count", title = "Scotia 2018 Night") +
  geom_bar(aes(fill = tides))+ 
  theme(text=element_text(size=13))+
  scale_x_discrete(limits = x_axis_cats_Scotia, labels = x_axis_cats_Scotia_cut, drop=FALSE) +
  ylim(75,0)+
  coord_flip()

#  Inner sound only
# x_axis_cats <- c("38-40" ,"36-38" , "36-24" , "32-34" , "30-32" , "28-30" , "26-28" , "24-26" , "22-24", 
#                  "20-22" , "18-20" , "16-18",  "14-16" , "12-14" , "10-12" , "8-10" ,  "6-8" ,   "4-6" , "2-4")
Scotia18_inner <- ggplot(Scotia18_fish_inner, aes(x=Depth_bin_RL))
Scotia18_inner +
  labs(x=" ", y= "Count", title = "Scotia 2018 Inner Sound") +
  geom_bar(aes(fill = Timing))+
  scale_x_discrete(limits = x_axis_cats_Scotia, drop=FALSE) +
  scale_y_reverse() +
  ylim(325,0)+
  coord_flip()
# same graph code used to check ebb flood but only flood schools detected
save.image()

# Full graph #####

ggarrange(Meygen15_depth_all, Scotia18_inner, 
          Meygen15.n, Meygen15.d,
          Scotia18_n, Scotia18_d, ncol = 2, nrow = 3)

Meygen15_depth_all
Meygen15.n
Meygen15.d
Scotia18_All
Scotia18_d
Scotia18_n
Scotia18_inner

?ggarrange

######################## Exports #################################
write.csv(Meygen15_fish,"C:/Users/james/Desktop/Meygen15_fish.csv")
write.csv(Scotia18_fish,"C:/Users/james/Desktop/Scotia18_fish.csv")
write.csv(Scotia16_fish,"C:/Users/james/Desktop/Scotia16_fish.csv")

# Fish summaries
summary(Meygen15_fish$Depth_bin_RL)
# 32-34 30-32 28-30 26-28 24-26 22-24 20-22 18-20 16-18 14-16 12-14 10-12  8-10   6-8   4-6   2-4 
# 3     4     7     7     12    15    36    38    49    31    39    50     133    303   259   16

summary(factor(Scotia16_fish$Depth_bin_RL))
# 14-16 16-18 18-20 20-22 22-24 24-26 26-28 28-30 30-32 32-34 34-36 36-38 40-42 42-44 44-46 56-58 58-60 62-64 68-70 76-78 
# 1     1     4     2     3     2     6     9     3     6     2     1     2     6     2     1     2     1     1     1

summary(Scotia18_fish$Depth_bin_RL)
# 14-16 16-18 18-20 20-22 24-26 26-28 28-30 30-32 32-34 34-36 36-38 38-40 40-42 42-44 50-52 52-54 54-56 60-62 62-64 66-68 68-70 70-72 72-74 74-76 76-78 78-80 80-82 
# 98    61     9     6     1     2     3     6     8     4    11     4     7     1     2     4     5     1     1     4     3     2     4     7    12     2     1 

### Sample sizes #####
# 2015
summary(Meygen15_fish$Timing)
summary(Meygen15_fish.d$tides)
summary(Meygen15_fish.n$tides)

# 2016
summary(Scotia16_fish$Timing)
summary(Scotia16_fish.d$tides)
summary(Scotia16_fish.n$tides)

summary(Scotia16_fish_inner$Timing)
summary(Scotia16_fish_inner.d$tides)
summary(Scotia16_fish_inner.n$tides)

#2018
summary(Scotia18_fish$Timing)
summary(Scotia18_fish.d$tides)
summary(Scotia18_fish.n$tides)

summary(Scotia18_fish_inner$Timing)
summary(Scotia18_fish_inner.d$tides)
summary(Scotia18_fish_inner.n$tides)


################################ Hydrodynamic variables ###########
### Import FLOWBEC 2015 ####
# Processed by Ana Couto May 2020
ADCP15 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/ADCP/adcp_2015.csv", header=T)
ADCP15 <- na.omit(ADCP15)
View(ADCP15)
summary(ADCP15$vel)

ADCP15_full<-data.frame(matrix(NA,77103,3))
# View(Area_full)
colnames(ADCP15_full) <- c("Area","Value", "Method")

head(ADCP15_full)

dim(ADCP15_full)

ADCP15_full$Area<- "Area1"

ADCP15_full$Value <- ADCP15$vel

ADCP15_full$Method <- "ADCP 2015" 

View(FV_Area_full)

##### Select areas from Scotia 2018 data ####
Scotia_hydro <- read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2018/birds_2018_circular_despiked.csv", header=TRUE)
dim(Scotia_hydro)
str(Scotia_hydro)
# Ana made mistake on column headings so needs corrected other wise will drive me insane
colnames(Scotia_hydro)[3]<- "long"
colnames(Scotia_hydro)[4]<- "lat"
str(Scotia_hydro)

# Subset area 1
Scotia_hydro_A1 <- Scotia_hydro[Scotia_hydro$lat > -3.142187 & Scotia_hydro$lat < -3.131853 & Scotia_hydro$lon > 58.655385 & Scotia_hydro$lon < 58.660772,]
dim(Scotia_hydro_A1)

# Subset area 2
Scotia_hydro_A2 <- Scotia_hydro[Scotia_hydro$lat > -3.106709 & Scotia_hydro$lat < -3.086666 & Scotia_hydro$lon > 58.656902 & Scotia_hydro$lon < 58.665468,]
dim(Scotia_hydro_A2)

# Subset area 3
Scotia_hydro_A3 <- Scotia_hydro[Scotia_hydro$lat > -3.086000 & Scotia_hydro$lat < -3.075666 & Scotia_hydro$lon > 58.688613 & Scotia_hydro$lon < 58.694158,]
dim(Scotia_hydro_A3)

# Subset area 4
Scotia_hydro_A4 <- Scotia_hydro[Scotia_hydro$lat > -3.140000 & Scotia_hydro$lat < -3.129666 & Scotia_hydro$lon > 58.707613 & Scotia_hydro$lon < 58.713000,]
dim(Scotia_hydro_A4)

# Subset area 5
Scotia_hydro_A5 <- Scotia_hydro[Scotia_hydro$lat > -3.167000 & Scotia_hydro$lat < -3.156666 & Scotia_hydro$lon > 58.672613 & Scotia_hydro$lon < 58.678000,]
dim(Scotia_hydro_A5)

S_Area1 <- Scotia_hydro_A1
S_Area2 <- Scotia_hydro_A2
S_Area3 <- Scotia_hydro_A3
S_Area4 <- Scotia_hydro_A4
S_Area5 <- Scotia_hydro_A5


#### Processing ####
# S_Area1
dim(S_Area1) #3789   31
# S_Area2
dim(S_Area2) #7919   31
# S_Area3
dim(S_Area3) #2609   31
# S_Area4
dim(S_Area4) #2123   31
# S_Area5
dim(S_Area5) #3020   31
# 
# S_Areas_mean <- c(mean(Area1means),mean(Area2means),mean(Area3means),mean(Area4means),mean(Area5means)) 
# S_Areas_var <- c(var(Area1means),var(Area2means),var(Area3means),var(Area4means),var(Area5means)) 
# S_Area <- c(1:5)
# S_Areas_mean
# S_Areas_var

S_Area_full<-data.frame(matrix(NA,19460,3))
View(S_Area_full)
dim(S_Area_full)
colnames(S_Area_full) <- c("Area","Value","Method")

S_Area_full$Area[1:3789] <- "Area1"
S_Area_full$Value[1:3789] <- S_Area1$velocity
S_Area_full$Area[3790:11708] <- "Area2"
S_Area_full$Value[3790:11708] <- S_Area2$velocity
S_Area_full$Area[11709:14317] <- "Area3"
S_Area_full$Value[11709:14317] <- S_Area3$velocity
S_Area_full$Area[14318:16440] <- "Area4"
S_Area_full$Value[14318:16440] <- S_Area4$velocity
S_Area_full$Area[16441:19460] <- "Area5"
S_Area_full$Value[16441:19460] <- S_Area5$velocity
S_Area_full$Method <- "Scotia 2018"

View(S_Area_full)

## Stats ####
S_Area <- c(1:5)
S_Areas_mean
S_Areas_var

##### Select areas from FVCOM 2015####
#### Area 1 ####
### Velocity ##
FV_Area1 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area1_FLOWBEC15.csv", header=F)
## Calculate mean, median and variance #
View(FV_Area1)
dim(FV_Area1)
#16

# # Extract lats and longs
# FV_Area1_lat <- FV_Area1[,c(1154)]
# FV_Area1_lon <- FV_Area1[,c(1155)]
# # Remove lat and lon data from the Area1 dataset
# FV_Area1 <- FV_Area1[,c(1:1153)]

# Depth bin velocity means
FV_Area1means <- rep(NA,dim(FV_Area1)[1])
for(i in 1:dim(FV_Area1)[1]){
  FV_Area1means[i] <- mean(FV_Area1[,i])
}
#print(FV_Area1means)

# # Time bin velocity means
# FV_Area1means <- rep(NA,dim(FV_Area1)[2])
# for(i in 1:dim(FV_Area1)[2]){
#   FV_Area1means[i] <- mean(FV_Area1[,i])
# }
# print(FV_Area1means)
# summary(FV_Area1means)

# ### MaxSheer ## AFTER HITTING ISSUES DECIDED I ONLY NEED VELOCITY AND CAN ARGUE IT
# View(FV_Area1)
# 
# # convert to matrix for one to one ness with new empty data frame
# FV_Area1_MS <- as.matrix(FV_Area1)
# #View(FV_Area1_MS)
# 
# # Calculate max sheers for each depth bin in new matrix 
# FA1_MS.mat <- matrix(NA, nrow = dim(FV_Area1_MS)[1], ncol = dim(FV_Area1_MS)[2])
# #View(FA1_MS.mat)
# for(j in 1:dim(FV_Area1_MS)[2]){
#   for(i in 1:dim(FV_Area1_MS)[1]){
#     FA1_MS.mat[i,j] <- (FV_Area1[i,j] - FV_Area1[i+1,j+1])/2  # assumes depth bins of 2metres - check this (it varies slightly over time) -WRONG
#   }
# }
# #View(FA1_MS.mat)
# 
# # Convert to dataframe
# FA1_MS <- as.data.frame(FA1_MS.mat)
# #View(FA1_MS)
# dim(FA1_MS)
# # Give column names something reference able
# colnames(FA1_MS) <- paste("C",c(1:1155))
# #View(FA1_MS)
# dim(FA1_MS)
# # remove bottom layer, max sheer not possible
# FA1_MS <- FA1_MS[-c(16),]
# dim(FA1_MS)
# # we want to know the maximums of sheer so root them to one
# FA1_MS<- sqrt(FA1_MS^2)
# # and test
# max(FA1_MS[,1])
# 
# # calculate max sheers for all 15minute time windows (columns)
# FV_Area1MS <- rep(NA,dim(FV_Area1)[2])
# for(i in 1:length(FV_Area1MS)){
#   FV_Area1MS[i] <- max(FA1_MS[,i])
# }

# mean(FV_Area1MS, na.rm=TRUE)




#### Area 2 ####
FV_Area2 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area2_FLOWBEC15.csv", header=F)

## Calculate mean, median and variance #
View(FV_Area2)
dim(FV_Area2)
#18

FV_Area2means <- rep(NA,dim(FV_Area2)[1])

for(i in 1:dim(FV_Area2)[1]){
  FV_Area2means[i] <- mean(FV_Area2[,i])
}
print(FV_Area2means)

#### Area 3 ####
FV_Area3 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area3_FLOWBEC15.csv", header=F)

## Calculate mean, median and variance #
# View(Area3)
dim(FV_Area3)
#23

FV_Area3means <- rep(NA,dim(FV_Area3)[1])

for(i in 1:dim(FV_Area3)[1]){
  FV_Area3means[i] <- mean(FV_Area3[,i])
}
print(FV_Area3means)

#### Area 4 ####
FV_Area4 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area4_FLOWBEC15.csv", header=F)

## Calculate mean, median and variance #
# View(Area4)
dim(FV_Area4)
#19

FV_Area4means <- rep(NA,dim(FV_Area4)[1])

for(i in 1:dim(FV_Area4)[1]){
  FV_Area4means[i] <- mean(FV_Area4[,i])
}
print(FV_Area4means)

#### Area 5 ####
FV_Area5 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area5_FLOWBEC15.csv", header=F)

## Calculate mean, median and variance #
# View(Area5)
dim(FV_Area5)
#16
Element <- c(1:dim(FV_Area5)[1])

FV_Area5means <- rep(NA,dim(FV_Area5)[1])

for(i in 1:dim(FV_Area5)[1]){
  FV_Area5means[i] <- mean(FV_Area5[,i])
}
print(FV_Area5means)

###### FVCOM 2015 Summaries ####
FV_Areas_mean <- c(mean(FV_Area1means),mean(FV_Area2means),mean(FV_Area3means),mean(FV_Area4means),mean(FV_Area5means)) 
FV_Areas_var <- c(var(FV_Area1means),var(FV_Area2means),var(FV_Area3means),var(FV_Area4means),var(FV_Area5means)) 

## Initial plotting
FV_Area <- c(1:5)
FV_Areas_mean
FV_Areas_var

FV_Area_full<-data.frame(matrix(NA,92,3))
# View(Area_full)
colnames(FV_Area_full) <- c("Area","Value", "Method")

FV_Area_full$Area[1:16] <- "Area1"
FV_Area_full$Area[17:34] <- "Area2"
FV_Area_full$Area[35:57] <- "Area3"
FV_Area_full$Area[58:77] <- "Area4"
FV_Area_full$Area[78:92] <- "Area5"

FV_Area_full$Value[1:16] <- FV_Area1means
FV_Area_full$Value[17:34] <- FV_Area2means
FV_Area_full$Value[35:57] <- FV_Area3means
FV_Area_full$Value[58:76] <- FV_Area4means
FV_Area_full$Value[77:92] <- FV_Area5means

FV_Area_full$Method <- "FVCOM 2015" 

View(FV_Area_full)


## Stats ####
S_Area <- c(1:5)
S_Areas_mean
S_Areas_var


### Import FVCOM 2016 ####
##### Select areas from FVCOM 2016####
#### Area 1 ####
### Velocity ##
F16_Area1 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area1_Scotia16.csv", header=F)
## Calculate mean, median and variance #
View(F16_Area1)
dim(F16_Area1)
#16

# Depth bin velocity means
F16_Area1means <- rep(NA,dim(F16_Area1)[1])
for(i in 1:dim(F16_Area1)[1]){
  F16_Area1means[i] <- mean(F16_Area1[,i])
}
#print(F_Area1means)

# # Time bin velocity means
# F_Area1means <- rep(NA,dim(F_Area1)[2])
# for(i in 1:dim(F_Area1)[2]){
#   F_Area1means[i] <- mean(F_Area1[,i])
# }
# print(F_Area1means)
# summary(F_Area1means)

# ### MaxSheer ## AFTER HITTING ISSUES DECIDED I ONLY NEED VELOCITY AND CAN ARGUE IT
# View(F_Area1)
# 
# # convert to matrix for one to one ness with new empty data frame
# F_Area1_MS <- as.matrix(F_Area1)
# #View(F_Area1_MS)
# 
# # Calculate max sheers for each depth bin in new matrix 
# FA1_MS.mat <- matrix(NA, nrow = dim(F_Area1_MS)[1], ncol = dim(F_Area1_MS)[2])
# #View(FA1_MS.mat)
# for(j in 1:dim(F_Area1_MS)[2]){
#   for(i in 1:dim(F_Area1_MS)[1]){
#     FA1_MS.mat[i,j] <- (F_Area1[i,j] - F_Area1[i+1,j+1])/2  # assumes depth bins of 2metres - check this (it varies slightly over time) -WRONG
#   }
# }
# #View(FA1_MS.mat)
# 
# # Convert to dataframe
# FA1_MS <- as.data.frame(FA1_MS.mat)
# #View(FA1_MS)
# dim(FA1_MS)
# # Give column names something reference able
# colnames(FA1_MS) <- paste("C",c(1:1155))
# #View(FA1_MS)
# dim(FA1_MS)
# # remove bottom layer, max sheer not possible
# FA1_MS <- FA1_MS[-c(16),]
# dim(FA1_MS)
# # we want to know the maximums of sheer so root them to one
# FA1_MS<- sqrt(FA1_MS^2)
# # and test
# max(FA1_MS[,1])
# 
# # calculate max sheers for all 15minute time windows (columns)
# F_Area1MS <- rep(NA,dim(F_Area1)[2])
# for(i in 1:length(F_Area1MS)){
#   F_Area1MS[i] <- max(FA1_MS[,i])
# }

# mean(F_Area1MS, na.rm=TRUE)




#### Area 2 ####
F_Area2 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area2_Scotia16.csv", header=F)

## Calculate mean, median and variance #
# View(F_Area2)
dim(F_Area2)[1]
#18

F_Area2means <- rep(NA,dim(F_Area2)[1])

for(i in 1:dim(F_Area2)[1]){
  F_Area2means[i] <- mean(F_Area2[,i])
}
print(F_Area2means)

#### Area 3 ####
F_Area3 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area3_Scotia16.csv", header=F)

## Calculate mean, median and variance #
# View(Area3)
dim(F_Area3)[1]
#23


F_Area3means <- rep(NA,dim(F_Area3)[1])

for(i in 1:dim(F_Area3)[1]){
  F_Area3means[i] <- mean(F_Area3[,i])
}
print(F_Area3means)

#### Area 4 ####
F_Area4 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area4_Scotia16.csv", header=F)

## Calculate mean, median and variance #
# View(Area4)
dim(F_Area4)[1]
#19


F_Area4means <- rep(NA,dim(F_Area4)[1])

for(i in 1:dim(F_Area4)[1]){
  F_Area4means[i] <- mean(F_Area4[,i])
}
print(F_Area4means)

#### Area 5 ####
F_Area5 <-  read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Area5_Scotia16.csv", header=F)

## Calculate mean, median and variance #
# View(Area5)
dim(F_Area5)[1]
#16

F_Area5means <- rep(NA,dim(F_Area5)[1])

for(i in 1:dim(F_Area5)[1]){
  F_Area5means[i] <- mean(F_Area5[,i])
}
print(F_Area5means)

#### FVCOM 2016 Summarys ####
F_Areas_mean <- c(mean(Area1means),mean(Area2means),mean(Area3means),mean(Area4means),mean(Area5means)) 
F_Areas_var <- c(var(Area1means),var(Area2means),var(Area3means),var(Area4means),var(Area5means)) 

## Initial plotting
F_Area <- c(1:5)
F_Areas_mean
F_Areas_var

F_Area_full<-data.frame(matrix(NA,92,3))
# View(Area_full)
colnames(F_Area_full) <- c("Area","Value", "Method")

F_Area_full$Area[1:16] <- "Area1"
F_Area_full$Area[17:34] <- "Area2"
F_Area_full$Area[35:57] <- "Area3"
F_Area_full$Area[58:77] <- "Area4"
F_Area_full$Area[78:92] <- "Area5"

F_Area_full$Value[1:16] <- F_Area1means
F_Area_full$Value[17:34] <- F_Area2means
F_Area_full$Value[35:57] <- F_Area3means
F_Area_full$Value[58:76] <- F_Area4means
F_Area_full$Value[77:92] <- F_Area5means

F_Area_full$Method <- "FVCOM 2016" 

View(F_Area_full)


## Stats ####
S_Area <- c(1:5)
S_Areas_mean
S_Areas_var



##### Select areas from FVCOM 2018 ####
#### Area 1 ####
### Velocity ##
F18_Area1 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/FVCOM/Area1_Scotia.csv", header=F)
## Calculate mean, median and variance #
View(F18_Area1)
dim(F18_Area1)
#16

## Extract lats and longs
#F18_Area1_lat <- F18_Area1[,c(___)] lats and longs not extracted
#F18_Area1_lon <- F18_Area1[,c(___)]
## Remove lat and lon data from the Area1 dataset
#F18_Area1 <- F18_Area1[,c(1:1153)]

# Depth bin velocity means
F18_Area1means <- rep(NA,dim(F18_Area1)[1])
for(i in 1:dim(F18_Area1)[1]){
  F18_Area1means[i] <- mean(F18_Area1[,i])
}
print(F18_Area1means)

# # Time bin velocity means
# F18_Area1means <- rep(NA,dim(F18_Area1)[2])
# for(i in 1:dim(F18_Area1)[2]){
#   F18_Area1means[i] <- mean(F18_Area1[,i])
# }
# print(F18_Area1means)
# summary(F18_Area1means)

# ### MaxSheer ## AFTER HITTING ISSUES DECIDED I ONLY NEED VELOCITY AND CAN ARGUE IT
# View(F18_Area1)
# 
# # convert to matrix for one to one ness with new empty data frame
# F18_Area1_MS <- as.matrix(F18_Area1)
# #View(F18_Area1_MS)
# 
# # Calculate max sheers for each depth bin in new matrix 
# FA1_MS.mat <- matrix(NA, nrow = dim(F18_Area1_MS)[1], ncol = dim(F18_Area1_MS)[2])
# #View(FA1_MS.mat)
# for(j in 1:dim(F18_Area1_MS)[2]){
#   for(i in 1:dim(F18_Area1_MS)[1]){
#     FA1_MS.mat[i,j] <- (F18_Area1[i,j] - F18_Area1[i+1,j+1])/2  # assumes depth bins of 2metres - check this (it varies slightly over time) -WRONG
#   }
# }
# #View(FA1_MS.mat)
# 
# # Convert to dataframe
# FA1_MS <- as.data.frame(FA1_MS.mat)
# #View(FA1_MS)
# dim(FA1_MS)
# # Give column names something reference able
# colnames(FA1_MS) <- paste("C",c(1:1155))
# #View(FA1_MS)
# dim(FA1_MS)
# # remove bottom layer, max sheer not possible
# FA1_MS <- FA1_MS[-c(16),]
# dim(FA1_MS)
# # we want to know the maximums of sheer so root them to one
# FA1_MS<- sqrt(FA1_MS^2)
# # and test
# max(FA1_MS[,1])
# 
# # calculate max sheers for all 15minute time windows (columns)
# F18_Area1MS <- rep(NA,dim(F18_Area1)[2])
# for(i in 1:length(F18_Area1MS)){
#   F18_Area1MS[i] <- max(FA1_MS[,i])
# }

# mean(F18_Area1MS, na.rm=TRUE)




#### Area 2 ####
F18_Area2 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/FVCOM/Area2_n.csv", header=F)

## Calculate mean, median and variance #
# View(F18_Area2)
dim(F18_Area2)[1]
#18
Element <- c(1:dim(F18_Area2)[1])

# Extract lats and longs
# F18_Area2_lat <- F18_Area2[,c(1154)]
# F18_Area2_lon <- F18_Area2[,c(1155)]
# Remove lat and lon data from the Area1 dataset
# F18_Area2 <- F18_Area2[,c(1:1153)]

F18_Area2means <- rep(NA,dim(F18_Area2)[1])

for(i in 1:dim(F18_Area2)[1]){
  F18_Area2means[i] <- mean(F18_Area2[,i])
}
print(F18_Area2means)

#### Area 3 ####
F18_Area3 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/FVCOM/Area3_n.csv", header=F)

## Calculate mean, median and variance #
# View(Area3)
dim(F18_Area3)[1]
#23
Element <- c(1:dim(F18_Area3)[1])

# Extract lats and longs
# F18_Area3_lat <- F18_Area3[,c(1154)]
# F18_Area3_lon <- F18_Area3[,c(1155)]
# Remove lat and lon data from the Area1 dataset
# F18_Area3 <- F18_Area3[,c(1:1153)]

F18_Area3means <- rep(NA,dim(F18_Area3)[1])

for(i in 1:dim(F18_Area3)[1]){
  F18_Area3means[i] <- mean(F18_Area3[,i])
}
print(F18_Area3means)

#### Area 4 ####
F18_Area4 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/FVCOM/Area4_n.csv", header=F)

## Calculate mean, median and variance #
# View(Area4)
dim(F18_Area4)[1]
#19
Element <- c(1:dim(F18_Area4)[1])

# Extract lats and longs
# F18_Area4_lat <- F18_Area4[,c(1154)]
# F18_Area4_lon <- F18_Area4[,c(1155)]
# Remove lat and lon data from the Area1 dataset
# F18_Area4 <- F18_Area4[,c(1:1153)]

F18_Area4means <- rep(NA,dim(F18_Area4)[1])

for(i in 1:dim(F18_Area4)[1]){
  F18_Area4means[i] <- mean(F18_Area4[,i])
}
print(F18_Area4means)

#### Area 5 ####
F18_Area5 <-  read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/FVCOM/Area5_n.csv", header=F)

## Calculate mean, median and variance #
# View(Area5)
dim(F18_Area5)[1]
#16
Element <- c(1:dim(F18_Area5)[1])

# Extract lats and longs
# F18_Area5_lat <- F18_Area5[,c(1154)]
# F18_Area5_lon <- F18_Area5[,c(1155)]
# Remove lat and lon data from the Area1 dataset
# F18_Area5 <- F18_Area5[,c(1:1153)]

F18_Area5means <- rep(NA,dim(F18_Area5)[1])

for(i in 1:dim(F18_Area5)[1]){
  F18_Area5means[i] <- mean(F18_Area5[,i])
}
print(F18_Area5means)

#### FVCOM Summarys ####
F18_Areas_mean <- c(mean(F18_Area1means),mean(F18_Area2means),mean(F18_Area3means),mean(F18_Area4means),mean(F18_Area5means)) 
F18_Areas_var <- c(var(F18_Area1means),var(F18_Area2means),var(F18_Area3means),var(F18_Area4means),var(F18_Area5means)) 

## Initial plotting
F18_Area <- c(1:5)
F18_Areas_mean
F18_Areas_var

F18_Area_full<-data.frame(matrix(NA,92,3))
# View(Area_full)
colnames(F18_Area_full) <- c("Area","Value", "Method")

F18_Area_full$Area[1:16] <- "Area1"
F18_Area_full$Area[17:34] <- "Area2"
F18_Area_full$Area[35:57] <- "Area3"
F18_Area_full$Area[58:77] <- "Area4"
F18_Area_full$Area[78:92] <- "Area5"

F18_Area_full$Value[1:16] <- F18_Area1means
F18_Area_full$Value[17:34] <- F18_Area2means
F18_Area_full$Value[35:57] <- F18_Area3means
F18_Area_full$Value[58:76] <- F18_Area4means
F18_Area_full$Value[77:92] <- F18_Area5means

F18_Area_full$Method <- "FVCOM 2018" 

View(F18_Area_full)

# Graphing FVCOM 2018 ####
FVCOM18 <- ggplot(F18_Area_full, aes(x=Area,y=Value, fill=factor(Method))) +
  geom_boxplot() + 
  labs(fill = "Method") + 
  theme(text=element_text(size=13))+
  # geom_point(position=position_jitterdodge(),alpha=0.3) +
  scale_y_continuous(breaks=seq(0,6,0.2))+
  theme_bw(base_size = 16)
FVCOM18


#####################################################################
### COMBINING fvcom AND SCOTIA DATASETS ####
head(ADCP15_full)


Vel_full <- rbind(S_Area_full,F18_Area_full, F_Area_full, FV_Area_full,  ADCP15_full)
str(Vel_full)
View(Vel_full)
Vel_full_summary <- summary(Vel_full)
Vel_full_summary

# there are 2/3 still spiked values in there as outliers
Vel_full <- Vel_full[!(Vel_full$Value >= 5),] 
Vel_full_summary <- summary(Vel_full)
Vel_full_summary
Vel_full <- na.omit(Vel_full)


## Graphing Hydrodynamics ####
# Graph all data together
boxplot(Value~Area, data=Vel_full)

# Rename areas to habitat units
Vel_full$Area <- as.factor(Vel_full$Area)
levels(Vel_full$Area)<- c("HU1", "HU2", "HU3", "HU4", "HU5")

# remove FVCOM 2016 from Areas 2-5
cutrows <- c(1:18631,18708:96839)

Vel_full_cut1 <- Vel_full[cutrows,]
Vel_full_cut1 <- na.omit(Vel_full_cut1)


Vel_full_cut1$Method <- as.factor(Vel_full_cut1$Method)
levels(Vel_full_cut1$Method) <- c("FLOWBEC-4D ADCP 2015", "SSM 2015", "SSM 2016", "SSM 2018", "Scotia 2018")
levels(as.factor(Vel_full_cut1$Method))

# Step 1: Call the pdf command to start the plot
png(file = "E:/Offline_saves/Chapter 2 - Rprocessing+analysis/R/Graphics/Vel_n.png",   # The directory you want to save the file in
    width = 2000, # The width of the plot in inches
    height = 728) # The height of the plot in inches

# Step 2: Create the plot with R code
# Graph used in paper
Vel <- ggplot(Vel_full_cut1, aes(x=Area,y=Value, fill=factor(Method))) +
  geom_boxplot() + 
  labs(fill = "Method", y = "Velocity (ms-1)", x = "Habitat unit", cex.axis = 2) +
  scale_fill_manual(values=c("deeppink2","deeppink1","chartreuse","coral","coral3"))+
  scale_y_continuous(breaks=seq(0,6,0.5),limits =c(0.0,4.2))+
  theme_bw(base_size = 30)
Vel

# Step 3: Run dev.off() to create the file!
dev.off()

# Vel <- ggplot(Vel_full, aes(x=Area,y=Value, fill=factor(Method))) +
#   geom_boxplot() +
#   labs(fill = "Method", y = "Velocity m/s", x = "Habitat unit") +
#   scale_fill_manual(values=c("gray90","gray70","gray50","gray30","gray10" ))+
#   # geom_point(position=position_jitterdodge(),alpha=0.3) +
#   scale_y_continuous(breaks=seq(0,6,0.2),limits =c(0.0,4.2))+
#   theme_bw(base_size = 16)
# Vel

# ## Fancier graph to make discussion interpretation easier
# Vel_fancy <- Vel_full[Vel_full$Method == c("Scotia 2018","FVCOM 2016",
#                                            "FVCOM 2018"),]
# 
# # Step 1: Call the pdf command to start the plot
# png(file = "C:/Users/r01jdc17/Desktop/Offline_saves/Chapter 2 - Rprocessing+analysis/Vel_n.png",   # The directory you want to save the file in
#     width = 2000, # The width of the plot in inches
#     height = 728) # The height of the plot in inches
# 
# # Step 2: Create the plot with R code
# Vel.n <- ggplot(Vel_fancy, aes(x=Area,y=Value, fill=factor(Method))) +
#   geom_boxplot() + 
#   scale_fill_manual(values=c("gray50","gray30","gray10" ))+
#   labs(fill = "Method", y = "Velocity m/s") + 
#   theme(text=element_text(size=13))+
#   # geom_point(position=position_jitterdodge(),alpha=0.3) +
#   scale_y_continuous(breaks=seq(0,6,0.2), limits =c(0.0,4.2))+
#   theme_bw(base_size = 16)
# Vel.n
# 
# # Step 3: Run dev.off() to create the file!
# dev.off()

# ## Fancier graph to make discussion interpretation easier
# Vel_f_mini <- Vel_full[Vel_full$Method == c("ADCP 2015","FVCOM 2015"),]
# 
# # Step 1: Call the pdf command to start the plot
# png(file = "C:/Users/r01jdc17/Desktop/Offline_saves/Chapter 2 - Rprocessing+analysis/Vel_n_mini.png",   # The directory you want to save the file in
#     width = 2000, # The width of the plot in inches
#     height = 728) # The height of the plot in inches
# 
# # Step 2: Create the plot with R code
# Vel.n.s <- ggplot(Vel_f_mini, aes(x=Area,y=Value, fill=factor(Method))) +
#   geom_boxplot() +
#   scale_fill_manual(values=c("gray90","gray70")) +
#   labs(fill = "Method", y = "Velocity m/s") + 
#   theme(text=element_text(size=13))+
#   # geom_point(position=position_jitterdodge(),alpha=0.3) +
#   scale_y_continuous(breaks=seq(0,6,0.2), limits =c(0.0,4.2))+
#   theme_bw(base_size = 16)
# Vel.n.s
# 
# # Step 3: Run dev.off() to create the file!
# dev.off()



save.image()

# Statistical test ####
## Area 1 ####
Vel_A1 <- rbind(S_Area_full[1:3789,], F18_Area_full[1:16,]) #,F_Area_full[1:16,]
with(Vel_A1, shapiro.test(Value[Method == "FVCOM"])) # p=0.625 = >0.05 therefore assume normality
with(Vel_A1, shapiro.test(Value[Method == "Scotia"])) # p= <2.2e-16 therefore assume non-normality

# Need to do non-parametric testing Wilcoxon tests
wilcox.test(Value~Method, data = Vel_A1, exact = FALSE)
# p = <0.05 therefore indicates groups are significantly different

## Area 2 ####
Vel_A2 <- rbind(S_Area_full[3790:11708,],F_Area_full[17:34,])
# From Area1 assuming non-normality for all looks justified on graph check.
# Need to do non-parametric testing Wilcoxon tests
wilcox.test(Value ~ Method, data = Vel_A2, exact = FALSE)
# p = <0.05 therefore indicates groups are significantly different

## Area 3 ####
Vel_A3 <- rbind(S_Area_full[11709:14317,],F_Area_full[35:57,])

# Need to do non-parametric testing Wilcoxon tests
wilcox.test(Value ~ Method, data = Vel_A3, exact = FALSE)
# p = > 0.05 therefore indicates groups are not significantly different

## Area 4 ####
Vel_A4 <- rbind(S_Area_full[14318:16440,],F_Area_full[58:76,])

# Need to do non-parametric testing Wilcoxon tests
wilcox.test(Value ~ Method, data = Vel_A4, exact = FALSE)
# p = > 0.05 therefore indicates groups are not significantly different

## Area 5 ####
Vel_A5 <- rbind(S_Area_full[16441:19460,],F_Area_full[78:92,])

# Need to do non-parametric testing Wilcoxon tests
wilcox.test(Value ~ Method, data = Vel_A5, exact = FALSE)
# p = <0.05 therefore indicates groups are significantly different

## Top predator areas ####
TP_FVCOM <- rbind(F_Area_full[17:34,],F_Area_full[78:92,])
# Need to do non-parametric testing Wilcoxon tests
wilcox.test(Value ~ Area, data = TP_FVCOM, exact = FALSE)
# p = <0.05 therefore indicates groups are significantly different

TP_Scotia <- rbind(S_Area_full[3790:11708,],S_Area_full[16441:19460,])
# Need to do non-parametric testing Wilcoxon tests
l <- wilcox.test(Value ~ Area, data = TP_Scotia, exact = FALSE)
# p = <0.05 therefore indicates groups are significantly different
save.image()



####################### BIRDS #######################################
#### 2016 ####
obs2016 <- read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2016/Observer/Seabird data/Scotia_0816S_Bird obs_with_ship_locations_June 2016 - COMPLETEEdit.csv", header=TRUE)
obs2016$Species <- factor(obs2016$Species)

obs2016$Spcs_Cat <- NA

str(obs2016)

for(i in 1:length(obs2016$Spcs_Cat)){
  obs2016$Spcs_Cat[i] <- ifelse(obs2016$Species[i] == "F.", 
                                 obs2016$Spcs_Cat[i] <- "Surface",
                          ifelse(obs2016$Species[i] == "KI", 
                                 obs2016$Spcs_Cat[i] <- "Surface", 
                          ifelse(obs2016$Species[i] == "RA", 
                                 obs2016$Spcs_Cat[i] <- "Pelagic",
                          ifelse(obs2016$Species[i] == "PU", 
                                 obs2016$Spcs_Cat[i] <- "Pelagic",
                          ifelse(obs2016$Species[i] == "GU", 
                                 obs2016$Spcs_Cat[i] <- "Pelagic",
                          ifelse(obs2016$Species[i] == "SA", 
                                 obs2016$Spcs_Cat[i] <- "Benthic",
                          ifelse(obs2016$Species[i] == "GU", 
                                 obs2016$Spcs_Cat[i] <- "Benthic",
                                 obs2016$Spcs_Cat[i] <-NA)))))))
}

obs2016$Spcs_Cat
obs2016$Count
write.csv(obs2016, "C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2016/Observer/Seabird data/Observations2016.csv", row.names = FALSE)
View(obs2016)

obs2016_S <- obs2016[obs2016$Spcs_Cat=="Surface",]
View(obs2016_S)
sum(as.numeric(obs2016_S$Number), na.rm = T)

obs2016_P <- obs2016[obs2016$Spcs_Cat=="Pelagic",]
View(obs2016_P)
sum(as.numeric(obs2016_P$Number), na.rm = T)

obs2016_B <- obs2016[obs2016$Spcs_Cat=="Benthic",]
View(obs2016_B)
sum(as.numeric(obs2016_B$Number), na.rm = T)

obs2016_NA <- obs2016[obs2016$Spcs_Cat=="NA",]
View(obs2016_NA)
sum(as.numeric(obs2016_NA$Number), na.rm = T)

#### 2018 ####
obs2018 <- read.csv("C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2018/Observer/Bird_lat_long_Scotia2018.csv", header=TRUE)
obs2018$Species <- factor(obs2018$Species)

obs2018$Spcs_Cat <- NA
for(i in 1:length(obs2018$Spcs_Cat)){
  obs2018$Spcs_Cat[i] <- ifelse(obs2018$Species[i] == "F.", 
                                obs2018$Spcs_Cat[i] <- "Surface",
                                ifelse(obs2018$Species[i] == "KI", 
                                       obs2018$Spcs_Cat[i] <- "Surface", 
                                       ifelse(obs2018$Species[i] == "RA", 
                                              obs2018$Spcs_Cat[i] <- "Pelagic",
                                              ifelse(obs2018$Species[i] == "PU", 
                                                     obs2018$Spcs_Cat[i] <- "Pelagic",
                                                     ifelse(obs2018$Species[i] == "GU", 
                                                            obs2018$Spcs_Cat[i] <- "Pelagic",
                                                            ifelse(obs2018$Species[i] == "SA", 
                                                                   obs2018$Spcs_Cat[i] <- "Benthic",
                                                                   ifelse(obs2018$Species[i] == "GU", 
                                                                          obs2018$Spcs_Cat[i] <- "Benthic",
                                                                          obs2018$Spcs_Cat[i] <-NA)))))))
}
obs2018$Spcs_Cat

write.csv(obs2018, "C:/Users/james/OneDrive - University of Aberdeen/Data - Code/Scotia 2018/Observer/Observations2018.csv", row.names = FALSE)

obs2018_S <- obs2018[obs2018$Spcs_Cat=="Surface",]
View(obs2018_S)
sum(as.numeric(obs2018_S$Number), na.rm = T)

obs2018_P <- obs2018[obs2018$Spcs_Cat=="Pelagic",]
View(obs2018_P)
sum(as.numeric(obs2018_P$Number), na.rm = T)

obs2018_B <- obs2018[obs2018$Spcs_Cat=="Benthic",]
View(obs2018_B)
sum(as.numeric(obs2018_B$Number), na.rm = T)

obs2018_NA <- obs2018[obs2018$Spcs_Cat=="NA",]
View(obs2018_NA)
sum(as.numeric(obs2018_NA$Number), na.rm = T)

### Bird graphics ####
## Bird analysis now moves into ArcGIS for mapping by Spcs_cat groups

### R version of GIS process with boundaries
# http://rstudio-pubs-static.s3.amazonaws.com/274869_ce4a66ed37e646cf9404c09554a152e2.html
install.packages("adehabitatHR")
library("sp","rgdal", "rgeos")
library("raster", "adehabitatHR")

# obs2018pts <- readOGR("./data", "Camden_house_sales")
# kde.output <- adehabitatHR::kernelUD(obs2018, h="href", grid = 1000)

obs2016_Pr <- raster("E:/Offline_saves/Chapter 3 - HATOPO-SSR/QGIS/obs2016_P_w1.tif")
image(obs2016_Pr)

########## SPAEF for distribution maps ######################
#install.packages("pracma") #histc is in pracma, uncomment and install first
library(pracma)

# read ascii
mask <- read.delim('E:/Offline_saves/Chapter 4 - Distribution Simulations/SPAEF/SPAEF_codes_R_Python_Matlab//map_files/mask_1km.asc', header = FALSE, sep = ",", dec = ".")
class(mask)
dimens=dim(mask)
mask=array(as.numeric(unlist(mask)), dim=c(dimens[1], dimens[2]))
class(mask)
View(mask)
image(mask)

# Array of data observed data comes here
map1 <- read.delim('E:/Offline_saves/Chapter 4 - Distribution Simulations/SPAEF/SPAEF_codes_R_Python_Matlab/map_files/obs.asc', header = FALSE, sep = "\t", dec = ".")
class(map1)
# View(map1)
map1=array(as.numeric(unlist(map1)), dim=c(dimens[1], dimens[2]))
class(map1)
map1=map1[as.logical(mask)]
class(map1)
View(map1)

# You can test other asc files here
map2 <- read.delim('E:/Offline_saves/Chapter 4 - Distribution Simulations/SPAEF/SPAEF_codes_R_Python_Matlab/map_files/sim_1.asc', header = FALSE, sep = "\t", dec = ".") 
class(map2)
View(map2)
map2=array(as.numeric(unlist(map2)), dim=c(dimens[1], dimens[2]))
class(map2)
View(map2)
map2=map2[as.logical(mask)]
class(map2)
View(map2)

obs <- map1[ map1 != -9999 ]
sim <- map2[ map2 != -9999 ]

#CORR
alpha=cor(obs,sim)

#coefficient of variation
cv_obs=sd(obs)/mean(obs);
cv_sim=sd(sim)/mean(sim);

beta=cv_sim/cv_obs;

#HISTOmatch
obs=(obs-mean(obs))/sd(obs)
sim=(sim-mean(sim))/sd(sim)

bins=floor(sqrt(length(obs)))

h1 <- hist(obs, breaks=bins, freq=TRUE, plot=TRUE)
h2 <- hist(sim, breaks=bins, freq=TRUE, plot=TRUE) #False makes Density instead of frequency, try it

a=histc(obs, h1$breaks)
b=histc(sim, h1$breaks)
c=cbind(a$cnt, b$cnt)
d <- pmin(c[,1],c[,2])
overlap=sum(d)
histogram_match=overlap/sum(a$cnt)
gamma=histogram_match

spaef = 1- sqrt( (alpha-1)^2 + (beta-1)^2 + (gamma-1)^2 )

#print(paste0("SPAEF: ", round(spaef, digits = 6)))
cat("SPAEF: ", spaef)

###################
# 2016
mask16 <- raster("E:/Offline_saves/Chapter 3 - HATOPO-SSR/QGIS/Mask20161.tif")
image(mask16)
dimens=dim(mask16)
mask16=array(as.numeric(unlist(as.data.frame(mask16))), dim=c(dimens[1], dimens[2]))
mask16
View(mask16)
# P
s2016_P_rast <- raster("E:/Offline_saves/Chapter 3 - HATOPO-SSR/QGIS/obs2016_P_w1.tif")
dimens=dim(s2016_P_rast)
s2016_P_rast=array(as.numeric(unlist(s2016_P_rast)), dim=c(dimens[1], dimens[2]))
image(s2016_P_rast)
s2016_P_rast


# 2018
mask18 <- raster("E:/Offline_saves/Chapter 3 - HATOPO-SSR/QGIS/mask20181.tif")
dimens=dim(mask18)
mask18=array(as.numeric(unlist(mask18)), dim=c(dimens[1], dimens[2]))
image(mask18)
mask18
# P
s2018_R <- raster("E:/Offline_saves/Chapter 3 - HATOPO-SSR/QGIS/obs2018_P_w1.tif")
dimens=dim(s2018_R)
s2018_R=array(as.numeric(unlist(s2018_R)), dim=c(dimens[1], dimens[2]))
image(s2018_R)
s2018_R


#####################################################################################


obs <- map1[ map1 != -9999 ]
sim <- map2[ map2 != -9999 ]


#CORR
alpha=cor(obs,sim)

#coefficient of variation
cv_obs=sd(obs)/mean(obs);
cv_sim=sd(sim)/mean(sim);

beta=cv_sim/cv_obs;

#HISTOmatch
obs=(obs-mean(obs))/sd(obs)
sim=(sim-mean(sim))/sd(sim)

bins=floor(sqrt(length(obs)))

h1 <- hist(obs, breaks=bins, freq=TRUE, plot=TRUE)
h2 <- hist(sim, breaks=bins, freq=TRUE, plot=TRUE) #False makes Density instead of frequency, try it

a=histc(obs, h1$breaks)
b=histc(sim, h1$breaks)
c=cbind(a$cnt, b$cnt)
d <- pmin(c[,1],c[,2])
overlap=sum(d)
histogram_match=overlap/sum(a$cnt)
gamma=histogram_match

spaef = 1- sqrt( (alpha-1)^2 + (beta-1)^2 + (gamma-1)^2 )

#print(paste0("SPAEF: ", round(spaef, digits = 6)))
cat("SPAEF: ", spaef)


######## Survey period graphs #######

fullF15 <- read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Month_FLOWBEC15.csv", header=TRUE)
fullS16 <- read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Month_Scotia16 - Real.csv", header=TRUE)
fullS18 <- read.csv("C:/Users/r01jdc17/OneDrive - University of Aberdeen/Data - Code/MeyGen 2015/FVCOM/Month_Scotia18.csv", header=TRUE)

# FLOWBEC-4D:   8th to 19th October
# Scotia 2016: 22nd to 25th June
# Scotia 2018: 21st to 24th July

### Month of FVCOM data Oct 2015 ####
View(fullF15)
dim(fullF15)
z <- fullF15[1,]
View(z)
w <- c(1:dim(fullF15)[2])
timestepF15 <- as.data.frame(w)
timestepF15
timestepF15$w <- w
timestepF15$z <- z[,1]
timestepF15$datetime[1] <- "2015/10/08 00:00:00"

start <- as.POSIXct("2015/10/08 00:00:00")
interval <- 60
end <- start + as.difftime(13, units="days")
datetimeF15 <- seq(from=start, by=interval*15, to=end)
timestepF15$datetime <- datetimeF15[-1249]
timestepF15$Tidal_Velocity <- NA

View(timestepF15)

# Step 1: Call the pdf command to start the plot
png(file = "E:/Offline_saves/Chapter 2 - Rprocessing+analysis/R/Graphics/Month_FLOWBEC15_.png",   # The directory you want to save the file in
    width = 2000, # The width of the plot in inches
    height = 728) # The height of the plot in inches

# Step 2: Create the plot with R code
par(adj = 0.5, mar = c(6,4,4,2)+0.5)
plot(timestepF15$datetime,z, type = "l", ylab = "Velocity (ms-1)", xlab = "Time",
      xaxt="n", cex.lab = 2, cex.axis = 2, ylim=c(0,4))
axis.POSIXct(1,timestepF15$datetime,at=seq(as.Date("2015-10-08"), as.Date("2015-10-20"),by="days"), format = "%d-%m-%y", cex.axis = 2, adj=0.2)
rect(1444258800,0,1445203800,4,col = rgb(0.5,0.5,0.5,1/4)) # Additional low-level plotting commands

# Step 3: Run dev.off() to create the file!
dev.off()

### Month of FVCOM data Jun 2016 ####
View(fullS16)
z16 <- fullS16[1,]
dim(z16)
w16 <- c(1:dim(fullS16)[2])
timestepS16 <- as.data.frame(w16)
timestepS16
timestepS16$w <- w16
# timestepS16$z <- z16[,1]

timestepS16$datetime[1] <- "2016/06/19 00:00:00"

start <- as.POSIXct("2016/06/19 00:00:00")
interval <- 60
end <- start + as.difftime(13, units="days")
datetimeS16 <- seq(from=start, by=interval*15, to=end)
timestepS16$datetime <- NA
length(timestepS16$datetime)
timestepS16$datetime <- datetimeS16[c(1:1247)]
timestepS16$Tidal_Velocity <- NA

View(timestepS16)

# Step 1: Call the pdf command to start the plot
png(file = "E:/Offline_saves/Chapter 2 - Rprocessing+analysis/R/Graphics/Month_SCOTIA16.png",   # The directory you want to save the file in
    width = 2000, # The width of the plot in inches
    height = 728) # The height of the plot in inches

# Step 2: Create the plot with R code
par(adj = 0.5, mar = c(6,4,4,2)+0.5)
plot(timestepS16$datetime,z16, type = "l", ylab = "Velocity (ms-1)", xlab = "Time",
     xaxt="n", cex.lab = 2, cex.axis = 2, ylim=c(0,4))
axis.POSIXct(1,timestepS16$datetime,at=seq(as.Date("2016/06/19"), as.Date("2016-07-01"),by="days"), format = "%d-%m-%y", cex.axis = 2)
rect(1466550000,0,1466809200,4,col = rgb(0.5,0.5,0.5,1/4)) # Additional low-level plotting commands
# as.numeric(as.POSIXct("2016-06-22 00:00:00")) # work out rectangle numbers from dates

# Step 3: Run dev.off() to create the file!
dev.off()


### Month of FVCOM data Jul 2018 ####
View(fullS18)
dim(fullS18)
z18 <- fullS18[1,]
dim(z18)
w18 <- c(1:dim(fullS18)[2])

timestepS18 <- as.data.frame(w18)
timestepS18
timestepS18$w <- w18
# timestepS18$z <- z18[,1]

timestepS18$datetime[1] <- "2018/07/18 00:00:00"

start <- as.POSIXct("2018/07/18 00:00:00")
interval <- 60
end <- start + as.difftime(13, units="days")
datetimeS18 <- seq(from=start, by=interval*15, to=end)
timestepS18$datetime <- NA
length(timestepS18$datetime)
timestepS18$datetime <- datetimeS18[c(1:1247)]
timestepS18$Tidal_Velocity <- NA

View(timestepS18)

# Step 1: Call the pdf command to start the plot
png(file = "E:/Offline_saves/Chapter 2 - Rprocessing+analysis/R/Graphics/Month_SCOTIA18_.png",   # The directory you want to save the file in
    width = 2000, # The width of the plot in inches
    height = 728) # The height of the plot in inches

# Step 2: Create the plot with R code
par(adj = 0.5, mar = c(6,4,4,2)+0.5)
plot(timestepS18$datetime,z18, type = "l", ylab = "Velocity (ms-1)", xlab = "Time",
     xaxt="n", cex.lab = 2, cex.axis = 2, ylim=c(0,4))
axis.POSIXct(1,timestepS18$datetime,at=seq(as.Date("2018/07/18"), as.Date("2018-07-31"),by="days"), format = "%d-%m-%y", cex.axis = 2)
rect(1532212200,0,1532471400,4,col = rgb(0.5,0.5,0.5,1/4)) # Additional low-level plotting commands
# as.numeric(as.POSIXct("2018-07-21 23:30:00")) # work out rectangle numbers from dates

# 21st to 24th July

# Step 3: Run dev.off() to create the file!
dev.off()


#######################################################################################
#### Tidal period sync of fish data ####
## MeyGen periods that match each year

View(Meygen15_fish)

# 2016 October 8th-9th and 18th onwards
FLBC_16data_1 <- Meygen15_fish[Meygen15_fish$Date_M ==20151008,]
FLBC_16data_2 <- Meygen15_fish[Meygen15_fish$Date_M >= 20151018,]
FLBC_16data <- rbind(FLBC_16data_1,FLBC_16data_2)
dim(FLBC_16data) # 209

FLOWBEC_16period <- ggplot(FLBC_16data, aes(x=Depth_bin_RL))
FLOWBEC_16period +
  labs(x="Depth bin", y= "Count") +
  theme(text=element_text(size=13))+
  geom_bar(aes(fill = tides), position = position_stack(reverse = TRUE))+
  scale_x_discrete(limits = x_axis_cats_FLOWBEC, drop=FALSE) +
  ylim(0,100)+
  coord_flip()

# 2018 October 12th-15th
FLBC_18data <- Meygen15_fish[Meygen15_fish$Date_M >= 20151012 & Meygen15_fish$Date_M <= 20151015,]
dim(FLBC_18data) # 290

# 2018 period
FLOWBEC_18period <- ggplot(FLBC_18data, aes(x=Depth_bin_RL))
FLOWBEC_18period +
  labs(x="Depth bin", y= "Count") +
  theme(text=element_text(size=13))+
  geom_bar(aes(fill = tides), position = position_stack(reverse = TRUE))+
  scale_x_discrete(limits = x_axis_cats_FLOWBEC, drop=FALSE) +
  ylim(0,100)+
  coord_flip()

### Stats and percentages ###
summary(FLBC_16data$Depth_bin_RL)
dim(FLBC_16data)


## FLOWBEC to Scotia18 ####
# October 12th-15th
# All fish data
Scotia18_All <- ggplot(Scotia18_fish, aes(x=Depth_bin_RL))
Scotia18_All +
  labs(x=" ", y= "Count", title = "Scotia 2018") +
  geom_bar(aes(fill = Timing))+
  scale_x_discrete(limits = x_axis_cats_Scotia, drop=FALSE) +
  scale_y_reverse() +
  ylim(325,0)+
  coord_flip()


#######################################################################################
# Stats ####
# Process and Reporting
# hist()
# wilcox.test()
# (W= 7964, p =0.017) test stat value and p-value (in this example there is a significnat difference between groups)



# Surface birds group size
obs2016_S_na <- na.omit(obs2016_S)
obs2018_S_na <- na.omit(obs2018_S)
View(obs2016_S_na)
par(mfrow=c(1,1))
hist(as.numeric(obs2016_S_na$Number), breaks = 20)
hist(as.numeric(obs2018_S_na$Number), breaks = 20)
wilcox.test(as.numeric(obs2016_S_na$Number),as.numeric(obs2018_S_na$Number))

# Pelagic birds group size
obs2016_P_na <- na.omit(obs2016_P)
obs2018_P_na <- na.omit(obs2018_P)
View(obs2016_P_na)
par(mfrow=c(1,1))
hist(as.numeric(obs2016_P_na$Number), breaks = 20)
hist(as.numeric(obs2018_P_na$Number), breaks = 20)
wilcox.test(as.numeric(obs2016_P_na$Number),as.numeric(obs2018_P_na$Number))

# Benthic birds group size
obs2016_B_na <- na.omit(obs2016_B)
obs2018_B_na <- na.omit(obs2018_B)
View(obs2016_B_na)
par(mfrow=c(1,1))
hist(as.numeric(obs2016_B_na$Number), breaks = 20)
hist(as.numeric(obs2018_B_na$Number), breaks = 20)
wilcox.test(as.numeric(obs2016_B_na$Number),as.numeric(obs2018_B_na$Number))
wilcox.test(as.numeric(obs2016_B_na$Number),as.numeric(obs2018_B_na$Number))$statistic

# Inner Sound usage
obs2016_na_IS <- na.omit(obs2016[obs2016$Lat_dd < 58.668,])
obs2018_na_IS <- na.omit(obs2018[obs2018$lat < 58.668,])
dim(obs2016_na_IS)
dim(obs2018_na_IS)
hist(as.numeric(obs2016_B_na$Number), breaks = 20)
hist(as.numeric(obs2018_B_na$Number), breaks = 20)


wilcox.test(as.numeric(obs2016_na_IS$Number),as.numeric(obs2018_na_IS$Number))


############################
# ?anova
t.test(Value~Method, data=Vel_A1)


oneway.test(Value~Area, data = S_Area_full, var.equal = TRUE)

oneway.test(Value~Area, data = F_Area_full, var.equal = TRUE)


Vel_model <- lm(Value ~  Area + Method, data= Vel_full)
summary(Vel_model)

display(Vel_model)
anova(Vel_model)

# 
# summary(anova(aov(Value~Method, data=Vel_full)))
# 
t.test(Value~Method, data=Vel_full)
# 





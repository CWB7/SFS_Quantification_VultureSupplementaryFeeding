
#Christiaan W Brink 2018/10/10 - Analysis of a survey done with the managers of vulture Supplementary Feeding Site (SFS)
# Publication associated with this analysis: Brink et al. 2020 Animal Conservation 23, 491-501(doi:10.1111/acv.12561)


# Data wrangling with dplyr
rm(list=ls())


library (ggplot2)
library(dplyr)


getwd()

#import data function used


library(readr)
surveyData <- read_delim("VRMapDATA_forR_20191103_FINAL_New.csv", 
                         ";", escape_double = FALSE, col_types = cols(Beef_kg_py = col_number(), 
                                                                      Equine_kg_y = col_number(), Game_kg_py = col_number(), 
                                                                      Latitude = col_number(), Longitude = col_number(), 
                                                                      Other_kg_y = col_number(), Pork_kg_y = col_number(), 
                                                                      Sheep_kg_y = col_number(), Total_kg_day = col_number(), 
                                                                      Total_kg_y = col_number()), trim_ws = TRUE)
View(surveyData)


## Basic statistics and calculations ########################################################################################


summary(surveyData)


sapply(surveyData, class) #gives the class of data (factor, numeric etc) of each column

SFS_SA_Active <- subset (surveyData, Country == "South Africa" & Status_Category == "Active" & Total_kg_y > 0)
View(SFS_SA_Active)


attach(SFS_SA_Active)

summary(SFS_SA_Active)  #n=132 [South African Verrified SFS who provided provisioning rates]

sum(Total_kg_y)
# 3112932
mean(Total_kg_y)
# 23582.82
sd(Total_kg_y)
# 38844.17
mean(Total_kg_day)
# 64.61122
sd(Total_kg_day)
# 106.4226

# Extrapulated total being provisioned per year (correcting for SFS not providing Prov R, had taken averages of specific farm types to apply as per animal conservation instructions (had to calculate this backwards from previous faulty data in older paper versions)
ExtrapTotal = sum(Total_kg_y) + (187914.2308)
ExtrapTotal #3300846

EnergeticReq_Total <- 4003.6


### Contribution total for South Africa, Lesotho and Swaziland region ####################################################################

SFS_Lesotho <- subset (surveyData, Country == "Lesotho" & Status_Category == "Active" & Total_kg_y > 0)
sum(SFS_Lesotho$Total_kg_y)#4408.478

SFS_Swaziland <- subset (surveyData, Country == "Swaziland" & Status_Category == "Active" & Total_kg_y > 0)
sum(SFS_Swaziland$Total_kg_y) #11561.64

sum(SFS_Swaziland$Total_kg_y) + sum(SFS_Lesotho$Total_kg_y) + ExtrapTotal


Percentage_fulfillmetEnergReq <- ((ExtrapTotal + sum(SFS_Lesotho$Total_kg_y) + sum(SFS_Swaziland$Total_kg_y))/1000)/(EnergeticReq_Total)*100 # 16 tonnes from lesotho and swaziland
Percentage_fulfillmetEnergReq  #82.84585%

#############Calculate the number of SFS in different provinces and categories
SFS_SA_Active_AllVerified <- subset (surveyData, Country == "South Africa" & Status_Category == "Active" & Verified == "Y")
test <- table (SFS_SA_Active_AllVerified$Province)
test

# EC  FS  GT  KZN   LP  MP  NC  NW  WC
# 21  6   6   51    32  6   4   16  1

SFS_SA_All <- subset (surveyData, Country == "South Africa")
test2 <- table (SFS_SA_Active_AllVerified $Base_status)
test2

test3 <- table (SFS_SA_Active_AllVerified$Provisioning_Category)
test3  

# High       Low    Medium   Unknown Very High  Very Low 
# 15        21        21        11         8        67 

#Figuring out numbers of "Game Only" SFS, "Livestock Only" SFS and "Mixed
SFS_SA_GameOnly <- subset (surveyData, Country == "South Africa" & Status_Category == "Active" & Verified == "Y" & Game_kg_py == Total_kg_y & Game_kg_py > 0)
summary(SFS_SA_GameOnly) #26 SFS provide game only
GameOnlyTable <- table(SFS_SA_GameOnly$Base_status)
GameOnlyTable #8 sites exclusively in hunting season

SFS_SA_LivestockOnly <- subset (surveyData, Country == "South Africa" & Status_Category == "Active" & Verified == "Y" & Game_kg_py == 0 & Total_kg_y > 0)
summary(SFS_SA_LivestockOnly) #82 provide livestock only

SFS_SA_Mixed <- subset (surveyData, Country == "South Africa" & Status_Category == "Active" & Verified == "Y" & Game_kg_py > 0 & Total_kg_y > Game_kg_py)
summary(SFS_SA_Mixed) #24 SFS provide both game and livestock

24+26+82 # 132 active sites that provided provisioning rates


### Determining the contribution of each Provisioning category to the total #################################################################
#Total provisioned during the year 

#Very High 
VeryHigh <- subset(SFS_SA_Active, Provisioning_Category == "Very High")
VeryHigh_Persentage <- sum(VeryHigh$Total_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100     
VeryHigh_Persentage #38.42876%

#High 
High <- subset(SFS_SA_Active, Provisioning_Category == "High")
High_Persentage <- sum(High$Total_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100     
High_Persentage # 30.87576%

#Medium
Medium <- subset(SFS_SA_Active, Provisioning_Category == "Medium")
Medium_Persentage <- sum(Medium$Total_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100     
Medium_Persentage # 15.26831%

#Low
Low <- subset(SFS_SA_Active, Provisioning_Category == "Low")
Low_Persentage <- sum(Low$Total_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100     
Low_Persentage # 7.185161%

#VeryLow
VeryLow <- subset(SFS_SA_Active, Provisioning_Category == "Very Low")
VeryLow_Persentage <- sum(VeryLow$Total_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100     
VeryLow_Persentage # 8.242016%

## Persentage of each meat type provided #####################################################################################################


Beef_Persentage <- sum(SFS_SA_Active$Beef_kg_py)/sum(SFS_SA_Active$Total_kg_y)*100
Beef_Persentage  # 39.21943 %

Game_Persentage <- sum(SFS_SA_Active$Game_kg_py)/sum(SFS_SA_Active$Total_kg_y)*100
Game_Persentage  # 19.38126 %

Pork_Persentage <- sum(SFS_SA_Active$Pork_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100
Pork_Persentage  # 33.28814%

Sheep_Persentage <- sum(SFS_SA_Active$Sheep_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100
Sheep_Persentage  # 3.560661 %

Equine_Persentage <- sum(SFS_SA_Active$Equine_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100
Equine_Persentage  # 2.249952 %

Other_Persentage <- sum(SFS_SA_Active$Other_kg_y)/sum(SFS_SA_Active$Total_kg_y)*100
Other_Persentage  # 2.300565 %

Goat_Persentage <- 6591.498/3112932 * 100
Goat_Persentage # 0.2117456 %

Chicken_Persentage <- 30503.6/3112932 * 100
Chicken_Persentage # 0.9798993 %


## Provincial Totals ##########################################################################################################################

#LP  32
LP <- subset(SFS_SA_Active, Province == "LP")
LPsum <- sum(LP$Total_kg_y)
LPsum # 1198680
LPsum /sum(SFS_SA_Active$Total_kg_y) *100  # 38.50647 %
sum(LP$Beef_kg_py)/LPsum *100 # 35.9463 %
sum(LP$Game_kg_py)/LPsum *100 # 27.63802 %
sum(LP$Pork_kg_y)/LPsum *100 # 30.36316 %
sum(LP$Sheep_kg_y)/LPsum *100 # 4.124188 %
sum(LP$Equine_kg_y)/LPsum *100 # 0.1883166 %
sum(LP$Other_kg_y)/LPsum *100 # 1.74001 %

#NW  16
NW <- subset(SFS_SA_Active, Province == "NW")
NWsum <- sum(NW$Total_kg_y)
NWsum #616504.3
NWsum /sum(SFS_SA_Active$Total_kg_y) *100  #  19.80462 %
sum(NW$Beef_kg_py)/NWsum *100 # 15.02519 %
sum(NW$Game_kg_py)/NWsum *100 # 14.58806 %
sum(NW$Pork_kg_y)/NWsum *100 # 59.86928 %
sum(NW$Sheep_kg_y)/NWsum *100 # 3.827936 %
sum(NW$Equine_kg_y)/NWsum *100 # 1.403384 %
sum(NW$Other_kg_y)/NWsum *100 # 5.286145 %


#KZN  51
KZN <- subset(SFS_SA_Active, Province == "KZN")
KZNsum <- sum(KZN$Total_kg_y)
KZNsum # 752444.1
KZNsum /sum(SFS_SA_Active$Total_kg_y) *100  # 24.17155 %
sum(KZN$Beef_kg_py)/KZNsum *100 # 56.41399 %
sum(KZN$Game_kg_py)/KZNsum *100 # 10.93677 %
sum(KZN$Pork_kg_y)/KZNsum *100 # 25.29376 %
sum(KZN$Sheep_kg_y)/KZNsum *100 # 1.567972 %
sum(KZN$Equine_kg_y)/KZNsum *100 # 3.725013 %
sum(KZN$Other_kg_y)/KZNsum *100 # 2.062493 %

#EC  21
EC <- subset(SFS_SA_Active, Province == "EC")
ECsum <- sum(EC$Total_kg_y)
ECsum # 246776.5
ECsum /sum(SFS_SA_Active$Total_kg_y) *100  # 7.927462 %
sum(EC$Beef_kg_py)/ECsum *100 # 89.12903 %
sum(EC$Game_kg_py)/ECsum *100 #0 %
sum(EC$Pork_kg_y)/ECsum *100 # 2.236842 %
sum(EC$Sheep_kg_y)/ECsum *100 # 5.931015 %
sum(EC$Equine_kg_y)/ECsum *100 # 2.676019 %
sum(EC$Other_kg_y)/ECsum *100 # 0.02709739 %

#MP  6
MP <- subset(SFS_SA_Active, Province == "MP")
MPsum <- sum(MP$Total_kg_y)
MPsum #78402.94
MPsum /sum(SFS_SA_Active$Total_kg_y) *100 # 2.51862 %
sum(MP$Beef_kg_py)/MPsum *100 # 13.59761 %
sum(MP$Game_kg_py)/MPsum *100 #82.20142 %
sum(MP$Pork_kg_y)/MPsum *100 #0 %
sum(MP$Sheep_kg_y)/MPsum *100 # 0.4062858 %
sum(MP$Equine_kg_y)/MPsum *100 # 3.794683 %
sum(MP$Other_kg_y)/MPsum *100 #0 %

#GT  6
GT <- subset(SFS_SA_Active, Province == "GT")
GTsum <- sum(GT$Total_kg_y)
GTsum # 165054.4
GTsum /sum(SFS_SA_Active$Total_kg_y) *100 # 5.302218 %
sum(GT$Beef_kg_py)/GTsum *100 # 11.40446 %
sum(GT$Game_kg_py)/GTsum *100 # 12.66578 %
sum(GT$Pork_kg_y)/GTsum *100 # 64.5253 %
sum(GT$Sheep_kg_y)/GTsum *100 # 0.3475047 %
sum(GT$Equine_kg_y)/GTsum *100 # 11.05696 %
sum(GT$Other_kg_y)/GTsum *100 # 0 %

#FS  6
FS <- subset(SFS_SA_Active, Province == "FS")
FSsum <- sum(FS$Total_kg_y)
FSsum # 26244.59
FSsum /sum(SFS_SA_Active$Total_kg_y) *100 # 0.8430827 %
sum(FS$Beef_kg_py)/FSsum *100 # 54.18803 %
sum(FS$Game_kg_py)/FSsum *100 # 16.80581 %
sum(FS$Pork_kg_y)/FSsum *100 # 3.200659 %
sum(FS$Sheep_kg_y)/FSsum *100 # 12.18637 %
sum(FS$Equine_kg_y)/FSsum *100 # 3.778737 %
sum(FS$Other_kg_y)/FSsum *100 # 9.840393 %

#NC  4
NC<- subset(SFS_SA_Active, Province == "NC")
NCsum <- sum(NC$Total_kg_y)
NCsum # 16231.05
NCsum /sum(SFS_SA_Active$Total_kg_y) *100 # 0.5214071 %
sum(NC$Beef_kg_py)/NCsum *100 # 20.16294 %
sum(NC$Game_kg_py)/NCsum *100 # 61.85903 %
sum(NC$Pork_kg_y)/NCsum *100 # 0 %
sum(NC$Sheep_kg_y)/NCsum *100 # 3.92507 %
sum(NC$Equine_kg_y)/NCsum *100 # 14.05296 %
sum(NC$Other_kg_y)/NCsum *100 # 0 %

#WC  1
WC<- subset(SFS_SA_Active, Province == "WC")
WCsum <- sum(WC$Total_kg_y)
WCsum # 12594.13
WCsum /sum(SFS_SA_Active$Total_kg_y) *100 # 0.4045744 %
sum(WC$Beef_kg_py)/WCsum *100 # 47.24652 %
sum(WC$Game_kg_py)/WCsum *100 # 0 %
sum(WC$Pork_kg_y)/WCsum *100 # 0 %
sum(WC$Sheep_kg_y)/WCsum *100 # 52.75348 %
sum(WC$Equine_kg_y)/WCsum *100 # 0 %
sum(WC$Other_kg_y)/WCsum *100 # 0 %

WCsum + NCsum + FSsum + GTsum + LPsum + KZNsum + NWsum + ECsum + MPsum 

detach(SFS_SA_Active)

## Graphs ###################################################################################################################
### Barplot of Total contribution in each province

data <- tbl_df(SFS_SA_Active)

data

Prov_Sum <- data%>%
  group_by(Province) %>%
  summarise(count = n(), 
            TOTAL_kgpy = sum(Total_kg_y, na.rm = TRUE),
            Beef = sum(Beef_kg_py),
            Pork = sum(Pork_kg_y),
            Game = sum(Game_kg_py),
            Sheep = sum(Sheep_kg_y),
            Equine = sum(Equine_kg_y),
            Other= sum(Other_kg_y)
  )

Prov_Sum <- Prov_Sum[with(Prov_Sum, order(-TOTAL_kgpy)),]

Prov_Sum

barplot(Prov_Sum$TOTAL_kgpy/1000, width = c(.1, .1), col= "darkgrey",space = 0.5, main="", 
        names.arg = Prov_Sum$Province,
        ylab = "Total annual food provisioning (tonne)", cex.axis = 1.2, cex.names = 1.3, cex.lab = 1.4,
        ylim=c(0,1200))


### Cumulative Ranked barplot with an indication of each category contribution


RankedProvR <- SFS_SA_Active 

RankedProvR <- arrange(RankedProvR, desc(Total_kg_y))

sum(RankedProvR$Total_kg_y)

cum_no <- c()    #object that will become column in data that houses all the cumulative provisioning rates.

cum_no <- data_frame(RankedProvR$Total_kg_y)
cum_no$`RankedProvR$Total_kg_y` <- cum_no$`RankedProvR$Total_kg_y`/1000
cum_no

cum_no <-cumsum(cum_no)

sum(cum_no$`RankedProvR$Total_kg_y`)

cum_perc <- data_frame(cum_no$`RankedProvR$Total_kg_y`/3112.932 *100)
names(cum_perc) <- c("Cum_percentage")

par(mar= c(5,6,2,9))
plot(cum_perc$Cum_percentage, type = "h", 
     col = "darkgrey", frame = FALSE,
     lwd = 3,
     yaxt ="n", xaxt = "n",
     xaxs = "i", yaxs = "i",
     xlab = "SFS ranked according to provisioning rate catagory",
     ylab = "Cumulative persentage of annual total provisioning (%)",
     xlim = c(0,132), ylim = c(0,100),
     cex.lab = 1.2,cex.axis = 1.5)
axis(2, las=2, at = c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100), cex.axis = 1.1)
axis(1, as=0)

#axis(4, at = c (0, 36.06536,  68.2638, 84.18619, 91.27383, 100), labels= c("0", "36 %", "68 %", "84%", "91%", "100%" ), las = 2, cex.axis =0.9)
#axis(4, at = c (0, 18,  53, 77, 88, 96.5), labels = c("", "Very High", "High", "Medium", "Low", "Very Low"), las = 2, tick = FALSE, cex.axis = 1.2)
#axis(1, at =c(0, 10, 20, 30, 40, 50, 60, 70, 80, 90, 100, 110, 120, 130, cex.axis = 1.1))
#abline(h = c(36.06536,  68.2638, 84.18619, 91.27383), col= "black", lty =2, lwd = 2)
abline(h = c(0), col= "black", lty =1, lwd = 1.4)

#mtext(text= "Very High", side = 4, adj = 0.00001, cex = 1, las =2)     #to add text into margins of plot


#Determining line placement
36.06536
36.06536+32.19844 # 68.2638
36.06536+32.19844+15.92239 #84.18619
36.06536+32.19844+15.92239+7.08764 #91.27383
36.06536+32.19844+15.92239+7.08764+8.726173 #100

### Pie charts per province
#change data structure to be per Province
coln = Prov_Sum$Province # remember names of what is going to be columns

Transp_Prov <- as.data.frame(t(Prov_Sum[,-1]))

Transp_Prov <- Transp_Prov[-c(1, 2),]
colnames(Transp_Prov) <- coln

sapply(Transp_Prov, class)
######### Data now in the right format
#graphical parameters
kleur = rainbow(9)
lbl = labels("")


#LP
pie(Transp_Prov$LP, labels =lbl, col = kleur)

#KZN
pie(Transp_Prov$KZN, labels =lbl, col = kleur)

######################################################################################################################################
# To Clear working environment
rm(list=ls())
graphics.off()

# Read in Data
All_Pilot_Data <- read.csv ("C:/Users/Kim/Dropbox/Owen Lab_KFake/Research/Fall 2016 American Robin Study (WNV Pilot)/R Pilot Study/Pilot Study Data for R.csv", stringsAsFactors = FALSE)


#make treatment group column 
All_Pilot_Data["TxGroup"] <- ifelse(All_Pilot_Data$Bird <6, "Good", "Poor")  
Food_Pilot_Data["TxGroup"] <- ifelse(Food_Pilot_Data$Bird <6, "Good", "Poor")

#change VE group timepoint name 
All_Pilot_Data$Timepoint <- gsub("Post VE", "Post Food Restriction", All_Pilot_Data$Timepoint)

#make Log10 WBC column
All_Pilot_Data["LogWBC"] <- NA
All_Pilot_Data$LogWBC <- log10(All_Pilot_Data$WBC.10.000.RBC)

#make Log10 HL column
All_Pilot_Data["LogHL"] <- NA
All_Pilot_Data$LogHL <- log10(All_Pilot_Data$H.L)

#make sqrt_Hematocrit column
All_Pilot_Data["Sqrt_Hematocrit"] <- NA
All_Pilot_Data$Sqrt_Hematocrit <- sqrt(All_Pilot_Data$Hematocrit....)

#make sqrt_Hematocrit column
All_Pilot_Data["Sqrt_Haptoglobin"] <- NA
All_Pilot_Data$Sqrt_Haptoglobin <- sqrt(All_Pilot_Data$Haptoglobin)

#Make columns with numeric versions of score data
All_Pilot_Data["Fat.Score_Num"] <- NA
All_Pilot_Data$Fat.Score_Num = as.numeric (as.character(All_Pilot_Data$Fat.Score))

All_Pilot_Data["Muscle.Score_Num"] <- NA
All_Pilot_Data$Muscle.Score_Num = as.numeric (as.character(All_Pilot_Data$Muscle.Score))

All_Pilot_Data["Hemolysis.Score_Num"] <- NA
All_Pilot_Data$Hemolysis.Score_Num = as.numeric (as.character(All_Pilot_Data$Hemolysis.Score))

All_Pilot_Data["Hemagglutination.Score_Num"] <- NA
All_Pilot_Data$Hemagglutination.Score_Num = as.numeric (as.character(All_Pilot_Data$Hemagglutination.Score))

#Filter out other Timepoints
FR_Pilot_Data <- All_Pilot_Data %>%
  filter (Timepoint %in% c("Post Acclimation", "Post Food Restriction"))

# Food Data Grouping
Food_Pilot_Data <- Food_Pilot_Data %>%
  group_by(Date)


# WBC Analysis ---------------------------------------------------------------------

#WBC - filtering
WBC_Data <- All_Pilot_Data[, c("TxGroup", "Bird", "Timepoint", "WBC.10.000.RBC", "LogWBC")] %>%
  filter(Timepoint %in% c("Post Acclimation", "Post Food Restriction"))

Tx_PA_WBC <- WBC_Data[, c("TxGroup", "Timepoint", "WBC.10.000.RBC", "LogWBC")] %>%
  filter(Timepoint == "Post Acclimation") %>%
  filter (TxGroup == "Poor")
Tx_PFR_WBC <- WBC_Data[, c("TxGroup", "Timepoint", "WBC.10.000.RBC", "LogWBC")] %>%
  filter(Timepoint == "Post Food Restriction")%>%
  filter (TxGroup == "Poor")
Con_PA_WBC <- WBC_Data[, c("TxGroup", "Timepoint", "WBC.10.000.RBC", "LogWBC")] %>%
  filter(Timepoint == "Post Acclimation") %>%
  filter (TxGroup == "Good")
Con_PFR_WBC <- WBC_Data[, c("TxGroup", "Timepoint", "WBC.10.000.RBC", "LogWBC")] %>%
  filter(Timepoint == "Post Food Restriction") %>%
  filter (TxGroup == "Good")

#WBC - Shapiro-Wilk Test for normality - PASS
shapiro.test(Tx_PA_WBC$WBC.10.000.RBC)
shapiro.test(Tx_PFR_WBC$WBC.10.000.RBC)
shapiro.test(Con_PA_WBC$WBC.10.000.RBC)
shapiro.test(Con_PFR_WBC$WBC.10.000.RBC)

#WBC - Shapiro-Wilk Test for normality - PASS
shapiro.test(Tx_PA_WBC$LogWBC)
shapiro.test(Tx_PFR_WBC$LogWBC)
shapiro.test(Con_PA_WBC$LogWBC) #Bearly passes
shapiro.test(Con_PFR_WBC$LogWBC)


#WBC - bartlett for equal variance - Bearly PASS
bartlett.test(All_Pilot_Data$WBC.10.000.RBC, All_Pilot_Data$Timepoint)

#LogWBC - F-Test for equal variance - PASS
bartlett.test(All_Pilot_Data$LogWBC, All_Pilot_Data$Timepoint)

#LogWBC - t-test
t.test(Tx_PA_WBC$LogWBC, Tx_PFR_WBC$LogWBC, paired=TRUE)  

WBC_Summary <- summarySE(WBC_Data, measurevar = "LogWBC", groupvars = c("TxGroup", "Timepoint"))
WBC_Summary

WBC_Summary["Trans_Mean"] <- NA
WBC_Summary$Trans_Mean <- (10^(WBC_Summary$LogWBC))

WBC_Summary["Trans_se"] <- NA
WBC_Summary$Trans_se <-10^(WBC_Summary$se)

WBC_Summary
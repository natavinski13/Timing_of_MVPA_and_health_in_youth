### Associations between Morning and Afternoon MVPA in children

#Libraries
library(readxl) #Read excel data
library(tidyverse) #Necessary for data wrangling, figures and diagnostic plots
library(broom) #Important for diagnostic plots
library(ppcor) #Partial correlation needs this package
library(GGally) #Not necessary, but creates cool graphs of correlation
library(naniar) #Replacing codes of missing data to NA
library(psych) # to use the function "with" to do the descriptive statistics
library(car) # Do the VIF
library(caret) #To produce non-linear regressions
library(lmtest) #Breusch-Pagan test
library(RVAideMemoire) # Necessary to do the shapiro test
library(psychometric) #provides confidence intervals for R2
library(gridExtra) # Put two plots side by side with ggplot
library(ggpubr) #Create plots to check normality
library(rstatix) #To use the function "add_significance"
library(scales) # Makes a break in the labels line (put in two or more when necessary)
library(ggiraphExtra)
library(srvyr) # Package to work with weighted data
library(quantreg) # Model quantile regression


data1 <- readxl::read_excel(
  "Database-Patterns - Final - Clean.xlsx", sheet = 1)

################################################################################
################################################################################
################################################################################


# CHILDREN AND ADOLESCENTS DATA


ChildrenData <- data1 %>% mutate(TotalST = `8_Média de Sedentary`+
                                   `9_Média de Sedentary`+`10_Média de Sedentary` +
                                   `11_Média de Sedentary`+`12_Média de Sedentary`+
                                   `13_Média de Sedentary`+`14_Média de Sedentary`+
                                   `15_Média de Sedentary` + `16_Média de Sedentary`+
                                   `17_Média de Sedentary`+`18_Média de Sedentary` +
                                   `19_Média de Sedentary`,
                                 MorningST = `8_Média de Sedentary`+
                                   `9_Média de Sedentary`+`10_Média de Sedentary` +
                                   `11_Média de Sedentary`,
                                 AfternoonST = `12_Média de Sedentary`+
                                   `13_Média de Sedentary`+`14_Média de Sedentary`+
                                   `15_Média de Sedentary` + `16_Média de Sedentary`+
                                   `17_Média de Sedentary`+`18_Média de Sedentary` +
                                   `19_Média de Sedentary`,
                                 TotalLPA = `8_Média de Light`+
                                   `9_Média de Light`+`10_Média de Light` +
                                   `11_Média de Light` + `12_Média de Light` +
                                   `13_Média de Light`+`14_Média de Light`+
                                   `15_Média de Light`+`16_Média de Light`+
                                   `17_Média de Light`+ `18_Média de Light`+
                                   `19_Média de Light`,
                                 MorningLPA = `8_Média de Light`+
                                   `9_Média de Light`+`10_Média de Light` +
                                   `11_Média de Light`,
                                 AfternoonLPA = `12_Média de Light` +
                                   `13_Média de Light`+`14_Média de Light`+
                                   `15_Média de Light`+`16_Média de Light`+
                                   `17_Média de Light`+ `18_Média de Light`+
                                   `19_Média de Light`,
                                 MorningMVPA = `8_Média de Total MVPA` +
                                   `9_Média de Total MVPA` +
                                   `10_Média de Total MVPA` +
                                   `11_Média de Total MVPA`,
                                 AfternoonMVPA =`12_Média de Total MVPA` +
                                   `13_Média de Total MVPA` +
                                   `14_Média de Total MVPA` +
                                   `15_Média de Total MVPA` +
                                   `16_Média de Total MVPA` +
                                   `17_Média de Total MVPA` +
                                   `18_Média de Total MVPA` +
                                   `19_Média de Total MVPA`)  |> 
  dplyr::mutate(TotalMVPA = MorningMVPA + AfternoonMVPA) |> 
  dplyr::rename("ID" = "ID...1", "HandgripR" = "HANDGRIP_RIGHT", "HandgripL" = "HANDGRIP_LEFT",
                "VaieVem" = "VAI_VEM", "Abdominal" = "ABD","ShuttleRun" = "SHUTTLE_RUN", 
                "PushUp" = "PUSH_UP", "HorizontalImpulse" = "HORIZONTAL_IMPULSE",
                "VerticalImpulse" = "VERTICAL_IMPULSE", "BackScratch" = "BACK_SCRATCH_MAX",
                "SitAndReach" = "SIT_AND_REACH_MAX", "Speed40m" = "SPEED_40M",
                "Speed20m" = "SPEED_20M",
                "TotalMorningWEAR" = "Total_Manha_WEAR",
                "TotalAfternoonWEAR" = "Total_Tarde_WEAR", "WeightFactor" = "Weight_factor") |> 
  dplyr::mutate(ShuttleRun = ifelse(ShuttleRun < 17.5, ShuttleRun, 999999)) |> 
  dplyr::mutate(Speed20m = ifelse(Speed20m < 10, Speed20m, 999999)) |>
  dplyr::mutate(Speed40m = ifelse(Speed40m < 10.96, Speed40m, 999999)) |>
  replace_with_na(replace = list (HandgripR = 999999,
                                  HandgripL = 999999,
                                  VaieVem = 999999,
                                  Abdominal = 999999,
                                  ShuttleRun = 999999,
                                  PushUp = 999999,
                                  HorizontalImpulse = 999999,
                                  VerticalImpulse = 999999,
                                  BackScratch = 999999,
                                  SitAndReach = 999999,
                                  Speed40m = 999999,
                                  Speed20m = 999999,
                                  MorningMVPA = 999999,
                                  AfternoonMVPA = 999999,
                                  TotalMVPA = 999999,
                                  BMI = 999999)) %>%
  mutate(Handgrip = ifelse(is.na(HandgripR), HandgripL, 
                           ifelse(is.na(HandgripL), HandgripR,       
                                  ifelse(HandgripR > HandgripL, HandgripR, HandgripL)))) %>% 
  subset(select = -c(HandgripL, HandgripR)) %>% 
  dplyr::select(ID, WeightFactor, Age, Sex, Weight, Height, BMI,
                Handgrip, VaieVem,Abdominal, ShuttleRun, PushUp, HorizontalImpulse,
                VerticalImpulse, Speed20m, Speed40m, MorningMVPA, AfternoonMVPA,
                TotalMVPA, BMI,TotalST, MorningST, AfternoonST, TotalLPA, 
                MorningLPA, AfternoonLPA,Total_WEAR, TotalMorningWEAR, 
                TotalAfternoonWEAR) 



ChildrenData$Sex <- factor(ChildrenData$Sex, levels = c(0:1), labels = c("Girls", "Boys"))
ChildrenData$ID <- factor(ChildrenData$ID)


summary(ChildrenData)


# DESCRIPTIVE STATISTICS ####

ggqqplot(ChildrenData, x = "Abdominal", facet.by = "Sex")


MeanVariables <- ChildrenData  |> 
  dplyr::summarise(n(),
            Age = mean(Age),
            Weight = mean(Weight),
            Height = mean(Height),
            BMI = mean(BMI),
            Handgrip = mean(Handgrip, na.rm = TRUE),
            VaieVem = mean(VaieVem, na.rm = TRUE),
            Abdominal = median(Abdominal, na.rm = TRUE),
            ShuttleRun = mean(ShuttleRun, na.rm = TRUE),
            PushUp = median(PushUp, na.rm = TRUE),
            HorizontalImpulse= mean(HorizontalImpulse, na.rm = TRUE),
            VerticalImpulse= mean(VerticalImpulse, na.rm = TRUE),
            Speed20m = median(Speed20m,  na.rm = TRUE),
            Speed40m = median(Speed40m,  na.rm = TRUE)) |>  
  dplyr::mutate_if(is.numeric, format, 1) # makes lot of decimal numbers


MeanVariables


MeanVariables2 <- ChildrenData  |> 
  dplyr::summarise(n(),
                   Age = sd(Age),
                   Weight = sd(Weight),
                   Height = sd(Height),
                   BMI = sd(BMI),
                   Handgrip = sd(Handgrip, na.rm = TRUE),
                   VaieVem = sd(VaieVem, na.rm = TRUE),
                   Abdominal = IQR(Abdominal, na.rm = TRUE),
                   ShuttleRun = sd(ShuttleRun, na.rm = TRUE),
                   PushUp = IQR(PushUp, na.rm = TRUE),
                   HorizontalImpulse= sd(HorizontalImpulse, na.rm = TRUE),
                   VerticalImpulse= sd(VerticalImpulse, na.rm = TRUE),
                   Speed20m = IQR(Speed20m,  na.rm = TRUE),
                   Speed40m = IQR(Speed40m,  na.rm = TRUE)) |> 
  dplyr::mutate_if(is.numeric, format, 2) # makes lot of decimal numbers



MeanVariables2

rm(MeanVariables, MeanVariables2)

#MVPA
MVPAVariables <- ChildrenData  |> 
  dplyr::group_by(Sex) |> 
  dplyr::summarise(TotalST = format(median(TotalST, na.rm = T), digits = 4),
            TotalLPA = format(median(TotalLPA, na.rm = T), digits = 4),
            TotalMVPA = format(median(TotalMVPA, na.rm = T), digits = 3),
            MorningMVPA = format(median(MorningMVPA, na.rm = T), digits = 3),
            AfternoonMVPA = format(median(AfternoonMVPA, na.rm = T), digits = 4)) |> 
  dplyr::mutate_if(is.numeric, format, 1) # makes lot of decimal numbers

MVPAVariables


SDMVPAVariables <- ChildrenData  |> 
  dplyr::group_by(Sex) |> 
  dplyr::summarise(TotalST = format(IQR(TotalST, na.rm = T), digits = 3),
            TotalLPA = format(IQR(TotalLPA, na.rm = T), digits = 3),
            TotalMVPA = format(IQR(TotalMVPA, na.rm = T), digits = 4),
            MorningMVPA = format(IQR(MorningMVPA, na.rm = T), digits = 4),
            AfternoonMVPA = format(IQR(AfternoonMVPA, na.rm = T), digits = 4)) |> 
  dplyr::mutate_if(is.numeric, format, 1) # makes lot of decimal numbers


SDMVPAVariables

rm(MVPAVariables, SDMVPAVariables)


#Comparing sex at the baseline ####

ChildrenData %>% t_test(Age ~ Sex) %>% add_significance()
ChildrenData %>% t_test(Weight ~ Sex) %>% add_significance()
ChildrenData %>% t_test(Height ~ Sex) %>% add_significance()
ChildrenData %>% t_test(BMI ~ Sex) %>% add_significance()
wilcox.test(TotalST ~ Sex, ChildrenData)
wilcox.test(TotalLPA ~ Sex, ChildrenData)
wilcox.test(TotalMVPA ~ Sex, ChildrenData)
wilcox.test(MorningMVPA ~ Sex, ChildrenData)
wilcox.test(AfternoonMVPA ~ Sex, ChildrenData)
wilcox.test(RelativeMorningMVPA ~ Sex, ChildrenData)
wilcox.test(RelativeAfternoonMVPA ~ Sex, ChildrenData)




ChildrenData %>% t_test(ShuttleRun ~ Sex) %>% add_significance()
wilcox.test(Speed20m ~ Sex, ChildrenData) %>% add_significance()
wilcox.test(Speed40m ~ Sex, ChildrenData) %>% add_significance()
ChildrenData %>% t_test(HorizontalImpulse ~ Sex) %>% add_significance()
ChildrenData %>% t_test(VerticalImpulse ~ Sex) %>% add_significance()
wilcox.test(PushUp ~ Sex, ChildrenData)
wilcox.test(Abdominal ~ Sex, ChildrenData)
ChildrenData %>% t_test(Handgrip ~ Sex) %>% add_significance()
ChildrenData %>% t_test(VaieVem ~ Sex) %>% add_significance()



wilcox.test(ChildrenData$MorningMVPA, ChildrenData$AfternoonMVPA)



Girls <- ChildrenData %>% 
  filter(Sex == "Girls")

wilcox.test(Girls$AfternoonMVPA, Girls$MorningMVPA)



# Creating models ####

# Here I will adjust for number of hours in each period (morning or afternoon)

ChildrenData <- ChildrenData |> 
  mutate(MorningMVPA = MorningMVPA/4,
         AfternoonMVPA = AfternoonMVPA/8,
         TotalMVPA = TotalMVPA/12) # ATTENTION TO THIS PART

# 20-m Shuttle run (vai e vem)

#Morning
VaieVemM1 <- rq(VaieVem ~ MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemM1)
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(VaieVem ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemM2)
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(VaieVem ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemM3)
summary.rq(VaieVemM3, se = "rank")




#Afternoon
VaieVemA1 <- rq(VaieVem ~ AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemA1)
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(VaieVem ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemA2)
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(VaieVem ~ AfternoonMVPA + Age + Sex +  BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemA3)
summary.rq(VaieVemA3, se = "rank")


#Total MVPA
VaieVemT1 <- rq(VaieVem ~ TotalMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemT1)
summary.rq(VaieVemT1, se = "rank")

VaieVemT2 <- rq(VaieVem ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(VaieVemT2)
summary.rq(VaieVemT2, se = "rank")


# Handgrip strength

#Morning
HandgripM1 <- rq(Handgrip ~ MorningMVPA, data = ChildrenData)
summary.rq(HandgripM1)
summary.rq(HandgripM1, se = "rank")

HandgripM2 <- rq(Handgrip ~ MorningMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(HandgripM2)
summary.rq(HandgripM2, se = "rank")

HandgripM3 <- rq(Handgrip ~ MorningMVPA + Age + Sex +  BMI + AfternoonMVPA, data = ChildrenData)
summary.rq(HandgripM3)
summary.rq(HandgripM3, se = "rank")

#Afternoon
HandgripA1 <- rq(Handgrip ~ AfternoonMVPA, data = ChildrenData)
summary.rq(HandgripA1)
summary.rq(HandgripA1, se = "rank")

HandgripA2 <- rq(Handgrip ~ AfternoonMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(HandgripA2)
summary.rq(HandgripA2, se = "rank")

HandgripA3 <- rq(Handgrip ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(HandgripA3)
summary.rq(HandgripA3, se = "rank")


#Total MVPA
HandgripT1 <- rq(Handgrip ~ TotalMVPA, data = ChildrenData)
summary.rq(HandgripT1)
summary.rq(HandgripT1, se = "rank")

HandgripT2 <- rq(Handgrip ~ TotalMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(HandgripT2)
summary.rq(HandgripT2, se = "rank")


# Standing long jump

#Morning
HImpulseM1 <- rq(HorizontalImpulse ~ MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseM1)
summary.rq(HImpulseM1, se = "rank")

HImpulseM2 <- rq(HorizontalImpulse ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseM2)
summary.rq(HImpulseM2, se = "rank")

HImpulseM3 <- rq(HorizontalImpulse ~ MorningMVPA + Age + Sex +  BMI + AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseM3)
summary.rq(HImpulseM3, se = "rank")

#Afternoon
HImpulseA1 <- rq(HorizontalImpulse ~ AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseA1)
summary.rq(HImpulseA1, se = "rank")

HImpulseA2 <- rq(HorizontalImpulse ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseA2)
summary.rq(HImpulseA2, se = "rank")

HImpulseA3 <- rq(HorizontalImpulse ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseA3)
summary.rq(HImpulseA3, se = "rank")


#Total MVPA
HImpulseT1 <- rq(HorizontalImpulse ~ TotalMVPA, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseT1)
summary.rq(HImpulseT1, se = "rank")

HImpulseT2 <- rq(HorizontalImpulse ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(HImpulseT2)
summary.rq(HImpulseT2, se = "rank")


# Vertical jump

#Morning
VImpulseM1 <- rq(VerticalImpulse ~ MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseM1)
summary.rq(VImpulseM1, se = "rank")

VImpulseM2 <- rq(VerticalImpulse ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseM2)
summary.rq(VImpulseM2, se = "rank")

VImpulseM3 <- rq(VerticalImpulse ~ MorningMVPA + Age + Sex +  BMI + AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseM3)
summary.rq(VImpulseM3, se = "rank")

#Afternoon
VImpulseA1 <- rq(VerticalImpulse ~ AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseA1)
summary.rq(VImpulseA1, se = "rank")


VImpulseA2 <- rq(VerticalImpulse ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseA2)
summary.rq(VImpulseA2, se = "rank")

VImpulseA3 <- rq(VerticalImpulse ~ AfternoonMVPA + Age + Sex + + BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseA3)
summary.rq(VImpulseA3, se = "rank")


#Total MVPA
VImpulseT1 <- rq(VerticalImpulse ~ TotalMVPA, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseT1)
summary.rq(VImpulseT1, se = "rank")


VImpulseT2 <- rq(VerticalImpulse ~ TotalMVPA + Age + Sex +  BMI, tau = 0.5, data = ChildrenData)
summary.rq(VImpulseT2)
summary.rq(VImpulseT2, se = "rank")


# Push-ups

#Morning
PushupM1 <- rq(PushUp ~ MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(PushupM1)
summary.rq(PushupM1, se = "rank")

PushupM2 <- rq(PushUp ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(PushupM2)
summary.rq(PushupM2, se = "rank")

PushupM3 <- rq(PushUp ~ MorningMVPA + Age + Sex +  BMI + AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(PushupM3)
summary.rq(PushupM3, se = "rank")

#Afternoon
PushupA1 <- rq(PushUp ~ AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(PushupA1)
summary.rq(PushupA1, se = "rank")

PushupA2 <- rq(PushUp ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(PushupA2)
summary.rq(PushupA2, se = "rank")

PushupA3 <- rq(PushUp ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(PushupA3)
summary.rq(PushupA3, se = "rank")

#Total MVPA
PushupT1 <- rq(PushUp ~ TotalMVPA, tau = 0.5, data = ChildrenData)
summary.rq(PushupT1)
summary.rq(PushupT1, se = "rank")

PushupT2 <- rq(PushUp ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(PushupT2)
summary.rq(PushupT2, se = "rank")



# Curl-ups

#Morning
AbdominalM1 <- rq(Abdominal ~ MorningMVPA, data = ChildrenData)
summary.rq(AbdominalM1)
summary.rq(AbdominalM1, se = "rank")

AbdominalM2 <- rq(Abdominal ~ MorningMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(AbdominalM2)
summary.rq(AbdominalM2, se = "rank")

AbdominalM3 <- rq(Abdominal ~ MorningMVPA + Age + Sex +  BMI + AfternoonMVPA, data = ChildrenData)
summary.rq(AbdominalM3)
summary.rq(AbdominalM3, se = "rank")


#Afternoon
AbdominalA1 <- rq(Abdominal ~ AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(AbdominalA1)
summary.rq(AbdominalA1, se = "rank")

AbdominalA2 <- rq(Abdominal ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(AbdominalA2)
summary.rq(AbdominalA2, se = "rank")

AbdominalA3 <- rq(Abdominal ~ AfternoonMVPA + Age + Sex + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(AbdominalA3)
summary.rq(AbdominalA3, se = "rank")

#Total MVPA
AbdominalT1 <- rq(Abdominal ~ TotalMVPA, tau = 0.5, data = ChildrenData)
summary.rq(AbdominalT1)
summary.rq(AbdominalT1, se = "rank")

AbdominalT2 <- rq(Abdominal ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(AbdominalT2)
summary.rq(AbdominalT2, se = "rank")



# Speed 20 m

#Morning
Speed20mM1 <- rq(Speed20m ~ MorningMVPA, data = ChildrenData)
summary.rq(Speed20mM1)
summary.rq(Speed20mM1, se = "rank")

Speed20mM2 <- rq(Speed20m ~ MorningMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(Speed20mM2)
summary.rq(Speed20mM2, se = "rank")

Speed20mM3 <- rq(Speed20m ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, data = ChildrenData)
summary.rq(Speed20mM3)
summary.rq(Speed20mM3, se = "rank")

#Afternoon
Speed20mA1 <- rq(Speed20m ~ AfternoonMVPA, data = ChildrenData)
summary.rq(Speed20mA1)
summary.rq(Speed20mA1, se = "rank")

Speed20mA2 <- rq(Speed20m ~ AfternoonMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(Speed20mA2)
summary.rq(Speed20mA2, se = "rank")

Speed20mA3 <- rq(Speed20m ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(Speed20mA3)
summary.rq(Speed20mA3, se = "rank")


#Total MVPA
Speed20mT1 <- rq(Speed20m ~ TotalMVPA, data = ChildrenData)
summary.rq(Speed20mT1)
summary.rq(Speed20mT1, se = "rank")

Speed20mT2 <- rq(Speed20m ~ TotalMVPA + Age + Sex +  BMI, data = ChildrenData)
summary.rq(Speed20mT2)
summary.rq(Speed20mT2, se = "rank")


# Speed 40 m

#Morning
Speed40mM1 <- rq(Speed40m ~ MorningMVPA, data = ChildrenData)
summary.rq(Speed40mM1)
summary.rq(Speed40mM1, se = "rank")

Speed40mM2 <- rq(Speed40m ~ MorningMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(Speed40mM2)
summary.rq(Speed40mM2, se = "rank")

Speed40mM3 <- rq(Speed40m ~ MorningMVPA + Age + Sex +  BMI + AfternoonMVPA, data = ChildrenData)
summary.rq(Speed40mM3)
summary.rq(Speed40mM3, se = "rank")

#Afternoon
Speed40mA1 <- rq(Speed40m ~ AfternoonMVPA, data = ChildrenData)
summary.rq(Speed40mA1)
summary.rq(Speed40mA1, se = "rank")

Speed40mA2 <- rq(Speed40m ~ AfternoonMVPA + Age + Sex + BMI, data = ChildrenData)
summary.rq(Speed40mA2)
summary.rq(Speed40mA2, se = "rank")

Speed40mA3 <- rq(Speed40m ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(Speed40mA3)
summary.rq(Speed40mA3, se = "rank")


#Total MVPA
Speed40mT1 <- rq(Speed40m ~ TotalMVPA, data = ChildrenData)
summary.rq(Speed40mT1)
summary.rq(Speed40mT1, se = "rank")

Speed40mT2 <- rq(Speed40m ~ TotalMVPA + Age + Sex +  BMI, data = ChildrenData)
summary.rq(Speed40mT2)
summary.rq(Speed40mT2, se = "rank")


# 4x10m shuttlerun

#Morning
ShuttlerunM1 <- rq(ShuttleRun ~ MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunM1)
summary.rq(ShuttlerunM1, se = "rank")

ShuttlerunM2 <- rq(ShuttleRun ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunM2)
summary.rq(ShuttlerunM2, se = "rank")

ShuttlerunM3 <- rq(ShuttleRun ~ MorningMVPA + Age + Sex +  BMI + AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunM3)
summary.rq(ShuttlerunM3, se = "rank")

#Afternoon
ShuttlerunA1 <- rq(ShuttleRun ~ AfternoonMVPA, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunA1)
summary.rq(ShuttlerunA1, se = "rank")

ShuttlerunA2 <- rq(ShuttleRun ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunA2)
summary.rq(ShuttlerunA2, se = "rank")

ShuttlerunA3 <- rq(ShuttleRun ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunA3)
summary.rq(ShuttlerunA3, se = "rank")

#Total MVPA
ShuttlerunT1 <- rq(ShuttleRun ~ TotalMVPA, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunT1)
summary.rq(ShuttlerunT1, se = "rank")

ShuttlerunT2 <- rq(ShuttleRun ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = ChildrenData)
summary.rq(ShuttlerunT2)
summary.rq(ShuttlerunT2, se = "rank")


# BMI

#Morning
BMIM1 <- rq(BMI ~ MorningMVPA, data = ChildrenData)
summary.rq(BMIM1)
summary.rq(BMIM1, se = "rank")

BMIM2 <- rq(BMI ~ MorningMVPA + Age + Sex, data = ChildrenData)
summary.rq(BMIM2)
summary.rq(BMIM2, se = "rank")

BMIM3 <- rq(BMI ~ MorningMVPA + Age + Sex + AfternoonMVPA, data = ChildrenData)
summary.rq(BMIM3)
summary.rq(BMIM3, se = "rank")

#Afternoon
BMIA1 <- rq(BMI ~ AfternoonMVPA, data = ChildrenData)
summary.rq(BMIA1)
summary.rq(BMIA1, se = "rank")

BMIA2 <- rq(BMI ~ AfternoonMVPA + Age + Sex, data = ChildrenData)
summary.rq(BMIA2)
summary.rq(BMIA2, se = "rank")

BMIA3 <- rq(BMI ~ AfternoonMVPA + Age + Sex + MorningMVPA, tau = 0.5, data = ChildrenData)
summary.rq(BMIA3)
summary.rq(BMIA3, se = "rank")



#Total MVPA
BMIT1 <- rq(BMI ~ TotalMVPA, data = ChildrenData)
summary.rq(BMIT1)
summary.rq(BMIT1, se = "rank")

BMIT2 <- rq(BMI ~ TotalMVPA + Age + Sex, data = ChildrenData)
summary.rq(BMIT2)
summary.rq(BMIT2, se = "rank")


rm(BMIA1, BMIA2, BMIA3, BMIM1, BMIM2, BMIM3, AbdominalA1, AbdominalA2, AbdominalA3,
   AbdominalM1, AbdominalM2, AbdominalM3, HandgripA1, HandgripA2, HandgripA3,
   HandgripM1, HandgripM2, HandgripM3, HImpulseA1, HImpulseA2, HImpulseA3,
   HImpulseM1, HImpulseM2, HImpulseM3, PushupA1, PushupA2, PushupA3, PushupM1,
   PushupM2, PushupM3, ShuttlerunA1, ShuttlerunA2, ShuttlerunA3, ShuttlerunM1,
   ShuttlerunM2, ShuttlerunM3, Speed20mA1, Speed20mA2, Speed20mA3, Speed20mM1,
   Speed20mM2, Speed20mM3,Speed40mA1, Speed40mA2, Speed40mA3, Speed40mM1,
   Speed40mM2, Speed40mM3, VaieVemA1, VaieVemA2, VaieVemA3, VaieVemM1, VaieVemM2,
   VaieVemM3, VImpulseA1, VImpulseA2, VImpulseA3, VImpulseM1, VImpulseM2, 
   VImpulseM3, AbdominalT1, AbdominalT2, BMIT1, BMIT2, HandgripT1, HandgripT2,
   HImpulseT1, HImpulseT2, PushupT1, PushupT2, ShuttlerunT1, ShuttlerunT2, 
   Speed20mT1, Speed20mT2, Speed40mT1, Speed40mT2, VaieVemT1, VaieVemT2,
   VImpulseT1, VImpulseT2, data1)



# Sensitivity analysis ###

#For morning

Below <- ChildrenData %>% 
  dplyr::filter(MorningMVPA <= median(MorningMVPA, na.rm = T))

Above <- ChildrenData %>% 
  dplyr::filter(MorningMVPA > median(MorningMVPA, na.rm = T))

summary(Below$MorningMVPA)
summary(Above$MorningMVPA)


# Creating models for morning MVPA ####

#20m shuttlerun

#Below
VaieVemM1 <- rq(VaieVem ~ MorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1)
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(VaieVem ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2)
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(VaieVem ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3)
summary.rq(VaieVemM3, se = "rank")



#Above
VaieVemA1 <- rq(VaieVem ~ MorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1)
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(VaieVem ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2)
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(VaieVem ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3)
summary.rq(VaieVemA3, se = "rank")


# Handgrip

#Below
HandgripM1 <- rq(Handgrip ~ MorningMVPA, data = Below)
summary.rq(HandgripM1)
summary.rq(HandgripM1, se = "rank")

HandgripM2 <- rq(Handgrip ~ MorningMVPA + Age + Sex + BMI, data = Below)
summary.rq(HandgripM2)
summary.rq(HandgripM2, se = "rank")

HandgripM3 <- rq(Handgrip ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, data = Below)
summary.rq(HandgripM3)
summary.rq(HandgripM3, se = "rank")

#Above
HandgripA1 <- rq(Handgrip ~ MorningMVPA, data = Above)
summary.rq(HandgripA1)
summary.rq(HandgripA1, se = "rank")

HandgripA2 <- rq(Handgrip ~ MorningMVPA + Age + Sex + BMI, data = Above)
summary.rq(HandgripA2)
summary.rq(HandgripA2, se = "rank")

HandgripA3 <- rq(Handgrip ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(HandgripA3)
summary.rq(HandgripA3, se = "rank")


# Horizontal impulse

#Below
HImpulseM1 <- rq(HorizontalImpulse ~ MorningMVPA, tau = 0.5, data = Below)
summary.rq(HImpulseM1)
summary.rq(HImpulseM1, se = "rank")

HImpulseM2 <- rq(HorizontalImpulse ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(HImpulseM2)
summary.rq(HImpulseM2, se = "rank")

HImpulseM3 <- rq(HorizontalImpulse ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(HImpulseM3)
summary.rq(HImpulseM3, se = "rank")

#Above
HImpulseA1 <- rq(HorizontalImpulse ~ MorningMVPA, tau = 0.5, data = Above)
summary.rq(HImpulseA1)
summary.rq(HImpulseA1, se = "rank")

HImpulseA2 <- rq(HorizontalImpulse ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(HImpulseA2)
summary.rq(HImpulseA2, se = "rank")

HImpulseA3 <- rq(HorizontalImpulse ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(HImpulseA3)
summary.rq(HImpulseA3, se = "rank")



# Vertical Impulse

#Below
VImpulseM1 <- rq(VerticalImpulse ~ MorningMVPA, tau = 0.5, data = Below)
summary.rq(VImpulseM1)
summary.rq(VImpulseM1, se = "rank")

VImpulseM2 <- rq(VerticalImpulse ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VImpulseM2)
summary.rq(VImpulseM2, se = "rank")

VImpulseM3 <- rq(VerticalImpulse ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VImpulseM3)
summary.rq(VImpulseM3, se = "rank")

#Above
VImpulseA1 <- rq(VerticalImpulse ~ MorningMVPA, tau = 0.5, data = Above)
summary.rq(VImpulseA1)
summary.rq(VImpulseA1, se = "rank")


VImpulseA2 <- rq(VerticalImpulse ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VImpulseA2)
summary.rq(VImpulseA2, se = "rank")

VImpulseA3 <- rq(VerticalImpulse ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VImpulseA3)
summary.rq(VImpulseA3, se = "rank")



# Pushup

#Below
PushupM1 <- rq(PushUp ~ MorningMVPA, tau = 0.5, data = Below)
summary.rq(PushupM1, se = "iid")
summary.rq(PushupM1, se = "rank")

PushupM2 <- rq(PushUp ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(PushupM2, se = "iid")
summary.rq(PushupM2, se = "rank")

PushupM3 <- rq(PushUp ~ MorningMVPA + Age + Sex + AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(PushupM3, se = "iid")
summary.rq(PushupM3, se = "rank")

#Above
PushupA1 <- rq(PushUp ~ MorningMVPA, tau = 0.5, data = Above)
summary.rq(PushupA1, se = "iid")
summary.rq(PushupA1, se = "rank")

PushupA2 <- rq(PushUp ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(PushupA2)
summary.rq(PushupA2, se = "rank")

PushupA3 <- rq(PushUp ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA , tau = 0.5, data = Above)
summary.rq(PushupA3, se = "iid")
summary.rq(PushupA3, se = "rank")





# Abdominal (curl-ups)

#Below
AbdominalM1 <- rq(Abdominal ~ MorningMVPA, data = Below)
summary.rq(AbdominalM1)
summary.rq(AbdominalM1, se = "rank")

AbdominalM2 <- rq(Abdominal ~ MorningMVPA + Age + Sex + BMI, data = Below)
summary.rq(AbdominalM2)
summary.rq(AbdominalM2, se = "rank")

AbdominalM3 <- rq(Abdominal ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, data = Below)
summary.rq(AbdominalM3)
summary.rq(AbdominalM3, se = "rank")


#Above
AbdominalA1 <- rq(Abdominal ~ MorningMVPA, tau = 0.5, data = Above)
summary.rq(AbdominalA1)
summary.rq(AbdominalA1, se = "rank")

AbdominalA2 <- rq(Abdominal ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(AbdominalA2)
summary.rq(AbdominalA2, se = "rank")

AbdominalA3 <- rq(Abdominal ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(AbdominalA3)
summary.rq(AbdominalA3, se = "rank")


# Speed 20 m

#Below
Speed20mM1 <- rq(Speed20m ~ MorningMVPA, data = Below)
summary.rq(Speed20mM1, se = "iid")
summary.rq(Speed20mM1, se = "rank")

Speed20mM2 <- rq(Speed20m ~ MorningMVPA + Age + Sex + BMI, data = Below)
summary.rq(Speed20mM2, se = "iid")
summary.rq(Speed20mM2, se = "rank")

Speed20mM3 <- rq(Speed20m ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, data = Below)
summary.rq(Speed20mM3, se = "iid")
summary.rq(Speed20mM3, se = "rank")

#Above
Speed20mA1 <- rq(Speed20m ~ MorningMVPA, data = Above)
summary.rq(Speed20mA1, se = "iid")
summary.rq(Speed20mA1, se = "rank")

Speed20mA2 <- rq(Speed20m ~ MorningMVPA + Age + Sex + BMI, data = Above)
summary.rq(Speed20mA2, se = "iid")
summary.rq(Speed20mA2, se = "rank")

Speed20mA3 <- rq(Speed20m ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(Speed20mA3, se = "iid")
summary.rq(Speed20mA3, se = "rank")


# Speed 40 m

#Below
Speed40mM1 <- rq(Speed40m ~ MorningMVPA, data = Below)
summary.rq(Speed40mM1, se = "iid")
summary.rq(Speed40mM1, se = "rank")

Speed40mM2 <- rq(Speed40m ~ MorningMVPA + Age + Sex + BMI, data = Below)
summary.rq(Speed40mM2, se = "iid")
summary.rq(Speed40mM2, se = "rank")

Speed40mM3 <- rq(Speed40m ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, data = Below)
summary.rq(Speed40mM3, se = "iid")
summary.rq(Speed40mM3, se = "rank")

#Above
Speed40mA1 <- rq(Speed40m ~ MorningMVPA, data = Above)
summary.rq(Speed40mA1, se = "iid")
summary.rq(Speed40mA1, se = "rank")

Speed40mA2 <- rq(Speed40m ~ MorningMVPA + Age + Sex + BMI, data = Above)
summary.rq(Speed40mA2, se = "iid")
summary.rq(Speed40mA2, se = "rank")

Speed40mA3 <- rq(Speed40m ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(Speed40mA3, se = "iid")
summary.rq(Speed40mA3, se = "rank")



# 4x10m shuttlerun

#Below
ShuttlerunM1 <- rq(ShuttleRun ~ MorningMVPA, tau = 0.5, data = Below)
summary.rq(ShuttlerunM1, se = "iid")
summary.rq(ShuttlerunM1, se = "rank")


ShuttlerunM2 <- rq(ShuttleRun ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ShuttlerunM2, se = "iid")
summary.rq(ShuttlerunM2, se = "rank")

ShuttlerunM3 <- rq(ShuttleRun ~  MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ShuttlerunM3, se = "iid")
summary.rq(ShuttlerunM3, se = "rank")

#Above
ShuttlerunA1 <- rq(ShuttleRun ~ MorningMVPA, tau = 0.5, data = Above)
summary.rq(ShuttlerunA1, se = "iid")
summary.rq(ShuttlerunA1, se = "rank")

ShuttlerunA2 <- rq(ShuttleRun ~ MorningMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ShuttlerunA2, se = "iid")
summary.rq(ShuttlerunA2, se = "rank")

ShuttlerunA3 <- rq(ShuttleRun ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ShuttlerunA3, se = "iid")
summary.rq(ShuttlerunA3, se = "rank")



# BMI

#Below
BMIM1 <- rq(BMI ~ MorningMVPA, data = Below)
summary.rq(BMIM1)
summary.rq(BMIM1, se = "rank")

BMIM2 <- rq(BMI ~ MorningMVPA + Age + Sex, data = Below)
summary.rq(BMIM2)
summary.rq(BMIM2, se = "rank")

BMIM3 <- rq(BMI ~ MorningMVPA + Age + Sex + AfternoonMVPA, data = Below)
summary.rq(BMIM3)
summary.rq(BMIM3, se = "rank")

#Above
BMIA1 <- rq(BMI ~ MorningMVPA, data = Above)
summary.rq(BMIA1)
summary.rq(BMIA1, se = "rank")

BMIA2 <- rq(BMI ~ MorningMVPA + Age + Sex, data = Above)
summary.rq(BMIA2)
summary.rq(BMIA2, se = "rank")

BMIA3 <- rq(BMI ~ MorningMVPA + Age + Sex + AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(BMIA3)
summary.rq(BMIA3, se = "rank")




# Creating models for afternoon MVPA ####

#For afternoon

Below <- ChildrenData %>% 
  dplyr::filter(AfternoonMVPA <= median(AfternoonMVPA, na.rm = T))

Above <- ChildrenData %>% 
  dplyr::filter(AfternoonMVPA > median(AfternoonMVPA, na.rm = T))

summary(Below$AfternoonMVPA)
summary(Above$AfternoonMVPA)


#20m shuttlerun

#Below
VaieVemM1 <- rq(VaieVem ~ AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1)
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(VaieVem ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2)
summary.rq(VaieVemM2, se = "rank")

VaieVemM3 <- rq(VaieVem ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM3)
summary.rq(VaieVemM3, se = "rank")




#Above
VaieVemA1 <- rq(VaieVem ~ AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1)
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(VaieVem ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2)
summary.rq(VaieVemA2, se = "rank")

VaieVemA3 <- rq(VaieVem ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA3)
summary.rq(VaieVemA3, se = "rank")



# Handgrip

#Below
HandgripM1 <- rq(Handgrip ~ AfternoonMVPA, data = Below)
summary.rq(HandgripM1)
summary.rq(HandgripM1, se = "rank")

HandgripM2 <- rq(Handgrip ~ AfternoonMVPA + Age + Sex + BMI, data = Below)
summary.rq(HandgripM2)
summary.rq(HandgripM2, se = "rank")

HandgripM3 <- rq(Handgrip ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, data = Below)
summary.rq(HandgripM3)
summary.rq(HandgripM3, se = "rank")

#Above
HandgripA1 <- rq(Handgrip ~ AfternoonMVPA, data = Above)
summary.rq(HandgripA1)
summary.rq(HandgripA1, se = "rank")

HandgripA2 <- rq(Handgrip ~ AfternoonMVPA + Age + Sex + BMI, data = Above)
summary.rq(HandgripA2)
summary.rq(HandgripA2, se = "rank")

HandgripA3 <- rq(Handgrip ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(HandgripA3)
summary.rq(HandgripA3, se = "rank")


# Horizontal impulse

#Below
HImpulseM1 <- rq(HorizontalImpulse ~ AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(HImpulseM1)
summary.rq(HImpulseM1, se = "rank")

HImpulseM2 <- rq(HorizontalImpulse ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(HImpulseM2)
summary.rq(HImpulseM2, se = "rank")

HImpulseM3 <- rq(HorizontalImpulse ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Below)
summary.rq(HImpulseM3)
summary.rq(HImpulseM3, se = "rank")

#Above
HImpulseA1 <- rq(HorizontalImpulse ~ AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(HImpulseA1)
summary.rq(HImpulseA1, se = "rank")

HImpulseA2 <- rq(HorizontalImpulse ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(HImpulseA2)
summary.rq(HImpulseA2, se = "rank")

HImpulseA3 <- rq(HorizontalImpulse ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(HImpulseA3)
summary.rq(HImpulseA3, se = "rank")




# Vertical Impulse

#Below
VImpulseM1 <- rq(VerticalImpulse ~ AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(VImpulseM1)
summary.rq(VImpulseM1, se = "rank")

VImpulseM2 <- rq(VerticalImpulse ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VImpulseM2)
summary.rq(VImpulseM2, se = "rank")

VImpulseM3 <- rq(VerticalImpulse ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Below)
summary.rq(VImpulseM3)
summary.rq(VImpulseM3, se = "rank")

#Above
VImpulseA1 <- rq(VerticalImpulse ~ AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(VImpulseA1)
summary.rq(VImpulseA1, se = "rank")


VImpulseA2 <- rq(VerticalImpulse ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VImpulseA2)
summary.rq(VImpulseA2, se = "rank")

VImpulseA3 <- rq(VerticalImpulse ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(VImpulseA3)
summary.rq(VImpulseA3, se = "rank")


# Pushup

#Below
PushupM1 <- rq(PushUp ~ AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(PushupM1, se = "iid")
summary.rq(PushupM1, se = "rank")

PushupM2 <- rq(PushUp ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(PushupM2, se = "iid")
summary.rq(PushupM2, se = "rank")

PushupM3 <- rq(PushUp ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Below)
summary.rq(PushupM3, se = "iid")
summary.rq(PushupM3, se = "rank")

#Above
PushupA1 <- rq(PushUp ~ AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(PushupA1, se = "iid")
summary.rq(PushupA1, se = "rank")

PushupA2 <- rq(PushUp ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(PushupA2)
summary.rq(PushupA2, se = "rank")

PushupA3 <- rq(PushUp ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA , tau = 0.5, data = Above)
summary.rq(PushupA3, se = "iid")
summary.rq(PushupA3, se = "rank")



# Abdominal (curl-ups)

#Below
AbdominalM1 <- rq(Abdominal ~ AfternoonMVPA, data = Below)
summary.rq(AbdominalM1)
summary.rq(AbdominalM1, se = "rank")

AbdominalM2 <- rq(Abdominal ~ AfternoonMVPA + Age + Sex + BMI, data = Below)
summary.rq(AbdominalM2)
summary.rq(AbdominalM2, se = "rank")

AbdominalM3 <- rq(Abdominal ~ MorningMVPA + Age + Sex + BMI + AfternoonMVPA, data = Below)
summary.rq(AbdominalM3)
summary.rq(AbdominalM3, se = "rank")


#Above
AbdominalA1 <- rq(Abdominal ~ AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(AbdominalA1)
summary.rq(AbdominalA1, se = "rank")

AbdominalA2 <- rq(Abdominal ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(AbdominalA2)
summary.rq(AbdominalA2, se = "rank")

AbdominalA3 <- rq(Abdominal ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(AbdominalA3)
summary.rq(AbdominalA3, se = "rank")


# Speed 20 m

#Below
Speed20mM1 <- rq(Speed20m ~ AfternoonMVPA, data = Below)
summary.rq(Speed20mM1, se = "iid")
summary.rq(Speed20mM1, se = "rank")

Speed20mM2 <- rq(Speed20m ~ AfternoonMVPA + Age + Sex + BMI, data = Below)
summary.rq(Speed20mM2, se = "iid")
summary.rq(Speed20mM2, se = "rank")

Speed20mM3 <- rq(Speed20m ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, data = Below)
summary.rq(Speed20mM3, se = "iid")
summary.rq(Speed20mM3, se = "rank")

#Above
Speed20mA1 <- rq(Speed20m ~ AfternoonMVPA, data = Above)
summary.rq(Speed20mA1, se = "iid")
summary.rq(Speed20mA1, se = "rank")

Speed20mA2 <- rq(Speed20m ~ AfternoonMVPA + Age + Sex + BMI, data = Above)
summary.rq(Speed20mA2, se = "iid")
summary.rq(Speed20mA2, se = "rank")

Speed20mA3 <- rq(Speed20m ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(Speed20mA3, se = "iid")
summary.rq(Speed20mA3, se = "rank")



# Speed 40 m

#Below
Speed40mM1 <- rq(Speed40m ~ AfternoonMVPA, data = Below)
summary.rq(Speed40mM1, se = "iid")
summary.rq(Speed40mM1, se = "rank")

Speed40mM2 <- rq(Speed40m ~ AfternoonMVPA + Age + Sex + BMI, data = Below)
summary.rq(Speed40mM2, se = "iid")
summary.rq(Speed40mM2, se = "rank")

Speed40mM3 <- rq(Speed40m ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, data = Below)
summary.rq(Speed40mM3, se = "iid")
summary.rq(Speed40mM3, se = "rank")

#Above
Speed40mA1 <- rq(Speed40m ~ AfternoonMVPA, data = Above)
summary.rq(Speed40mA1, se = "iid")
summary.rq(Speed40mA1, se = "rank")

Speed40mA2 <- rq(Speed40m ~ AfternoonMVPA + Age + Sex + BMI, data = Above)
summary.rq(Speed40mA2, se = "iid")
summary.rq(Speed40mA2, se = "rank")

Speed40mA3 <- rq(Speed40m ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(Speed40mA3, se = "iid")
summary.rq(Speed40mA3, se = "rank")



# 4x10m shuttlerun

#Below
ShuttlerunM1 <- rq(ShuttleRun ~ AfternoonMVPA, tau = 0.5, data = Below)
summary.rq(ShuttlerunM1, se = "iid")
summary.rq(ShuttlerunM1, se = "rank")


ShuttlerunM2 <- rq(ShuttleRun ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ShuttlerunM2, se = "iid")
summary.rq(ShuttlerunM2, se = "rank")

ShuttlerunM3 <- rq(ShuttleRun ~  AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Below)
summary.rq(ShuttlerunM3, se = "iid")
summary.rq(ShuttlerunM3, se = "rank")

#Above
ShuttlerunA1 <- rq(ShuttleRun ~ AfternoonMVPA, tau = 0.5, data = Above)
summary.rq(ShuttlerunA1, se = "iid")
summary.rq(ShuttlerunA1, se = "rank")

ShuttlerunA2 <- rq(ShuttleRun ~ AfternoonMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ShuttlerunA2, se = "iid")
summary.rq(ShuttlerunA2, se = "rank")

ShuttlerunA3 <- rq(ShuttleRun ~ AfternoonMVPA + Age + Sex + BMI + MorningMVPA, tau = 0.5, data = Above)
summary.rq(ShuttlerunA3, se = "iid")
summary.rq(ShuttlerunA3, se = "rank")



# BMI

#Below
BMIM1 <- rq(BMI ~ AfternoonMVPA, data = Below)
summary.rq(BMIM1)
summary.rq(BMIM1, se = "rank")

BMIM2 <- rq(BMI ~ AfternoonMVPA + Age + Sex, data = Below)
summary.rq(BMIM2)
summary.rq(BMIM2, se = "rank")

BMIM3 <- rq(BMI ~ AfternoonMVPA + Age + Sex + MorningMVPA, data = Below)
summary.rq(BMIM3)
summary.rq(BMIM3, se = "rank")

#Above
BMIA1 <- rq(BMI ~ AfternoonMVPA, data = Above)
summary.rq(BMIA1)
summary.rq(BMIA1, se = "rank")

BMIA2 <- rq(BMI ~ AfternoonMVPA + Age + Sex, data = Above)
summary.rq(BMIA2)
summary.rq(BMIA2, se = "rank")

BMIA3 <- rq(BMI ~ AfternoonMVPA + Age + Sex + MorningMVPA, tau = 0.5, data = Above)
summary.rq(BMIA3)
summary.rq(BMIA3, se = "rank")




# Creating models for total MVPA ####

#For afternoon

Below <- ChildrenData %>% 
  dplyr::filter(TotalMVPA <= median(TotalMVPA, na.rm = T))

Above <- ChildrenData %>% 
  dplyr::filter(TotalMVPA > median(TotalMVPA, na.rm = T))

summary(Below$TotalMVPA)
summary(Above$TotalMVPA)


#20m shuttlerun

#Below
VaieVemM1 <- rq(VaieVem ~ TotalMVPA, tau = 0.5, data = Below)
summary.rq(VaieVemM1)
summary.rq(VaieVemM1, se = "rank")

VaieVemM2 <- rq(VaieVem ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VaieVemM2)
summary.rq(VaieVemM2, se = "rank")


#Above
VaieVemA1 <- rq(VaieVem ~ TotalMVPA, tau = 0.5, data = Above)
summary.rq(VaieVemA1)
summary.rq(VaieVemA1, se = "rank")

VaieVemA2 <- rq(VaieVem ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VaieVemA2)
summary.rq(VaieVemA2, se = "rank")



# Handgrip

#Below
HandgripM1 <- rq(Handgrip ~ TotalMVPA, data = Below)
summary.rq(HandgripM1)
summary.rq(HandgripM1, se = "rank")

HandgripM2 <- rq(Handgrip ~ TotalMVPA + Age + Sex + BMI, data = Below)
summary.rq(HandgripM2)
summary.rq(HandgripM2, se = "rank")


#Above
HandgripA1 <- rq(Handgrip ~ TotalMVPA, data = Above)
summary.rq(HandgripA1)
summary.rq(HandgripA1, se = "rank")

HandgripA2 <- rq(Handgrip ~ TotalMVPA + Age + Sex + BMI, data = Above)
summary.rq(HandgripA2)
summary.rq(HandgripA2, se = "rank")



# Horizontal impulse

#Below
HImpulseM1 <- rq(HorizontalImpulse ~ TotalMVPA, tau = 0.5, data = Below)
summary.rq(HImpulseM1)
summary.rq(HImpulseM1, se = "rank")

HImpulseM2 <- rq(HorizontalImpulse ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(HImpulseM2)
summary.rq(HImpulseM2, se = "rank")


#Above
HImpulseA1 <- rq(HorizontalImpulse ~ TotalMVPA, tau = 0.5, data = Above)
summary.rq(HImpulseA1)
summary.rq(HImpulseA1, se = "rank")

HImpulseA2 <- rq(HorizontalImpulse ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(HImpulseA2)
summary.rq(HImpulseA2, se = "rank")



# Vertical Impulse

#Below
VImpulseM1 <- rq(VerticalImpulse ~ TotalMVPA, tau = 0.5, data = Below)
summary.rq(VImpulseM1)
summary.rq(VImpulseM1, se = "rank")

VImpulseM2 <- rq(VerticalImpulse ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(VImpulseM2)
summary.rq(VImpulseM2, se = "rank")


#Above
VImpulseA1 <- rq(VerticalImpulse ~ TotalMVPA, tau = 0.5, data = Above)
summary.rq(VImpulseA1)
summary.rq(VImpulseA1, se = "rank")


VImpulseA2 <- rq(VerticalImpulse ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(VImpulseA2)
summary.rq(VImpulseA2, se = "rank")



# Pushup

#Below
PushupM1 <- rq(PushUp ~ TotalMVPA, tau = 0.5, data = Below)
summary.rq(PushupM1, se = "iid")
summary.rq(PushupM1, se = "rank")

PushupM2 <- rq(PushUp ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(PushupM2, se = "iid")
summary.rq(PushupM2, se = "rank")


#Above
PushupA1 <- rq(PushUp ~ TotalMVPA, tau = 0.5, data = Above)
summary.rq(PushupA1, se = "iid")
summary.rq(PushupA1, se = "rank")

PushupA2 <- rq(PushUp ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(PushupA2)
summary.rq(PushupA2, se = "rank")



# Abdominal (curl-ups)

#Below
AbdominalM1 <- rq(Abdominal ~ TotalMVPA, data = Below)
summary.rq(AbdominalM1)
summary.rq(AbdominalM1, se = "rank")

AbdominalM2 <- rq(Abdominal ~ TotalMVPA + Age + Sex + BMI, data = Below)
summary.rq(AbdominalM2)
summary.rq(AbdominalM2, se = "rank")


#Above
AbdominalA1 <- rq(Abdominal ~ TotalMVPA, tau = 0.5, data = Above)
summary.rq(AbdominalA1)
summary.rq(AbdominalA1, se = "rank")

AbdominalA2 <- rq(Abdominal ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(AbdominalA2)
summary.rq(AbdominalA2, se = "rank")



# Speed 20 m

#Below
Speed20mM1 <- rq(Speed20m ~ TotalMVPA, data = Below)
summary.rq(Speed20mM1, se = "iid")
summary.rq(Speed20mM1, se = "rank")

Speed20mM2 <- rq(Speed20m ~ TotalMVPA + Age + Sex + BMI, data = Below)
summary.rq(Speed20mM2, se = "iid")
summary.rq(Speed20mM2, se = "rank")


#Above
Speed20mA1 <- rq(Speed20m ~ TotalMVPA, data = Above)
summary.rq(Speed20mA1, se = "iid")
summary.rq(Speed20mA1, se = "rank")

Speed20mA2 <- rq(Speed20m ~ TotalMVPA + Age + Sex + BMI, data = Above)
summary.rq(Speed20mA2, se = "iid")
summary.rq(Speed20mA2, se = "rank")



# Speed 40 m

#Below
Speed40mM1 <- rq(Speed40m ~ TotalMVPA, data = Below)
summary.rq(Speed40mM1, se = "iid")
summary.rq(Speed40mM1, se = "rank")

Speed40mM2 <- rq(Speed40m ~ TotalMVPA + Age + Sex + BMI, data = Below)
summary.rq(Speed40mM2, se = "iid")
summary.rq(Speed40mM2, se = "rank")


#Above
Speed40mA1 <- rq(Speed40m ~ TotalMVPA, data = Above)
summary.rq(Speed40mA1, se = "iid")
summary.rq(Speed40mA1, se = "rank")

Speed40mA2 <- rq(Speed40m ~ TotalMVPA + Age + Sex + BMI, data = Above)
summary.rq(Speed40mA2, se = "iid")
summary.rq(Speed40mA2, se = "rank")



# 4x10m shuttlerun

#Below
ShuttlerunM1 <- rq(ShuttleRun ~ TotalMVPA, tau = 0.5, data = Below)
summary.rq(ShuttlerunM1, se = "iid")
summary.rq(ShuttlerunM1, se = "rank")


ShuttlerunM2 <- rq(ShuttleRun ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Below)
summary.rq(ShuttlerunM2, se = "iid")
summary.rq(ShuttlerunM2, se = "rank")


#Above
ShuttlerunA1 <- rq(ShuttleRun ~ TotalMVPA, tau = 0.5, data = Above)
summary.rq(ShuttlerunA1, se = "iid")
summary.rq(ShuttlerunA1, se = "rank")

ShuttlerunA2 <- rq(ShuttleRun ~ TotalMVPA + Age + Sex + BMI, tau = 0.5, data = Above)
summary.rq(ShuttlerunA2, se = "iid")
summary.rq(ShuttlerunA2, se = "rank")



# BMI

#Below
BMIM1 <- rq(BMI ~ TotalMVPA, data = Below)
summary.rq(BMIM1)
summary.rq(BMIM1, se = "rank")

BMIM2 <- rq(BMI ~ TotalMVPA + Age + Sex, data = Below)
summary.rq(BMIM2)
summary.rq(BMIM2, se = "rank")


#Above
BMIA1 <- rq(BMI ~ TotalMVPA, data = Above)
summary.rq(BMIA1)
summary.rq(BMIA1, se = "rank")

BMIA2 <- rq(BMI ~ TotalMVPA + Age + Sex, data = Above)
summary.rq(BMIA2)
summary.rq(BMIA2, se = "rank")





#figure of the forest plot

# Create the plot divided in two


DataForFigure1 <- data.frame(
  Test = c("Speed 20 m", "Speed 20 m", "Speed 20 m", "Speed 40 m", "Speed 40 m", "Speed 40 m",
           "4x10-m shuttle run", "4x10-m shuttle run", "4x10-m shuttle run", "BMI", "BMI", "BMI"),
  TimeOfTheDay = c("Morning", "Afternoon","Total", 
                   "Morning", "Afternoon","Total",
                   "Morning", "Afternoon","Total",
                   "Morning", "Afternoon","Total"),
  Coefficient = c(-0.012, -0.005, -0.016,
                  -0.025, -0.013, -0.037,
                  -0.000, -0.030, -0.036,
                  -0.085, 0.023, -0.043),
  Lower = c(-0.023, -0.020, -0.035,
            -0.037, -0.023, -0.056,
            -0.022, -0.057, -0.062,
            -0.136, -0.055, -0.125),
  Upper = c(0.001, 0.003, -0.006,
            -0.012, 0.012, -0.011,
            0.041, 0.004, 0.014,
            -0.007, 0.092, 0.003),
  stringsAsFactors = T) 

DataForFigure1 <- DataForFigure1 %>% 
  dplyr::mutate(
    Test = forcats::fct_relevel(Test, "BMI", "4x10-m shuttle run",
                       "Speed 40 m","Speed 20 m"),
    TimeOfTheDay = forcats::fct_relevel(TimeOfTheDay, "Total", "Afternoon", "Morning"))



Plot1 <- ggplot(DataForFigure1, aes(x = Coefficient, y = Test, color = TimeOfTheDay)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(xmin = Lower, xmax = Upper, width = 0.35),
                position = position_dodge(width = 0.5), linewidth = 0.95) +
  geom_point(stat = "identity", size = 2.4,
             position = position_dodge(width = 0.5)) + 
  scale_color_manual(name = NULL,
                     breaks = c("Afternoon", "Morning", "Total"),
                     labels= c("Afternoon MVPA", "Morning MVPA", "Total MVPA"),
                     values = c("#ed0000", "#00468b", "#9caac6")) +
  labs(color = NULL, y = "", x = "Coefficient (95% CI)") + 
  labs(shape = NULL, y = "", x = "Coefficient (95% CI)") +
  theme_classic() + 
  theme(text = element_text(size = 13, family = "serif"), 
        axis.text = element_text(color = "black"),
        legend.position = "bottom")




DataForFigure2 <- data.frame(
  Test = c("20-m shuttle run", "20-m shuttle run", "20-m shuttle run",
           "Handgrip", "Handgrip", "Handgrip",
           "Standing long jump","Standing long jump", "Standing long jump", 
           "Vertical jump","Vertical jump", "Vertical jump", 
           "Push-ups", "Push-ups", "Push-ups", 
           "Curl-ups", "Curl-ups", "Curl-ups"),
  TimeOfTheDay = c("Morning", "Afternoon","Total", 
                   "Morning", "Afternoon","Total",
                   "Morning", "Afternoon","Total",
                   "Morning", "Afternoon","Total", 
                   "Morning", "Afternoon","Total",
                   "Morning", "Afternoon","Total"),
  Coefficient = c(0.425, 1.071, 1.470,
                  -0.013, -0.086, -0.101,
                  0.750, 0.778, 1.453,
                  -0.179, 0.176, -0.009,
                  0.052, 0.274, 0.348,
                  -0.087, 0.907, 0.788),
  Lower = c(0.024, 0.574, 1.043,
            -0.115, -0.232, -0.233,
            0.022, 0.171, 0.719,
            -0.353, -0.064, -0.200,
            -0.103, 0.098, 0.153,
            -0.520, 0.455, 0.233),
  Upper = c(0.842, 1.382, 1.864,
            0.142, 0.054, 0.025,
            1.240, 1.395, 2.254,
            0.031, 0.423, 0.252,
            0.260, 0.437, 0.563,
            0.462, 1.387, 1.514),
  stringsAsFactors = T
) 

DataForFigure2 <- DataForFigure2 %>% 
  dplyr::mutate(
    Test = fct_relevel(Test, "Curl-ups","Push-ups","Vertical jump", 
                       "Standing long jump", "Handgrip","20-m shuttle run"),
    TimeOfTheDay = forcats::fct_relevel(TimeOfTheDay, "Total", "Afternoon", "Morning"))



Plot2 <- ggplot(DataForFigure2, aes(x = Coefficient, y = Test, color = TimeOfTheDay)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_errorbar(aes(xmin = Lower, xmax = Upper, width = 0.35),
                position = position_dodge(width = 0.5), linewidth = 0.95) +
  geom_point(stat = "identity", size = 2.4,
             position = position_dodge(width = 0.5)) + 
  scale_color_manual(name = NULL,
                     breaks = c("Afternoon", "Morning", "Total"),
                     labels= c("Afternoon MVPA", "Morning MVPA", "Total MVPA"),
                     values = c("#ed0000", "#00468b", "#9caac6")) +
  labs(color = NULL, y = "", x = "Coefficient (95% CI)") + 
  labs(shape = NULL, y = "", x = "Coefficient (95% CI)") +
  theme_classic() + 
  theme(text = element_text(size = 13, family = "serif"), 
        axis.text = element_text(color = "black"),
        legend.position = "bottom")
  


FigureReady <- ggarrange(Plot2, Plot1,
          ncol = 2, nrow = 1)  +
  theme(plot.margin = margin(0.1,0.1,0.1,0.1, "cm")) #Change margin size


#Saving in svg


ggsave(filename = "Figure1.pdf", plot = FigureReady,
       width = 9, height = 4.5,
       path = "G:/Meu Drive/ARTIGOS PUBLICADOS E A SEREM PUBLICADOS/Artigos em andamento e parados/Artigo associações timing da AF crianças/Documentos e análises/Figures")

ggsave(filename = "Figure1.svg", plot = FigureReady,
       width = 9, height = 4.5,
       path = "G:/Meu Drive/ARTIGOS PUBLICADOS E A SEREM PUBLICADOS/Artigos em andamento e parados/Artigo associações timing da AF crianças/Documentos e análises/Figures")

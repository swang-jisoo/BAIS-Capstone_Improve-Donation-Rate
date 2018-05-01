#University of Iowa - BAIS Capstone Spring 2017
#Ruffalo Noel Levitz Project 
#Group 1, Team 6 - Jisoo Lee, Jordan Kloewer, Ran An, Xuan Zhang

#clean workspace
rm(list = ls())

#load neccessary library
library(readxl) #to load excel format of dataset
library(tidyr) #to reform dataset
library(dplyr) #to reform dataset
library(ggplot2) #to plot graph
library("ggExtra") #to plot graph
library(lattice) #to plot graph
library(grid) #to plot graph
library(gridExtra) #to plot graph
library(cowplot)  #to plot graph
library(colorspace) #to plot graph
library(RColorBrewer) #to plot graph
library(ROSE) #to balance dataset
library(caret) #to find correlations
library(rattle) #to build models
library(randomForest) #for randomForest


#===== 1. load original dataset =====================================================
org <- read_excel("303043_RNL_PhonathonData.xlsx", sheet = 1) #252756 obs of 62 var
#save dataset as csv file and call the csv
write.csv(org, file = "RNL_Data.csv", row.names = F)
cln <- read.csv("RNL_Data_Original.csv") #252756 obs of 62 var 
str(cln); summary(cln) #exmaine 
#===== end of loading

#===== 2. clean dataset =============================================================
#===== summary of cleaning ==========================================================
#[1] "RNL_ID"                     
#[2] "CLIENT_ID": remove; unnecessary identifier                   
#[3] "ZIP": remove; use STATE instead                          
#[4] "STATE": remove; use CRegion instead to reduce levels; clean incorrect records; change NA to Undetermined                      
#[5] "PHONE_RESEARCH": remove; no value to investigation               
#[6] "WIRELESS_RESEARCH": remove; no value to investigation           
#[7] "FIRST_GRADYR": remove; use YearSinceGrad instead to have numeric values                 
#[8] "SCHOOL": change values as full description and combine similar values; change NA to Undetermined                      
#[9] "AGE": clean outliers                          
#[10] "PAYMENT_TYPE": remove; no value to investigation                
#[11] "FY17_PLEDGE_AMOUNT": remove; no value to investigation           
#[12] "FY17_MATCHING_AMOUNT": remove; no value to investigation        
#[13] "FY17_TOTAL_AMOUNT": remove; no value to investigation            
#[14] "FY17_PLEDGE_AMOUNT_FULFILLED": remove; no value to investigation
#[15] "RECTYPE": change values as abbreviation and combine similar values; change NA to Undetermined                      
#[16] "GENDER": change NA to Undetermined                     
#[17] "TITLE": remove; no value to investigation                         
#[18] "FSTGFTA": change NA to 0 because no record means no donation                    
#[19] "FSTGFTD": convert into FSTGFTY and FSTGFTM to keep year and month only                       
#[20] "LSTGFTD": convert into LSTGFTY and LSTGFTM to keep year and month only                      
#[21] "LRGGFTD": convert into LRGGFTY and LRGGFTM to keep year and month only                       
#[22] "MAJORDON": remove; each school has different formula for calculating this column                    
#[23] "AFFIL1": remove; use AffilCount instead                       
#[24] "AFFIL2": remove; use AffilCount instead  
#[25] "AFFIL3": remove; use AffilCount instead                         
#[26] "NumGifts": change NA to 0 because no record means no donation                    
#[27] "TotalGift": change NA to 0 because no record means no donation                    
#[28] "AvgGift": change NA to 0 because no record means no donation                     
#[29] "MinGift": change NA to 0 because no record means no donation                      
#[30] "MaxGift": change NA to 0 because no record means no donation                     
#[31] "NumDegrees": change NA to 0 because no 0 value originally so 0 will imply no record                   
#[32] "RecentGradYr": remove; use YearSinceRecentGrad to have numeric value                
#[33] "CC": remove; no value to investigation                             
#[34] "CP": remove; no value to investigation                           
#[35] "DD": remove; no value to investigation                            
#[36] "INC": remove; no value to investigation                          
#[37] "XX": remove; no value to investigation                            
#[38] "BLANK": remove; no value to investigation                        
#[39] "Pledge": remove; use Pldg instead                        
#[40] "SuccCont"                    
#[41] "Pldg"                         
#[42] "YearSinceGrad": change NA to 0 because no 0 value originally so 0 will imply no record                 
#[43] "lstgfta": change NA to 0 because no record means no donation                       
#[44] "lrggfta": remove; use MaxGift instead    
#[45] "AffilCount": change NA to 0 because no record means no affiliation                   
#[46] "client"                      
#[47] "Ask_Amount_1": change NA to 0 because no 0 value originally so 0 will imply no record                   
#[48] "Ask_Amount_2": change NA to 0 because no 0 value originally so 0 will imply no record                  
#[49] "Ask_Amount_3": change NA to 0 because no 0 value originally so 0 will imply no record                   
#[50] "MULTIDON": remove; use NumGifts instead                    
#[51] "CONSECUTIVE_YEARS_OF_GIVING": remove; use NumGifts instead 
#[52] "LAST_FIS_YEAR_OF_GIVING": remove; no value to investigation   
#[53] "P_DONOR_CATEGORY": remove; incompleted variable             
#[54] "LastGradYr": remove; use YearSinceRecentGrad to have numeric value                  
#[55] "CONS_YEARS": remove; use NumGifts instead                    
#[56] "InState"                     
#[57] "CurrentAge": remove; use AGE instead                   
#[58] "TotAttempts": change NA to 0 because no record means no attempts                 
#[59] "SYSTEM_TIME": remove; focus on TALK_TIME                  
#[60] "TALK_TIME": clear ourliers; change NA to 0 because no record means no talk time                   
#[61] "PledgeAmt": remove; no value to investigation                     
#[62] "PledgeTot": remove; no value to investigation       
#===== end of summary 

#===== codes of cleaning ============================================================
#examine STATE 
levels(cln$STATE) #62 levels #error: "19702" #"FC" #"XX" #"ON" not sure
#remove errors in STATE
#cln$STATE = 19702 & ON has been removed due to ZIP error
cln <- cln[-c(which(cln$STATE == "19702"),
              which(cln$STATE == "FC"),
              which(cln$STATE == "XX"),
              which(cln$STATE == "ON")), ]
levels(cln$STATE)[levels(cln$STATE) == "19702"] <- NA
levels(cln$STATE)[levels(cln$STATE) == "FC"] <- NA
levels(cln$STATE)[levels(cln$STATE) == "XX"] <- NA
levels(cln$STATE)[levels(cln$STATE) == "ON"] <- NA
levels(cln$STATE) #58 levels
#reduce levels of STATE below 32 for random forest model function
#ref - state abbreviations: http://www.50states.com/abbreviations.htm
#ref - region classification: https://en.wikipedia.org/wiki/List_of_regions_of_the_United_States
cln$CRegion <- cln$STATE
levels(cln$CRegion) <- c("Military", "Military", "Pacific", "ESCentral", "Military",    #"AA" "AE" "AK" "AL" "AP"
                         "WSCentral", "Mountain", "Pacific", "Mountain", "NewEngland",    #"AR" "AZ" "CA" "CO" "CT"
                         "Commonwealth", "SAtlantic", "SAtlantic", "Commonwealth", "SAtlantic",    #"DC" "DE" "FL" "FM" "GA"
                         "Commonwealth", "Pacific", "WNCentral", "Mountain", "ENCentral",    #"GU" "HI" "IA" "ID" "IL"
                         "ENCentral", "WNCentral", "ESCentral", "WSCentral", "NewEngland",    #"IN" "KS" "KY" "LA" "MA"
                         "SAtlantic", "NewEngland", "ENCentral", "WNCentral", "WNCentral",    #"MD" "ME" "MI" "MN" "MO"
                         "ESCentral", "Mountain", "SAtlantic", "WNCentral", "WNCentral",    #"MS" "MT" "NC" "ND" "NE"
                         "NewEngland", "MidAtlantic", "Mountain", "Mountain", "MidAtlantic",    #"NH" "NJ" "NM" "NV" "NY"
                         "ENCentral", "WSCentral", "Pacific", "MidAtlantic", "Commonwealth",    #"OH" "OK" "OR" "PA" "PR"
                         "NewEngland", "SAtlantic", "WNCentral", "ESCentral", "WSCentral",    #"RI" "SC" "SD" "TN" "TX"
                         "Mountain", "SAtlantic", "Commonwealth", "NewEngland", "Pacific",    #"UT" "VA" "VI" "VT" "WA"
                         "ENCentral", "SAtlantic", "Mountain")    #"WI" "WV" "WY"
levels(cln$CRegion) #12 levels
#now remain: 252751 obs of 63 var

#examine SCHOOL
levels(cln$SCHOOL) #36 levels
#remove duplicated levels of SCHOOL
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "."] <- NA
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "A & S"] <- "Arts and Sciences"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "AF&NR"] <- "Agriculture and Natural Resources"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "BUS"] <- "Business and Economics"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "College of Arts & Sciences"] <- "Arts and Sciences"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "EDUC"] <- "Education & Human Development"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "ENGR"] <- "Engineering"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "GRAD"] <- "Graduate Studies"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "H E S"] <- "Human Environmental Sciences"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "HP"] <- "Health Professions"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "JOURN"] <- "Journalism"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "NAT R"] <- "Natural Resources"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "Not UD"] <- "Not University of Delaware"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "SO WK"] <- "Social Work"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "School of Business & Economics"] <- "Business and Economics"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "School of Edu/Hum Performance"] <- "Education & Human Development"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "School of Health Sciences"] <- "Health Sciences"
levels(cln$SCHOOL)[levels(cln$SCHOOL) == "VET M"] <- "Veterinary Medicine"
levels(cln$SCHOOL) #25 levels

#examine AGE
boxplot.stats(cln$AGE)
#remove outliers >102 and =3 (total 26 rows) 
cln <- cln[-c(which(cln$AGE > 102),
              which(cln$AGE == 3)),]
#now remain: 252725 obs of 62 var
#check if AGE and CurrentAge column is same
identical(cln$AGE, cln$CurrentAge) #TRUE >> delete CurrentAge

#examine RECTYPE 
levels(cln$RECTYPE) #24 levels
#change full written to shorten ver. in RECTYPE
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Alumnus/ae"] <- "AL"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Faculty/Staff-UMC"] <- "FS"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Faculty/Staff-UMca"] <- "FS"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Friend"] <- "FR"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Graduate"] <- "GA"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Non Graduate"] <- "AN"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Parent"] <- "PA"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Past Parent"] <- "PP"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Resident"] <- "RE"
levels(cln$RECTYPE)[levels(cln$RECTYPE) == "Retired-UMC"] <- "RF"
levels(cln$RECTYPE) #15 levels

#reform FSTGFTD, LSTGFTD, and LRGGFTD
#Format the date 
cln$FSTGFTD<-as.Date(as.character(cln$FSTGFTD),"%Y%m%d")  #1912-12-20 row:43209 
cln$LSTGFTD<-as.Date(as.character(cln$LSTGFTD),"%Y%m%d") 
cln$LRGGFTD<-as.Date(as.character(cln$LRGGFTD),"%Y%m%d")
#Keep year and month
cln <- separate(cln, FSTGFTD, c("y","m"))
names(cln)[names(cln) == "y"] <- "FSTGFTY"
names(cln)[names(cln) == "m"] <- "FSTGFTM"
cln <- separate(cln, LSTGFTD, c("y","m"))
names(cln)[names(cln) == "y"] <- "LSTGFTY"
names(cln)[names(cln) == "m"] <- "LSTGFTM"
cln <- separate(cln, LRGGFTD, c("y","m"))
names(cln)[names(cln) == "y"] <- "LRGGFTY"
names(cln)[names(cln) == "m"] <- "LRGGFTM"
#now remain: 252725 obs of 66 var

#check errors in MaxGift and lrggfta
cln1 <- cln
cln1 <- subset(cln1, MaxGift !="NA") 
cln1 <- subset(cln1, MaxGift !=0) 
cln1 <- subset(cln1, lrggfta !="NA") 
count <-0 #check if MaxGift and lrggfta are same
for(i in 1:nrow(cln1)) { 
  if(cln1$MaxGift[i]!=cln1$lrggfta[i]) { 
    #print(cln1$CLIENT_ID[i]) 
    count=count+1 
  } 
} #5103 rows have difference between MaxGift and lrggfta 
#check if largest gift amount > Total gift amount
cln1 <- subset(cln, lrggfta > TotalGift) 
#2685 rows of lrggfta has error
#check if max gift amount > Total gift amount 
cln2 <- subset(cln, MaxGift > TotalGift) 
#no error, all Max Gift are smaller than the Total Gift >> delete lrggfta

#create YearSinceRecentGrad (numeric) to use instead of RecentGradYr and LastGradYr
#check if LastGradYr and RecentGradYr are identical 
identical(cln$LastGradYr, cln$RecentGradYr) #FALSE
#create YearSinceRecentGrad if no row has both LastGradYr and RecentGradYr
j <- 0 #count num of rows having NA in both columns
for(i in 1:nrow(cln)){
  if(!is.na(cln$LastGradYr[i]) && !is.na(cln$RecentGradYr[i])){
    print(i)
  }
  if(is.na(cln$LastGradYr[i]) && is.na(cln$RecentGradYr[i])){
    j <- j+1
  }
} #no row >> combine two columns #j = 57679
cln$YearSinceRecentGrad <- cln$LastGradYr
for(i in 1:nrow(cln)){
    if(!is.na(cln$RecentGradYr[i])){
        cln$YearSinceRecentGrad[i] <- cln$RecentGradYr[i]
    }
}
cln$YearSinceRecentGrad <- 2017 - cln$YearSinceRecentGrad
cln$YearSinceRecentGrad <- as.integer(cln$YearSinceRecentGrad)
#now remain: 252725 obs of 67 var

#check TALK_TIME
boxplot.stats(cln$TALK_TIME)
#error: negative values 
cln$TALK_TIME[which(cln$TALK_TIME < 0)] <- 0

#Remove all irrelevant columns 
cln$CLIENT_ID <- NULL 
cln$ZIP <- NULL 
cln$STATE <- NULL
cln$PHONE_RESEARCH <- NULL 
cln$WIRELESS_RESEARCH <- NULL 
cln$FIRST_GRADYR <- NULL 
cln$PAYMENT_TYPE <- NULL
cln$FY17_PLEDGE_AMOUNT <- NULL
cln$FY17_MATCHING_AMOUNT <- NULL
cln$FY17_TOTAL_AMOUNT <- NULL
cln$FY17_PLEDGE_AMOUNT_FULFILLED <- NULL
cln$TITLE <- NULL 
cln$MAJORDON <- NULL 
cln$AFFIL1 <- NULL 
cln$AFFIL2 <- NULL 
cln$AFFIL3 <- NULL 
cln$RecentGradYr <- NULL
cln$CC <- NULL 
cln$CP <- NULL 
cln$DD <- NULL 
cln$INC <- NULL 
cln$XX <- NULL 
cln$BLANK <- NULL 
cln$Pledge <- NULL 
cln$lrggfta <- NULL 
cln$MULTIDON <- NULL 
cln$CONSECUTIVE_YEARS_OF_GIVING <- NULL 
cln$LAST_FIS_YEAR_OF_GIVING <- NULL
cln$P_DONOR_CATEGORY <- NULL 
cln$LastGradYr <- NULL
cln$CONS_YEARS <- NULL 
cln$CurrentAge <- NULL 
cln$SYSTEM_TIME <- NULL
cln$PledgeAmt <- NULL
cln$PledgeTot <- NULL
#now remain: 252725 obs of 32 var

#handle missing data
summary(cln) #examine
#1) AGE: check AGE - NumGift & run model w/o NA & w/ NA into mean
#2) change NA to 0 or UNDERTERMINED  
levels(cln$SCHOOL) <- c(levels(cln$SCHOOL), "UN")
cln$SCHOOL[is.na(cln$SCHOOL)] <- "UN"    #undetermined
levels(cln$RECTYPE) <- c(levels(cln$RECTYPE), "UN")
cln$RECTYPE[is.na(cln$RECTYPE)] <- "UN"    #undetermined
cln$GENDER[is.na(cln$GENDER)] <- "U"    #undetermined
cln$FSTGFTA[is.na(cln$FSTGFTA)] <- 0    #no record means no donation
cln$NumGifts[is.na(cln$NumGifts)] <- 0    #no record means no donation
cln$TotalGift[is.na(cln$TotalGift)] <- 0    #no record means no donation
cln$AvgGift[is.na(cln$AvgGift)] <- 0    #no record means no donation
cln$MinGift[is.na(cln$MinGift)] <- 0    #no record means no donation
cln$MaxGift[is.na(cln$MaxGift)] <- 0    #no record means no donation
cln$NumDegrees[is.na(cln$NumDegrees)] <- 0    #no 0 value originally; 0 will imply no record
cln$YearSinceGrad[is.na(cln$YearSinceGrad)] <- 0    #no 0 value originally; 0 will imply no record
cln$lstgfta[is.na(cln$lstgfta)] <- 0    #no record means no recent donation
cln$AffilCount[is.na(cln$AffilCount)] <- 0     #no record means no affiliation
cln$Ask_Amount_1[is.na(cln$Ask_Amount_1)] <- 0     #no 0 value originally; 0 will imply no record
cln$Ask_Amount_2[is.na(cln$Ask_Amount_2)] <- 0     #no 0 value originally; 0 will imply no record
cln$Ask_Amount_3[is.na(cln$Ask_Amount_3)] <- 0     #no 0 value originally; 0 will imply no record
cln$TotAttempts[is.na(cln$TotAttempts)] <- 0    #no record means no attempt
cln$TALK_TIME[is.na(cln$TALK_TIME)] <- 0    #no record means no talk time
levels(cln$CRegion) <- c(levels(cln$CRegion), "UN")    #extend levels 
cln$CRegion[is.na(cln$CRegion)] <- "UN"   #undetermined
cln$YearSinceRecentGrad[is.na(cln$YearSinceRecentGrad)] <- 0    #no 0 value originally; 0 will imply no record

#rewrite a new csv file for cleaned dataset
write.csv(cln, file = "RNL_Data_Cleaned.csv", row.names = FALSE)
#===== end of codes
#===== end of cleaning

#===== 3. build models ==============================================================
#===== summary of modeling ==========================================================
#steps to follow: 
#1) select target and predictor variables
#2) split dataset into train and testset
#3) train models with trainset and get results of models with testset
#4) plot graphs of results of best chosen model
#===== end of summary

#===== codes of modeling to predict chance of donation ==============================
    #===== 1) select target and predictor variables =================================
#For predicting model whether to donate
pdct <- cln
str(pdct); summary(pdct) #examine
#correct class of variables 
pdct$SuccCont <- as.factor(pdct$SuccCont)
pdct$InState <- as.factor(pdct$InState)

#create binary/categorical target variable column: NumGifts >> Donor01
pdct$Donor01 <- factor(pdct$NumGifts) 
levels(pdct$Donor01) <- c("N", #0
                          "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y", #1-10
                          "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y", #11-20
                          "Y","Y","Y","Y","Y","Y","Y","Y","Y","Y", #21-30
                          "Y","Y","Y","Y","Y","Y") #31-36
levels(pdct$Donor01) <- c(0,1) #Donated = 1
levels(pdct$Donor01) #examine 

#remove unnecessary columns below for model because they are results of donation
#if a record has value for these columns, it means it must lead Donor01 == 1
pdct$RNL_ID <- NULL 
pdct$FSTGFTA <- NULL
pdct$FSTGFTY <- NULL
pdct$FSTGFTM <- NULL
pdct$LSTGFTY <- NULL
pdct$LSTGFTM <- NULL
pdct$LRGGFTY <- NULL
pdct$LRGGFTM <- NULL
pdct$NumGifts <- NULL #use Donor01 instead
pdct$TotalGift <- NULL
pdct$AvgGift <- NULL
pdct$MinGift <- NULL
pdct$MaxGift <- NULL
pdct$lstgfta <- NULL
pdct$Pldg <- NULL
#now remain: 252725 obs of 18 var

str(pdct); names(pdct) #examine
#[1] "SCHOOL" F              "AGE" i                 "RECTYPE" F               
#[4] "GENDER" F              "NumDegrees" num        "SuccCont" F              
#[7] "YearSinceGrad" num     "AffilCount" num        "client" F               
#[10] "Ask_Amount_1" num     "Ask_Amount_2" num      "Ask_Amount_3" num        
#[13] "InState" F            "TotAttempts" num       "TALK_TIME" num          
#[16] "CRegion" F            "YearSinceRecentGrad" num "Donor01" F

        #===== visualize variables to check interesting findings ====================
#Donation vs. SuccCont vs. TotAttempts
#SuccCont may not have huge impact on donation ** 
#>> Pickup rate may not improve donation rate
#TotAttempts may have more influences on donation
#More attempts, more succconts, and maybe slightly more donation
opar <- par(mfrow = c(2, 1))
spineplot(as.factor(TotAttempts) ~ SuccCont, 
          data = subset(pdct, Donor01 == "0"), 
          main = "Donation - No", 
          xlab = "Successful Contact", ylab = "Total Attempted Contacts",
          xaxlabels = c("No", "Yes"),
          col = c("#ffffff", "#ffffe5", "#ffffd9", "#edf8b1", "#ccebc5", 
                  "#c7e9b4", "#a8ddb5", "#7bccc4", "#7fcdbb", "#4eb3d3", 
                  "#41b6c4", "#1d91c0", "#0868ac", "#08519c", "#084081", 
                  "#08306b", "#225ea8", "#253494", "#081d58", "#252525", "#000000"))
spineplot(as.factor(TotAttempts) ~ SuccCont, 
          data = subset(pdct, Donor01 == "1"), 
          main = "Donation - Yes", 
          xlab = "Successful Contact", ylab = "Total Attempted Contacts",
          xaxlabels = c("No", "Yes"),
          col = c("#ffffff", "#ffffe5", "#ffffd9", "#edf8b1", "#ccebc5", 
                  "#c7e9b4", "#a8ddb5", "#7bccc4", "#7fcdbb", "#4eb3d3", 
                  "#41b6c4", "#1d91c0", "#0868ac", "#08519c", "#084081", 
                  "#08306b", "#225ea8", "#253494", "#081d58", "#252525", "#000000"))
par(opar) 

#Donation vs. client :may have relationship
ggplot(pdct) + 
    geom_bar(aes(Donor01, fill = client), position = "fill") +
    labs(title = "Donation vs. Client", 
         x = "Donated or Not", y = "Proportion",
         fill = "Client") +
    scale_x_discrete(label = c("No", "Yes")) +
    scale_fill_manual(values = c("#5ab4ac", "#f6e8c3", "#f5f5f5"))

#Donation vs. SCHOOL :no differences
ggplot(pdct) + 
    geom_bar(aes(Donor01, fill = SCHOOL), position = "fill") +
    labs(title = "Donation vs. SCHOOL", 
         x = "Donated or Not", y = "Proportion",
         fill = "School") +
    scale_x_discrete(label = c("No", "Yes")) 

#Donation vs. RECTYPE: may have relationship
ggplot(pdct) + 
    geom_bar(aes(Donor01, fill = RECTYPE), position = "fill") +
    labs(title = "Donation vs. RECTYPE", 
         x = "Donated or Not", y = "Proportion",
         fill = "RECTYPE") +
    scale_x_discrete(label = c("No", "Yes"))

#Donation vs. NumDegrees: no big differences
barchart(prop.table(xtabs(~Donor01 + NumDegrees, data=pdct), 1),
         auto.key = list(title = "Number of Degrees", 
                         space = "top", columns = 7, cex.title = 1, size = 2),
         main = "Donation vs. NumDegrees",
         xlab = "Proportion", ylab = "Donated or Not",
         ylim = c("No","Yes"),
         par.settings = list(superpose.polygon=list(col=c("#f2f0f7", "#dadaeb", 
                                                          "#bcbddc", "#9e9ac8", 
                                                          "#807dba", "#6a51a3", 
                                                          "#4a1486"), 
                                                    border="transparent")))

#Donation vs. CRegion vs. AGE: 
#CRegion - no impact; AGE - older, more donation; CRegion and AGE may have relationship
data <- pdct[complete.cases(pdct),] %>%
    group_by(CRegion, Donor01) %>% 
    summarise(AGE = mean(AGE), n = n()) %>%
    arrange(desc(Donor01), desc(n), desc(AGE))
rank <- rank(subset(data, Donor01 == 1)$n)
ggplot(data) +
    geom_point(aes(x = AGE, y = reorder(CRegion, c(rank, rank)), 
                   color = Donor01, size = n)) +
    labs(title = "Donation vs. CRegion vs. AGE", 
         x = "Age", y = "CRegion",
         color = "Donated or Not", size = "Num of Clients")

#CRegion vs. InState: InState - can be deleted
ggplot(pdct) + 
    geom_bar(aes(CRegion, fill = InState), position = "fill") +
    labs(title = "CRegion vs. InState", 
         x = "Region", y = "Proportion",
         fill = "Living In-state") +
    scale_fill_manual(labels = c("No", "Yes"),
                      values = c("#525252", "#d9d9d9")) +
    theme(axis.text.x = element_text(angle = 90))

#Donation vs. AGE vs. Gender:
#AGE and Gender has no relationship
#no gender difference on donation
ggplot(pdct[complete.cases(pdct),]) + 
    geom_density(aes(x = AGE, color = GENDER, fill = Donor01), alpha = .4) +
    labs(title = "Donation vs. AGE vs. Gender", 
         x = "Age", y = "Density", 
         color = "Gender", fill = "Donated or Not") +
    scale_color_manual(labels = c("Female", "Male", "Undetermined"),
                      values = c("#ce1256", "#225ea8", "#525252")) +
    scale_fill_manual(labels = c("No", "Yes"),
                      values = c("#bdbdbd", "#99d8c9"))

#Donation vs. Ask_Amount
#High correlation among Ask_Amount series
p1 <- ggplot(pdct) + 
    geom_point(aes(Ask_Amount_1, Ask_Amount_2, color=Donor01)) + 
    scale_color_manual(labels = c("No","Yes"),
                       values = c('#bdbdbd','#99d8c9'))
legend1 <- get_legend(p1)
p1 <- p1 + theme(legend.position="none")
p1 
p2 <- ggplot(pdct) + 
    geom_point(aes(Ask_Amount_3, Ask_Amount_2, color=Donor01)) + 
    scale_color_manual(values = c('#bdbdbd','#99d8c9')) +
    theme(legend.position="none")
p2
data <- pdct[, c(18,10,11,12)] %>% gather(series, value, -1)
p3 <- ggplot(subset(data, Donor01 == 1)) + 
    geom_density(aes(x = value, color = series, fill = Donor01), alpha = .4) +
    scale_x_continuous(limits = c(1, 1500)) +
    labs(x = "$ Amount of Ask", y = "Density", 
         color = "Ask Amount") +
    scale_color_manual(labels = c("1st", "2nd", "3rd"),
                       values = c("#3288bd", "#ffffbf", "#d53e4f")) +
    scale_fill_manual(values = "#99d8c9") +
    guides(fill = F)
legend2 <- get_legend(p3)
p3 <- p3 + guides(color = F)
p3
p4 <- ggplot(subset(data, Donor01 == 0)) + 
    geom_density(aes(x = value, color = series, fill = Donor01), alpha = .4) +
    scale_x_continuous(limits = c(1, 1500)) +
    labs(x = "$ Amount of Ask", y = "Density") +
    scale_color_manual(values = c("#3288bd", "#ffffbf", "#d53e4f")) +
    scale_fill_manual(values = "#bdbdbd") +
    guides(color = F, fill = F)
p4
p5 <- ggplot(data %>% group_by(Donor01, series) %>% summarise(mean = mean(value))) +
    geom_point(aes(Donor01, mean, color = series)) +
    labs(x = "Donated or Not", y = "Avg $ Amount of Ask") +
    scale_x_discrete(label = c("No", "Yes")) +
    scale_color_manual(values = c("#3288bd", "#ffffbf", "#d53e4f")) +
    guides(color = F)
p5
blankPlot <- ggplot()+geom_blank(aes(1,1))+
    theme(
        plot.background = element_blank(), 
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), 
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        axis.line = element_blank()
    )
p <- ggdraw() +
    draw_plot(p1, 0, .5, .3, .5) +
    draw_plot(p2, 0, 0, .3, .5) +
    draw_plot(p3, .3, .5, .3, .5) +
    draw_plot(p4, .3, 0, .3, .5) +
    draw_plot(p5, .6, 0, .3, .5) +
    draw_plot(legend1, .6, .5, .15, .5) +
    draw_plot(legend2, .8, .5, .15, .5) 
p

#More graphs is plotted by using Tableau
#e.g) Gender, Age, Degrees, State, InState, Affiliations, TalkTime

        #===== end of visualization =================================================
#extract highly correlated predictor variables to improve interpretability
#1) between numerical variables
corMatrix <- cor(pdct[complete.cases(pdct), c(2,5,7:8,10:12,14:15,17)]) 
highCor <- findCorrelation(corMatrix, cutoff = .75)
print(corMatrix); print(highCor)
#remove: 1 AGE; 7 Ask_Amount_3; 6 Ask_Amount_2; 3 YearSinceGrad
#instead of AGE, because of bigger num of NA
pdct$YearSinceGrad <- NULL 
pdct$YearSinceRecentGrad <- NULL
#instead of 2, because it is more related with amount of donation; refer correlation below
pdct$Ask_Amount_1 <- NULL 
pdct$Ask_Amount_3 <- NULL

#correlation between Ask_Amounts and AvgGift
corDf <- cln[complete.cases(cln), c(15,25:27)] #Ask_Amounts and AvgGift 
corMatrix <- cor(corDf) 
print(corMatrix) 
#           AvgGift         Ask_Amount_1    Ask_Amount_2    Ask_Amount_3
#AvgGift    1.00000000      0.07171262      0.07179192      0.0716460
barchart(corMatrix[1,2:4], 
         main = "Correlation between Ask_Amounts and AvgGift", 
         xlab = "Correlation with Average Gift Amount",
         col = rgb(.2,.4,.9, alpha = .6))

str(pdct); names(pdct) #examine #252725 obs of 14 var
#[1] "SCHOOL" f          "AGE"  i            "RECTYPE" f         
#[4] "GENDER" f          "NumDegrees" num    "SuccCont" f              
#[7] "AffilCount" num    "client" f          "Ask_Amount_2" num          
#[10] "InState" f        "TotAttempts" num   "TALK_TIME" num      
#[13] "CRegion" f        "Donor01" f  

#2) between categorical variables
corDf <- pdct[complete.cases(pdct), c(1,3:4,6,8,10,13)]
for(i in 1:ncol(corDf)){ 
    for(j in 1:ncol(corDf)){
        if(i != j){
            chiTb <- table(corDf[,i], corDf[,j]) #2 way table for chisq.test
            #remove small values to avoid error
            smId <- which(chiTb <= quantile(chiTb, .5), arr.ind = T)*(-1) 
            smIDr <- count(smId[,1])
            smIDc <- count(smId[,2])
            chiTb <- chiTb[smIDr$row[which(smIDr$n == ncol(chiTb))],
                           smIDc$col[which(smIDc$n == nrow(chiTb))]] 
            if(!is.table(chiTb)){
                print(c(names(corDf[c(i,j)]),"0"))
            } else if(nrow(chiTb) == 0 | ncol(chiTb) == 0){
                print(c(names(corDf[c(i,j)]),"1"))
            } else {
                chisq.test(chiTb) #chi-squared test
                print(c(names(corDf[c(i,j)]), chisq.test(chiTb)$p.value))
            }
        }
    }
} #no highly correlated variables

#rewrite a new csv file for predicting model, whether to donate
#252725 obs of 14 var
write.csv(pdct, file = "RNL_Data_pdct.csv", row.names = FALSE) 

    #===== 2) split dataset into train and testset ==================================
#remove NA of AGE 
pdct_rmAGE <- pdct[complete.cases(pdct),]
#sample 80% of original data for trainset
#randomly generate index, of which number will be 80% of nrow(pdct)
trainset_i <- sample(1:nrow(pdct_rmAGE),round(0.8*nrow(pdct_rmAGE)))
trainset_rmAGE <- pdct_rmAGE[trainset_i,]
testset_rmAGE <- pdct_rmAGE[-trainset_i,]
prop.table(table(trainset_rmAGE$Donor01)) #examine #about 30-35% of minority

#replace NA of AGE to median #45
pdct_medAGE <- pdct
pdct_medAGE$AGE[is.na(pdct_medAGE$AGE)] <- median(pdct_medAGE$AGE, na.rm = T)
#sample 80% of original data for trainset
#randomly generate index, of which number will be 80% of nrow(pdct)
trainset_i <- sample(1:nrow(pdct_medAGE),round(0.8*nrow(pdct_medAGE)))
trainset_medAGE <- pdct_medAGE[trainset_i,]
testset_medAGE <- pdct_medAGE[-trainset_i,]
prop.table(table(trainset_medAGE$Donor01)) #examine #about 30-35% of minority

#split by client; complete cases
#choose pdct_rmAGE as base dataset as it results better
#table(pdct_rmAGE[complete.cases(pdct_rmAGE),]$client) #all Winston-S is deleted
pdct_UD <- subset(pdct_rmAGE, client == "UDelaware") #fliter client type
pdct_UD <- pdct_UD[, -8] #delete client column
trainset_i <- sample(1:nrow(pdct_UD),round(0.8*nrow(pdct_UD)))
trainset_UD <- pdct_UD[trainset_i,]
testset_UD <- pdct_UD[-trainset_i,]
pdct_UM <- subset(pdct_rmAGE, client == "UMissouri") #fliter client type
pdct_UM <- pdct_UM[, -8] #delete client column
trainset_i <- sample(1:nrow(pdct_UM),round(0.8*nrow(pdct_UM)))
trainset_UM <- pdct_UM[trainset_i,]
testset_UM <- pdct_UM[-trainset_i,]
prop.table(table(trainset_UD$Donor01)) #examine #about 20% of minority
prop.table(table(trainset_UM$Donor01)) #examine #about 50% of minority

#resample train set to balance target variables
#base on trainset_rmAGE #model trained with rmAGE performs better than medAGE
trainset_rmAGE_under <- ovun.sample(Donor01 ~., data = trainset_rmAGE, 
                                 method = "under", p = 0.5, seed = 1)$data
trainset_rmAGE_both <- ovun.sample(Donor01 ~., data = trainset_rmAGE, 
                                method = "both", p = 0.5, seed = 1)$data
prop.table(table(trainset_rmAGE_under$Donor01)) #examine #about 50% of minority
prop.table(table(trainset_rmAGE_both$Donor01)) #examine #about 50% of minority

#base on trainset_UM
trainset_UM_under <- ovun.sample(Donor01 ~., data = trainset_UM, 
                                    method = "under", p = 0.5, seed = 1)$data
trainset_UM_both <- ovun.sample(Donor01 ~., data = trainset_UM, 
                                   method = "both", p = 0.5, seed = 1)$data
prop.table(table(trainset_UM_under$Donor01)) #examine #about 50% of minority
prop.table(table(trainset_UM_both$Donor01)) #examine #about 50% of minority


#save dataset
write.csv(pdct_rmAGE, file = "RNL_Data_pdct_rmAGE.csv", row.names = FALSE) 
write.csv(trainset_rmAGE, file = "RNL_Data_Trainset_rmAGE.csv", row.names = FALSE) 
write.csv(testset_rmAGE, file = "RNL_Data_Testset_rmAGE.csv", row.names = FALSE) 
write.csv(pdct_medAGE, file = "RNL_Data_pdct_medAGE.csv", row.names = FALSE) 
write.csv(trainset_medAGE, file = "RNL_Data_Trainset_medAGE.csv", row.names = FALSE) 
write.csv(testset_medAGE, file = "RNL_Data_Testset_medAGE.csv", row.names = FALSE) 
write.csv(trainset_UD, file = "RNL_Data_trainset_UD.csv", row.names = FALSE) 
write.csv(testset_UD, file = "RNL_Data_Testset_UD.csv", row.names = FALSE) 
write.csv(trainset_UM, file = "RNL_Data_trainset_UM.csv", row.names = FALSE) 
write.csv(testset_UM, file = "RNL_Data_Testset_UM.csv", row.names = FALSE) 
write.csv(trainset_rmAGE_under, file = "RNL_Data_trainset_rmAGE_under.csv", row.names = FALSE) 
write.csv(trainset_rmAGE_both, file = "RNL_Data_trainset_rmAGE_both.csv", row.names = FALSE) 
write.csv(trainset_UM_under, file = "RNL_Data_trainset_UM_under.csv", row.names = FALSE) 
write.csv(trainset_UM_both, file = "RNL_Data_trainset_UM_both.csv", row.names = FALSE) 

    #===== 3) train models with trainset and get results of models with testset =====
#model: decision tree
#tune decision tree parameters
minbuckets <- c(5, 7, 10, 15, 30) 
maxdepths <- c(5, 10, 15, 20, 25, 30)
cps <- c(0.0001, 0.001, 0.01, 0.1)

low_error1 <- 1
low_error2 <- 1
low_f <- 0

#to use different dataset, change name of dataset in code below
#use set of datasets like below:
#1) trainset_rmAGE / testset_rmAGE
#2) trainset_medAGE / testset_medAGE
#3) trainset_rmAGE_under / testset_rmAGE
#4) trainset_rmAGE_both / testset_rmAGE
#5) trainset_UD / testset_UD
#6) trainset_UM / testset_UM
#7) trainset_UM_under / testset_UM
#8) trainset_UM_both / testset_UM

for(cp in cps){
    for(maxdepth in maxdepths){
        for(minbucket in minbuckets){
            #model functions will automatically select important features
            dt <- rpart(Donor01 ~ ., data = trainset_rmAGE, #CHANGE TRAINSET
                        method = "class",
                        control = rpart.control(usesurrogate = 0,
                                                maxsurrogate = 0,
                                                minsplit = 3 * minbucket,
                                                minbucket = minbucket,
                                                maxdepth = maxdepth,
                                                cp = cp))
            dtem <- predict(dt, type = "class")
            dtprop <- errorMatrix(trainset_rmAGE$Donor01, dtem) #CHANGE TRAINSET
            dt_error1 <- dtprop[1,2] / (dtprop[1,1] + dtprop[1,2])
            dt_error2 <- dtprop[2,1] / (dtprop[2,1] + dtprop[2,2])
            dt_precision <- dtprop[2,2] / (dtprop[2,2] + dtprop[1,2])
            dt_recall <- dtprop[2,2] / (dtprop[2,1] + dtprop[2,2])
            dt_f <- 2*dt_precision*dt_recall / (dt_precision + dt_recall)
            
            pr <- predict(dt, testset_rmAGE, type = "class") #CHANGE TESTSET
            per <- table(testset_rmAGE$Donor01, pr, #CHANGE TESTSET
                         useNA="ifany",
                         dnn=c("Actual", "Predicted"))
            #if errorMatrix function doesn't work, update your RStudio version 
            perprop <- errorMatrix(testset_rmAGE$Donor01, pr) #CHANGE TESTSET
            pr_error1 <- per[1,2] / (per[1,1] + per[1,2])
            pr_error2 <- per[2,1] / (per[2,1] + per[2,2])
            pr_precision <- per[2,2] / (per[2,2] + per[1,2])
            pr_recall <- per[2,2] / (per[2,1] + per[2,2])
            pr_f <- 2*pr_precision*pr_recall / (pr_precision + pr_recall)
            
            #print non-overfitting models
            #result printed at the last is the best result to choose
            if(1- dt_f <= 1- pr_f){ #avoid over-fitting
                if(pr_error2 <= low_error2){ #lowest type 1 error
                    if(pr_error1 <= low_error1){ #lowest type 2 error
                        if(pr_f >= low_f){ #highest f
                            low_error1 <- pr_error1
                            low_error2 <- pr_error2
                            low_f <- pr_f
                            print(paste(3*minbucket, minbucket, 
                                        maxdepth, cp, sep = " "))
                            print(round(dtprop, 4))
                            print(round(perprop, 4))
                            print(round(pr_f, 4))
                        }
                    }
                }
            }
        }
    }
}

#model: random forest
#tune random forest parameters
mtrys <- c(3, 4, 5, 6, 7, 9, 12) #3, 4, 5, 6, 7, 9, 12
ntrees <- c(30, 40, 50, 60, 70, 90, 120, 150) #30, 40, 50, 60, 70, 90, 120, 150

low_error1 <- 1
low_error2 <- 1
low_f <- 0

#to use different dataset, change name of dataset in code below
#use set of datasets like below:
#1) trainset_rmAGE testset_rmAGE
#2) trainset_medAGE testset_medAGE
#3) trainset_rmAGE_under testset_rmAGE
#4) trainset_rmAGE_both testset_rmAGE
#5) trainset_UD testset_UD
#6) trainset_UM testset_UM
#7) trainset_UM_under testset_UM
#8) trainset_UM_both testset_UM

for(mtry in mtrys){
    for(ntree in ntrees){
        #model functions will automatically select important features
        rf <- randomForest(Donor01 ~ .,
                           data = trainset_rmAGE, #CHANGE TRAINSET 
                           ntree=ntree, mtry=mtry, importance = T)
        rf_error1 <- rf$confusion[1,3]
        rf_error2 <- rf$confusion[2,3] 
        rf_precision <- rf$confusion[2,2] / (rf$confusion[2,2] + rf$confusion[1,2])
        rf_recall <- rf$confusion[2,2] / (rf$confusion[2,1] + rf$confusion[2,2])
        rf_f <- 2*rf_precision*rf_recall / (rf_precision + rf_recall)
        
        pr <- predict(rf, testset_rmAGE) #CHANGE TESTSET
        per <- table(testset_rmAGE$Donor01, pr, #CHANGE TESTSET
                     useNA="ifany",
                     dnn=c("Actual", "Predicted"))
        #if errorMatrix function doesn't work, update your RStudio version 
        perprop <- errorMatrix(testset_rmAGE$Donor01, pr) #CHANGE TESTSET
        pr_error1 <- per[1,2] / (per[1,1] + per[1,2])
        pr_error2 <- per[2,1] / (per[2,1] + per[2,2])
        pr_precision <- per[2,2] / (per[2,2] + per[1,2])
        pr_recall <- per[2,2] / (per[2,1] + per[2,2])
        pr_f <- 2*pr_precision*pr_recall / (pr_precision + pr_recall)
        
        #print non-overfitting models
        #result printed at the last is the best result to choose
        if(1- rf_f <= 1- pr_f){ #avoid over-fitting
            if(pr_error2 <= low_error2){ #lowest type 1 error
                if(pr_error1 <= low_error1){ #lowest type 2 error
                    if(pr_f >= low_f){ #highest f
                        low_error1 <- pr_error1
                        low_error2 <- pr_error2
                        low_f <- pr_f
                        print(paste(ntree, mtry, sep = " "))
                        print(round(rf$confusion, 4))
                        print(round(perprop, 4))
                        print(round(pr_f, 4))
                    }
                }
            }
        }
    }
}

#plot important variables graph for best model
#need to re-run the model code by limiting the parameter to critical number
barchart(importance(rf)[order(importance(rf)[,3], decreasing = F), 3], 
         main = "Variable Importance Random Forest trainset_rmAGE_under", 
         xlab = "Accuracy")

#model: svm
#run through rattle
#rattle()

#===== end of modeling
    #===== 4) plot graphs of results ================================================
#save prediction results from Rattle and load data
result_rmAGE_under <- read.csv("testset_rmAGE_score_all.csv")
result_UD <- read.csv("testset_UD_score_all.csv")

#rmAGE_under: results vs. TotAttempts
ggplot(result_rmAGE_under, aes(TotAttempts, rf, color = rf), alpha = .4) + 
    geom_point() + geom_smooth() +
    labs(title = "Possibility of Donation by Total Attempts", 
         x = "Total Attempts", y = "Predicted Donation Rate")  +
    scale_color_gradient(low = "#c51b7d", high = "#4d9221") +
    guides(color = F)

#rmAGE_under: results vs. AGE
ggplot(result_rmAGE_under, aes(AGE, rf, color = rf), alpha = .4) + 
    geom_point() + geom_smooth() +
    labs(title = "Possibility of Donation by Age", 
         x = "Age", y = "Predicted Donation Rate") +
    scale_color_gradient(low = "#c51b7d", high = "#4d9221") +
    guides(color = F)

#rmAGE_under: results vs. SCHOOL
ggplot(result_rmAGE_under ) + 
    geom_point(aes(rf, SCHOOL, color = rf), size = .2, alpha = .2, position = "jitter") + 
    labs(title = "Possibility of Donation by School", 
         x = "Predicted Donation Rate", y = "School") +
    scale_color_gradient(low = "#c51b7d", high = "#4d9221") +
    guides(color = F)

#rmAGE_under: results vs. RECTYPE
ggplot(result_rmAGE_under ) + 
    geom_point(aes(rf, RECTYPE, color = rf), size = .2, alpha = .2, position = "jitter") + 
    labs(title = "Possibility of Donation by RECTYPE", 
         x = "Predicted Donation Rate", y = "RecType")  +
    scale_color_gradient(low = "#c51b7d", high = "#4d9221") +
    guides(color = F)

#rmAGE_under: results vs. SuccCont
ggplot(result_rmAGE_under ) + 
    geom_point(aes(rf, SuccCont, color = rf), size = .2, alpha = .2, position = "jitter") + 
    labs(title = "Possibility of Donation by SuccCont", 
         x =  "Predicted Donation Rate", y = "Successful Contact") +
    scale_y_continuous(breaks = c(0,1), label = c("No", "Yes"))  +
    scale_color_gradient(low = "#c51b7d", high = "#4d9221") +
    guides(color = F)

#rmAGE_under: results vs. Talk_Time
ggplot(result_rmAGE_under, aes(TALK_TIME, rf, color = rf), alpha = .4) + 
    geom_point() + geom_smooth() +
    labs(title = "Possibility of Donation by Talk Time", 
         x = "Talk Time", y = "Predicted Donation Rate") +
    scale_x_continuous(breaks = c(0, 100, 500, 1000, 1500)) +
    scale_color_gradient(low = "#c51b7d", high = "#4d9221") +
    guides(color = F)

#UD: results vs. Ask_Amount_2
ggplot(subset(result_UD, Ask_Amount_2 > 0), aes(Ask_Amount_2, rf, color = rf), alpha = .4) + 
    geom_point() + geom_smooth() +
    labs(title = "Possibility of Donation by Ask_Amount_2", 
         x = "$ Amount of Donation Asked", y = "Predicted Donation Rate") +
    scale_x_continuous(breaks = c(0, 300, 500, 800, 1000, 2000, 4000, 6000, 8000)) +
    scale_color_gradient(low = "#c51b7d", high = "#4d9221") +
    guides(color = F)
#===== end of modeling codes


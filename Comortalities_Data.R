library(readr)
Commute_grt_than_15_mins <- read_csv("Comortalities/data_130753Commutetime>10wlking.csv")
Commute_grt_than_15_mins= Commute_grt_than_15_mins[c(3,6)]
Commute_grt_than_15_mins$countyFIPS= as.numeric(Commute_grt_than_15_mins$countyFIPS)
names(Commute_grt_than_15_mins) = paste0('Commute_grt_15.',names(Commute_grt_than_15_mins)  )




Commute_Public_Transport_45_mins <- read_csv("Comortalities/data_130412transportime>45.csv")
Commute_Public_Transport_60_mins= Commute_Public_Transport_45_mins[,c(3,6)]
Commute_Public_Transport_60_mins$countyFIPS= as.numeric(Commute_Public_Transport_60_mins$countyFIPS)
names(Commute_Public_Transport_60_mins)= paste0('Public_Transport.',names(Commute_Public_Transport_60_mins))








County_Male_Percentages <- read_csv("Comortalities/data_125635malepercentage.csv")
County_Male_Percentages= County_Male_Percentages[,c(3,6)]
County_Male_Percentages$countyFIPS= as.numeric(County_Male_Percentages$countyFIPS)
names(County_Male_Percentages)= paste0('Male_Percentages.',names(County_Male_Percentages))
glimpse(County_Male_Percentages)


County_schools_closeto_Highway <- read_csv("Comortalities/data_124945Percentofschoolwithin150mfromgihway.csv")
glimpse(County_schools_closeto_Highway)
County_schools_closeto_Highway= County_schools_closeto_Highway[,c(3,6)]
County_schools_closeto_Highway$countyFIPS= as.numeric(County_schools_closeto_Highway$countyFIPS)
names(County_schools_closeto_Highway)= paste0('Schools_closeto_Highway.',names(County_schools_closeto_Highway))

County_fertilityrate <- read_csv("Comortalities/data_fertilityrate.csv")
glimpse(County_fertilityrate)
County_fertilityrate= County_fertilityrate[,c(3,6)]
County_fertilityrate$countyFIPS= as.numeric(County_fertilityrate$countyFIPS)
names(County_fertilityrate)= paste0('Fertility_Rate.',names(County_fertilityrate))

County_Radon_Testing <- read_csv("Comortalities/Radon.csv")
glimpse(County_Radon_Testing)
County_Radon_Testing= County_Radon_Testing[,c(3,6)]
County_Radon_Testing$countyFIPS= as.numeric(County_Radon_Testing$countyFIPS)
names(County_Radon_Testing)= paste0('Randon_Tested_Homes.',names(County_Radon_Testing))


County_Hospital_Beds<- read_csv("Comortalities/hospitalbeds.csv")
glimpse(County_Hospital_Beds)
County_Hospital_Beds= County_Hospital_Beds[,c(3,6)]
County_Hospital_Beds$countyFIPS= as.numeric(County_Hospital_Beds$countyFIPS)
names(County_Hospital_Beds)= paste0('Hospital_Beds.',names(County_Hospital_Beds))


County_Percent_Leadtesting_children <- read_csv("Comortalities/PercentofLeadtestinginchildren.csv")
glimpse(County_Percent_Leadtesting_children)
County_Percent_Leadtesting_children= County_Percent_Leadtesting_children[,c(3,6)]
County_Percent_Leadtesting_children$countyFIPS= as.numeric(County_Percent_Leadtesting_children$countyFIPS)
names(County_Percent_Leadtesting_children)= paste0('Lead_Testing_Children.',names(County_Percent_Leadtesting_children))

County_Oral_Cancer <-read_csv("Comortalities/Oralcancer.csv")
County_Oral_Cancer$`Data Comment`=NULL
County_Oral_Cancer$Race_Ethnicity= County_Oral_Cancer$`Race Ethnicity`
County_Oral_Cancer$`Race Ethnicity`= NULL

County_Oral_Cancer_pvt= reshape2::dcast(County_Oral_Cancer,stateFIPS +State+   countyFIPS +County + Year~ Gender+Race_Ethnicity, value.var = 'Value')
glimpse(County_Oral_Cancer_pvt)
County_Oral_Cancer_pvt= County_Oral_Cancer_pvt[,-c(1,2,4,5)]
County_Oral_Cancer_pvt$countyFIPS= as.numeric(County_Oral_Cancer_pvt$countyFIPS)
names(County_Oral_Cancer_pvt)=paste0("Oral_Cancer.", names(County_Oral_Cancer_pvt) )
names(County_Oral_Cancer_pvt)= gsub(" |\\(|\\)","",names(County_Oral_Cancer_pvt))
                                    


County_Lung_Cancer <- read_csv("Comortalities/LungCancer.csv")
County_Lung_Cancer$`Data Comment`=NULL
County_Lung_Cancer$Race_Ethnicity= County_Lung_Cancer$`Race Ethnicity`
County_Lung_Cancer$`Race Ethnicity`= NULL

County_Lung_Cancer_pvt= reshape2::dcast(County_Lung_Cancer,stateFIPS +State+   countyFIPS +County + Year~ Gender+Race_Ethnicity, value.var = 'Value')
County_Lung_Cancer_pvt[,c(1,2,4,5)]=NULL
County_Lung_Cancer_pvt$countyFIPS= as.numeric(County_Lung_Cancer_pvt$countyFIPS)
names(County_Lung_Cancer_pvt)=paste0('Lung_Cancer.',names(County_Lung_Cancer_pvt))
glimpse(County_Lung_Cancer_pvt)
names(County_Lung_Cancer_pvt)= gsub(" |\\(|\\)|\\/","",names(County_Lung_Cancer_pvt))



County_Asthma <- read_csv("Comortalities/Asthma.csv")
County_Asthma_Pivot= reshape2::dcast(County_Asthma, stateFIPS +State+ countyFIPS+County+Year~Gender, value.var= 'Value' )
County_Asthma_Pivot[,c(1,2,4,5)]= NULL
County_Asthma_Pivot$countyFIPS= as.numeric(County_Asthma_Pivot$countyFIPS)
names(County_Asthma_Pivot)= paste0('Asthma.',names(County_Asthma_Pivot))
names(County_Asthma_Pivot)= gsub(" |\\(|\\)|\\/","",names(County_Asthma_Pivot))
glimpse(County_Asthma_Pivot)

County_Airquality <- read_csv("Comortalities/data_airquality.csv")
County_Airquality= County_Airquality[,c(3,6)]
glimpse(County_Airquality)
County_Airquality$countyFIPS= as.numeric(County_Airquality$countyFIPS)
names(County_Airquality)= paste0('AirQaility.',names(County_Airquality))

County_COPD <- read_csv("Comortalities/Copd.csv")
County_COPD$`Data Comment`=NULL

County_COPD_Pivot= data.table::dcast(County_COPD, stateFIPS + State + countyFIPS + County + Year ~ Gender,  value.var = "Value")
County_COPD_Pivot[,c('stateFIPS','State','County','Year')]=NULL
County_COPD_Pivot$countyFIPS= as.numeric(County_COPD_Pivot$countyFIP)
names(County_COPD_Pivot)= paste0('COPD.',names(County_COPD_Pivot))
names(County_COPD_Pivot)= gsub(" |\\(|\\)|\\/","",names(County_COPD_Pivot))
glimpse(County_COPD_Pivot)



library(dplyr)
library(tidyr)

EPH_data= County_Male_Percentages %>% 
  left_join(Commute_grt_than_15_mins,by=c("Male_Percentages.countyFIPS" = "Commute_grt_15.countyFIPS")) %>%
  left_join(Commute_Public_Transport_60_mins,by=c("Male_Percentages.countyFIPS" = "Public_Transport.countyFIPS")) %>%
  left_join(County_schools_closeto_Highway,by=c("Male_Percentages.countyFIPS" = "Schools_closeto_Highway.countyFIPS")) %>%
  left_join(County_fertilityrate,by=c("Male_Percentages.countyFIPS" = "Fertility_Rate.countyFIPS")) %>%
  left_join(County_Radon_Testing,by=c("Male_Percentages.countyFIPS" = "Randon_Tested_Homes.countyFIPS")) %>%
  left_join(County_Hospital_Beds,by=c("Male_Percentages.countyFIPS" = "Hospital_Beds.countyFIPS")) %>%
  left_join(County_Percent_Leadtesting_children,by=c("Male_Percentages.countyFIPS" = "Lead_Testing_Children.countyFIPS")) %>%
  left_join(County_Oral_Cancer_pvt,by=c("Male_Percentages.countyFIPS" = "Oral_Cancer.countyFIPS")) %>%
  left_join(County_Lung_Cancer_pvt,by=c("Male_Percentages.countyFIPS" = "Lung_Cancer.countyFIPS")) %>%
  left_join(County_Asthma_Pivot,by=c("Male_Percentages.countyFIPS" = "Asthma.countyFIPS")) %>%
  left_join(County_Airquality,by=c("Male_Percentages.countyFIPS" = "AirQaility.countyFIPS")) %>%
  left_join(County_COPD_Pivot,by=c("Male_Percentages.countyFIPS" = "COPD.countyFIPS"))
  
  
for(col in colnames(EPH_data)){
  
  if(!is.numeric(EPH_data[[col]])){
  EPH_data[[col]]=gsub("\\%|,","",EPH_data[[col]])
  EPH_data[[col]]=gsub("Suppressed",NA,EPH_data[[col]])}
  
  EPH_data[[col]]=as.numeric(EPH_data[[col]])
  
  if(length(EPH_data[[col]][!is.na(EPH_data[[col]])])< 0.7*length(EPH_data$COPD.Male)){
    EPH_data[[col]]=NULL
  }
  

}









  
  
  





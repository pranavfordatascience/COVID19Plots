us_county_csv_url='https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'

df_us_county_data= read.csv(us_county_csv_url, stringsAsFactors = F,header = T)


library(dplyr)
library(ggplot2)
# source('/Comortalities/Comortalities_Data.R',local = T)

#sort(df_us_county_data$date)


df_agg_us_county_data = df_us_county_data %>%
  group_by(county,state) %>%
  arrange( desc(date) ) %>%
  slice(1)
  
df_us_county_svi= read.csv('SVI2018_US_COUNTY.csv', stringsAsFactors=F, header=T)





df_us_county_svi_covid= df_us_county_svi %>% 
  inner_join(df_us_county_data,by = c('FIPS'="fips"))

# glimpse(df_us_county_svi_covid)

df_us_county_svi_covid[df_us_county_svi_covid==-999]=NA

#ggplot(data=df_us_county_svi_covid,aes(x=df_us_county_svi_covid$M_PCI,y=df_us_county_svi_covid$cases,fill='brown'))+geom_smooth()+ggplot(data=df_us_county_svi_covid,aes(x=df_us_county_svi_covid$M_UNEMP,y=df_us_county_svi_covid$cases,fill='red'))

# is.numeric(df_us_county_svi_covid$AREA_SQMI)

for(col in colnames(df_us_county_svi_covid)){ 
  #print(col)
  #print(class(df_us_county_svi_covid[,col]))
  if(is.numeric(df_us_county_svi_covid[,col]))
  {
    #print(df_us_county_svi_covid[,col])
    df_us_county_svi_covid[,col]= ifelse(is.na(df_us_county_svi_covid[,col]),median(df_us_county_svi_covid[,col], na.rm =T),df_us_county_svi_covid[,col])
  }
  
}

functDfCasesforDate <- function(seldate) {
  
  df_us_county_svi_covid_All_dates= df_us_county_svi_covid
  
  df_us_county_svi_covid= df_us_county_svi_covid[which(df_us_county_svi_covid$date==seldate),]
  
  outliers=boxplot(df_us_county_svi_covid$cases*1000/df_us_county_svi_covid$E_TOTPOP, plot=FALSE)$out
  
  df_us_county_svi_covid_whole= df_us_county_svi_covid
  
  df_us_county_svi_covid=df_us_county_svi_covid[-which((df_us_county_svi_covid$cases*1000/df_us_county_svi_covid$E_TOTPOP) %in% outliers),]
  
  
  R_Rate_County= df_us_county_svi_covid_All_dates[(df_us_county_svi_covid_All_dates$date<seldate),c('date','FIPS','cases')] %>%
    arrange(FIPS,desc(date)) %>%
    group_by(FIPS) %>% 
    mutate(R_rate= lag(cases)-cases) %>% 
    mutate(Mean_R_Rate= mean(R_rate, na.rm = T)) %>% 
    select(FIPS,Mean_R_Rate) %>% 
    distinct(FIPS,Mean_R_Rate) %>% 
    arrange(desc(Mean_R_Rate))
  
  df_us_county_svi_covid= df_us_county_svi_covid %>% 
    left_join(R_Rate_County, by='FIPS')
  
  return (df_us_county_svi_covid)
}


 R_Rate_County= df_us_county_svi_covid[(df_us_county_svi_covid$date<'2020-04-04'),c('date','FIPS','cases')] %>%
  arrange(FIPS,desc(date)) %>% 
   group_by(FIPS) %>% 
   mutate(R_rate= lag(cases)-cases) %>% 
   mutate(Mean_R_Rate= mean(R_rate, na.rm = T)) %>% 
   select(FIPS,Mean_R_Rate) %>% 
   distinct(FIPS,Mean_R_Rate) %>% 
   arrange(desc(Mean_R_Rate))
 
test_df= functDfCasesforDate('2020-04-04')[,c('FIPS','Mean_R_Rate')]
   

   
  
df_us_county_svi_covid[(df_us_county_svi_covid$date<'2020-04-04'),c('date','FIPS','cases')]



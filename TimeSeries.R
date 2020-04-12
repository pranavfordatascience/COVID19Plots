

library(tidyr)

install_github("tidyverse/tidyr")

df_us_county_counts= df_us_county_data[,c('date','county','state','fips','cases')]

library(reshape2)

top5counties= df_us_county_counts_bydate %>% 
  arrange(desc(df_us_county_counts_bydate$`2020-04-04`)) %>% 
  slice(1:5)

top5counties$fips

ggplot(df_us_county_counts[which(df_us_county_counts$fips == 36059),], aes(x=date, y=cases, group=1)) 

   xlab("Dates") + ylab("Cases")






df_us_county_counts_bydate =dcast(data = df_us_county_counts, formula = fips ~ date,fun.aggregate = mean,fill=0, value.var = "cases")


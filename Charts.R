library(ggpubr)
source('County_Counts_Factors.R')
  
# 
#   
 for (pdate in distinct(df_us_county_svi_covid,date)$date){
#   
#   
  if(nrow(functDfCasesforDate(pdate))>0) { 
    df1= lm(formula = cases*1000/E_TOTPOP ~ EP_POV, data = functDfCasesforDate(pdate))
    assign(paste('plot1',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_POV ))+
             #theme(axis.title.x = element_blank(), axis.title.y = element_blank())+
             labs(subtitle = '%Poverty vs Cases/1000 Population',
                  x = '%Poverty', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df1$coefficients[1], slope = df1$coefficients[2]))
    
    
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_UNEMP, data = functDfCasesforDate(pdate))
    assign(paste('plot2',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             labs(title = '% Unemplyment vs Cases/1000 Population',
                  x = '% Unemplyment', y = 'Cases/1000 Population ')+
             geom_point(aes(x = EP_UNEMP ))+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_PCI, data = functDfCasesforDate(pdate))
    assign(paste('plot3',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_PCI ))+
             labs(title = 'Per Capita Income vs Cases/1000 Population',
                  x = 'Per Capita Income', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_NOHSDP, data = functDfCasesforDate(pdate))
    assign(paste('plot4',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_NOHSDP ))+
             labs(title = '% People without high school diploma vs Cases/1000 Population',
                  x = '% People without high school diploma', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_AGE65, data = functDfCasesforDate(pdate))
    assign(paste('plot5',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_AGE65 ))+
             labs(title = '% Population above age of 65 years vs Cases/1000 Population',
                  x = '% Population above age of 65 years', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_AGE17, data = functDfCasesforDate(pdate))
    assign(paste('plot6',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_AGE17 ))+
             labs(title = '% Population above age upto 17 years vs Cases/1000 Population',
                  x = '% Population above age upto 17 years', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_DISABL, data = functDfCasesforDate(pdate))
    assign(paste('plot7',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_DISABL ))+
             labs(title = '% Population with Disabilities vs Cases/1000 Population',
                  x = '% Population with Disabilities', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_SNGPNT, data = functDfCasesforDate(pdate))
    assign(paste('plot8',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_SNGPNT ))+
             labs(title = '% Housholds with Single Parent vs Cases/1000 Population',
                  x = '% Housholds with Single Parent', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_MINRTY, data = functDfCasesforDate(pdate))
    assign(paste('plot9',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_MINRTY ))+
             labs(title = '% of Minority in Population vs Cases/1000 Population',
                  x = '% of Minority in Population', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_LIMENG, data = functDfCasesforDate(pdate))
    assign(paste('plot10',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_LIMENG ))+
             labs(title = '% Population not fluent in English vs Cases/1000 Population',
                  x = '% Population not fluent in English', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_MUNIT, data = functDfCasesforDate(pdate))
    assign(paste('plot11',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_MUNIT ))+
             labs(title = '% of Housing with units more than 10 vs Cases/1000 Population',
                  x = '% of Housing with units more than 10', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_MOBILE, data = functDfCasesforDate(pdate))
    assign(paste('plot12',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_MOBILE ))+
             labs(title = '% of Mobile Housing Units vs Cases/1000 Population',
                  x = '% of Mobile Housing Units', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_CROWD, data = functDfCasesforDate(pdate))
    assign(paste('plot13',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_CROWD ))+
             labs(title = '% of Housing with more people than rooms vs Cases/1000 Population',
                  x = '% of Housing with more people than rooms', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_NOVEH, data = functDfCasesforDate(pdate))
    assign(paste('plot14',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_NOVEH ))+
             labs(title = '% of Households with no vehicle vs Cases/1000 Population',
                  x = '% of Households with no vehicle', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
    
    df= lm(formula = cases*1000/E_TOTPOP ~ EP_GROUPQ, data = functDfCasesforDate(pdate))
    assign(paste('plot15',pdate,sep='_'),ggplot(functDfCasesforDate(pdate), aes(y= cases*1000/E_TOTPOP))+
             geom_point(aes(x = EP_GROUPQ ))+
             labs(title = '% of institutionalized group quarters  vs Cases/1000 Population',
                  x = '% of institutionalized group quarters', y = 'Cases/1000 Population ')+
             geom_abline(intercept = df$coefficients[1], slope = df$coefficients[2]))
  }
 }

  


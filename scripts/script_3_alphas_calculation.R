#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 3: Estimation of alpha values  ####

   # Methodology: This script has been used to estimate the Groundwater recession constant, and in turn the alpha values.
   # Therefore, it can be considered the first part of the groundwater index estimation, as the obtained alpha values 
   # have been used as reference when using the digital filter.

   #For each basin, three representative peaks between 2010-2019 have been selected, and a linear regression has been adjusted to 
   # the baseflow recession curve in order to determine the groundwater recession constant (α). The initial point for the recession 
   #curve has been selected when the direct runoff peak ends. The estimated slope of the regression is the α value. A minimum of 10 
   # days of recession curve was selected as criteria. A minimum determination coefficient of 0.80 was also used as criteria. 
   
   # The linear adjustment regression has been performed with the stats::lm function. With this function, two coefficients are obtained in this case:
   # Intercept is the log() of the streamflow when time (t) = 0. 
   # The other coefficient is the Slope of the lineal regression, which is the groundwater recession coefficient * t, being t the number of days after t0.


   # As input data, the files with streamflow data and wih the subbasins data have been used.
   # An output csv file with the obtained alpha values and the determination coefficient has been generated (3_alpha_estimation)     

   # Used libraries
   library(readr)
   library(tidyverse)
   library(lubridate)
   #library(plotly)
   library(gt)
   library(patchwork)
   
   # Streamflow data
   gauging_data_tagus <- read.csv("used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% 
     tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
     .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))
   
   # File with IDs, names, regions and areas of the basin, and gauging stations codes  
    basins_file <- read.csv("used_files/Created_csv/1_basins_file.csv") 
   
   
   # alpha estimation for subbasins
    
   # Basin 1, Navaluenga, gauging code = 3231, region = IMP
   #Alphas obtained : 0.9641022, 0.9776144, 0.9721647
   navaluenga <- gauging_data_tagus %>% filter(., cod == 3231) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(navaluenga, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line()) # Used for choose representative peaks
   # 2975-3120
   # 660-765
   # 1100-1280
   
   peak_1_nav <- navaluenga[c(2975:3120),]
   ggplotly(ggplot(peak_1_nav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()) # Used for locate the recession curve initial point
   # Q0 (maximum baseflow) is when x = 59
   plot(peak_1_nav$obs_flow[59:85]~ peak_1_nav$date[59:85], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_nav$obs_flow[59:85])~ peak_1_nav$date[59:85], type = "l")
   reg_pk1 <- lm(log(peak_1_nav$obs_flow[59:85])~ seq(1, length(peak_1_nav$date[59:85]), 1))
   summary(reg_pk1) # alpha = -0.036558, R2 = 0.97
   
   2.71828182846^-(0.036558) #alpha parameter
   1-(0.036558)
   
   peak_2_nav <- navaluenga[c(660:765),]
   ggplotly(ggplot(peak_2_nav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 41
   plot(peak_2_nav$obs_flow[41:100]~ peak_2_nav$date[41:100], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_nav$obs_flow[41:100])~ peak_2_nav$date[41:100], type = "l")
   reg_pk2 <- lm(log(peak_2_nav$obs_flow[41:100])~seq(1, length(peak_2_nav$date[41:100]), 1))
   summary(reg_pk2) #alpha = -0.02264 , R2 = 0.97
   
   2.71828182846^-(0.02264)
   1-(0.02264)
   
   peak_3_nav <- navaluenga[c(1100:1280),]
   ggplotly(ggplot(peak_3_nav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 95
   plot(peak_3_nav$obs_flow[95:170]~ peak_3_nav$date[95:170], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_nav$obs_flow[95:170])~ peak_3_nav$date[95:170], type = "l")
   reg_pk3 <- lm(log(peak_3_nav$obs_flow[95:170])~seq(1, length(peak_3_nav$date[95:170]), 1))
   summary(reg_pk3) #alpha = -0.02823 , R2 = 0.92
   
   2.71828182846^-(0.02823)
   1-(0.02823)
  
   
   # Basin 2, Matallana, gauging code = 3049, region = IMP. 
   #Alphas obtained : 0.9645728, 0.9420848, 0.9469329
   matallana <- gauging_data_tagus %>% filter(., cod == 3049) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(matallana, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1110-1370
   # 1910-2130
   # 2620-2890
   
   peak_1_mtlln <- matallana[c(1110:1370),]
   ggplotly(ggplot(peak_1_mtlln, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 197
   plot(peak_1_mtlln$obs_flow[197:217]~ peak_1_mtlln$date[197:217], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_mtlln$obs_flow[197:217])~ peak_1_mtlln$date[197:217], type = "l")
   reg_pk1 <- lm(log(peak_1_mtlln$obs_flow[197:217])~seq(1, length(peak_1_mtlln$date[197:217]), 1))
   summary(reg_pk1) # alpha = -0.03607, R2 = 0.99
   
   2.71828182846^-(0.03607)
   1-(0.03607)
   
   peak_2_mtlln <- matallana[c(1910:2130),]
   ggplotly(ggplot(peak_2_mtlln, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 150
   plot(peak_2_mtlln$obs_flow[150:190]~ peak_2_mtlln$date[150:190], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_mtlln$obs_flow[150:190])~ peak_2_mtlln$date[150:190], type = "l")
   reg_pk2 <- lm(log(peak_2_mtlln$obs_flow[150:190])~seq(1, length(peak_2_mtlln$date[150:190]), 1))
   summary(reg_pk2) # alpha = -0.05966, R2 = 0.99
   
   2.71828182846^-(0.05966)
   1-(0.05966)
   
   peak_3_mtlln <- matallana[c(2620:2890),]
   ggplotly(ggplot(peak_3_mtlln, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 192
   plot(peak_3_mtlln$obs_flow[192:230]~ peak_3_mtlln$date[192:230], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_mtlln$obs_flow[192:230])~ peak_3_mtlln$date[192:230], type = "l")
   reg_pk3 <- lm(log(peak_3_mtlln$obs_flow[192:230])~seq(1, length(peak_3_mtlln$date[192:230]), 1))
   summary(reg_pk3) # alpha = -0.054527, R2 = 0.99
   
   2.71828182846^-(0.054527)
   1-(0.054527)
   
   # Basin 3, Villarejo de Montalban, gauging code = 3211, region = IMP
   #Alphas obtained : 0.9589081, 0.9260473, 0.9681386
   villarejo <- gauging_data_tagus %>% filter(., cod == 3211) %>% filter(year(date) > 2009)
   
   # Peak selection: Three representative peaks
   ggplotly(ggplot(villarejo, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1140-1260
   # 2980-3050
   # 1450-1585
   
   peak_1_villj <- villarejo[c(1140:1260),]
   ggplotly(ggplot(peak_1_villj, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 54
   plot(peak_1_villj$obs_flow[54:73]~ peak_1_villj$date[54:73], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_villj$obs_flow[54:73])~ peak_1_villj$date[54:73], type = "l")
   reg_pk1 <- lm(log(peak_1_villj$obs_flow[54:73])~seq(1, length(peak_1_villj$date[54:73]), 1))
   summary(reg_pk1) # alpha = -0.04196, R2 = 0.97
   
   2.71828182846^-(0.04196)
   1-(0.04196)
   
   peak_2_villj <- villarejo[c(2980:3050),]
   ggplotly(ggplot(peak_2_villj, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 21
   plot(peak_2_villj$obs_flow[21:35]~ peak_2_villj$date[21:35], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_villj$obs_flow[21:35])~ peak_2_villj$date[21:35], type = "l")
   reg_pk2 <- lm(log(peak_2_villj$obs_flow[21:35])~seq(1, length(peak_2_villj$date[21:35]), 1))
   summary(reg_pk2) # alpha = -0.07683, R2 = 0.90
   
   2.71828182846^-(0.07683)
   1-(0.07683)
   
   peak_3_villj <- villarejo[c(1450:1585),]
   ggplotly(ggplot(peak_3_villj, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 58
   plot(peak_3_villj$obs_flow[58:96]~ peak_3_villj$date[58:96], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_villj$obs_flow[58:96])~ peak_3_villj$date[58:96], type = "l")
   reg_pk3 <- lm(log(peak_3_villj$obs_flow[58:96])~seq(1, length(peak_3_villj$date[58:96]), 1))
   summary(reg_pk3) # alpha = -0.03238, R2 = 0.98
   
   2.71828182846^-(0.03238)
   1-(0.03238)
   
   
   # Basin 4, Peralejo, gauging code = 3001, region = CRB
   #Alphas obtained : 0.9856145, 0.9834357, 0.9760807
   peralejo <- gauging_data_tagus %>% filter(., cod == 3001) %>% filter(year(date) > 2009)
   
   # Peak selection: Three representative peaks
   ggplotly(ggplot(peralejo, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1030-1310
   # 1440-1710
   # 3360-3475
   
   peak_1_perjo <- peralejo[c(1030:1310),]
   ggplotly(ggplot(peak_1_perjo, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 215
   plot(peak_1_perjo$obs_flow[215:245]~ peak_1_perjo$date[215:245], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_perjo$obs_flow[215:245])~ peak_1_perjo$date[215:245], type = "l")
   reg_pk1 <- lm(log(peak_1_perjo$obs_flow[215:245])~seq(1, length(peak_1_perjo$date[215:245]), 1))
   summary(reg_pk1) # alpha = -0.01449, R2 = 0.95
   
   2.71828182846^-(0.01449)
   1-(0.01449)
   
   peak_2_perjo <- peralejo[c(1440:1710),]
   ggplotly(ggplot(peak_2_perjo, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 93
   plot(peak_2_perjo$obs_flow[93:120]~ peak_2_perjo$date[93:120], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_perjo$obs_flow[93:120])~ peak_2_perjo$date[93:120], type = "l")
   reg_pk2 <- lm(log(peak_2_perjo$obs_flow[93:120])~seq(1, length(peak_2_perjo$date[93:120]), 1))
   summary(reg_pk2) # alpha = -0.016703, R2 = 0.93
   
   2.71828182846^-(0.016703)
   1-(0.016703)
   
   peak_3_perjo <- peralejo[c(3360:3475),]
   ggplotly(ggplot(peak_3_perjo, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 55
   plot(peak_3_perjo$obs_flow[55:90]~ peak_3_perjo$date[55:90], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_perjo$obs_flow[55:90])~ peak_3_perjo$date[55:90], type = "l")
   reg_pk3 <- lm(log(peak_3_perjo$obs_flow[55:90])~seq(1, length(peak_3_perjo$date[55:90]), 1))
   summary(reg_pk3) # alpha = -0.02421, R2 = 0.97
   
   2.71828182846^-(0.02421)
   1-(0.02421)
   
   
   # Basin 5, Priego Escabas, gauging code = 3045, region = CRB
   #Alphas obtained : 0.9897529, 0.9707561, 0.965227
   priegoes <- gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(priegoes, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 980-1360
   # 2160-2450
   # 2970-3180
   
   peak_1_pres <- priegoes[c(980:1360),]
   ggplotly(ggplot(peak_1_pres, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 220
   plot(peak_1_pres$obs_flow[220:320]~ peak_1_pres$date[220:320], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_pres$obs_flow[220:320])~ peak_1_pres$date[220:320], type = "l")
   reg_pk1 <- lm(log(peak_1_pres$obs_flow[220:320])~seq(1, length(peak_1_pres$date[220:320]), 1))
   summary(reg_pk1) #alpha = -0.0103 , R2 = 0.95
   
   2.71828182846^-(0.0103)
   1-(0.0103)
   
   peak_2_pres <- priegoes[c(2160:2450),]
   ggplotly(ggplot(peak_2_pres, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 170
   plot(peak_2_pres$obs_flow[170:200]~ peak_2_pres$date[170:200], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_pres$obs_flow[170:200])~ peak_2_pres$date[170:200], type = "l")
   reg_pk2 <- lm(log(peak_2_pres$obs_flow[170:200])~seq(1, length(peak_2_pres$date[170:200]), 1))
   summary(reg_pk2) #alpha = -0.02968 , R2 = 0.98
   
   2.71828182846^-(0.02968)
   1-(0.02968)
   
   peak_3_pres <- priegoes[c(2970:3180),]
   ggplotly(ggplot(peak_3_pres, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 65
   plot(peak_3_pres$obs_flow[65:90]~ peak_3_pres$date[65:90], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_pres$obs_flow[65:90])~ peak_3_pres$date[65:90], type = "l")
   reg_pk3 <- lm(log(peak_3_pres$obs_flow[65:90])~seq(1, length(peak_3_pres$date[65:90]), 1))
   summary(reg_pk3) #alpha = -0.035392 , R2 = 0.93
   
   2.71828182846^-(0.035392)
   1-(0.035392)
   
   
   # Basin 6, Santa MarC-a del Val, gauging code = 3040, region = CRB
   #Alphas obtained : 0.9151292, 0.9387557, 0.9657213
   santamaria <- gauging_data_tagus %>% filter(., cod == 3040) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(santamaria, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 800-1000
   # 1170-1320
   # 3100-3200
   
   peak_1_stama <- santamaria[c(800:1000),]
   ggplotly(ggplot(peak_1_stama, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 120
   plot(peak_1_stama$obs_flow[120:140]~ peak_1_stama$date[120:140], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_stama$obs_flow[120:140])~ peak_1_stama$date[120:140], type = "l")
   reg_pk1 <- lm(log(peak_1_stama$obs_flow[120:140])~seq(1, length(peak_1_stama$date[120:140]),1))
   summary(reg_pk1) #alpha = -0.08869 , R2 = 0.96
   
   2.71828182846^-(0.08869 )
   1-(0.08869)
   
   peak_2_stama <- santamaria[c(1170:1320),]
   ggplotly(ggplot(peak_2_stama, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 85
   plot(peak_2_stama$obs_flow[85:100]~ peak_2_stama$date[85:100], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_stama$obs_flow[85:100])~ peak_2_stama$date[85:100], type = "l")
   reg_pk2 <- lm(log(peak_2_stama$obs_flow[85:100])~seq(1, length(peak_2_stama$date[85:100]), 1))
   summary(reg_pk2) #alpha = -0.0632 , R2 = 0.93
   
   2.71828182846^-(0.0632)
   1-(0.0632)
   
   peak_3_stama <- santamaria[c(3100:3200),]
   ggplotly(ggplot(peak_3_stama, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 36
   plot(peak_3_stama$obs_flow[36:70]~ peak_3_stama$date[36:70], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_stama$obs_flow[36:70])~ peak_3_stama$date[36:70], type = "l")
   reg_pk3 <- lm(log(peak_3_stama$obs_flow[36:70])~seq(1, length(peak_3_stama$date[36:70]), 1))
   summary(reg_pk3) #alpha = -0.03488 , R2 = 0.97
   
   2.71828182846^-(0.03488)
   1-(0.03488)
   
   
   # Basin 7, Jabalera, gauging code = 3249, region = DTAL
   #Alphas obtained : 0.9462959, 0.9489312, 0.943499
   javalera <- gauging_data_tagus %>% filter(., cod == 3249) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(javalera, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1150-1290
   # 2220-2400
   # 2960-3160
   
   peak_1_java <- javalera[c(1150:1290),]
   ggplotly(ggplot(peak_1_java, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 45
   plot(peak_1_java$obs_flow[45:60]~ peak_1_java$date[45:60], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_java$obs_flow[45:60])~ peak_1_java$date[45:60], type = "l")
   reg_pk1 <- lm(log(peak_1_java$obs_flow[45:60])~seq(1, length(peak_1_java$date[45:60]), 1))
   summary(reg_pk1) #alpha = -0.0552 , R2 = 0.97
   
   2.71828182846^-(0.0552)
   1-(0.0552)
   
   peak_2_java <- javalera[c(2220:2400),]
   ggplotly(ggplot(peak_2_java, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 114
   plot(peak_2_java$obs_flow[114:148]~ peak_2_java$date[114:148], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_java$obs_flow[114:148])~ peak_2_java$date[114:148], type = "l")
   reg_pk2 <- lm(log(peak_2_java$obs_flow[114:148])~seq(1, length(peak_2_java$date[114:148]), 1))
   summary(reg_pk2) #alpha = -0.052419 , R2 = 0.96
   
   2.71828182846^-(0.052419)
   1-(0.052419)
   
   peak_3_java <- javalera[c(2960:3160),]
   ggplotly(ggplot(peak_3_java, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 126
   plot(peak_3_java$obs_flow[126:149]~ peak_3_java$date[126:149], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_java$obs_flow[126:149])~ peak_3_java$date[126:149], type = "l")
   reg_pk3 <- lm(log(peak_3_java$obs_flow[126:149])~seq(1, length(peak_3_java$date[126:149]), 1))
   summary(reg_pk3) #alpha = -0.05816 , R2 = 0.87
   
   2.71828182846^-(0.05816)
   1-(0.05816)
   
   
   # Basin 8, Huete, gauging code = 3172, region = DTAL
   #Alphas obtained : 0.9726986, 0.9664391, 0.9698062
   huete <- gauging_data_tagus %>% filter(., cod == 3172) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(huete, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 34-200
   # 1080-1330
   # 3350-3480
   
   peak_1_huet <- huete[c(34:200),]
   ggplotly(ggplot(peak_1_huet, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 41
   plot(peak_1_huet$obs_flow[41:68]~ peak_1_huet$date[41:68], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_huet$obs_flow[41:68])~ peak_1_huet$date[41:68], type = "l")
   reg_pk1 <- lm(log(peak_1_huet$obs_flow[41:68])~seq(1, length(peak_1_huet$date[41:68]), 1))
   summary(reg_pk1) #alpha = -0.027681, R2 = 0.96
   
   2.71828182846^-(0.027681)
   1-(0.027681)
   
   peak_2_huet <- huete[c(1080:1330),]
   ggplotly(ggplot(peak_2_huet, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 120
   plot(peak_2_huet$obs_flow[120:133]~ peak_2_huet$date[120:133], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_huet$obs_flow[120:133])~ peak_2_huet$date[120:133], type = "l")
   reg_pk2 <- lm(log(peak_2_huet$obs_flow[120:133])~seq(1, length(peak_2_huet$date[120:133]), 1))
   summary(reg_pk2) #alpha = -0.034137 , R2 = 0.98
   
   2.71828182846^-(0.034137)
   1-(0.034137)
   
   peak_3_huet <- huete[c(3350:3480),]
   ggplotly(ggplot(peak_3_huet, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 83
   plot(peak_3_huet$obs_flow[83:100]~ peak_3_huet$date[83:100], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_huet$obs_flow[83:100])~ peak_3_huet$date[83:100], type = "l")
   reg_pk3 <- lm(log(peak_3_huet$obs_flow[83:100])~seq(1, length(peak_3_huet$date[83:100]), 1))
   summary(reg_pk3) #alpha = -0.030659 , R2 = 0.92
   
   2.71828182846^-(0.030659)
   1-(0.030659)
   
   
   # Basin 9, Torote, gauging code = 3193, region = DTAL
   #Alphas obtained : 0.9632243, 0.9357191, 0.9063679
   torote <- gauging_data_tagus %>% filter(., cod == 3193) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(torote, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1430-1570
   # 1130-1230
   # 2950-3100
   
   peak_1_toro <- torote[c(1430:1570),]
   ggplotly(ggplot(peak_1_toro, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 84
   plot(peak_1_toro$obs_flow[84:117]~ peak_1_toro$date[84:117], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_toro$obs_flow[84:117])~ peak_1_toro$date[84:117], type = "l")
   reg_pk1 <- lm(log(peak_1_toro$obs_flow[84:117])~seq(1, length(peak_1_toro$date[84:117]), 1))
   summary(reg_pk1) #alpha = -0.037469, R2 = 0.95
   
   2.71828182846^-(0.037469)
   1-(0.037469)
   
   peak_2_toro <- torote[c(1130:1230),]
   ggplotly(ggplot(peak_2_toro, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 64
   plot(peak_2_toro$obs_flow[64:83]~ peak_2_toro$date[64:83], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_toro$obs_flow[64:83])~ peak_2_toro$date[64:83], type = "l")
   reg_pk2 <- lm(log(peak_2_toro$obs_flow[64:83])~seq(1, length(peak_2_toro$date[64:83]), 1))
   summary(reg_pk2) #alpha = -0.06644 , R2 = 0.97
   
   2.71828182846^-(0.06644)
   1-(0.06644)
   
   peak_3_toro <- torote[c(2950:3100),]
   ggplotly(ggplot(peak_3_toro, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 52
   plot(peak_3_toro$obs_flow[52:63]~ peak_3_toro$date[52:63], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_toro$obs_flow[52:63])~ peak_3_toro$date[52:63], type = "l")
   reg_pk3 <- lm(log(peak_3_toro$obs_flow[52:63])~seq(1, length(peak_3_toro$date[52:63]), 1))
   summary(reg_pk3) #alpha = -0.09831 , R2 = 0.98
   
   2.71828182846^-(0.09831)
   1-(0.09831)
   
   
   # Basin 10, La Pueblanueva, gauging code = 3251, region = DTAL
   #Alphas obtained : 0.9432395, 0.944205, 0.8910097
   pueblanueva <- gauging_data_tagus %>% filter(., cod == 3251) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(pueblanueva, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 40-100
   # 1450-1590
   # 2980-3060
   
   peak_1_puebla <- pueblanueva[c(40:100),]
   ggplotly(ggplot(peak_1_puebla, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 29
   plot(peak_1_puebla$obs_flow[29:40]~ peak_1_puebla$date[29:40], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_puebla$obs_flow[29:40])~ peak_1_puebla$date[29:40], type = "l")
   reg_pk1 <- lm(log(peak_1_puebla$obs_flow[29:40])~seq(1, length(peak_1_puebla$date[29:40]), 1))
   summary(reg_pk1) #alpha = -0.058435, R2 = 0.96
   
   2.71828182846^-(0.058435)
   1-(0.058435)
   
   peak_2_puebla <- pueblanueva[c(1450:1590),]
   ggplotly(ggplot(peak_2_puebla, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 61
   plot(peak_2_puebla$obs_flow[61:90]~ peak_2_puebla$date[61:90], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_puebla$obs_flow[61:90])~ peak_2_puebla$date[61:90], type = "l")
   reg_pk2 <- lm(log(peak_2_puebla$obs_flow[61:90])~seq(1, length(peak_2_puebla$date[61:90]), 1))
   summary(reg_pk2) #alpha = -0.057412, R2 = 0.96
   
   2.71828182846^-(0.057412)
   1-(0.057412)
   
   peak_3_puebla <- pueblanueva[c(2980:3060),]
   ggplotly(ggplot(peak_3_puebla, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 21
   plot(peak_3_puebla$obs_flow[21:36]~ peak_3_puebla$date[21:36], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_puebla$obs_flow[21:36])~ peak_3_puebla$date[21:36], type = "l")
   reg_pk3 <- lm(log(peak_3_puebla$obs_flow[21:36])~seq(1, length(peak_3_puebla$date[21:36]), 1))
   summary(reg_pk3) #alpha = -0.1154 , R2 = 0.97
   
   2.71828182846^-(0.1154)
   1-(0.1154)
   
   
   # Basin 11, Priego Trabaque, gauging code = 3186, region = DTBJ
   #Alphas obtained : 0.9208851 , 0.9474719, 0.9608115
   priegotra <- gauging_data_tagus %>% filter(., cod == 3186) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(priegotra, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1100-1280
   # 1460-1630
   # 3370-3460
   
   peak_1_prietra <- priegotra[c(1100:1280),]
   ggplotly(ggplot(peak_1_prietra, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 89
   plot(peak_1_prietra$obs_flow[89:104]~ peak_1_prietra$date[89:104], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_prietra$obs_flow[89:104])~ peak_1_prietra$date[89:104], type = "l")
   reg_pk1 <- lm(log(peak_1_prietra$obs_flow[89:104])~seq(1, length(peak_1_prietra$date[89:104]), 1))
   summary(reg_pk1) #alpha = -0.08242, R2 = 0.95
   
   2.71828182846^-(0.08242)
   1-(0.08242)
   
   peak_2_prietra <- priegotra[c(1460:1630),]
   ggplotly(ggplot(peak_2_prietra, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 48
   plot(peak_2_prietra$obs_flow[48:60]~ peak_2_prietra$date[48:60], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_prietra$obs_flow[48:60])~ peak_2_prietra$date[48:60], type = "l")
   reg_pk2 <- lm(log(peak_2_prietra$obs_flow[48:60])~seq(1, length(peak_2_prietra$date[48:60]), 1))
   summary(reg_pk2) #alpha = -0.053958, R2 = 0.87
   
   2.71828182846^-(0.053958)
   1-(0.053958)
   
   peak_3_prietra <- priegotra[c(3370:3460),]
   ggplotly(ggplot(peak_3_prietra, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 37
   plot(peak_3_prietra$obs_flow[37:60]~ peak_3_prietra$date[37:60], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_prietra$obs_flow[37:60])~ peak_3_prietra$date[37:60], type = "l")
   reg_pk3 <- lm(log(peak_3_prietra$obs_flow[37:60])~seq(1, length(peak_3_prietra$date[37:60]), 1))
   summary(reg_pk3) #alpha = -0.039977 , R2 = 0.99
   
   2.71828182846^-(0.039977)
   1-(0.039977)
   
   
   # Basin 12, La Peraleja, gauging code = 3173, region = DTBJ
   #Alphas obtained : 0.921143, 0.9540082, 0.9075379
   peraleja <- gauging_data_tagus %>% filter(., cod == 3173) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(peraleja, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1140-1295
   # 1470-1630
   # 2990-3060
   
   peak_1_perja <- peraleja[c(1140:1295),]
   ggplotly(ggplot(peak_1_perja, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 83
   plot(peak_1_perja$obs_flow[83:97]~ peak_1_perja$date[83:97], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_perja$obs_flow[83:97])~ peak_1_perja$date[83:97], type = "l")
   reg_pk1 <- lm(log(peak_1_perja$obs_flow[83:97])~seq(1, length(peak_1_perja$date[83:97]), 1))
   summary(reg_pk1) #alpha = -0.02312, R2 = 0.97
   
   2.71828182846^-(0.02312)
   1-(0.02312)
   
   peak_2_perja <- peraleja[c(1470:1630),]
   ggplotly(ggplot(peak_2_perja, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 93
   plot(peak_2_perja$obs_flow[93:149]~ peak_2_perja$date[93:149], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_perja$obs_flow[93:149])~ peak_2_perja$date[93:149], type = "l")
   reg_pk2 <- lm(log(peak_2_perja$obs_flow[93:149])~seq(1, length(peak_2_perja$date[93:149]), 1))
   summary(reg_pk2) #alpha = -0.047083, R2 = 0.93
   
   2.71828182846^-(0.047083)
   1-(0.047083)
   
   peak_3_perja <- peraleja[c(2990:3060),]
   ggplotly(ggplot(peak_3_perja, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 40
   plot(peak_3_perja$obs_flow[40:55]~ peak_3_perja$date[40:55], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_perja$obs_flow[40:55])~ peak_3_perja$date[40:55], type = "l")
   reg_pk3 <- lm(log(peak_3_perja$obs_flow[40:55])~seq(1, length(peak_3_perja$date[40:55]), 1))
   summary(reg_pk3) #alpha = -0.09702 , R2 = 0.90
   
   2.71828182846^-(0.09702)
   1-(0.09702)
   
   
   # Basin 13, Villasequilla de Yepes, gauging code = 3164, region = DTBJ
   #Alphas obtained :  0.9771452, 0.967856, 0.9517413
   villaseq <- gauging_data_tagus %>% filter(., cod == 3164) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(villaseq, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1310-1700
   # 2128-2420
   # 2975-3172
   
   peak_1_villas <- villaseq[c(1310:1700),]
   ggplotly(ggplot(peak_1_villas, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 222
   plot(peak_1_villas$obs_flow[222:238]~ peak_1_villas$date[222:238], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_villas$obs_flow[222:238])~ peak_1_villas$date[222:238], type = "l")
   reg_pk1 <- lm(log(peak_1_villas$obs_flow[222:238])~seq(1, length(peak_1_villas$date[222:238]), 1))
   summary(reg_pk1) #alpha = -0.02312, R2 = 0.97
   
   2.71828182846^-(0.02312)
   1-(0.08214)
   
   peak_2_villas <- villaseq[c(2128:2420),]
   ggplotly(ggplot(peak_2_villas, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 209
   plot(peak_2_villas$obs_flow[209:242]~ peak_2_villas$date[209:242], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_villas$obs_flow[209:242])~ peak_2_villas$date[209:242], type = "l")
   reg_pk2 <- lm(log(peak_2_villas$obs_flow[209:242])~seq(1, length(peak_2_villas$date[209:242]), 1))
   summary(reg_pk2) #alpha = -0.032672, R2 = 0.94
   
   2.71828182846^-(0.032672)
   1-(0.032672)
   
   peak_3_villas <- villaseq[c(2975:3172),]
   ggplotly(ggplot(peak_3_villas, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 107
   plot(peak_3_villas$obs_flow[107:128]~ peak_3_villas$date[107:128], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_villas$obs_flow[107:128])~ peak_3_villas$date[107:128], type = "l")
   reg_pk3 <- lm(log(peak_3_villas$obs_flow[107:128])~seq(1, length(peak_3_villas$date[107:128]), 1))
   summary(reg_pk3) #alpha = -0.049462 , R2 = 0.98
   
   2.71828182846^-(0.049462)
   1-(0.049462)
   

   # Basin 14, Valverde de los Arroyos, gauging code = 3165, region = Mix
   #Alphas obtained : 0.9130451, 0.9254178, 0.9206273
   valverde <- gauging_data_tagus %>% filter(., cod == 3165) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(valverde, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 270-350
   # 1630-1850
   # 2657-2920
   
   peak_1_valv <- valverde[c(270:350),]
   ggplotly(ggplot(peak_1_valv, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 45
   plot(peak_1_valv$obs_flow[45:65]~ peak_1_valv$date[45:65], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_valv$obs_flow[45:65])~ peak_1_valv$date[45:65], type = "l")
   reg_pk1 <- lm(log(peak_1_valv$obs_flow[45:65])~seq(1, length(peak_1_valv$date[45:65]), 1))
   summary(reg_pk1) #alpha = -0.09097, R2 = 0.91
   
   2.71828182846^-(0.09097)
   1-(0.09097)
   
   peak_2_valv <- valverde[c(1630:1850),]
   ggplotly(ggplot(peak_2_valv, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 157
   plot(peak_2_valv$obs_flow[157:180]~ peak_2_valv$date[157:180], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_valv$obs_flow[157:180])~ peak_2_valv$date[157:180], type = "l")
   reg_pk2 <- lm(log(peak_2_valv$obs_flow[157:180])~seq(1, length(peak_2_valv$date[157:180]), 1))
   summary(reg_pk2) #alpha = -0.07751, R2 = 0.99
   
   2.71828182846^-(0.07751)
   1-(0.07751)
   
   peak_3_valv <- valverde[c(2657:2920),]
   ggplotly(ggplot(peak_3_valv, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 206
   plot(peak_3_valv$obs_flow[206:221]~ peak_3_valv$date[206:221], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_valv$obs_flow[206:221])~ peak_3_valv$date[206:221], type = "l")
   reg_pk3 <- lm(log(peak_3_valv$obs_flow[206:221])~seq(1, length(peak_3_valv$date[206:221]), 1))
   summary(reg_pk3) #alpha = -0.0827, R2 = 0.99
   
   2.71828182846^-(0.0827)
   1-(0.0827)
   
   
   # Basin 15, Malpica, gauging code = 3212, region = Mix
   #Alphas obtained : 0.876867, 0.8598477, 0.8595038
   malpica <- gauging_data_tagus %>% filter(., cod == 3212) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(malpica, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1135-1250
   # 2970-3060
   # 3390-3430
   
   peak_1_malp <- malpica[c(1135:1250),]
   ggplotly(ggplot(peak_1_malp, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 58
   plot(peak_1_malp$obs_flow[58:68]~ peak_1_malp$date[58:68], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_malp$obs_flow[58:68])~ peak_1_malp$date[58:68], type = "l")
   reg_pk1 <- lm(log(peak_1_malp$obs_flow[58:68])~seq(1, length(peak_1_malp$date[58:68]), 1))
   summary(reg_pk1) #alpha = -0.1314, R2 = 0.85
   
   2.71828182846^-(0.1314)
   1-(0.1314)
   
   peak_2_malp <- malpica[c(2970:3060),]
   ggplotly(ggplot(peak_2_malp, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 61
   plot(peak_2_malp$obs_flow[61:71]~ peak_2_malp$date[61:71], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_malp$obs_flow[61:71])~ peak_2_malp$date[61:71], type = "l")
   reg_pk2 <- lm(log(peak_2_malp$obs_flow[61:71])~seq(1, length(peak_2_malp$date[61:71]), 1))
   summary(reg_pk2) #alpha = -0.151, R2 = 0.95
   
   2.71828182846^-(0.151)
   1-(0.151)
   
   peak_3_malp <- malpica[c(3390:3430),]
   ggplotly(ggplot(peak_3_malp, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 16
   plot(peak_3_malp$obs_flow[16:26]~ peak_3_malp$date[16:26], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_malp$obs_flow[16:26])~ peak_3_malp$date[16:26], type = "l")
   reg_pk3 <- lm(log(peak_3_malp$obs_flow[16:26])~seq(1, length(peak_3_malp$date[16:26]), 1))
   summary(reg_pk3) #alpha = -0.1514, R2 = 0.97
   
   2.71828182846^-(0.1514)
   1-(0.1514)
   
   
   # Basin 16, Taravillas, gauging code = 3268, region = Mix
   #Alphas obtained : 0.9753392, 0.9488866, 0.9444335
   taravillas <- gauging_data_tagus %>% filter(., cod == 3268) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(taravillas, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1450-1630
   # 2970-3170
   # 3380-3485
   
   peak_1_tarav <- taravillas[c(1450:1630),]
   ggplotly(ggplot(peak_1_tarav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 76
   plot(peak_1_tarav$obs_flow[76:117]~ peak_1_tarav$date[76:117], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_tarav$obs_flow[76:117])~ peak_1_tarav$date[76:117], type = "l")
   reg_pk1 <- lm(log(peak_1_tarav$obs_flow[76:117])~seq(1, length(peak_1_tarav$date[76:117]), 1))
   summary(reg_pk1) #alpha = -0.02497, R2 = 0.98
   
   2.71828182846^-(0.02497)
   1-(0.02497)
   
   peak_2_tarav <- taravillas[c(2970:3170),]
   ggplotly(ggplot(peak_2_tarav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 66
   plot(peak_2_tarav$obs_flow[66:87]~ peak_2_tarav$date[66:87], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_tarav$obs_flow[66:87])~ peak_2_tarav$date[66:87], type = "l")
   reg_pk2 <- lm(log(peak_2_tarav$obs_flow[66:87])~seq(1, length(peak_2_tarav$date[66:87]), 1))
   summary(reg_pk2) #alpha = -0.052466, R2 = 0.95
   
   2.71828182846^-(0.052466)
   1-(0.052466)
   
   peak_3_tarav <- taravillas[c(3380:3485),]
   ggplotly(ggplot(peak_3_tarav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 31
   plot(peak_3_tarav$obs_flow[31:49]~ peak_3_tarav$date[31:49], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_tarav$obs_flow[31:49])~ peak_3_tarav$date[31:49], type = "l")
   reg_pk3 <- lm(log(peak_3_tarav$obs_flow[31:49])~seq(1, length(peak_3_tarav$date[31:49]), 1))
   summary(reg_pk3) #alpha = -0.05717, R2 = 0.99
   
   2.71828182846^-(0.05717)
   1-(0.05717)
   
   
   # Basin 17, Romanones, gauging code = 3237, region = Mix
   #Alphas obtained : 0.9898914, 0.9860147, 0.9903093
   romanones <- gauging_data_tagus %>% filter(., cod == 3237) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(romanones, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 224-630
   # 2060-2420
   # 2970-3168
   
   peak_1_roman <- romanones[c(224:630),]
   ggplotly(ggplot(peak_1_roman, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 307
   plot(peak_1_roman$obs_flow[307:350]~ peak_1_roman$date[307:350], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_roman$obs_flow[307:350])~ peak_1_roman$date[307:350], type = "l")
   reg_pk1 <- lm(log(peak_1_roman$obs_flow[307:350])~seq(1, length(peak_1_roman$date[307:350]), 1))
   summary(reg_pk1) #alpha = -0.01016, R2 = 0.95
   
   2.71828182846^-(0.01016)
   1-(0.01016)
   
   peak_2_roman <- romanones[c(2060:2420),]
   ggplotly(ggplot(peak_2_roman, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 301
   plot(peak_2_roman$obs_flow[301:336]~ peak_2_roman$date[301:336], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_roman$obs_flow[301:336])~ peak_2_roman$date[301:336], type = "l")
   reg_pk2 <- lm(log(peak_2_roman$obs_flow[301:336])~seq(1, length(peak_2_roman$date[301:336]), 1))
   summary(reg_pk2) #alpha = -0.014084, R2 = 0.90
   
   2.71828182846^-(0.014084)
   1-(0.014084)
   
   peak_3_roman <- romanones[c(2970:3168),]
   ggplotly(ggplot(peak_3_roman, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 121
   plot(peak_3_roman$obs_flow[121:170]~ peak_3_roman$date[121:170], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_roman$obs_flow[121:170])~ peak_3_roman$date[121:170], type = "l")
   reg_pk3 <- lm(log(peak_3_roman$obs_flow[121:170])~seq(1, length(peak_3_roman$date[121:170]), 1))
   summary(reg_pk3) #alpha = -0.009738, R2 = 0.98
   
   2.71828182846^-(0.009738)
   1-(0.009738)
   

   # Basin 18, Ventosa, gauging code = 3030, region = Mix
   #Alphas obtained :  0.9925588, 0.9768892, 0.9488667
   ventosa <- gauging_data_tagus %>% filter(., cod == 3030) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(ventosa, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1470-1670
   # 1150-1320
   # 3380-3420
   
   peak_1_vent <- ventosa[c(1470:1670),]
   ggplotly(ggplot(peak_1_vent, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 61
   plot(peak_1_vent$obs_flow[61:101]~ peak_1_vent$date[61:101], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_vent$obs_flow[61:101])~ peak_1_vent$date[61:101], type = "l")
   reg_pk1 <- lm(log(peak_1_vent$obs_flow[61:101])~seq(1, length(peak_1_vent$date[61:101]), 1))
   summary(reg_pk1) #alpha = -0.007469, R2 = 0.92
   
   2.71828182846^-(0.007469)
   1-(0.007469 )
   
   peak_2_vent <- ventosa[c(1150:1320),]
   ggplotly(ggplot(peak_2_vent, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 48
   plot(peak_2_vent$obs_flow[48:63]~ peak_2_vent$date[48:63], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_vent$obs_flow[48:63])~ peak_2_vent$date[48:63], type = "l")
   reg_pk2 <- lm(log(peak_2_vent$obs_flow[48:63])~seq(1, length(peak_2_vent$date[48:63]), 1))
   summary(reg_pk2) #alpha = -0.023382, R2 = 0.86
   
   2.71828182846^-(0.023382)
   1-(0.023382)
   
   peak_3_vent <- ventosa[c(3380:3420),]
   ggplotly(ggplot(peak_3_vent, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 26
   plot(peak_3_vent$obs_flow[26:38]~ peak_3_vent$date[26:38], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_vent$obs_flow[26:38])~ peak_3_vent$date[26:38], type = "l")
   reg_pk3 <- lm(log(peak_3_vent$obs_flow[26:38])~seq(1, length(peak_3_vent$date[26:38]), 1))
   summary(reg_pk3) #alpha = -0.052487, R2 = 0.95
   
   2.71828182846^-(0.052487)
   1-(0.052487)
   
   
   # Basin 19, Bujaralo, gauging code = 3060, region = Mix
   #Alphas obtained :  0.9775362, 0.9882891, 0.9617796
   bujaralo <- gauging_data_tagus %>% filter(., cod == 3060) %>% filter(year(date) > 2009)
   # Peak selection: Three representative peaks
   ggplotly(ggplot(bujaralo, aes( x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # 1440-1590
   # 2210-2450
   # 2970-3160
   
   peak_1_bujar <- bujaralo[c(1440:1590),]
   ggplotly(ggplot(peak_1_bujar, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 120
   plot(peak_1_bujar$obs_flow[120:145]~ peak_1_bujar$date[120:145], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_1_bujar$obs_flow[120:145])~ peak_1_bujar$date[120:145], type = "l")
   reg_pk1 <- lm(log(peak_1_bujar$obs_flow[120:145])~seq(1, length(peak_1_bujar$date[120:145]), 1))
   summary(reg_pk1) #alpha = -0.02272, R2 = 0.96
   
   2.71828182846^-(0.02272 )
   1-(0.02272 )
   
   peak_2_bujar <- bujaralo[c(2210:2450),]
   ggplotly(ggplot(peak_2_bujar, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 134
   plot(peak_2_bujar$obs_flow[134:212]~ peak_2_bujar$date[134:212], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_2_bujar$obs_flow[134:212])~ peak_2_bujar$date[134:212], type = "l")
   reg_pk2 <- lm(log(peak_2_bujar$obs_flow[134:212])~seq(1, length(peak_2_bujar$date[134:212]), 1))
   summary(reg_pk2) #alpha = -0.01178, R2 = 0.97
   
   2.71828182846^-(0.01178)
   1-(0.01178)
   
   peak_3_bujar <- bujaralo[c(2970:3160),]
   ggplotly(ggplot(peak_3_bujar, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   # Q0 (maximum baseflow) is when x = 63
   plot(peak_3_bujar$obs_flow[63:90]~ peak_3_bujar$date[63:90], type = "l")
   #Linear adjustment for estimating alpha
   plot(log(peak_3_bujar$obs_flow[63:90])~ peak_3_bujar$date[63:90], type = "l")
   reg_pk3 <- lm(log(peak_3_bujar$obs_flow[63:90])~seq(1, length(peak_3_bujar$date[63:90]), 1))
   summary(reg_pk3) #alpha = -0.03897, R2 = 0.99
   
   2.71828182846^-(0.03897)
   1-(0.03897)
   
   
   #Obtained alpha values for each of the basins through the recession curve adjustment and determination coefficients
   
   alphas <- c(0.9641022, 0.9776144, 0.9721647, 0.9645728,
               0.9420848, 0.9469329, 0.9589081, 0.9260473, 0.9681386, 0.9856145, 0.9834357, 
               0.9760807, 0.9897529, 0.9707561, 0.965227, 0.9151292, 0.9387557, 0.9657213, 
               0.9462959, 0.9489312, 0.943499, 0.9726986, 0.9664391, 0.9698062, 0.9632243, 
               0.9357191, 0.9063679, 0.9432395, 0.944205, 0.8910097, 0.9208851 , 0.9474719,
               0.9608115,  0.921143, 0.9540082, 0.9075379,  0.9771452, 0.967856, 0.9517413, 
               0.9130451, 0.9254178, 0.9206273, 0.876867, 0.8598477, 0.8595038, 0.9753392,
               0.9488866, 0.9444335, 0.9898914, 0.9860147, 0.9903093, 0.9925588, 0.9768892, 
               0.9488667, 0.9775362, 0.9882891, 0.9617796)
   
   det_coefs <- c(0.9728, 0.9673, 0.9223 ,0.9926 , 0.9889, 0.9856,
                  0.9711 , 0.9007 , 0.9789 , 0.9515 , 0.9288 , 0.969 , 0.9538 , 0.9776 , 0.9258 ,
                  0.96 , 0.9279 , 0.9742 , 0.972 , 0.9592 , 0.8686 , 0.9635 , 0.9807 , 0.923 , 0.9531 ,
                  0.9703, 0.9755, 0.9628 , 0.9612,  0.9748 , 0.9481 , 0.8649,  0.9856 , 0.9684, 0.9298, 
                  0.8952, 0.9678,  0.9395 , 0.9799 , 0.9059 , 0.987 , 0.9881, 0.8463 , 0.9524 , 0.9708 , 
                  0.9816, 0.9469, 0.9866, 0.9541 , 0.8964, 0.9802, 0.9226, 0.8626 , 0.9548 , 0.9638 , 0.9651 , 0.9854 )
   

   #### FINAL TABLE ####
   
   #Table with the Alphas values obtained with the code above
   
   Basins <- c()
   Basin_IDs <- c()
   regions <- c()
   for(i in 1:length(basins_file$Basin)){
     Basin <- c(rep(basins_file$Basin[i], 3))
     Basins <- c(Basins, Basin)
     Basin_ID <- c(rep(basins_file$Basin_ID[i],3))
     Basin_IDs <- c(Basin_IDs, Basin_ID)
     region <- c(rep(basins_file$region[i], 3))
     regions <- c(regions, region)
   }
   
   alphas_tibble <- tibble(Basins, Basin_IDs, regions, alphas, det_coefs) 
   alphas_tibble$regions <- factor(alphas_tibble$regions, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX"))
   
   
   # Saving the alpha values in a csv file
   alphas_tibble %>% write.csv(., "used_files/Created_csv/3_alpha_estimation.csv", quote = F, row.names = F)
   
   
   alphas_tibble <- read.csv("used_files/Created_csv/3_alpha_estimation.csv") %>% 
   mutate(regions = factor(alphas_tibble$regions, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX")))
   regions <-  factor(basins_file$region, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX"))
            
            
    tab_bas <- alphas_tibble %>% group_by(Basins) %>% summarise(alpha_bas = round(mean(alphas), 3), dcoef_bas = round(mean(det_coefs), 3)) %>% 
    inner_join(., basins_file[, c("Basin","Basin_ID")], c("Basins" = "Basin")) %>% arrange(., Basin_ID) %>% .[, c(1:3)]
    
    colnames(tab_bas) <- c("Subbasin", "Mean alpha value", "Alpha standard deviation", "Mean determination coefficient")
    apa(tab_bas)
    
    
    tab_reg <- alphas_tibble %>% group_by(Basins) %>% summarise(alpha_bas = mean(alphas), dcoef_bas = mean(det_coefs)) %>% cbind(regions) %>%  
    group_by(regions)   %>%  summarise(alpha_reg = round(mean(alpha_bas), 3), alpha_sd = round(sd(alpha_bas), 3), det_coef_reg = round(mean(dcoef_bas), 3)) 
   
    colnames(tab_reg) <- c("Region", "Mean alpha value", "Alpha standard deviation", "Mean determination coefficient")
   
    view(tab_reg)
   
   
   
   # Graphical summary at region scale
   alphas_tibble %>% ggplot(., aes(x = regions))+geom_violin(aes(y = alphas, fill = regions))
   alphas_tibble %>% ggplot(., aes(x = regions))+geom_violin(aes(y = det_coefs, fill = regions))
   
   # Statistical summary at region and basin scale
   alphas_tibble %>% group_by(Basins) %>% summarise(max = max(alphas), min = min(alphas), mean = mean(alphas), sd = sd(alphas))
   alphas_tibble %>% group_by(regions) %>% summarise(max = max(alphas), min = min(alphas), mean = mean(alphas), sd = sd(alphas))
   
   basin_names <- basins_file[,c(1:2,5)] %>% mutate(Basins = Basin)
   
   final_alpha_tib_basin <-alphas_tibble %>%  group_by(Basins) %>% summarise(max = max(alphas), min = min(alphas), mean = mean(alphas), sd = sd(alphas))%>% 
     left_join(., basin_names[,c("Basins", "Basin_ID", "region")], by = "Basins") %>% arrange(., Basin_ID) %>% .[,c(7,6,1,4,5)] %>% 
     mutate(mean = round(mean, 3), sd = round(sd, 3))
   
   colnames(final_alpha_tib_basin) <- c("Region","Basin_ID","Basin","Mean alpha","Mean alpha standard deviation")
  
    gt(final_alpha_tib_basin)   

    
    final_alpha_tib_region <-alphas_tibble %>%  group_by(regions) %>% summarise(mean = mean(alphas), sd = sd(alphas))%>%  
      mutate(mean = round(mean, 3), sd = round(sd, 3)) 
    
    colnames(final_alpha_tib_region) <- c("Region","Mean alpha","Mean alpha standard deviation")
    
    gt(final_alpha_tib_region)
    

    
    
    #Plots
    # Hydrographs_comparisson_plot
   
   cod_ex <- c(3049, 3045, 3193,  3164)
   names_ex <- c("Matallana (IMP)", "Priego Escabas (CRB)", "Torote (DTAL)", "Villasequilla Yepes (DTBJ)")
   ex_tib <- tibble(cod_ex, names_ex, cod = cod_ex)
   
   hyd_comp <- gauging_data_tagus %>% filter(., cod %in% cod_ex, date > as.Date("2017-10-01"), date < as.Date("2018-09-30")) %>% left_join(., ex_tib, "cod") %>% 
     mutate(names = factor(names_ex, levels = c("Matallana (IMP)", "Priego Escabas (CRB)", "Torote (DTAL)", "Villasequilla Yepes (DTBJ)"))) %>% ggplot(., aes(x = date, y = obs_flow))+
     geom_area(color = "darkblue", fill = "skyblue", linewidth = 0.6)+facet_grid(facets = "names", scales = "free")+ theme_bw()+ 
     ylab("Streamflow (m³/s)")+xlab("Date") +theme(text = element_text(size = 12, color = "black"), strip.text.y = element_text(face = "bold", size = 12), axis.title.x = element_blank() )
   
   #ggsave(plot = hyd_comp, filename = "D:/Trabajo/Papers/Paper_caracterizacion/soft_data_obtaining/figs/hydrographs.png",
          device = "png", width = 12, height = 12, dpi = 600)
   

   # Alphas calculation example PLOT
   
   # Matallana peaks selection

   # Basin 4, Peralejo, gauging code = 3001, region = CRB
   #Alphas obtained : 0.9856145, 0.9834357, 0.9760807
   peralejo <- gauging_data_tagus %>% filter(., cod == 3001) %>% filter(year(date) > 2009)
   
   peak_1_perjo <- peralejo[c(1030:1310),]
   peak_2_perjo <- peralejo[c(1440:1710),]
   peak_3_perjo <- peralejo[c(3360:3475),]
   
   # Peak selection: Three representative peaks
   peaks_plot <- ggplot(peralejo, aes( x = date, y = obs_flow))+geom_area(fill = "skyblue", color = "darkblue")+
     annotate("rect",xmin = as.Date(peak_1_perjo$date[1]), xmax = as.Date(peak_1_perjo$date[length(peak_1_perjo$date)]), 
              ymin = -2, ymax = 59,  color = "blue", linetype = 2, fill = "transparent")+
     annotate("text", x = as.Date(peak_1_perjo$date[141]), y = 62, label = "Peak 1", size = 4, color = "black")+ 
     annotate("rect",xmin = as.Date(peak_2_perjo$date[1]), xmax = as.Date(peak_2_perjo$date[length(peak_2_perjo$date)]), 
              ymin = -2, ymax = 46,  color = "blue", linetype = 2, fill = "transparent")+
     annotate("text", x = as.Date(peak_2_perjo$date[135]), y = 49, label = "Peak 2", size = 4, color = "black")+ 
     annotate("rect",xmin = as.Date(peak_3_perjo$date[1]), xmax = as.Date(peak_3_perjo$date[length(peak_3_perjo$date)]), 
              ymin = -2, ymax = 30,  color = "blue", linetype = 2, fill = "transparent", linewidth = 0.6)+
     annotate("text", x = as.Date(peak_3_perjo$date[58]), y = 33, label = "Peak 3", size = 4, color = "black")+ 
     theme_bw()+ theme(axis.title.x = element_blank())+ylab("Streamflow (m³/s)")+theme(text = element_text(size = 12, colour = "black"))+
     ggtitle("Subbasin 4 hydrograph and selected peaks")+ theme(title = element_text(size = 10))
   

   recession_curve_plot <- ggplot(peak_3_perjo, aes(x =date, y = obs_flow))+geom_area(fill = "skyblue", color = "darkblue")+
     annotate("rect",xmin = as.Date(peak_3_perjo$date[55]), xmax = as.Date(peak_3_perjo$date[90]), 
              ymin = 0.9, ymax = 10,  color = "red", linetype = 2, fill = "transparent", linewidth = 0.6)+
     scale_y_log10() + 
     theme_bw()+ylab("Streamflow (m³/s)")+xlab("Date")+theme(text = element_text(size = 12, colour = "black"))+
     annotate("text", x = as.Date(peak_3_perjo$date[72]), y = 15, label = "Recession curve adjusted", size = 4, color = "black")+
     ggtitle("Peak 3, y axis logarithmic scale")+ theme(title = element_text(size = 10))+
     theme(axis.title.x = element_blank())+ylab("Streamflow (m³/s)")

   adj_recession_curve_plot <- ggplot(peak_3_perjo[c(55:90),], aes(x =date, y = log(obs_flow)))+geom_area(fill = "skyblue", color = "darkblue")+
     theme_bw()+ylab("ln(streamflow)")+xlab("Date")+theme(text = element_text(size = 12, colour = "black"))+
     ggtitle("Recession curve linear adjustment (slope = -0.024, RB2 = 0.97)")+ theme(title = element_text(size = 10))+
     annotate("segment", x = as.Date(peak_3_perjo$date[55]), xend = as.Date(peak_3_perjo$date[90]), 
                                y = (1.7659-0.02421*1), yend = log(peak_3_perjo$obs_flow[55])-0.02421*36, color = "black",
              linetype = 2, linewidth = 0.7)
   
   peaks_selection_plot <- peaks_plot / (recession_curve_plot / adj_recession_curve_plot)
   
   #ggsave(plot = peaks_selection_plot, filename = "figs/peaks_selection_adjustment.png",
   device = "png", width = 12, height = 10, dpi = 600)
   #ggsave(plot = peaks_selection_plot, filename = "figs/peaks_selection.tiff",
   device = "tiff", width = 10, height = 6, dpi = 600)



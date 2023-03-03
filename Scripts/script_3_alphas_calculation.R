#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 3: Estimation of alpha values  ####

# Methodology: This script has been used to estimate the Groundwater recession constant, and in turn the alpha values.
# Therefore, it can be considered the first part of the groundwater index estimation, as the obtained alpha values 
# have been used as reference when using the digital filter.

#For each basin, three representative peaks between 2010-2018 have been selected, and a linear regression has been adjusted to 
# the baseflow recession curve in order to determine the groundwater recession constant (α). The initial point for the recession 
#curve has been selected when the direct runoff peak ends. The estimated slope of the regression is the α value. A minimum of 10 
# days of recession curve was selected as criteria, but a longer recession curve has been preferred despite having some irregularities than a shorter one.
# A minimum determination coefficient of 0.80 was also used as criteria. 

# The linear adjustment regression has been performed with the stats::lm function. With this function, two coefficients are obtained in this case:
# Intercept is the log() of the streamflow when time (t) = 0. 
# The other coefficient is the Slope of the lineal regression, which is the groundwater recession coefficient * t, being t the number of days after t0.


# As input data, the files with streamflow data and wih the subbasins data have been used. The daily precipitation has been used to select the peaks and the
# start and end of the recession curve. Therefore, it is necessary to run the "Run this first" section.
# An output csv file with the obtained alpha values and the determination coefficient has been generated (3_alpha_estimation)     

# Used libraries
library(readr)
library(tidyverse)
library(lubridate)
library(plotly)
library(gt)
library(patchwork)


#### RUN THIS FIRST ####

# Streamflow data
gauging_data_tagus <- read.csv("Used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% 
  tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
  .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))

# File with IDs, names, regions and areas of the basin, and gauging stations codes  
basins_file <- read.csv("Used_files/Created_csv/1_basins_file.csv") 


# Daily precipitation obtention
path <- "Used_files/Data/Climate_data_extracted/pcp_spain/" # Directory where the precipitation file for each point of the grid is located
init_date <- as.Date("1951-01-01")
end_date <- as.Date("2019-12-31")
dates <- seq(init_date, end_date, 1) # A sequence of dates for the entire period with data is created
period_dates <- tibble(dates) %>% filter(., year(dates) %in% 2010:2018)
pcp_grid_points <-  read.csv("Used_files/Created_csv/2_ids_stations_file.csv") %>% arrange(., Basin_ID)  # File with IDs, names, and location of the grid points, and basins data  

# Loop for calculating the daily precipitation of each basin 
pcpday_bas_list <- list()
for(i in 1:length(unique(pcp_grid_points$Basin_ID))){  # i --> Basin ID
  filt_st <- filter(pcp_grid_points, Basin_ID == i) # Basin data and precipitations points inside
  stations <- filt_st[,1] #Precipitations points inside each basin
  pcps_sts <- c()
  for(n in 1:length(stations)){   # n --> Weather stations identifier within each basin
    st_dat <- read_table(paste(path, stations[n], "_PCP.txt", sep = ""), skip = 1, col_names = F) %>% #read the precipitation file for each point
      mutate(date = ymd(dates), pcp = X1) %>% .[,c("date", "pcp")] 
    colnames(st_dat) <- c("date", "pcp")
    pcp_st <- filter(st_dat, year(date) %in% 2010:2018) %>% .[,"pcp"] # Filtering with the study period
    pcps_sts <- tibble(pcps_sts, pcp_st, .name_repair = "unique")    # Table with annual precipitation data for all the points of a basin 
  }
  pcp_bas <- pcps_sts %>% apply(., 1, mean) %>% cbind(period_dates) %>% .[,c(2,1)]
  colnames(pcp_bas) <- c("date", "precipitation")
  pcpday_bas_list[[i]] <- pcp_bas  # List with the daily precipitation in each basin
}



#### gwrec_cnst estimation for subbasins ####

# Basin 1, Navaluenga, gauging code = 3231, region = IMP
#Alphas obtained : 
navaluenga_flow <-  gauging_data_tagus %>% filter(., cod == 3231) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
navaluenga_pcp <-   tibble(pcpday_bas_list[[1]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[1]][[1]]))) # Precipitation data
navaluenga_all <- navaluenga_flow %>% left_join(navaluenga_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(navaluenga_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 660-835
# 1100-1330
# 2975-3120

peak_1_nav <- navaluenga_all[c(660:835),]
ggplotly(ggplot(peak_1_nav, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Q0 (maximum baseflow) is when x = 45, end of recession when x = 106
plot(peak_1_nav$obs_flow[45:106]~ peak_1_nav$date[45:106], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_nav$obs_flow[45:106])~ peak_1_nav$date[45:106], type = "l")
reg_pk1 <- lm(log(peak_1_nav$obs_flow[45:106])~ seq(1, length(peak_1_nav$date[45:106]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0201099, R2 = 0.9518

2.71828182846^-(0.0201099) # gwrec_cnst parameter
1-(0.0201099)

peak_2_nav <- navaluenga_all[c(1100:1330),]
ggplotly(ggplot(peak_2_nav, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Q0 (maximum baseflow) is when x = 111, end of recession when x = 200
plot(peak_2_nav$obs_flow[111:200]~ peak_2_nav$date[111:200], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_nav$obs_flow[111:200])~ peak_2_nav$date[111:200], type = "l")
reg_pk2 <- lm(log(peak_2_nav$obs_flow[111:200])~ seq(1, length(peak_2_nav$date[111:200]), 1))
summary(reg_pk2) # gwrec_cnst = -0.032120, R2 = 0.9668

2.71828182846^-(0.0321200) # gwrec_cnst parameter
1-(0.032120)

peak_3_nav <- navaluenga_all[c(2975:3120),]
ggplotly(ggplot(peak_3_nav, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Q0 (maximum baseflow) is when x = 63, end of recession when x = 132
plot(peak_3_nav$obs_flow[63:132]~ peak_3_nav$date[63:132], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_nav$obs_flow[63:132])~ peak_3_nav$date[63:132], type = "l")
reg_pk3 <- lm(log(peak_3_nav$obs_flow[63:132])~ seq(1, length(peak_3_nav$date[63:132]), 1))
summary(reg_pk3) # gwrec_cnst = -0.036558, R2 = 0.97

2.71828182846^-(0.019701) # gwrec_cnst parameter
1-(0.019701)


# Basin 2, Matallana, gauging code = 3049, region = IMP
#Alphas obtained : 
matallana_flow <-  gauging_data_tagus %>% filter(., cod == 3049) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
matallana_pcp <-   tibble(pcpday_bas_list[[2]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[2]][[1]]))) # Precipitation data
matallana_all <- matallana_flow %>% left_join(matallana_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(matallana_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1110-1370
# 1910-2130
# 2620-2890

peak_1_mtlln <- matallana_all[c(1110:1370),]
ggplotly(ggplot(peak_1_mtlln, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 197, end of the recession when x = 217
plot(peak_1_mtlln$obs_flow[197:217]~ peak_1_mtlln$date[197:217], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_mtlln$obs_flow[197:217])~ peak_1_mtlln$date[197:217], type = "l")
reg_pk1 <- lm(log(peak_1_mtlln$obs_flow[198:217])~seq(1, length(peak_1_mtlln$date[198:217]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0359, R2 = 0.99

2.71828182846^-(0.0359172)
1-(0.0359172)

peak_2_mtlln <- matallana_all[c(1910:2130),]
ggplotly(ggplot(peak_2_mtlln, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 150, end of the recession when x = 210
plot(peak_2_mtlln$obs_flow[150:210]~ peak_2_mtlln$date[150:210], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_mtlln$obs_flow[150:210])~ peak_2_mtlln$date[150:210], type = "l")
reg_pk2 <- lm(log(peak_2_mtlln$obs_flow[150:210])~seq(1, length(peak_2_mtlln$date[150:210]), 1))
summary(reg_pk2) # gwrec_cnst = -0.0528, R2 = 0.98

2.71828182846^-(0.0528312)
1-(0.05966)

peak_3_mtlln <- matallana_all[c(2620:2890),]
ggplotly(ggplot(peak_3_mtlln, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 210, end of the recession when x = 254
plot(peak_3_mtlln$obs_flow[210:254]~ peak_3_mtlln$date[210:254], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_mtlln$obs_flow[210:254])~ peak_3_mtlln$date[210:254], type = "l")
reg_pk3 <- lm(log(peak_3_mtlln$obs_flow[210:254])~seq(1, length(peak_3_mtlln$date[210:254]), 1))
summary(reg_pk3) # gwrec_cnst = -0.054527, R2 = 0.99

2.71828182846^-(0.04505 )
1-(0.04505 )


# Basin 3, Villarejo de Montalban, gauging code = 3211, region = IMP
#Alphas obtained : 
villarejo_flow <-  gauging_data_tagus %>% filter(., cod == 3211) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
villarejo_pcp <-   tibble(pcpday_bas_list[[3]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[3]][[1]]))) # Precipitation data
villarejo_all <- villarejo_flow %>% left_join(villarejo_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(villarejo_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1140-1260
# 2980-3050
# 1450-1585

peak_1_villj <- villarejo_all[c(1140:1260),]
ggplotly(ggplot(peak_1_villj, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 54, end of the recession when x = 73
plot(peak_1_villj$obs_flow[54:73]~ peak_1_villj$date[54:73], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_villj$obs_flow[54:73])~ peak_1_villj$date[54:73], type = "l")
reg_pk1 <- lm(log(peak_1_villj$obs_flow[54:73])~seq(1, length(peak_1_villj$date[54:73]), 1))
summary(reg_pk1) # gwrec_cnst = -0.04196, R2 = 0.97

2.71828182846^-(0.04196)
1-(0.04196)

peak_2_villj <- villarejo_all[c(2980:3050),]
ggplotly(ggplot(peak_2_villj, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 50, end of the recession when x = 70
plot(peak_2_villj$obs_flow[50:70]~ peak_2_villj$date[50:70], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_villj$obs_flow[50:70])~ peak_2_villj$date[50:70], type = "l")
reg_pk2 <- lm(log(peak_2_villj$obs_flow[50:70])~seq(1, length(peak_2_villj$date[50:70]), 1))
summary(reg_pk2) # gwrec_cnst = -0.081, R2 = 0.96

2.71828182846^-(0.081247)
1-(0.081247)

peak_3_villj <- villarejo_all[c(1450:1585),]
ggplotly(ggplot(peak_3_villj, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 58, end of the recession when x = 96
plot(peak_3_villj$obs_flow[58:96]~ peak_3_villj$date[58:96], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_villj$obs_flow[58:96])~ peak_3_villj$date[58:96], type = "l")
reg_pk3 <- lm(log(peak_3_villj$obs_flow[58:96])~seq(1, length(peak_3_villj$date[58:96]), 1))
summary(reg_pk3) # gwrec_cnst = -0.03238, R2 = 0.98

2.71828182846^-(0.0323825)
1-(-0.0323825)


#  Basin 4, Peralejo, gauging code = 3001, region = CRB
#Alphas obtained : 
peralejo_flow <-  gauging_data_tagus %>% filter(., cod == 3001) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
peralejo_pcp <-   tibble(pcpday_bas_list[[4]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[4]][[1]]))) # Precipitation data
peralejo_all <- peralejo_flow %>% left_join(peralejo_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(peralejo_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1050-1450
# 1440-1750
# 280-650

peak_1_perjo <- peralejo_all[c(1050:1450),]
ggplotly(ggplot(peak_1_perjo, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 200, end of the recession when x = 400
plot(peak_1_perjo$obs_flow[200:400]~ peak_1_perjo$date[200:400], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_perjo$obs_flow[200:400])~ peak_1_perjo$date[200:400], type = "l")
reg_pk1 <- lm(log(peak_1_perjo$obs_flow[200:400])~seq(1, length(peak_1_perjo$date[200:400]), 1))
summary(reg_pk1) # gwrec_cnst = -0.005, R2 = 0.94

2.71828182846^-(5.084e-03)
1-(5.084e-03)

peak_2_perjo <- peralejo_all[c(1440:1750),]
ggplotly(ggplot(peak_2_perjo, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 93, end of the recession when x = 280
plot(peak_2_perjo$obs_flow[93:280]~ peak_2_perjo$date[93:280], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_perjo$obs_flow[93:280])~ peak_2_perjo$date[93:280], type = "l")
reg_pk2 <- lm(log(peak_2_perjo$obs_flow[93:280])~seq(1, length(peak_2_perjo$date[93:280]), 1))
summary(reg_pk2) # gwrec_cnst = -0.006, R2 = 0.91

2.71828182846^-(0.0063086)
1-(0.0063086)

peak_3_perjo <- peralejo_all[c(280:650),]
ggplotly(ggplot(peak_3_perjo, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 256, end of the recession when x = 356
plot(peak_3_perjo$obs_flow[256:356]~ peak_3_perjo$date[256:356], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_perjo$obs_flow[256:356])~ peak_3_perjo$date[256:356], type = "l")
reg_pk3 <- lm(log(peak_3_perjo$obs_flow[256:356])~seq(1, length(peak_3_perjo$date[256:356]), 1))
summary(reg_pk3) # gwrec_cnst = -0.008, R2 = 0.98

2.71828182846^-(0.008252)
1-(0.008252)


#  Basin 5, Priego Escabas, gauging code = 3045, region = CRB
#Alphas obtained : 
priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
priegoes_pcp <-   tibble(pcpday_bas_list[[5]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[5]][[1]]))) # Precipitation data
priegoes_all <- priegoes_flow %>% left_join(priegoes_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(priegoes_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 980-1500
# 2160-2550
# 2970-3180

peak_1_pres <- priegoes_all[c(980:1500),]
ggplotly(ggplot(peak_1_pres, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 225, end of the recession when x = 350
plot(peak_1_pres$obs_flow[225:350]~ peak_1_pres$date[225:350], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_pres$obs_flow[225:350])~ peak_1_pres$date[225:350], type = "l")
reg_pk1 <- lm(log(peak_1_pres$obs_flow[225:350])~seq(1, length(peak_1_pres$date[225:350]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0083961 , R2 = 0.96

2.71828182846^-(0.0083961)
1-(0.0083961)

peak_2_pres <- priegoes_all[c(2160:2550),]
ggplotly(ggplot(peak_2_pres, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 192, end of the recession when x = 327
plot(peak_2_pres$obs_flow[192:327]~ peak_2_pres$date[192:327], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_pres$obs_flow[192:327])~ peak_2_pres$date[192:327], type = "l")
reg_pk2 <- lm(log(peak_2_pres$obs_flow[192:327])~seq(1, length(peak_2_pres$date[192:327]), 1))
summary(reg_pk2) # gwrec_cnst = -0.004 , R2 = 0.84

2.71828182846^-(0.0045749)
1-(0.0045749)

peak_3_pres <- priegoes_all[c(2970:3280),]
ggplotly(ggplot(peak_3_pres, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 82, end of the recession when x = 240
plot(peak_3_pres$obs_flow[82:240]~ peak_3_pres$date[82:240], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_pres$obs_flow[82:240])~ peak_3_pres$date[82:240], type = "l")
reg_pk3 <- lm(log(peak_3_pres$obs_flow[82:240])~seq(1, length(peak_3_pres$date[82:240]), 1))
summary(reg_pk3) # gwrec_cnst = -0.006 , R2 = 0.85

2.71828182846^-(0.0063358)
1-(0.0063358)


# Basin 6, Santa Maria del Val, gauging code = 3040, region = CRB
#Alphas obtained :

santamaria_flow <-  gauging_data_tagus %>% filter(., cod == 3040) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
santamaria_pcp <-   tibble(pcpday_bas_list[[6]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[6]][[1]]))) # Precipitation data
santamaria_all <- santamaria_flow %>% left_join(santamaria_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(santamaria_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 800-1200
# 1170-1520
# 2700-3000

peak_1_stama <- santamaria_all[c(800:1200),]
ggplotly(ggplot(peak_1_stama, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 175, end of the recession when x = 270
plot(peak_1_stama$obs_flow[175:270]~ peak_1_stama$date[175:270], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_stama$obs_flow[175:270])~ peak_1_stama$date[175:270], type = "l")
reg_pk1 <- lm(log(peak_1_stama$obs_flow[175:270])~seq(1, length(peak_1_stama$date[175:270]),1))
summary(reg_pk1) # gwrec_cnst = -0.011 , R2 = 0.94

2.71828182846^-(0.0109783 )
1-(0.0109783)

peak_2_stama <- santamaria_all[c(1170:1520),]
ggplotly(ggplot(peak_2_stama, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 90, end of the recession when x = 250
plot(peak_2_stama$obs_flow[90:250]~ peak_2_stama$date[90:250], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_stama$obs_flow[90:250])~ peak_2_stama$date[90:250], type = "l")
reg_pk2 <- lm(log(peak_2_stama$obs_flow[90:250])~seq(1, length(peak_2_stama$date[90:250]), 1))
summary(reg_pk2) # gwrec_cnst = -0.011 , R2 = 0.96

2.71828182846^-(0.011652)
1-(0.011652)

peak_3_stama <- santamaria_all[c(2700:3000),]
ggplotly(ggplot(peak_3_stama, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 75,  end of the recession when x = 200
plot(peak_3_stama$obs_flow[75:200]~ peak_3_stama$date[75:200], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_stama$obs_flow[75:200])~ peak_3_stama$date[75:200], type = "l")
reg_pk3 <- lm(log(peak_3_stama$obs_flow[75:200])~seq(1, length(peak_3_stama$date[75:200]), 1))
summary(reg_pk3) # gwrec_cnst = -0.017 , R2 = 0.96

2.71828182846^-(0.0170622)
1-(0.0170622)


# Basin 7, Jabalera, gauging code = 3249, region = DTAL
#Alphas obtained :

jabalera_flow <-  gauging_data_tagus %>% filter(., cod == 3249) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
jabalera_pcp <-   tibble(pcpday_bas_list[[7]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[7]][[1]]))) # Precipitation data
jabalera_all <- jabalera_flow %>% left_join(jabalera_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(jabalera_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+
           geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection (pcp scale reduced)


# Peak selection: Three representative peaks
# 1150-1290
# 2220-2400
# 2960-3160

peak_1_java <- jabalera_all[c(1150:1370),]
ggplotly(ggplot(peak_1_java, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 80, end of the recession when x = 144
plot(peak_1_java$obs_flow[80:144]~ peak_1_java$date[80:144], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_java$obs_flow[80:144])~ peak_1_java$date[80:144], type = "l")
reg_pk1 <- lm(log(peak_1_java$obs_flow[80:144])~seq(1, length(peak_1_java$date[80:144]), 1))
summary(reg_pk1) # gwrec_cnst = -0.02 , R2 = 0.96

2.71828182846^-(0.0228438)
1-(0.0228438)

peak_2_java <- jabalera_all[c(2220:2400),]
ggplotly(ggplot(peak_2_java, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 108, end of the recession when x = 144
plot(peak_2_java$obs_flow[108:148]~ peak_2_java$date[108:148], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_java$obs_flow[108:148])~ peak_2_java$date[108:148], type = "l")
reg_pk2 <- lm(log(peak_2_java$obs_flow[108:148])~seq(1, length(peak_2_java$date[108:148]), 1))
summary(reg_pk2) # gwrec_cnst = -0.052419 , R2 = 0.94

2.71828182846^-(0.052419)
1-(0.052419)

peak_3_java <- jabalera_all[c(2960:3160),]
ggplotly(ggplot(peak_3_java, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 128, end of the recession when x = 180
plot(peak_3_java$obs_flow[128:180]~ peak_3_java$date[128:180], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_java$obs_flow[128:180])~ peak_3_java$date[128:180], type = "l")
reg_pk3 <- lm(log(peak_3_java$obs_flow[128:180])~seq(1, length(peak_3_java$date[128:180]), 1))
summary(reg_pk3) # gwrec_cnst = -0.03 , R2 = 0.9

2.71828182846^-(0.032707)
1-(0.032707)


# Basin 8, Huete, gauging code = 3172, region = DTAL
#Alphas obtained :

huete_flow <-  gauging_data_tagus %>% filter(., cod == 3172) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
huete_pcp <-   tibble(pcpday_bas_list[[8]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[8]][[1]]))) # Precipitation data
huete_all <- huete_flow %>% left_join(huete_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(huete_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection


# Peak selection: Three representative peaks
# 30-260
# 1080-1330
# 2960-3170

peak_1_huet <- huete_all[c(30:260),]
ggplotly(ggplot(peak_1_huet, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 158, end of the recession when x = 230
plot(peak_1_huet$obs_flow[158:230]~ peak_1_huet$date[158:230], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_huet$obs_flow[158:230])~ peak_1_huet$date[158:230], type = "l")
reg_pk1 <- lm(log(peak_1_huet$obs_flow[158:230])~seq(1, length(peak_1_huet$date[158:230]), 1))
summary(reg_pk1) # gwrec_cnst = -0.017, R2 = 0.9

2.71828182846^-(0.0172077)
1-(0.0172077)

peak_2_huet <- huete_all[c(1080:1400),]
ggplotly(ggplot(peak_2_huet, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 195, end of the recession when x = 255
plot(peak_2_huet$obs_flow[195:255]~ peak_2_huet$date[195:255], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_huet$obs_flow[195:255])~ peak_2_huet$date[195:255], type = "l")
reg_pk2 <- lm(log(peak_2_huet$obs_flow[195:255])~seq(1, length(peak_2_huet$date[195:255]), 1))
summary(reg_pk2) # gwrec_cnst = -0.034137 , R2 = 0.91

2.71828182846^-(0.0227024)
1-(0.0227024)

peak_3_huet <- huete_all[c(2960:3170),]
ggplotly(ggplot(peak_3_huet, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection
# Q0 (maximum baseflow) is when x = 130, end of the recession when x = 190
plot(peak_3_huet$obs_flow[130:190]~ peak_3_huet$date[130:190], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_huet$obs_flow[130:190])~ peak_3_huet$date[130:190], type = "l")
reg_pk3 <- lm(log(peak_3_huet$obs_flow[130:190])~seq(1, length(peak_3_huet$date[130:190]), 1))
summary(reg_pk3) # gwrec_cnst = -0.02762 , R2 = 0.86

2.71828182846^-(0.02762)
1-(0.02762)



# Basin 9, Torote, gauging code = 3193, region = DTAL
#Alphas obtained :

torote_flow <-  gauging_data_tagus %>% filter(., cod == 3193) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
torote_pcp <-   tibble(pcpday_bas_list[[9]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[9]][[1]]))) # Precipitation data
torote_all <- torote_flow %>% left_join(torote_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(torote_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection


# Peak selection: Three representative peaks
# 1430-1620
# 1130-1290
# 2950-3100

peak_1_toro <- torote_all[c(1430:1620),]
ggplotly(ggplot(peak_1_toro, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 97, end of the recession when x = 170
plot(peak_1_toro$obs_flow[97:170]~ peak_1_toro$date[97:170], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_toro$obs_flow[97:170])~ peak_1_toro$date[97:170], type = "l")
reg_pk1 <- lm(log(peak_1_toro$obs_flow[97:170])~seq(1, length(peak_1_toro$date[97:170]), 1))
summary(reg_pk1) # gwrec_cnst = -0.02806, R2 = 0.88

2.71828182846^-(0.02806)
1-(0.02806)

peak_2_toro <- torote_all[c(1130:1290),]
ggplotly(ggplot(peak_2_toro, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 75, end of the recession when x = 110
plot(peak_2_toro$obs_flow[75:110]~ peak_2_toro$date[75:110], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_toro$obs_flow[75:110])~ peak_2_toro$date[75:110], type = "l")
reg_pk2 <- lm(log(peak_2_toro$obs_flow[75:110])~seq(1, length(peak_2_toro$date[75:110]), 1))
summary(reg_pk2) # gwrec_cnst = -0.040494 , R2 = 0.85

2.71828182846^-(0.040494)
1-(0.040494)

peak_3_toro <- torote_all[c(2950:3100),]
ggplotly(ggplot(peak_3_toro, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 82,  end of the recession when x = 111
plot(peak_3_toro$obs_flow[82:111]~ peak_3_toro$date[82:111], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_toro$obs_flow[82:111])~ peak_3_toro$date[82:111], type = "l")
reg_pk3 <- lm(log(peak_3_toro$obs_flow[82:111])~seq(1, length(peak_3_toro$date[82:111]), 1))
summary(reg_pk3) # gwrec_cnst = -0.048 , R2 = 0.93

2.71828182846^-(0.047851)
1-(0.047851)


# Basin 10, La Pueblanueva, gauging code = 3251, region = DTAL
#Alphas obtained :

pueblanueva_flow <-  gauging_data_tagus %>% filter(., cod == 3251) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
pueblanueva_pcp <-   tibble(pcpday_bas_list[[10]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[10]][[1]]))) # Precipitation data
pueblanueva_all <- pueblanueva_flow %>% left_join(pueblanueva_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(pueblanueva_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 350-540
# 1450-1590
# 2980-3060

peak_1_puebla <- pueblanueva_all[c(350:540),]
ggplotly(ggplot(peak_1_puebla, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 96,  end of the recession when x = 124
plot(peak_1_puebla$obs_flow[96:124]~ peak_1_puebla$date[96:124], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_puebla$obs_flow[96:124])~ peak_1_puebla$date[96:124], type = "l")
reg_pk1 <- lm(log(peak_1_puebla$obs_flow[96:124])~seq(1, length(peak_1_puebla$date[96:124]), 1))
summary(reg_pk1) # gwrec_cnst = -0.034833, R2 = 0.85

2.71828182846^-(0.034833)
1-(0.034833)

peak_2_puebla <- pueblanueva_all[c(1450:1590),]
ggplotly(ggplot(peak_2_puebla, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 67, end of the recession when x = 99
plot(peak_2_puebla$obs_flow[67:99]~ peak_2_puebla$date[67:99], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_puebla$obs_flow[67:99])~ peak_2_puebla$date[67:99], type = "l")
reg_pk2 <- lm(log(peak_2_puebla$obs_flow[67:99])~seq(1, length(peak_2_puebla$date[67:99]), 1))
summary(reg_pk2) # gwrec_cnst = -0.04129, R2 = 0.94

2.71828182846^-(0.04129)
1-(0.04129)

peak_3_puebla <- pueblanueva_all[c(2980:3060),]
ggplotly(ggplot(peak_3_puebla, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 52, end of the recession when x = 80 
plot(peak_3_puebla$obs_flow[52:80]~ peak_3_puebla$date[52:80], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_puebla$obs_flow[52:80])~ peak_3_puebla$date[52:80], type = "l")
reg_pk3 <- lm(log(peak_3_puebla$obs_flow[52:80])~seq(1, length(peak_3_puebla$date[52:80]), 1))
summary(reg_pk3) # gwrec_cnst = -0.083 , R2 = 0.99

2.71828182846^-(0.083127)
1-(0.083127)


# Basin 11, Ventosa, gauging code = 3030, region = DTBJ
#Alphas obtained :

ventosa_flow <-  gauging_data_tagus %>% filter(., cod == 3030) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
ventosa_pcp <-   tibble(pcpday_bas_list[[11]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[11]][[1]]))) # Precipitation data
ventosa_all <- ventosa_flow %>% left_join(ventosa_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(ventosa_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1100-1400
# 1470-1670
# 400-670

peak_1_vent <- ventosa_all[c(1100:1400),]
ggplotly(ggplot(peak_1_vent, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 158, end of the recession when x = 260 
plot(peak_1_vent$obs_flow[158:260]~ peak_1_vent$date[158:260], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_vent$obs_flow[158:260])~ peak_1_vent$date[158:260], type = "l")
reg_pk1 <- lm(log(peak_1_vent$obs_flow[158:260])~seq(1, length(peak_1_vent$date[158:260]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0079368, R2 = 0.86

2.71828182846^-(0.0079368)
1-(0.0079368 )

peak_2_vent <- ventosa_all[c(1470:1670),]
ggplotly(ggplot(peak_2_vent, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 59, end of the recession when x = 147 
plot(peak_2_vent$obs_flow[59:147]~ peak_2_vent$date[59:147], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_vent$obs_flow[59:147])~ peak_2_vent$date[59:147], type = "l")
reg_pk2 <- lm(log(peak_2_vent$obs_flow[59:147])~seq(1, length(peak_2_vent$date[59:147]), 1))
summary(reg_pk2) # gwrec_cnst = -0.0071089, R2 = 0.93

2.71828182846^-(0.0071089)
1-(0.0071089)

peak_3_vent <- ventosa_all[c(400:670),]
ggplotly(ggplot(peak_3_vent, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 126, end of the recession when x = 170 
plot(peak_3_vent$obs_flow[126:170]~ peak_3_vent$date[126:170], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_vent$obs_flow[126:170])~ peak_3_vent$date[126:170], type = "l")
reg_pk3 <- lm(log(peak_3_vent$obs_flow[126:170])~seq(1, length(peak_3_vent$date[126:170]), 1))
summary(reg_pk3) # gwrec_cnst = -0.0097384, R2 = 0.92

2.71828182846^-(0.0097384)
1-(0.0097384)

# Basin 12, La Peraleja, gauging code = 3173, region = DTBJ
#Alphas obtained :

peraleja_flow <-  gauging_data_tagus %>% filter(., cod == 3173) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
peraleja_pcp <-   tibble(pcpday_bas_list[[12]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[12]][[1]]))) # Precipitation data
peraleja_all <- peraleja_flow %>% left_join(peraleja_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(peraleja_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1140-1295
# 1470-1630
# 2990-3060

peak_1_perja <- peraleja_all[c(1140:1295),]
ggplotly(ggplot(peak_1_perja, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 87, end of the recession when x = 146
plot(peak_1_perja$obs_flow[87:146]~ peak_1_perja$date[87:146], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_perja$obs_flow[87:146])~ peak_1_perja$date[87:146], type = "l")
reg_pk1 <- lm(log(peak_1_perja$obs_flow[87:146])~seq(1, length(peak_1_perja$date[87:146]), 1))
summary(reg_pk1) # gwrec_cnst = -0.035189, R2 = 0.84

2.71828182846^-(0.035189)
1-(0.035189)

peak_2_perja <- peraleja_all[c(1470:1630),]
ggplotly(ggplot(peak_2_perja, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 90, end of the recession when x = 150
plot(peak_2_perja$obs_flow[90:150]~ peak_2_perja$date[90:150], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_perja$obs_flow[90:150])~ peak_2_perja$date[90:150], type = "l")
reg_pk2 <- lm(log(peak_2_perja$obs_flow[90:150])~seq(1, length(peak_2_perja$date[90:150]), 1))
summary(reg_pk2) # gwrec_cnst = -0.048539, R2 = 0.92

2.71828182846^-(0.048539)
1-(0.048539)

peak_3_perja <- peraleja_all[c(2990:3060),]
ggplotly(ggplot(peak_3_perja, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 43, end of the recession when x = 67
plot(peak_3_perja$obs_flow[43:67]~ peak_3_perja$date[43:67], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_perja$obs_flow[43:67])~ peak_3_perja$date[43:67], type = "l")
reg_pk3 <- lm(log(peak_3_perja$obs_flow[43:67])~seq(1, length(peak_3_perja$date[43:67]), 1))
summary(reg_pk3) # gwrec_cnst = -0.080972 , R2 = 0.9

2.71828182846^-(0.080972)
1-(0.080972)


# Basin 13, Villasequilla de Yepes, gauging code = 3164, region = DTBJ
#Alphas obtained :

villaseq_flow <-  gauging_data_tagus %>% filter(., cod == 3164) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
villaseq_pcp <-   tibble(pcpday_bas_list[[13]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[13]][[1]]))) # Precipitation data
villaseq_all <- villaseq_flow %>% left_join(villaseq_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(villaseq_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/20))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1310-1700
# 2128-2420
# 2975-3172

peak_1_villas <- villaseq_all[c(1310:1700),]
ggplotly(ggplot(peak_1_villas, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 253, end of the recession when x = 391
plot(peak_1_villas$obs_flow[253:391]~ peak_1_villas$date[253:391], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_villas$obs_flow[253:391])~ peak_1_villas$date[253:391], type = "l")
reg_pk1 <- lm(log(peak_1_villas$obs_flow[253:391])~seq(1, length(peak_1_villas$date[253:391]), 1))
summary(reg_pk1) # gwrec_cnst = -0.009295, R2 = 0.91

2.71828182846^-(0.009295)
1-(0.009295)

peak_2_villas <- villaseq_all[c(2128:2520),]
ggplotly(ggplot(peak_2_villas, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 225, end of the recession when x = 310
plot(peak_2_villas$obs_flow[225:310]~ peak_2_villas$date[225:310], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_villas$obs_flow[225:310])~ peak_2_villas$date[225:310], type = "l")
reg_pk2 <- lm(log(peak_2_villas$obs_flow[225:310])~seq(1, length(peak_2_villas$date[225:310]), 1))
summary(reg_pk2) # gwrec_cnst = -0.0131711, R2 = 0.85

2.71828182846^-(0.0131711)
1-(0.0131711)

peak_3_villas <- villaseq_all[c(2975:3172),]
ggplotly(ggplot(peak_3_villas, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 118, end of the recession when x = 194
plot(peak_3_villas$obs_flow[118:194]~ peak_3_villas$date[118:194], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_villas$obs_flow[118:194])~ peak_3_villas$date[118:194], type = "l")
reg_pk3 <- lm(log(peak_3_villas$obs_flow[118:194])~seq(1, length(peak_3_villas$date[118:194]), 1))
summary(reg_pk3) # gwrec_cnst = -0.0140171 , R2 = 0.87

2.71828182846^-(0.0140171)
1-(0.0140171)


# Basin 14, Valverde de los Arroyos, gauging code = 3165, region = Mix
#Alphas obtained :

valverde_flow <-  gauging_data_tagus %>% filter(., cod == 3165) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
valverde_pcp <-   tibble(pcpday_bas_list[[14]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[14]][[1]]))) # Precipitation data
valverde_all <- valverde_flow %>% left_join(valverde_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(valverde_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 270-400
# 1630-1900
# 470-770

peak_1_valv <- valverde_all[c(270:400),]
ggplotly(ggplot(peak_1_valv, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation)))
# Q0 (maximum baseflow) is when x = 60, end of the recession when x = 100
plot(peak_1_valv$obs_flow[60:100]~ peak_1_valv$date[60:100], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_valv$obs_flow[60:100])~ peak_1_valv$date[60:100], type = "l")
reg_pk1 <- lm(log(peak_1_valv$obs_flow[60:100])~seq(1, length(peak_1_valv$date[60:100]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0459391, R2 = 0.98

2.71828182846^-(0.0459391)
1-(0.0459391)

peak_2_valv <- valverde_all[c(1630:1900),]
ggplotly(ggplot(peak_2_valv, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation)))
# Q0 (maximum baseflow) is when x = 163, end of the recession when x = 200
plot(peak_2_valv$obs_flow[163:200]~ peak_2_valv$date[163:200], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_valv$obs_flow[163:200])~ peak_2_valv$date[163:200], type = "l")
reg_pk2 <- lm(log(peak_2_valv$obs_flow[163:200])~seq(1, length(peak_2_valv$date[163:200]), 1))
summary(reg_pk2) # gwrec_cnst = -0.075323, R2 = 0.99

2.71828182846^-(0.075323)
1-(0.075323)

peak_3_valv <- valverde_all[c(470:770),]
ggplotly(ggplot(peak_3_valv, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation)))
# Q0 (maximum baseflow) is when x = 239, end of the recession when x = 293
plot(peak_3_valv$obs_flow[239:293]~ peak_3_valv$date[239:293], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_valv$obs_flow[239:293])~ peak_3_valv$date[239:293], type = "l")
reg_pk3 <- lm(log(peak_3_valv$obs_flow[239:293])~seq(1, length(peak_3_valv$date[239:293]), 1))
summary(reg_pk3) # gwrec_cnst = -0.04720, R2 = 0.96

2.71828182846^-(0.04720)
1-(0.04720)


# Basin 15, Malpica, gauging code = 3212, region = Mix
#Alphas obtained :

malpica_flow <-  gauging_data_tagus %>% filter(., cod == 3212) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
malpica_pcp <-   tibble(pcpday_bas_list[[15]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[15]][[1]]))) # Precipitation data
malpica_all <- malpica_flow %>% left_join(malpica_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(malpica_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1135-1250
# 2970-3060
# 2180-2380

peak_1_malp <- malpica_all[c(1135:1250),]
ggplotly(ggplot(peak_1_malp, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation)))
# Q0 (maximum baseflow) is when x = 60, end of the recession when x = 78
plot(peak_1_malp$obs_flow[60:78]~ peak_1_malp$date[60:78], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_malp$obs_flow[60:78])~ peak_1_malp$date[60:78], type = "l")
reg_pk1 <- lm(log(peak_1_malp$obs_flow[60:78])~seq(1, length(peak_1_malp$date[60:78]), 1))
summary(reg_pk1) # gwrec_cnst = -0.061719, R2 = 0.95

2.71828182846^-(0.061719)
1-(0.061719)

peak_2_malp <- malpica_all[c(2970:3100),]
ggplotly(ggplot(peak_2_malp, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation)))
# Q0 (maximum baseflow) is when x = 66, end of the recession when x = 120
plot(peak_2_malp$obs_flow[66:120]~ peak_2_malp$date[66:120], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_malp$obs_flow[66:120])~ peak_2_malp$date[66:120], type = "l")
reg_pk2 <- lm(log(peak_2_malp$obs_flow[66:120])~seq(1, length(peak_2_malp$date[66:120]), 1))
summary(reg_pk2) # gwrec_cnst =-0.034274, R2 = 0.95

2.71828182846^-(0.034274)
1-(0.034274)

peak_3_malp <- malpica_all[c(2180:2380),]
ggplotly(ggplot(peak_3_malp, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation)))
# Q0 (maximum baseflow) is when x = 154, end of the recession when x = 178
plot(peak_3_malp$obs_flow[154:178]~ peak_3_malp$date[154:178], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_malp$obs_flow[154:178])~ peak_3_malp$date[154:178], type = "l")
reg_pk3 <- lm(log(peak_3_malp$obs_flow[154:178])~seq(1, length(peak_3_malp$date[154:178]), 1))
summary(reg_pk3) # gwrec_cnst =  -0.101859, R2 = 0.97

2.71828182846^-( 0.101859)
1-(0.101859)


# Basin 16, Taravilla, gauging code = 3268, region = Mix
#Alphas obtained :

taravilla_flow <-  gauging_data_tagus %>% filter(., cod == 3268) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
taravilla_pcp <-   tibble(pcpday_bas_list[[16]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[16]][[1]]))) # Precipitation data
taravilla_all <- taravilla_flow %>% left_join(taravilla_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(taravilla_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/10))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1450-1630
# 2970-3287
# 1060-1500

peak_1_tarav <- taravilla_all[c(1450:1630),]
ggplotly(ggplot(peak_1_tarav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 91, end of the recession when x = 165
plot(peak_1_tarav$obs_flow[91:165]~ peak_1_tarav$date[91:165], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_tarav$obs_flow[91:165])~ peak_1_tarav$date[91:165], type = "l")
reg_pk1 <- lm(log(peak_1_tarav$obs_flow[91:165])~seq(1, length(peak_1_tarav$date[91:165]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0141037, R2 = 0.93

2.71828182846^-(0.0141037)
1-(0.0141037)

peak_2_tarav <- taravilla_all[c(2970:3287),]
ggplotly(ggplot(peak_2_tarav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 66, end of the recession when x = 235
plot(peak_2_tarav$obs_flow[127:235]~ peak_2_tarav$date[127:235], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_tarav$obs_flow[127:235])~ peak_2_tarav$date[127:235], type = "l")
reg_pk2 <- lm(log(peak_2_tarav$obs_flow[127:235])~seq(1, length(peak_2_tarav$date[127:235]), 1))
summary(reg_pk2) # gwrec_cnst = -0.0081211, R2 = 0.96

2.71828182846^-(0.0081211)
1-(0.0081211)

peak_3_tarav <- taravilla_all[c(1060:1500),]
ggplotly(ggplot(peak_3_tarav, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 210, end of the recession when x = 280
plot(peak_3_tarav$obs_flow[210:280]~ peak_3_tarav$date[210:280], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_tarav$obs_flow[210:280])~ peak_3_tarav$date[210:280], type = "l")
reg_pk3 <- lm(log(peak_3_tarav$obs_flow[210:280])~seq(1, length(peak_3_tarav$date[210:280]), 1))
summary(reg_pk3) # gwrec_cnst = -0.012024, R2 = 0.85

2.71828182846^-(0.012024)
1-(0.012024)


# Basin 17, Romanones, gauging code = 3237, region = Mix
#Alphas obtained :

romanones_flow <-  gauging_data_tagus %>% filter(., cod == 3237) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
romanones_pcp <-   tibble(pcpday_bas_list[[17]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[17]][[1]]))) # Precipitation data
romanones_all <- romanones_flow %>% left_join(romanones_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(romanones_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/20))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 224-700
# 637-970
# 2970-3200

peak_1_roman <- romanones_all[c(224:700),]
ggplotly(ggplot(peak_1_roman, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 307, end of the recession when x = 411
plot(peak_1_roman$obs_flow[309:411]~ peak_1_roman$date[309:411], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_roman$obs_flow[309:411])~ peak_1_roman$date[309:411], type = "l")
reg_pk1 <- lm(log(peak_1_roman$obs_flow[309:411])~seq(1, length(peak_1_roman$date[309:411]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0059120, R2 = 0.93

2.71828182846^-(0.0059120)
1-(0.0059120)

peak_2_roman <- romanones_all[c(637:1000),]
ggplotly(ggplot(peak_2_roman, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/30)))
# Q0 (maximum baseflow) is when x = 229, end of the recession when x = 350
plot(peak_2_roman$obs_flow[229:350]~ peak_2_roman$date[229:350], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_roman$obs_flow[229:350])~ peak_2_roman$date[229:350], type = "l")
reg_pk2 <- lm(log(peak_2_roman$obs_flow[229:350])~seq(1, length(peak_2_roman$date[229:350]), 1))
summary(reg_pk2) # gwrec_cnst = -0.0099407, R2 = 0.88

2.71828182846^-(0.0099407)
1-(0.0099407)

peak_3_roman <- romanones_all[c(2970:3200),]
ggplotly(ggplot(peak_3_roman, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 122, end of the recession when x = 190
plot(peak_3_roman$obs_flow[122:190]~ peak_3_roman$date[122:190], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_roman$obs_flow[122:190])~ peak_3_roman$date[122:190], type = "l")
reg_pk3 <- lm(log(peak_3_roman$obs_flow[122:190])~seq(1, length(peak_3_roman$date[122:190]), 1))
summary(reg_pk3) # gwrec_cnst = -0.0079618, R2 = 0.94

2.71828182846^-(0.0079618)
1-(0.0079618)


# Basin 18, Priego Trabaque, gauging code = 3186, region = Mix
#Alphas obtained :

priegotra_flow <-  gauging_data_tagus %>% filter(., cod == 3186) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
priegotra_pcp <-   tibble(pcpday_bas_list[[18]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[18]][[1]]))) # Precipitation data
priegotra_all <- priegotra_flow %>% left_join(priegotra_pcp, "date") %>% .[,c(1,2,4,3,5)]

priegotra_all$obs_flow[3038] <- priegotra_all$obs_flow[3039] #Anomalous value

ggplotly(ggplot(priegotra_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/20))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1100-1450
# 1460-1770
# 30-260

peak_1_prietra <- priegotra_all[c(1100:1400),]
ggplotly(ggplot(peak_1_prietra, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 100, end of the recession when x = 230
plot(peak_1_prietra$obs_flow[100:230]~ peak_1_prietra$date[100:230], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_prietra$obs_flow[100:230])~ peak_1_prietra$date[100:230], type = "l")
reg_pk1 <- lm(log(peak_1_prietra$obs_flow[100:230])~seq(1, length(peak_1_prietra$date[100:230]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0144870, R2 = 0.95

2.71828182846^-(0.0144870)
1-(0.0144870)

peak_2_prietra <- priegotra_all[c(1460:1630),]
ggplotly(ggplot(peak_2_prietra, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/10)))
# Q0 (maximum baseflow) is when x = 69, end of the recession when x = 140
plot(peak_2_prietra$obs_flow[69:140]~ peak_2_prietra$date[69:140], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_prietra$obs_flow[69:140])~ peak_2_prietra$date[69:140], type = "l")
reg_pk2 <- lm(log(peak_2_prietra$obs_flow[69:140])~seq(1, length(peak_2_prietra$date[69:140]), 1))
summary(reg_pk2) # gwrec_cnst = -0.0119962, R2 = 0.8

2.71828182846^-(0.0119962)
1-(0.0119962)

peak_3_prietra <- priegotra_all[c(30:260),]
ggplotly(ggplot(peak_3_prietra, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/20)))
# Q0 (maximum baseflow) is when x = 159, end of the recession when x = 225
plot(peak_3_prietra$obs_flow[159:225]~ peak_3_prietra$date[159:225], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_prietra$obs_flow[159:225])~ peak_3_prietra$date[159:225], type = "l")
reg_pk3 <- lm(log(peak_3_prietra$obs_flow[159:225])~seq(1, length(peak_3_prietra$date[159:225]), 1))
summary(reg_pk3) # gwrec_cnst = -0.0167074 , R2 = 0.99

2.71828182846^-(0.0167074)
1-(0.0167074)


# Basin 19, Bujaralo, gauging code = 3060, region = Mix
#Alphas obtained :

bujaralo_flow <-  gauging_data_tagus %>% filter(., cod == 3060) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
bujaralo_pcp <-   tibble(pcpday_bas_list[[19]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[19]][[1]]))) # Precipitation data
bujaralo_all <- bujaralo_flow %>% left_join(bujaralo_pcp, "date") %>% .[,c(1,2,4,3,5)]

ggplotly(ggplot(bujaralo_all, aes(x = seq(1, length(date), 1)))+geom_line(aes(y = obs_flow))+geom_col(aes(y = precipitation/5))) # Check streamflow data and peak selection

# Peak selection: Three representative peaks
# 1150-1370
# 2210-2500
# 2970-3200

peak_1_bujar <- bujaralo_all[c(1150:1370),]
ggplotly(ggplot(peak_1_bujar, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/5)))
# Q0 (maximum baseflow) is when x = 124, end of the recession when x = 160
plot(peak_1_bujar$obs_flow[124:160]~ peak_1_bujar$date[124:160], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_1_bujar$obs_flow[124:160])~ peak_1_bujar$date[124:160], type = "l")
reg_pk1 <- lm(log(peak_1_bujar$obs_flow[124:160])~seq(1, length(peak_1_bujar$date[124:160]), 1))
summary(reg_pk1) # gwrec_cnst = -0.0126649, R2 = 0.85

2.71828182846^-(0.0126649 )
1-(0.0126649 )

peak_2_bujar <- bujaralo_all[c(2210:2500),]
ggplotly(ggplot(peak_2_bujar, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation/5)))
# Q0 (maximum baseflow) is when x = 134, end of the recession when x = 212
plot(peak_2_bujar$obs_flow[146:239]~ peak_2_bujar$date[146:239], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_2_bujar$obs_flow[146:239])~ peak_2_bujar$date[146:239], type = "l")
reg_pk2 <- lm(log(peak_2_bujar$obs_flow[146:239])~seq(1, length(peak_2_bujar$date[146:239]), 1))
summary(reg_pk2) # gwrec_cnst = -0.0116980  , R2 = 0.96

2.71828182846^-(0.0116980)
1-(0.0116980)

peak_3_bujar <- bujaralo_all[c(2970:3200),]
ggplotly(ggplot(peak_3_bujar, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()+geom_col(aes(y = precipitation)))
# Q0 (maximum baseflow) is when x = 63
plot(peak_3_bujar$obs_flow[63:90]~ peak_3_bujar$date[63:90], type = "l")
#Linear adjustment for estimating alpha
plot(log(peak_3_bujar$obs_flow[63:90])~ peak_3_bujar$date[63:90], type = "l")
reg_pk3 <- lm(log(peak_3_bujar$obs_flow[129:200])~seq(1, length(peak_3_bujar$date[129:200]), 1))
summary(reg_pk3) # gwrec_cnst = -0.0109854  , R2 = 0.99

2.71828182846^-(0.0109854)
1-(0.0109854)




#### Results obtanied with the linear regression for each peak for the 19 subbasins.
# Number of days of the recession curve, recession constant value, alpha values and
# determination coefficients of the regression

ndays <-    
  c( 62,  90,  69,
     21,  61,  45,
     20,  21,  38,
     201, 188, 101,
     125, 135, 159,
     96, 161, 126,
     65,  41,  53,
     73,  61,  61,
     74,  35,  30,
     29,  33,  29,
     103,  89,  45,
     60,  61,  25,
     139,  86,  77,
     41,  38,  55,
     19,  55,  25,
     75, 109,  71,
     103, 122,  69,
     131,  72,  67,
     37,  94,  72)

gwrec_cnst <- 
  c(-0.0201, -0.0321, -0.0197,
    -0.0359, -0.0545, -0.0450,
    -0.0420, -0.0812, -0.0324,
    -0.0051, -0.0063, -0.0083,
    -0.0084, -0.0046, -0.0063,
    -0.0110, -0.0117, -0.0171,
    -0.0228, -0.0499, -0.0327,
    -0.0172, -0.0227, -0.0276,
    -0.0281, -0.0405, -0.0479,
    -0.0348, -0.0413, -0.0831,
    -0.0079, -0.0071, -0.0097,
    -0.0352, -0.0485, -0.0810,
    -0.0093, -0.0132, -0.0140,
    -0.0459, -0.0753, -0.0472,
    -0.0617, -0.0343, -0.1019,
    -0.0141, -0.0081, -0.0120,
    -0.0059, -0.0099, -0.0080,
    -0.0145, -0.0120, -0.0167,
    -0.0127, -0.0117, -0.0110)


alphas <- 
  c(0.9801, 0.9684, 0.9805,
    0.9647, 0.9485, 0.9559,
    0.9589, 0.9220, 0.9681,
    0.9949, 0.9937, 0.9918,
    0.9916, 0.9954, 0.9937,
    0.9891, 0.9884, 0.9831,
    0.9774, 0.9513, 0.9678,
    0.9829, 0.9776, 0.9728,
    0.9723, 0.9603, 0.9533,
    0.9658, 0.9596, 0.9202,
    0.9921, 0.9929, 0.9903,
    0.9654, 0.9526, 0.9222,
    0.9907, 0.9869, 0.9861,
    0.9551, 0.9274, 0.9539,
    0.9401, 0.9663, 0.9032,
    0.9860, 0.9919, 0.9880,
    0.9941, 0.9901, 0.9921,
    0.9856, 0.9881, 0.9834,
    0.9874, 0.9884, 0.9891)


det_coefs <- 
  c(0.9518, 0.9668, 0.8052,
    0.9914, 0.9828, 0.9917,
    0.9711, 0.9611, 0.9789,
    0.9406, 0.9125, 0.9786,
    0.9641, 0.8374, 0.8506,
    0.9413, 0.9644, 0.9577,
    0.9426, 0.9671, 0.8966,
    0.8991, 0.9052, 0.8612,
    0.8786, 0.8526, 0.9350,
    0.8540, 0.9408, 0.9887,
    0.8605, 0.9322, 0.9158,
    0.9654, 0.9234, 0.8973,
    0.9150, 0.8486, 0.8655,
    0.9836, 0.9870, 0.9627,
    0.9508, 0.9480, 0.9705,
    0.9333, 0.9575, 0.8538,
    0.9285, 0.8779, 0.9419,
    0.9530, 0.8021, 0.9224,
    0.8518, 0.9569, 0.9624)


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

alphas_tibble <- tibble(Basins, Basin_IDs, regions, ndays, det_coefs, gwrec_cnst, alphas) 
alphas_tibble$regions <- factor(alphas_tibble$regions, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX"))


# Saving the alpha values in a csv file
alphas_tibble %>% write.csv(., "Used_files/Created_csv/3_alpha_estimation.csv", quote = F, row.names = F)


alphas_tibble <- read.csv("Used_files/Created_csv/3_alpha_estimation.csv") %>% 
  mutate(regions = factor(.$regions, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX")))


tab_bas <- alphas_tibble %>% group_by(Basins, regions) %>% summarise(ndays = round(mean(ndays),0), dcoef_bas = round(mean(det_coefs), 3), 
                                                                     gwrec_ct = round(mean(gwrec_cnst),3),alpha_bas = round(mean(alphas), 3),
                                                                     alpha_min = min(alphas), alpha_max = max(alphas), alpha_range = paste(alpha_min , alpha_max, sep = "-")) %>% 
  inner_join(., basins_file[, c("Basin","Basin_ID")], c("Basins" = "Basin")) %>% 
  mutate(bas_id = paste(Basin_ID, Basins, sep = " ")) %>% arrange(., Basin_ID) %>% .[, c(11, 2:6,9)]

tabb_bas <- tab_bas

colnames(tabb_bas) <- c("ID and Subbasin", "Region", "Average recession curve duration (days)", 
                        "Mean determination coefficient", "Mean recession constant", "Mean alpha value", "Alpha value range")
gt(tabb_bas)


tab_reg <- tab_bas %>% group_by(regions) %>% 
  summarise(ndays = round(mean(ndays),0), dcoef_reg = round(mean(dcoef_bas), 3), 
            alpha_reg = round(mean(alpha_bas), 3), alpha_sd = round(sd(alpha_bas), 3)) %>% 
  mutate(regions = factor(regions, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX")))

colnames(tab_reg) <- c("Region", "Average recession curve duration (days)", "Mean determination coefficient",
                       "Mean alpha value",  "Alpha standard deviation")


gt(tab_reg)



# Graphical summary at region scale
alphas_tibble %>% ggplot(., aes(x = regions))+geom_violin(aes(y = alphas, fill = regions))
alphas_tibble %>% ggplot(., aes(x = regions))+geom_violin(aes(y = det_coefs, fill = regions))

# Statistical summary at region and basin scale
alphas_tibble %>% group_by(Basins) %>% summarise(max = max(alphas), min = min(alphas), mean = mean(alphas), sd = sd(alphas))
alphas_tibble %>% group_by(regions) %>% summarise(max = max(alphas), min = min(alphas), mean = mean(alphas), sd = sd(alphas))



#Plots
# Hydrographs_comparisson_plot

cod_ex <- c(3049, 3045, 3193,  3164)
names_ex <- c("Matallana (IMP)", "Priego Escabas (CRB)", "Torote (DTAL)", "Villasequilla Yepes (DTBJ)")
ex_tib <- tibble(cod_ex, names_ex, cod = cod_ex)

hyd_comp <- gauging_data_tagus %>% filter(., cod %in% cod_ex, date > as.Date("2017-10-01"), date < as.Date("2018-09-30")) %>% left_join(., ex_tib, "cod") %>% 
  mutate(names = factor(names_ex, levels = c("Matallana (IMP)", "Priego Escabas (CRB)", "Torote (DTAL)", "Villasequilla Yepes (DTBJ)"))) %>% ggplot(., aes(x = date, y = obs_flow))+
  geom_area(color = "darkblue", fill = "skyblue", linewidth = 0.6)+facet_grid(facets = "names", scales = "free")+ theme_bw()+ 
  ylab("Streamflow (m³/s)")+xlab("Date") +theme(text = element_text(size = 12, color = "black"), strip.text.y = element_text(face = "bold", size = 12), axis.title.x = element_blank() )

#ggsave(plot = hyd_comp, filename = "Figures/hydrographs.png",
device = "png", width = 12, height = 12, dpi = 600)


# gwrec_cnsts calculation example PLOT

# Matallana peaks selection

# Basin 4, Peralejo, gauging code = 3001, region = CRB
#Alphas obtained : 0.9949, 0.9937, 0.9918
peralejo <- gauging_data_tagus %>% filter(., cod == 3001) %>% filter(year(date) > 2009)

peak_1_perjo <- peralejo[c(1050:1310),]
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

#ggsave(plot = peaks_selection_plot, filename = "Figures/peaks_selection_adjustment.png",
device = "png", width = 12, height = 10, dpi = 600)
#ggsave(plot = peaks_selection_plot, filename = "Figures/peaks_selection.tiff",
device = "tiff", width = 10, height = 6, dpi = 600)

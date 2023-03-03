#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 4: Groundwater contribution estimation using a baseflow filter function ####  


   #Methodology: Baseflow indexes have been calculated for each basin using the Baseflow filter proposed by Eckhardt, K. (2005) (Ecuation 19) and writted in R by
   # Hendrik Rathjens. This filter have two parameters: BFIMax, which is the maximum expected value of baseflow contribution expected for one day, and alpha, 
   # which is a constant obtained from the groundwater recession constant (alpha = e^α). For each basin, a range of possible values of alohas was calculated in the 
   # Script 3 (alphas_calculation), and the BFIMax values have been established depending on the lithology and properties of each river and adjusting it for different 
   # peaks (note that these peaks were not always the same as the used for the alpha estimation). When appropriated values of alpha and BFIMax has been chosen, 
   # the groundwater contribution has been estimated. 
   # Daily precipitation graphs have been used to determine a realistic baseflow contribution rate
   
   # NOTE THAT
   # For running the script FIRSTLY is necessary to run the "RUN THIS FIRST" section. This section contains the alpha values obtained, the streamflow data,
   # the code used to create a list with the daily precipitation for each basin and the baseflow filter function used to separate the hydrograph components.
    
   # As input data, the files with streamflow data and wih the subbasins data have been used.
   # An output csv file with the obtained baseflow index and the values of the parameters has been generadted (4_groundwater_results)     

   # Used libraries
   library(readr)
   library(tidyverse)
   library(lubridate)
   library(plotly)
   library(gt)
   library(patchwork)  

   
   
   ####RUN THIS FIRST####
   
   # Baseflow Filter Function, written by Hendrik Rathjens. The two_param method has been used in all the cases
   # Baseflow filter equation source
   #https://doi.org/10.1002/hyp.5675
   #https://user.engineering.uiowa.edu/~flood/handouts/HO-L17-Baseflow-Separation.pdf
   # Recommended values of BFImax --> Perennial streams, porous aquifers (0.8), hard rock aquifers (0.25); Ephemeral streams porous aquifers (0.5)

   baseflow_sep <- function(df=NA, Q="Q",
                            alpha=0.98,
                            BFIma=0.5,
                            method="two_param")
   {
     Q <- df[colnames(df) == Q][,1]
     Q[is.na(Q)] <- -9999.9
     R <- as.vector(matrix(data=NA, nrow=length(Q), ncol=1))
     B <- as.vector(matrix(data=NA, nrow=length(Q), ncol=1))
     R[1] <- 0
     B[1] <- 0
     # Nathan McMahon (1990): Evaluation of Automated techniques for base flow and recession Analyses (WRR 26, 1465-1473)
     if(method=="one_param") {    
       for(i in 2:length(Q)){
         if(Q[i] != -9999.9){
           R[i] <- alpha * R[i-1] + (1+alpha)/2 * (Q[i]-Q[i-1])
           if(R[i] < 0)    {R[i] <- 0}
           if(R[i] > Q[i]) {R[i] <- Q[i]}
           B[i] <- Q[i]-R[i]
         } else {
           R[i] <- NA
           B[i] <- NA
         }
       }
     }
     #Eckhardt (2005): How to construct recursive digital filters for baseflow separation (Hydrological Processes, 19, 507-515)
     if(method=="two_param") {
       for(i in 2:length(Q)){
         if(Q[i] != -9999.9){
           B[i] <- ((1-BFIma)*alpha * B[i-1] + (1-alpha)*BFIma* Q[i]) /
             (1-alpha*BFIma)
           if(B[i] > Q[i]){B[i] <-Q[i]} 
           R[i] <- Q[i]-B[i]
         } else {
           R[i] <- NA
           B[i] <- NA
         }
       }
     }
     return(data.frame(B,R))
   }
   
   
   # File with IDs, names, regions and areas of the basin, and gauging stations codes  
   basins_file <- read.csv("Used_files/Created_csv/1_basins_file.csv") 
   
   # Gaugin stations data
   gauging_data_tagus <- read.csv("Used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% 
     tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
     .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))
   
   #Alphas calculated in previous script
   alphas_tibble <- read.csv("Used_files/Created_csv/3_alpha_estimation.csv")
   
   
   alphas_tibble$regions <- factor(alphas_tibble$regions, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX"))
   # Statistical summary at region and basin scale
   alphas_tibble %>% group_by(Basins) %>% summarise(max = max(alphas), min = min(alphas), mean = mean(alphas), sd = sd(alphas), ID = mean(Basin_IDs)) %>% arrange(., ID)
   alphas_tibble %>% group_by(regions) %>% summarise(max = max(alphas), min = min(alphas), mean = mean(alphas), sd = sd(alphas))
   
   
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
   
   

   #### Baseflow index estimation ####
    
   # Basin 1, Navaluenga, gauging code = 3231, region = IMP
   #Mean Alpha obtained : 0.976, Max 0.98, MIN 0.968
   
   navaluenga_flow <-  gauging_data_tagus %>% filter(., cod == 3231) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1)) # Flow data
   navaluenga_pcp <-   tibble(pcpday_bas_list[[1]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[1]][[1]]))) # Precipitation data
   navaluenga_all <- navaluenga_flow %>% left_join(navaluenga_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(navaluenga_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line()) # Check streamflow data and peak selection
   
   #Filter parameters
   alfa <- 0.975
   bfi_max <- 0.25
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param") # Run baseflow filter with the selected parameters
   n <- navaluenga_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R) # Tibble with all the data (date, pcp, flow, baseflow)
     
   # Peaks selected:
   # 660-765
   # 1100-1280
   # 2975-3120

   peak_1 <- n[c(650:820),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(1000:1350),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2975:3120),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R)) #Calculating the baseflow contribution
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
    ggplotly(bf_plot)
   
   
   
   # Basin 2, Matallana, gauging code = 3049, region = IMP. 
   #Mean Alpha obtained : 0.956, Max 0.965, Min 0.949
   
   matallana_flow <-  gauging_data_tagus %>% filter(., cod == 3049) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   matallana_pcp <-   tibble(pcpday_bas_list[[3]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[3]][[1]]))) 
   matallana_all <- matallana_flow %>% left_join(matallana_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(matallana_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.965
   bfi_max <- 0.2
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- matallana_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peaks selected:
   # 1100-1400
   # 1910-2130
   # 2620-2890
   
   peak_1 <- n[c(1100:1400),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   peak_2 <- n[c(1910:2130),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2620:2890),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   ggplotly(bf_plot)
   
   
   # Basin 3, Villarejo de Montalban, gauging code = 3211, region = IMP
   #Mean Alpha obtained : 0.95, Max 0.965, Min 0.922
   villarejo_flow <-  gauging_data_tagus %>% filter(., cod == 3211) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   villarejo_pcp <-   tibble(pcpday_bas_list[[4]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[4]][[1]]))) 
   villarejo_all <- villarejo_flow %>% left_join(villarejo_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(villarejo_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
  
   #Filter parameters
   alfa <- 0.951
   bfi_max <- 0.25
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- villarejo_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peaks selected:
   # 1140-1260
   # 2980-3050
   # 1450-1585
   
   peak_1 <- n[c(1140:1260),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(2980:3050),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(1450:1585),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))

   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   ggplotly(bf_plot)
   
   
   
   # Basin 4, Peralejo, gauging code = 3001, region = CRB
   #Mean Alpha obtained : 0.993, Max 0.995, Min 0.9918
   
   peralejo_flow <-  gauging_data_tagus %>% filter(., cod == 3001) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   peralejo_pcp <-   tibble(pcpday_bas_list[[5]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[5]][[1]]))) 
   peralejo_all <- peralejo_flow %>% left_join(peralejo_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(peralejo_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.993
   bfi_max <- 0.55
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- peralejo_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peaks selected:
   # 1030-1310
   # 1440-1710
   # 2170-2500
   
   peak_1 <- n[c(1030:1450),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   peak_2 <- n[c(1440:1710),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2170:2400),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
    
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   
   # Basin 5, Priego Escabas, gauging code = 3045, region = CRB
   #Mean Alpha obtained : 0.994, Max 0.9954,  Min 0.9916
   
   priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   priegoes_pcp <-   tibble(pcpday_bas_list[[6]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[6]][[1]]))) 
   priegoes_all <- priegoes_flow %>% left_join(priegoes_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(priegoes_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.995
   bfi_max <- 0.6
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- priegoes_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peaks selected:
   # 980-1360
   # 2160-2450
   # 2970-3180
   
   peak_1 <- n[c(980:1360),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(2160:2450),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2970:3180),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
    ggplotly(bf_plot)
   
   # Basin 6, Santa MarC-a del Val, gauging code = 3040, region = CRB
   #Mean Alpha obtained : 0.987, Max 0.989, Min 0.983
   santamaria_flow <-  gauging_data_tagus %>% filter(., cod == 3040) %>% filter(year(date) %in% 2011:2018) %>% mutate(day = seq(1, length(date), 1))
   santamaria_pcp <-   tibble(pcpday_bas_list[[7]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[7]][[1]]))) 
   santamaria_all <- santamaria_flow %>% left_join(santamaria_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(santamaria_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.995
   bfi_max <- 0.5
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- santamaria_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   # 660-1000
   # 1080-1300
   # 2600-2820
   
   peak_1 <- n[c(660:1000),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(1080:1300),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2600:2820),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 7, Jabalera, gauging code = 3249, region = DTAL
   #Mean Alpha obtained : 0.966, Max 0.9775, Min 0.95
   
   javalera_flow <-  gauging_data_tagus %>% filter(., cod == 3249) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   javalera_pcp <-   tibble(pcpday_bas_list[[8]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[8]][[1]]))) 
   javalera_all <- javalera_flow %>% left_join(javalera_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(javalera_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.987
   bfi_max <- 0.4
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- javalera_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   # Peak selection
   # 1150-1290
   # 2220-2400
   # 2960-3160
   
   peak_1 <- n[c(1150:1290),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(2220:2400),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2960:3160),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 8, Huete, gauging code = 3172, region = DTAL
   #Mean Alpha obtained : 0.978, Max 0.983, Min 973
   
   huete_flow <-  gauging_data_tagus %>% filter(., cod == 3172) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   huete_pcp <-   tibble(pcpday_bas_list[[9]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[9]][[1]]))) 
   huete_all <- huete_flow %>% left_join(huete_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(huete_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.992
   bfi_max <- 0.4
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- huete_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 250-600
   # 1080-1330
   # 2970-3170
   
   peak_1 <- n[c(250:600),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(1080:1330),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2970:3170),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 9, Torote, gauging code = 3193, region = DTAL
   #Mean Alpha obtained : 0.962, Max 0.972, Min 953
   
   torote_flow <-  gauging_data_tagus %>% filter(., cod == 3193) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   torote_pcp <-   tibble(pcpday_bas_list[[10]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[10]][[1]]))) 
   torote_all <- torote_flow %>% left_join(torote_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(torote_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.978
   bfi_max <- 0.4
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- torote_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1430-1570
   # 1130-1230
   # 2950-3100
   
   peak_1 <- n[c(1430:1570),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(1130:1230),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
    (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2950:3100),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
 
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
    
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 10, La Pueblanueva, gauging code = 3251, region = DTAL
   #Mean Alpha obtained : 0.949, Max 0.966, Min 0.92
   
   pueblanueva_flow <-  gauging_data_tagus %>% filter(., cod == 3193) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   pueblanueva_pcp <-   tibble(pcpday_bas_list[[11]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[11]][[1]]))) 
   pueblanueva_all <- pueblanueva_flow %>% left_join(pueblanueva_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(pueblanueva_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.978
   bfi_max <- 0.35
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- pueblanueva_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 40-100
   # 1450-1590
   # 2980-3060
   
   peak_1 <- n[c(40:100),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   peak_2 <- n[c(1450:1590),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2980:3060),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 11, Ventosa, gauging code = 3030, region = DTBJ
   #Mean Alpha obtained : 0.992, Max 0.993, 0.99
   
   ventosa_flow <-  gauging_data_tagus %>% filter(., cod == 3030) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   ventosa_pcp <-   tibble(pcpday_bas_list[[19]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[19]][[1]]))) 
   ventosa_all <- ventosa_flow %>% left_join(ventosa_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(ventosa_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.998
   bfi_max <- 0.5
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- ventosa_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1110-1340
   # 1830-2000
   # 2970-3160
   
   peak_1 <- n[c(1110:1340),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(1830:2000),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2970:3160),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 12, La Peraleja, gauging code = 3173, region = DTBJ
   #Mean Alpha obtained : 0.947, Max 0.966, Min 0.922
   
   peraleja_flow <-  gauging_data_tagus %>% filter(., cod == 3173) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   peraleja_pcp <-   tibble(pcpday_bas_list[[13]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[13]][[1]]))) 
   peraleja_all <- peraleja_flow %>% left_join(peraleja_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(peraleja_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.99
   bfi_max <- 0.35
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- peraleja_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1140-1295
   # 1470-1630
   # 3000-3060
   
   peak_1 <- n[c(1140:1290),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   peak_2 <- n[c(1470:1630),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(3000:3060),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
 
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
    ggplotly(bf_plot)
 
   # Basin 13, Villasequilla de Yepes, gauging code = 3164, region = DTBJ
   #Mean Alpha obtained : 0.988, Max 0.991, Min 0.986
   
   villaseq_flow <-  gauging_data_tagus %>% filter(., cod == 3164) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   villaseq_pcp <-   tibble(pcpday_bas_list[[14]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[14]][[1]]))) 
   villaseq_all <- villaseq_flow %>% left_join(villaseq_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(villaseq_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.998
   bfi_max <- 0.3
   
   obs_data[132,] <- obs_data[131,] # Anomalous record
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- villaseq_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1310-1700
   # 2130-2420
   # 2975-3172
   
   
   peak_1 <- n[c(1310:1700),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(2130:2420),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2975:3170),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   
   # Basin 14, Valverde de los Arroyos, gauging code = 3165, region = Mix
   #Mean Alpha obtained : 0.945, Max 0.955, Min 0.927
   
   valverde_flow <-  gauging_data_tagus %>% filter(., cod == 3165) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   valverde_pcp <-   tibble(pcpday_bas_list[[15]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[15]][[1]]))) 
   valverde_all <- valverde_flow %>% left_join(valverde_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(valverde_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.98
   bfi_max <- 0.15
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- valverde_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 270-350
   # 1630-1850
   # 2340-2600
   
   peak_1 <- n[c(270:350),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   peak_2 <- n[c(1630:1850),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2340:2600),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))

   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 15, Malpica, gauging code = 3212, region = Mix
   #Mean Alpha obtained : 0.937, Max 0.966, Min 0.903
   
   malpica_flow <-  gauging_data_tagus %>% filter(., cod == 3212) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   malpica_pcp <-   tibble(pcpday_bas_list[[16]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[16]][[1]]))) 
   malpica_all <- malpica_flow %>% left_join(malpica_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(malpica_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.985
   bfi_max <- 0.25
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- malpica_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1130-1250
   # 2970-3060
   # 1440-1600
   
   peak_1 <- n[c(1130:1250),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(2970:3060),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(1440:1600),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 16, Taravillas, gauging code = 3268, region = Mix
   #Mean Alpha obtained : 0.989, Max 0.992, Min 0.986
   
   taravillas_flow <-  gauging_data_tagus %>% filter(., cod == 3268) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   taravillas_pcp <-   tibble(pcpday_bas_list[[17]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[17]][[1]]))) 
   taravillas_all <- taravillas_flow %>% left_join(taravillas_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(taravillas_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.995
   bfi_max <- 0.55
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- taravillas_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1450-1630
   # 2970-3170
   # 1730-2070
   
   peak_1 <- n[c(1450:1630),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(2970:3170),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(1730:2070),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   # Basin 17, Romanones, gauging code = 3237, region = Mix
   #Mean Alpha obtained : 0.992, Max 0.994, Min 0.99
   
   romanones_flow <-  gauging_data_tagus %>% filter(., cod == 3237) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   romanones_pcp <-   tibble(pcpday_bas_list[[18]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[18]][[1]]))) 
   romanones_all <- romanones_flow %>% left_join(romanones_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(romanones_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.998
   bfi_max <- 0.35
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- romanones_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 224-630
   # 630-960
   # 2430-2800
   
   peak_1 <- n[c(27:240),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(630:960),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2430:2800),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   
   ggplotly(bf_plot)
   
   # Basin 18, Priego Trabaque, gauging code = 3186, region = MIX
   #Mean Alpha obtained : 0.986, Max 0.988, Min 0.983
   
   priegotra_flow <-  gauging_data_tagus %>% filter(., cod == 3186) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   priegotra_pcp <-   tibble(pcpday_bas_list[[12]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[12]][[1]]))) 
   
   priegotra_flow[3038,] <- priegotra_flow[3037,] # Anomalous record
   
   priegotra_all <- priegotra_flow %>% left_join(priegotra_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(priegotra_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.99
   bfi_max <- 0.4
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- priegotra_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1100-1280
   # 1460-1630
   # 28-270
   
   peak_1 <- n[c(1100:1280),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(1460:1630),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(28:270),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)
   
   
   # Basin 19, Bujaralo, gauging code = 3060, region = Mix
   #Mean Alpha obtained : 0.988, Max 0.989, Min 0.987
   
   bujaralo_flow <-  gauging_data_tagus %>% filter(., cod == 3060) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   bujaralo_pcp <-   tibble(pcpday_bas_list[[19]]) %>% mutate(day = seq(1, length(pcpday_bas_list[[19]][[1]])))
   bujaralo_all <- bujaralo_flow %>% left_join(bujaralo_pcp, "date") %>% .[,c(1,2,4,3,5)]
   
   obs_data <- as.data.frame(bujaralo_all)
   ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())
   
   #Filter parameters
   alfa <- 0.995
   bfi_max <- 0.45
   
   bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param")
   n <- bujaralo_all %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
   
   # Peak selection
   # 1440-1690
   # 2100-2500
   # 2970-3160
   
   peak_1 <- n[c(1440:1690),]
   bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_2 <- n[c(2100:2500),]
   bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
   
   peak_3 <- n[c(2970:3160),]
   bf_plot_3 <- ggplot(peak_3, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot3 <- ggplot(peak_3, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot3 / bf_plot_3 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

   sum(bfsep$B) / (sum(bfsep$B)+sum(bfsep$R))
   
   bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
     geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
     ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
   pcp_plot <- ggplot(n, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
     xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
   (pcp_plot / bf_plot )+plot_layout(widths = c(2, 2), heights = c(3, 5))
   ggplotly(bf_plot)

  
   #### RESULTS ####
   
   # Values of the Baseflow (groundwater) rates obtained with the code above and the used parameters values
    

    alpha_used <- c(0.975,0.965,0.951,
                    0.993,0.995,0.995,
                    0.987,0.992,0.978,0.978,
                    0.998,0.99,0.998,
                    0.98,0.985,0.995,0.998,0.99,0.995)

    bfimax_used <- c(0.25, 0.2, 0.25,
                     0.55, 0.6, 0.5,
                     0.4, 0.4, 0.4, 0.35,
                     0.5, 0.35, 0.3, 
                     0.15, 0.25, 0.55, 0.35, 0.4, 0.45)
      
    bf_rates <- c(   0.24, 0.19, 0.24, 
                     0.5, 0.54, 0.35,
                     0.34, 0.36, 0.34, 0.3,
                     0.43, 0.25, 0.23, 
                     0.14, 0.19, 0.48, 0.29, 0.36, 0.42)
    
    # Saving the obtained values in a csv file
    gw_tibble <-  tibble(basins_file[,c("Basin","Basin_ID","region")],alpha = alpha_used, BFImax = bfimax_used, BF_Rate = bf_rates) 
     write.csv(gw_tibble, "Used_files/Created_csv/4_groundwater_results.csv", quote = F, row.names = F)
  
    
    
     
   #Final Table with all the baseflow data at basin level
     gw_basins_tibble <- alphas_tibble %>% group_by(Basins) %>% summarise(alpha_mean = mean(alphas), alpha_sd = sd(alphas), ID = mean(Basin_IDs)) %>% 
     arrange(., ID) %>% mutate(bf_rate = gw_tibble$BF_Rate)  %>% mutate(alpha_mean = round(alpha_mean, 3), alpha_sd = round(alpha_sd, 3), baseflow_rt = bf_rates) %>% 
     cbind(region = basins_file$region) %>%  .[,c("ID", "Basins", "alpha_mean", "alpha_sd", "baseflow_rt", "region")]
     gw_basins_tibble$region <- factor(gw_tibble$region, levels = c("IMP", "CRB", "DTAL", "DTBJ", "MIX"))
     

   #Final Table with all the baseflow data at region level
   alpha_regions <-  alphas_tibble %>% group_by(regions) %>% summarise( alpha_mean = mean(alphas), alpha_sd = sd(alphas))
   gw_basins_tibble %>% group_by(region) %>% summarise(BF_index = mean(baseflow_rt)) %>% cbind(alpha_regions) %>% .[,c(1,4,5,2)]
   

   
   
   #### Plots #### 
   
   # Parameters effect assessment
   
   priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018) %>% mutate(day = seq(1, length(date), 1))
   obs_data <- as.data.frame(priegoes_flow)
   
   # Alpha effect
   bfsep <- c()
   bf <- c()
   bfs <- c()
   alpha <- c()
   alphas <- c()
   id <- c()
   ids <- c()
   
   np <- 30 # Number of simulations
   alpha_min <- 0.9 # Minimum alpha value
   alpha_max <- 0.995 # Maximum alpha value
   BFI_MAX <-0.6 # BFIMax value
   
   alpha_var <- (alpha_max-alpha_min)/np # Alpha value variation
   
   for(i in 1:np){ # Running the filter 30 times changing alpha value
     bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alpha_min+(i*alpha_var), BFIma = BFI_MAX, method = "two_param")
     bf <- bfsep$B
     bf[1] <- bf[2]
     bfs <- c(bfs, bf)
     alpha <-  rep(alpha_min+(i*alpha_var), length(bf))
     alphas <- c(alphas, alpha)
     id <- rep(paste("run_",i), length(bf)) 
     ids <- c(ids, id)
   }
   
   baseflows_alpha_chg <- tibble(bfs, alphas, ids, obs = rep(obs_data$obs_flow,np), days = rep(1:length(bf), np)) 
   
   asss_alpha <- ggplot(baseflows_alpha_chg)+ geom_line(aes(x = days, y = bfs, group = ids, colour = alphas), size = 1)+ # Plot of the simulations
     geom_line(aes(x= days, y = obs),  colour = "black", size = 1.1)+ 
     scale_colour_gradient(
       high = "#132B43",
       low = "#E0FFFF",
       space = "Lab",
       na.value = "red",
       guide = "colourbar",
       aesthetics = "colour")+
     xlim(c(1140,1220))+ylim(c(0,45))+xlab("Time (days)")+ylab("Streamflow (m³/s)")+labs(colour = "Alpha value")+
     ggtitle("Alpha parameter effect (BFIMax = 0.6)")+theme_bw()+theme(text = element_text(size = 18), axis.text = element_text(size = 16, color = "black"))
   
    alpha_plot <- asss_alpha
    
   # BFImax effect
   bfsep <- c()
   bfs <- c()
   bfis <- c()
   alpha <- c()
   alphas <- c()
   id <- c()
   ids <- c()
   
   np <- 30  # Number of simulations
   bfi_min <- 0.1 # Minimum BFIMax value
   bfi_max <- 0.8 # Maximum BFIMax value
   
   bfi_var <-(bfi_max - bfi_min)/np  # BFIMax value variation
   
   alpha_used <-  0.985 # alpha value
   
   for(i in 1:np){ # Running the filter 30 times changing alpha value
     bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alpha_used, BFIma = bfi_min+(i*bfi_var), method = "two_param")
     bf <- bfsep$B
     bf[1] <- bf[2]
     bfs <- c(bfs, bf)
     bfi_used <-  rep(bfi_min+(i*bfi_var), length(bf))
     bfis <- c(bfis, bfi_used)
     id <- rep(paste("run_",i), length(bf)) 
     ids <- c(ids, id)
   }
   
   baseflows_bfi_chg <- tibble(bfs, bfis, ids, obs = rep(obs_data$obs_flow,np), days = rep(1:length(bf), np)) 
   
   asss_bfimax <- ggplot(baseflows_bfi_chg)+ geom_line(aes(x = days, y = bfs, group = ids, colour = bfis), size = 1)+ # Plot of the simulations
     geom_line(aes(x= days, y = obs),  colour = "black", size = 1.1)+ 
     scale_colour_gradient(
       high = "#132B43",
       low = "#E0FFFF",
       space = "Lab",
       na.value = "red",
       guide = "colourbar",
       aesthetics = "colour")+
     xlim(c(1140,1220))+ylim(c(0,45))+xlab("Time (days)")+ylab("Streamflow (m³/s)")+labs(colour = "BFIMax value")+
     ggtitle("BFIMax parameter effect (alpha = 0.985)")+theme_bw()+theme(text = element_text(size = 18), axis.text = element_text(size = 16, color = "black"))
   
   bfi_plot <- asss_bfimax

   
  library(patchwork)
  pars_effect_plot <-  bfi_plot + alpha_plot # Plot of the two parameters effect
  
  #ggsave(plot = pars_effect_plot, filename = "Figures/parameters_effect_plot.tiff",
         device = "png", width = 18, height = 12, dpi = 600)
  
  
  
  # Baseflow filter example plot  
  
  # Example of baseflow filter application to Subbasin 4
  # Mean Alpha obtained : 0.982, Max 0.986, Min 0.976
  peralejo_flow <-  gauging_data_tagus %>% filter(., cod == 3001) %>% 
    filter(year(date) %in% 2010:2018) %>% 
    mutate(day = seq(1, length(date), 1))
  peralejo_pcp <- pcpday_bas_list[[4]] 
  # Joining pcp and streamflow data by date and creating data frame
  peralejo_all <- peralejo_flow %>% left_join(peralejo_pcp, "date") %>% 
    as.data.frame(.[,c(1,2,4,3,5)])
  #Filter parameters
  alfa <- 0.982
  bfi_max <- 0.5
  # Running the filter
  bfsep <- baseflow_sep(df = peralejo_all, 
                        Q = "obs_flow", 
                        alpha = alfa, 
                        BFIma =bfi_max, 
                        method = "two_param")
  bfsep_tibble <- peralejo_all %>% mutate(baseflow = bfsep$B, #baseflow
                                          runoff = bfsep$R)   #runoff
  # Calculating the streamflow contribution
  sum(bfsep_tibble$baseflow) / (sum(bfsep_tibble$baseflow)+sum(bfsep_tibble$runoff))
  # Checking the adjustment for one of the peaks
  peak_2 <- bfsep_tibble[c(1440:1710),]
  bf_plot_2 <- ggplot(peak_2, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
    geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
    ylab("Streamflow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")
  pcp_plot2 <- ggplot(peak_2, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill =     "skyblue")+  scale_y_reverse()+theme_bw()+
    xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
  plot_b4 <- (pcp_plot2 / bf_plot_2 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 
  
  #ggsave(plot = plot_b4, filename = "Figures/subb_4_bfsep.png",
         device = "png", width = 10, height = 8, dpi = 600)
  #ggsave(plot = plot_b4, filename = "Figures/subb_4_bfsep.tiff",
         device = "tiff", width = 10, height = 8, dpi = 600)
  



  # Example plot of time scale effect on baseflow separation
  
  # Example with Basin 6, Priego Escabas, gauging code = 3045, region = CRB
  #Mean Alpha obtained : 0.975, Max 0.99,  Min 0.965
  
  priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018) %>% 
    mutate(day = seq(1, length(date), 1))
  
  obs_data <- as.data.frame(priegoes_flow)
  
  #Filter parameters
  alfa <- 0.985
  bfi_max <- 0.6
  
  bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param") # Running the filter
  n <- priegoes_flow %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)
  
  # Peak example:  1070:1300
  peak_1 <- n[c(1070:1300),]
  bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Total streamflow"))+ 
    geom_area(aes(y = baseflow, fill = "Estimated baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
    ylab("Streamflow (m³/s)")+xlab("")+ theme(text = element_text(size = 14), legend.position = "NN")+labs(fill = "")
  
  # Entire period
  bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Total streamflow"))+ 
    geom_area(aes(y = baseflow, fill = "Estimated baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
    ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 14), legend.position = "NN")+labs(fill = "") + 
    annotate("rect",xmin = as.Date(peak_1$date[1]), xmax = as.Date(peak_1$date[length(peak_1$date)]), 
             ymin = -2, ymax = 45,  color = "blue", linetype = 2, fill = "transparent")
  
  # Plot comparing the peak regarding the entire time period
  plott <- bf_plot_1 /bf_plot 
  
  #ggsave(plot = plott, filename = "Figures/subb_4_bfsep.png",
         device = "png", width = 14, height = 10, dpi = 600)

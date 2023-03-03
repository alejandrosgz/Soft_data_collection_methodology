#### SCRIPTS FOR ANALYSING HYDROLOGICAL PROCESSES: Calculation of Runoff coefficient and Baseflow index ####
#### Script 5: Generating plots of the manuscript


# Libraries and datasets

library(patchwork)
library(tidyverse)
library(readr)
library(lubridate)



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
  
  #Eckhardt (2005): How to construct recursive digital filters for baseflow separation (Hydrological Processes, 19, 507-515)
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
  return(data.frame(B,R))
}

basins_file <- read.csv("Used_files/Created_csv/1_basins_file.csv") 

# File with gauging data
gauging_data_tagus <- read.csv("Used_files/Data/Gauging_data/afliq.csv", sep = ";") %>% 
  tibble(.,"cod" = indroea, "date" = fecha, "obs_flow" = caudal) %>% 
  .[, c("cod", "date", "obs_flow")] %>% mutate(date = dmy(date))



#### 1. Parameters effect assessment ####

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


pars_effect_plot <-  bfi_plot + alpha_plot # Plot of the two parameters effect


#### 2. Alphas calculation example PLOT ####

# Basin 5, Priego Escabas, gauging code = 3045, region = CRB
priegoes_flow <- gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) > 2009)
peak_1_pries <- priegoes_flow[c(1000:1440),]
peak_2_pries <- priegoes_flow[c(2160:2500),]
peak_3_pries <- priegoes_flow[c(2950:3180),]

# Peak selection: Three representative peaks
peaks_plot <- ggplot(peralejo, aes( x = date, y = obs_flow))+geom_area(fill = "skyblue", color = "darkblue")+
  annotate("rect",xmin = as.Date(peak_1_pries$date[1]), xmax = as.Date(peak_1_pries$date[length(peak_1_pries$date)]), 
           ymin = -2, ymax = 59,  color = "blue", linetype = 2, fill = "transparent")+
  annotate("text", x = as.Date(peak_1_pries$date[220]), y = 61, label = "Peak 1", size = 4, color = "black")+ 
  annotate("rect",xmin = as.Date(peak_2_pries$date[1]), xmax = as.Date(peak_2_pries$date[length(peak_2_pries$date)]), 
           ymin = -2, ymax = 70,  color = "blue", linetype = 2, fill = "transparent")+
  annotate("text", x = as.Date(peak_2_pries$date[170]), y = 72, label = "Peak 2", size = 4, color = "black")+ 
  annotate("rect",xmin = as.Date(peak_3_pries$date[1]), xmax = as.Date(peak_3_pries$date[length(peak_3_pries$date)]), 
           ymin = -2, ymax = 34,  color = "blue", linetype = 2, fill = "transparent", linewidth = 0.6)+
  annotate("text", x = as.Date(peak_3_pries$date[115]), y = 36, label = "Peak 3", size = 4, color = "black")+ 
  theme_bw()+ theme(axis.title.x = element_blank())+ylab("Streamflow (m³/s)")+theme(text = element_text(size = 11, colour = "black"))+
  ggtitle("Subbasin 5 hydrograph and selected peaks")+ theme(title = element_text(size = 11))

recession_curve_plot <- ggplot(peak_1_pries, aes(x =date, y = obs_flow))+geom_area(fill = "skyblue", color = "darkblue")+ 
  annotate("rect",xmin = as.Date(peak_1_pries$date[205]), xmax = as.Date(peak_1_pries$date[335]), 
           ymin = 0.9, ymax = 10,  color = "red", linetype = 2, fill = "transparent", linewidth = 0.6)+
  scale_y_log10() + 
  theme_bw()+ylab("Streamflow (m³/s)")+xlab("Date")+theme(text = element_text(size = 11, colour = "black"))+
  annotate("text", x = as.Date(peak_1_pries$date[270]), y = 15, label = "Recession curve adjusted", size = 4, color = "black")+
  ggtitle("Peak 1, y axis logarithmic scale")+ theme(title = element_text(size = 11))+
  theme(axis.title.x = element_blank())+ylab("Streamflow (m³/s)")

adj_recession_curve_plot <- ggplot(peak_1_pries[c(205:335),], aes(x =date, y = log(obs_flow)))+geom_area(fill = "skyblue", color = "darkblue")+
  theme_bw()+ylab("ln(streamflow)")+xlab("Date")+theme(text = element_text(size = 11, colour = "black"))+
  ggtitle("Recession curve linear adjustment (slope = -0.0084, R² = 0.96)")+ theme(title = element_text(size = 11))+
  annotate("segment", x = as.Date(peak_1_pries$date[205]), xend = as.Date(peak_1_pries$date[335]), 
           y = (1.308-0.0084*1), yend = log(peak_1_pries$obs_flow[205])-0.0084*130, color = "black",
           linetype = 2, linewidth = 0.7)+theme(title = element_text(size = 11))

peaks_selection_plot <- peaks_plot / (recession_curve_plot / adj_recession_curve_plot)

#ggsave(plot = peaks_selection_plot, filename = "Figures/peaks_selection_adjustment_b5.png", device = "png",dpi = 600, 
height = 10, width = 12 )

#### 3. Example plot baseflow filter ####

# Example of baseflow filter application to Subbasin 4
# Mean Alpha obtained : 0.982, Max 0.986, Min 0.976

# First: Daily precipitation obtention
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

# Basin 5, Priego Escabas, gauging code = 3045, region = CRB
#Mean Alpha obtained : 0.994, Max 0.9954,  Min 0.9916

priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018)
priegoes_pcp <-   tibble(pcpday_bas_list[[5]])  
priegoes_all <- priegoes_flow %>% left_join(priegoes_pcp, "date")

obs_data <- as.data.frame(priegoes_all)
ggplotly(ggplot(obs_data, aes(x = seq(1, length(date), 1), y = obs_flow))+geom_line())

#Filter parameters
alfa <- 0.994
bfi_max <- 0.6

# Running the filter
bfsep <- baseflow_sep(df = prieg_esc_all, 
                      Q = "obs_flow", 
                      alpha = alfa, 
                      BFIma =bfi_max, 
                      method = "two_param")
bfsep_tibble <- prieg_esc_all %>% mutate(baseflow = bfsep$B, #baseflow
                                         runoff = bfsep$R)   #runoff
# Calculating the streamflow contribution
sum(bfsep_tibble$baseflow) / (sum(bfsep_tibble$baseflow)+sum(bfsep_tibble$runoff))
# Checking the adjustment for one of the peaks
peak_1 <- bfsep_tibble[c(1030:1320),]
bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Observed"))+ 
  geom_area(aes(y = baseflow, fill = "Baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
  ylab("Streamflow (m³/s)")+xlab("")+ theme(text = element_text(size = 15))+labs(fill = "")+theme(legend.position = "no")
pcp_plot1 <- ggplot(peak_1, aes( x=date))+geom_bar(aes(y = precipitation), stat = "identity", position = "identity", fill = "skyblue")+  scale_y_reverse()+theme_bw()+
  xlab(label = "")+theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), text = element_text(size = 15))+ylab("Precipitation (mm)")
plot_b5 <- (pcp_plot1 / bf_plot_1 )+plot_layout(widths = c(2, 2), heights = c(3, 5)) 

#ggsave(plot = plot_b5, filename = "Figures/subb_5_bfsep_N.png", device = "png",dpi = 600, 
       height = 10, width = 12 )

#### 4. Time scale effect on baseflow filter plot ####

# Example with Basin 5, Priego Escabas, gauging code = 3045, region = CRB
#Mean Alpha obtained : 0.975, Max 0.99,  Min 0.965

priegoes_flow <-  gauging_data_tagus %>% filter(., cod == 3045) %>% filter(year(date) %in% 2010:2018) %>% 
  mutate(day = seq(1, length(date), 1))

obs_data <- as.data.frame(priegoes_flow)

#Filter parameters
alfa <- 0.994
bfi_max <- 0.6

bfsep <- baseflow_sep(df = obs_data, Q = "obs_flow", alpha = alfa, BFIma =bfi_max, method = "two_param") # Running the filter
n <- priegoes_flow %>% mutate(baseflow = bfsep$B, runoff = bfsep$R)

# Peak example:  1070:1300
peak_1 <- n[c(2952:3180),]
bf_plot_1 <- ggplot(peak_1, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Total streamflow"))+ 
  geom_area(aes(y = baseflow, fill = "Estimated baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
  ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 14), legend.position = "NN")+labs(fill = "")

# Entire period
bf_plot <- ggplot(n, aes(x = date))+ geom_area(aes(y = obs_flow, fill = "Total streamflow"))+ 
  geom_area(aes(y = baseflow, fill = "Estimated baseflow"))+ scale_fill_manual(values = c("dimgray", "cornflowerblue"))+theme_bw()+ 
  ylab("Flow (m³/s)")+xlab("")+ theme(text = element_text(size = 14), legend.position = "NN")+labs(fill = "") + 
  annotate("rect",xmin = as.Date(peak_1$date[1]), xmax = as.Date(peak_1$date[length(peak_1$date)]), 
           ymin = -1, ymax = 35,  color = "blue", linetype = 2, fill = "transparent")+ylim(c(-2,40))

# Plot comparing the peak regarding the entire time period
plott <-  bf_plot/ bf_plot_1


ggsave(plot = plott, filename = "Figures/bfi_filter_timescale.png", device = "png",dpi = 600, 
height = 10, width = 12 )

#### 5. Hydrographs comparisson plot #### 

cod_ex <- c(3049, 3045, 3193,  3164)
names_ex <- c("Matallana (IMP)", "Priego Escabas (CRB)", "Torote (DTH)", "Villasequilla Yepes (DTL)")
ex_tib <- tibble(cod_ex, names_ex, cod = cod_ex)

hyd_comp <- gauging_data_tagus %>% filter(., cod %in% cod_ex, date > as.Date("2017-10-01"), date < as.Date("2018-09-30")) %>% left_join(., ex_tib, "cod") %>% 
  mutate(names = factor(names_ex, levels = c("Matallana (IMP)", "Priego Escabas (CRB)", "Torote (DTH)", "Villasequilla Yepes (DTL)"))) %>% ggplot(., aes(x = date, y = obs_flow))+
  geom_area(color = "darkblue", fill = "skyblue", linewidth = 0.6)+facet_grid(facets = "names", scales = "free")+ theme_bw()+ 
  ylab("Streamflow (m³/s)")+xlab("Date") +theme(text = element_text(size = 12, color = "black"), strip.text.y = element_text(face = "bold", size = 12), axis.title.x = element_blank() )

ggsave(plot = hyd_comp, filename = "Figures/hydrographs_comp.png", device = "png",dpi = 600, 
       height = 12, width = 12 )



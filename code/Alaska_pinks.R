#Analysis by Rich Brenner 
#Updated 6 Sept. 2017

#load----
library(arm)
library(gdata)
library(tidyverse)
options(scipen=9999)

#data----
#Here I start with area-under-the-curve (AUC) data for individual streams, not
#yet adjusted for (divided by) stream life.
#The data were taken from the tab 'AUC_Pivot_Index' from the spreadsheet made by
#Steve Moffitt (recently retired ADF&G, Cordova). Spreadsheet titled:
#   'PWS_AUC_1962-2016_Subset for 2015 index streams.xls'
#These data have already been subsetted to only include the ~134 streams
#determined in 2015 to be used for future surveys. There were ~215 streams
#originally surveyed between 1963-2014, but Steve did not include all of these here.
#In the future: recreate district-wide adjusted-AUC estimates from the original raw  
#data to make this exercise reproducible! To do this:


#(1) From the complete raw aerial survey data containing all stream surveys, 
#    only the ~134 "current" streams would be retained for the new escapement goal.
#(2) Raw counts for individual streams flown >= 3 times would be integrated across
#    the season using trapezoidal integration. This = area-under-the-curve (AUC).
#(3) AUC would be adjusted for stream life. For chum salmon, an average stream life
#    of 12.6 days is used for the entire PWS. For pink salmon, stream-specific stream
#    life is used. AUC/stream life = adjusted-AUC
#(4) Adjusted-AUCs of individual streams are summed within each district. But, for 
#    pink salmon goals, districts 222 (Northern) and 229 (Unakwik) are summed together.
# R. Brenner 6 Sept. 2016
PWSPinkChum <- read_csv("data/PWS_Pink_Chum_EG.csv")

#adjust area-under-the-curve values for stream life
PWSPinkChum %>%  
  mutate(pink_adjstd = round(Sum_P_AUC/pink_strm_life, digits = 0),
         chum_adjstd = round(Sum_C_AUC/chum_strm_life, digits = 0)) -> PWSPinkChum
glimpse(PWSPinkChum)

#add distict names
PWSPinkChum %>%
 mutate(dist_name == "" )-> PWSPinkChum #add a variable called dist_name
      PWSPinkChum$dist_name[PWSPinkChum$district == "221"] <- "Eastern"
      PWSPinkChum$dist_name[PWSPinkChum$district == "222"] <- "Northern"
      PWSPinkChum$dist_name[PWSPinkChum$district == "223"] <- "Coghill"
      PWSPinkChum$dist_name[PWSPinkChum$district == "224"] <- "Northeastern"
      PWSPinkChum$dist_name[PWSPinkChum$district == "225"] <- "Eshamy"
      PWSPinkChum$dist_name[PWSPinkChum$district == "226"] <- "Southwestern"
      PWSPinkChum$dist_name[PWSPinkChum$district == "227"] <- "Monthague"
      PWSPinkChum$dist_name[PWSPinkChum$district == "228"] <- "Southeastern"
      PWSPinkChum$dist_name[PWSPinkChum$district == "229"] <- "Unakwik"




#For pink salmon analyses, combine districts 222 and 229 by renaming 229 as 222.  
  PWSPinkChum %>%   
    mutate(district = ifelse(district == 229, 222, district)) -> out #'out' is a temporary file for
                                                                  #PINK analyses only! Used
                                                                  #to separate odd vs even yrs.
                                                                  #below
  
  
  #Similar to above for pink salmon analyses, combine districts 222 and 229 by renaming Unakwik as Northern.  
  out %>%   
    mutate(dist_name = ifelse(dist_name == "Unakwik", "Northern", dist_name)) -> out #'out' is a temporary file for
  #PINK analyses only! Used
  #to separate odd vs even yrs.
  #below 
  
  
  #combine district and dist_names for graphing purposes (for facet labels)
out$dist_num_name <- interaction(out$district, out$dist_name, sep = "_")
  

   #For pink salmon analyses, combine districts 222 and 229 by renaming 229_Unakwik as 222_Northern.  
  out %>%   
    mutate(dist_num_name = ifelse(dist_num_name == "229_Unakwik", "222_Northern", dist_num_name)) -> out #'out' is a temporary file for
  #PINK analyses only! Used to separate odd vs even yrs.below

  
#Summarize even-year pink stocks by district. Note that 229 and 222 have already been combined
district_pink_even <- out %>% 
  group_by(year, broodline, district, dist_num_name) %>% 
  filter(broodline == "Even")%>%
  summarize(pink_dist = sum(pink_adjstd), #sums each year x broodline x district combination
            n = n() #counts the number of streams surveyed per district
  )  
glimpse(district_pink_even)


#Summarize odd-year pink stocks by district. Note that 229 and 222 have already been combined
district_pink_odd <- out %>% 
  group_by(year, broodline, district, dist_num_name) %>% 
  filter(broodline == "Odd")%>%
  summarize(pink_dist = sum(pink_adjstd),
            n = n() #counts the number of streams surveyed per district
)
glimpse(district_pink_odd)


######################################################################################
#calculate 20th and 60th percentiles for EVEN year pink salmon for each district
#for proposed spawning escapement goals.
#In general, years were generally excluded from the calculation of percentiles and % harvest 
#if area-under-the-curve estimates could not be obtained for more than 70% of the aerial index 
#streams for one or more districts. This meant excluding 1964-1980 and 2016 from the even-year 
#pink salmon goals, excluding 1963-1979 from the odd-year pink salmon goals, and excluding 
#1963-1980 and 2016 from the chum salmon escapement goals. Future analyses could use 
#Bayesian methods to reconstruct escapements during years that 
#were excluded from these estimates (I hope so).


probs <- c(0.20, 0.60)
even_pink_quantiles <- district_pink_even %>%
  filter(year >"1980", year != 2016) %>%  #Only includes years from 1982-present, but NOT 2016
  group_by(district) %>%  
  summarize(p = list(probs), q = list(quantile(pink_dist, probs))) %>%
  unnest()
glimpse(even_pink_quantiles)
even_pink_quantiles

#upper quantiles....use this for the figures
up_pink_even <- even_pink_quantiles %>%
  filter(p == .6)

low_pink_even <- even_pink_quantiles %>%
  filter(p== .2)


#######################################################################################
#calculate 25th and 75th percentiles for ODD year pink salmon for each district
#for proposed spawning escapement goals
probs <- c(0.25, 0.75)
odd_pink_quantiles <- district_pink_odd %>%
  filter(year >"1979") %>%  #Only includes years from 1981-present, but not 2016
  group_by(district) %>%  
  summarize(p = list(probs), q = list(quantile(pink_dist, probs))) %>%
  unnest()
glimpse(odd_pink_quantiles)
odd_pink_quantiles

#upper quantiles....use this for the figures
up_pink_odd <- odd_pink_quantiles %>%
  filter(p == .75)

low_pink_odd <- odd_pink_quantiles %>%
  filter(p== .25)

########################################################################################
#Summarize CHUM harvests across districts for each year, we can drop broodlines
#combine district and dist_names for graphing purposes (for facet labels)

chum_out <- PWSPinkChum  #creates a copy of the PWSPinkChum dataset to be used for chum analyses

#adds a field called "district_num_name"
chum_out$dist_num_name <- interaction(chum_out$district, chum_out$dist_name, sep = "_")

#summarizes the chum data, removes unwanted districts, and removes NAs
district_chum_sum <- chum_out %>%  
  filter(district != "225", district !="226", district !="227", #Only districts with escapement goals
         district != "229")%>%
  group_by(year, district, dist_num_name, dist_name) %>%
  summarize(chum_dist = sum(chum_adjstd, na.rm = TRUE),
            n = n()#counts the number of streams per district
) 
glimpse(district_chum_sum)


#######################################################################################
#calculate 20th and 60th percentiles for CHUM salmon for each chum district
#for proposed spawning escapement goals
probs <- c(0.20, 0.60)
chum_quantiles <- district_chum_sum %>%
  filter(year >"1979", year != "2016") %>%  #Only includes years from 1980-present, but not 2016
  group_by(district, dist_num_name) %>%  
  summarize(p = list(probs), q = list(quantile(chum_dist, probs))) %>%
  unnest()
glimpse(chum_quantiles)
chum_quantiles


#upper and lower quantiles for individual districts....for geom_hlines....maybe
#upper quantiles
upper_chum <- chum_quantiles %>%
  filter(p == .6)
upper_chum

lower_chum <- chum_quantiles %>%
  filter(p== .2)
lower_chum

######################################################################################
#FIGURES
#The following "coerces" objects to a labeller function to be used in the ggplot: 
chum_string <- as_labeller(c('221' = "Eastern (221)", 
                             '222' = "Northern (222)", 
                             '223' = "Coghill (223)", 
                             '224' = "Northwestern (224)",
                             '228' = "Southeastern (228)"))

#Figure of CHUM salmon escapements and proposed goals
chum_goals <- ggplot (data = district_chum_sum) +
  theme_bw() +
  geom_point(mapping = aes(x = year, y = chum_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  #geom_hline(data=upper_chum, aes(yintercept = q))+ #add upper bound for  escapement goal
  geom_hline(data=lower_chum, aes(yintercept = q))+ #add lower bound for escapement goal
  facet_wrap(~ district,  labeller = as_labeller((chum_string)), ncol=2, scales = "free_y")+
ggsave("figures/chum_goals.png", dpi=400, width=8, height=5, units='in')
chum_goals


#Figure of PINK salmon escapments and proposed goals
#First, use as_labeller to coerce the labels into a form that can be used by
#the ggplot "labeller" functin within the facet_wrap
pink_string <- as_labeller(c('221' = "Eastern (221)", 
                             '222' = "Northern (222)", 
                             '223' = "Coghill (223)", 
                             '224' = "Northwestern (224)",
                             '225' = "Eshamy (225)",
                             '226' = "Southwestern (226)",
                             '227' = "Montague (227)",
                             '228' = "Southeastern (228)"))

#Proposed goal for EVEN year pink salmon
p_even <- ggplot (data = district_pink_even) +
  theme_bw()+
  geom_point(mapping = aes(x = year, y = pink_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1964, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  geom_hline(data=up_pink_even, aes(yintercept = q))+ #add upper line for  escapement goal
  geom_hline(data=low_pink_even, aes(yintercept = q))+#add lower line for escapement goal
  facet_wrap(~ district, labeller = as_labeller((pink_string)), ncol = 2, scales = "free_y")
ggsave("figures/p_even.png", dpi=400, width=8, height=5, units='in')
p_even





#Proposed goal for ODD year pink salmon
p_odd <- ggplot(data = district_pink_odd) +
  theme_bw()+
  geom_point(mapping = aes(x = year, y = pink_dist)) +
  labs(x = "Years", y = "Escapement") +
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005) + #shade years w/ too few surveys
  geom_hline(data=up_pink_odd, aes(yintercept = q))+ #add upper line for  escapement goal
  geom_hline(data=low_pink_odd, aes(yintercept = q))+#add lower line for escapement goal
  facet_wrap(~ district, labeller = as_labeller((pink_string)),ncol = 2, scales = "free_y")
ggsave("figures/p_odd.png", dpi=400, width=8, height=5, units='in')
p_odd


##############################################################
#PRead Harvest date obtained from the 2017 PWS wild chum salmon forecast spreadsheet
c_harvest<- read_csv("data/Chum_Harvest_Rate.csv")
c_harvest

#Estimate maximum and minimum harvests for PWS chum salmon
#adjusted_AUC is from the ~214 area index streams, not the reduced stream count of 134 streams
#maximum harvest estimate from area-under-the-curve (AUC) adjusted for stream life.
#minimum harvest estimate expands adjusted_AUC escapement estimate to account for observer
#efficiency of 0.436 and the proportion of overall escapement represented by the ~214 streams
#surveyed (0.80). Estimates are from Fried et al., Fish and Shellfish Study I (EVOS)
#and based on pink salmon!
c_harvest %>% 
  mutate(max_harv = Harvest/(Harvest+adjusted_AUC)*100) %>%
  mutate(min_harv = Harvest/((adjusted_AUC/0.436/.80)+Harvest)*100) %>%
  mutate(min_run = Harvest + adjusted_AUC) %>%
  mutate(max_run = Harvest + (adjusted_AUC/0.436/0.80)) -> c_harvest
write_csv(c_harvest, "data/c_harvest.csv")

#Plot of maximum and minimum harvest rates for PWS wild chum salmon
chum_perc_harv <- ggplot(data = c_harvest)+
  theme_bw()+
  geom_line(aes(x = Year, y = max_harv))+
  geom_line(linetype = "dashed", aes(x = Year, y = min_harv))+
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  labs(x = "Years", y = "% \n Harvest")
chum_perc_harv
ggsave("figures/chum_perc_harv.png", dpi=400, width=5, height=4, units='in')


#Estimated number of wild chum harvested in PWS
chum_harv <- ggplot(data = c_harvest)+
  theme_bw()+
  geom_line(aes(x = Year, y = Harvest/1000))+
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  labs(x = "Years", y = "Harvest (1000s)")
chum_harv

chum_total <- ggplot(data = c_harvest)+
  theme_bw()+
  geom_line(aes(x = Year, y = max_run/1000))+
  geom_line(linetype = "dashed", aes(x = Year, y = min_run/1000))+
  geom_rect(xmin=1963, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  labs(x = "Years", y = "Run Size (1000s)")
chum_total


#Wrap the 3 figures above into a singe figure 1 column???
source("http://peterhaschke.com/Code/multiplot.R")#Is multiplot in any package??
png("figures/chum_all.png", unit = "in", width = 5.87, height = 4.69, res = 400)
chum_all <- multiplot(chum_harv, chum_total,chum_perc_harv,  cols = 1)
dev.off()
 ##won't save the entire multiplot???

########################################################################

##############################################################
#Read Harvest date obtained from the 2017 PWS wild PINK salmon forecast spreadsheet
#the harvest rate is calculated using estimates of (wild harvest/(wild harvest + escapement))
p_harvest<- read_csv("data/Pink_Harvest_Rate.csv")
p_harvest

p_harvest -> out  #the out file is only used to separate broodlines

#First, separate even- and odd- broodlines
#Summarize even-year pink stocks by district. Note that 229 and 222 have already been combined
p_harvest_even <- out %>% 
  group_by(Year, broodline, adjusted_AUC) %>% 
  filter(broodline == "Even", Year > "1962")%>%
  group_by(Year,adjusted_AUC)
glimpse(p_harvest_even)


#Summarize odd-year pink stocks by district. Note that 229 and 222 have already been combined
p_harvest_odd <- out %>% 
  group_by(Year, broodline, adjusted_AUC) %>% 
  filter(broodline == "Odd", Year > "1962")%>%
  group_by(Year,adjusted_AUC)
glimpse(p_harvest_odd)
  
#Estimate maximum and minimum harvests for PWS even- and odd-pink salmon stocks
#adjusted_AUC is from the ~214 area index streams, not the reduced stream count of 134 streams
#maximum harvest estimate from area-under-the-curve (AUC) adjusted for stream life.
#minimum harvest estimate expands adjusted_AUC escapement estimate to account for observer
#efficiency of 0.436 and the proportion of overall escapement represented by the ~214 streams
#surveyed (0.80). Estimates are from Fried et al., Fish and Shellfish Study I (EVOS)
#and based on pink salmon!
p_harvest_even %>% 
    mutate(max_harv = Harvest/(Harvest+adjusted_AUC)*100) %>%
    mutate(min_harv = Harvest/((adjusted_AUC/0.436/.80)+Harvest)*100) %>%
    mutate(min_run = Harvest + adjusted_AUC) %>%
    mutate(max_run = Harvest + (adjusted_AUC/0.436/0.80)) -> p_harvest_even
  write_csv(p_harvest_even, "data/p_harvest_even.csv")
  
p_harvest_odd %>% 
    mutate(max_harv = Harvest/(Harvest+adjusted_AUC)*100) %>%
    mutate(min_harv = Harvest/((adjusted_AUC/0.436/.80)+Harvest)*100) %>%
    mutate(min_run = Harvest + adjusted_AUC) %>%
    mutate(max_run = Harvest + (adjusted_AUC/0.436/0.80)) -> p_harvest_odd
write_csv(p_harvest_odd, "data/p_harvest_odd.csv")

  
  
#Plot of maximum and minimum harvest rates for PWS wild pink salmon: EVEN
pink_perc_harv_even <- ggplot(data = p_harvest_even)+
  theme_bw()+
  geom_line(aes(x = Year, y = max_harv))+
  geom_line(linetype = "dashed", aes(x = Year, y = min_harv))+
  geom_rect(xmin=1960, xmax=1980, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  labs(x = "Years", y = "% \n Harvest")
pink_perc_harv_even
ggsave("figures/pink_perc_harv_even.png", dpi=400, width=5, height=4, units='in')
  
#Plot of maximum and minimum harvest rates for PWS wild pink salmon: ODD
pink_perc_harv_odd <- ggplot(data = p_harvest_odd)+
  theme_bw()+
  geom_line(aes(x = Year, y = max_harv))+
  geom_line(linetype = "dashed", aes(x = Year, y = min_harv))+
  geom_rect(xmin=1960, xmax=1980, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  #geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=400000, alpha = .005)+ #shade years w/ too few surveys
  labs(x = "Years", y = "% \n Harvest")
pink_perc_harv_odd
ggsave("figures/pink_perc_harv_odd.png", dpi=400, width=5, height=4, units='in')


#Estimated number of wild pink harvested in PWS: EVEN
pink_harv_even <- ggplot(data = p_harvest_even)+
    theme_bw()+
    geom_line(aes(x = Year, y = Harvest/1000))+
    geom_rect(xmin=1960, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
    geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
    labs(x = "Years", y = "Harvest (1000s)")
pink_harv_even

#Estimated number of wild pink harvested in PWS: ODD
pink_harv_odd <- ggplot(data = p_harvest_odd)+
  theme_bw()+
  geom_line(aes(x = Year, y = Harvest/1000))+
  geom_rect(xmin=1960, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  #geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  labs(x = "Years", y = "Harvest (1000s)")
pink_harv_odd


#Estimated total run size of wild pink salmon in PWS: EVEN
pink_total_even <- ggplot(data = p_harvest_even)+
    theme_bw()+
    geom_line(aes(x = Year, y = max_run/1000))+
    geom_line(linetype = "dashed", aes(x = Year, y = min_run/1000))+
    geom_rect(xmin=1960, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
    geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
    labs(x = "Years", y = "Run Size (1000s)")
pink_total_even
  
#Estimated total run size of wild pink salmon in PWS: ODD
pink_total_odd <- ggplot(data = p_harvest_odd)+
  theme_bw()+
  geom_line(aes(x = Year, y = max_run/1000))+
  geom_line(linetype = "dashed", aes(x = Year, y = min_run/1000))+
  geom_rect(xmin=1960, xmax=1980, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  #geom_rect(xmin=2015.5, xmax=2017, ymin=0, ymax=4000000, alpha = .005)+ #shade years w/ too few surveys
  labs(x = "Years", y = "Run Size (1000s)")
pink_total_odd
dev.off()


#Wrap the 3 figures for pink even year runs above into a single figure, 1 column
source("http://peterhaschke.com/Code/multiplot.R") #Is multiplot in any package??
png("figures/pink_even_all.png", unit = "in", width = 5.87, height = 4.69, res = 400)##won't save the entire multiplot???
pink_even_all <- multiplot(pink_harv_even, pink_total_even, pink_perc_harv_even,  cols = 1)
pink_even_all
dev.off()
  
  
#Wrap the 3 figures for pink even year runs above into a single figure, 1 column
source("http://peterhaschke.com/Code/multiplot.R") #Is multiplot in any package??
png("figures/pink_odd_all.png", unit = "in", width = 5.87, height = 4.69, res = 400)
pink_odd_all <- multiplot(pink_harv_odd, pink_total_odd, pink_perc_harv_odd,  cols = 1)
pink_odd_all
dev.off()




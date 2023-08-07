

#########################################################################################################################################
### prep the data ####
#########################################################################################################################################
data_file_control <- "X:/Therese_Jackie/Sandy_soils/Development_database/completeDB/sites_control_merged.csv"



summary_control_data_all <- read_csv(data_file_control, 
                                     col_types = cols(rep_block = col_character()))

########################################################################################################################################

##label problem with Mt Damper
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site_sub = case_when(
      site_sub == "Mt Damper" ~ "Mt_Damper",
      TRUE ~ site_sub))

##label problem with Ouyen
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site_sub = case_when(
      site_sub == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site_sub))
summary_control_data_all <- summary_control_data_all %>% 
  mutate(
    site = case_when(
      site == "Oyen_Spade" ~ "Ouyen_Spade",
      TRUE ~ site))

########################################################################################################################################
##### DF 1 - step 1 is the soil modification




#```{r working out how many modification all 1, message=FALSE, warning=FALSE, include=FALSE}
#####################################################################################
#####  soil modifications      #####
#####################################################################################

### get the soil modification from the Descriptors

#1 soil modification with depth
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_depth =  str_extract(summary_control_data_all$Descriptors, "[^_]+"))#keep everything before the first_


#1.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_depth = case_when(
    soil_modification_depth == "Unmodified+DeepTill.18" ~ "Unmodified",
    TRUE ~ soil_modification_depth))

#how many modification_depth
soil_modification_depth <- summary_control_data_all %>% 
  distinct(soil_modification_depth) %>% 
  arrange(soil_modification_depth) %>% 
  filter(soil_modification_depth != "Unmodified")

count(soil_modification_depth) #20 excluding the unmodified


#1a working clms - soil modification without depth 1 only
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 =  str_extract(summary_control_data_all$Descriptors, "[^.]+"))#keep everything before the first .

#the Unmodified seems to have a problem this fixes it :)
summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 = str_extract(summary_control_data_all$soil_modification_1, "[^_]+")) #keep everything after the first _

#1a.1 Youndhusband has 'Unmodified+DeepTill.18' which needs to be recoded as "Unmodified"

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_1 = case_when(
    soil_modification_1 == "Unmodified+DeepTill" ~ "Unmodified",
    TRUE ~ soil_modification_1))

summary_control_data_all %>%  distinct(soil_modification_1) %>% arrange(soil_modification_1)





#1a working clms - 2 soil modification without depth 2

#temp <- summary_control_data_all %>%  distinct(soil_modification_depth) %>% arrange(soil_modification_depth)

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification_2 = case_when(
    soil_modification_depth == "Rip.50Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.60Spade.30" ~        "Rip+Spade",
    soil_modification_depth == "Rip.45IncRip+Spade.30" ~ "IncRip+Spade",
    soil_modification_depth == "Rip.60IncRip+Spade.30" ~ "IncRip+Spade",
    
    soil_modification_depth == "Rip.30IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.40IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.45IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.50IncRip" ~           "IncRip",
    soil_modification_depth == "Rip.60IncRip" ~           "IncRip",
    
    soil_modification_depth == "Pre" ~           "Unmodified",
    
    
    TRUE ~ "NA"
  ))



summary_control_data_all %>%  distinct(soil_modification_2) %>% arrange(soil_modification_2)


#2 all soil modification without depth (if 2 soil modification used keep this)	

summary_control_data_all <- summary_control_data_all %>% 
  mutate(soil_modification = case_when(
    soil_modification_2 == "NA" ~ soil_modification_1,
    TRUE ~ soil_modification_2
  ))


summary_control_data_all %>%  distinct(soil_modification) %>% arrange(soil_modification)

### remove the working clms
summary_control_data_all <- summary_control_data_all %>% 
  dplyr::select(-soil_modification_1,
                -soil_modification_2)


#how many modification without depth
soil_modification_depth <- summary_control_data_all %>% 
  distinct(soil_modification) %>% 
  arrange(soil_modification) %>% 
  filter(soil_modification != "Unmodified")

count(soil_modification_depth) #8 excluding the unmodified 



#####################################################################################
#####  soil amendments      #####
#####################################################################################



### get the soil Amendment from the Descriptors
# gets all the amendments
summary_control_data_all <- summary_control_data_all %>%
  mutate(amendment_all = sub("^[^_]*_", "", summary_control_data_all$Descriptors)) #keep everything after the first _
#pull out the first amendment listed but has rates
summary_control_data_all <- summary_control_data_all %>%
  mutate(amendment_1 = str_extract(summary_control_data_all$amendment_all, "[^.]+"))#keep everything before the first
#removed the rates in the first amendment listed
summary_control_data_all <- summary_control_data_all %>%
  mutate(amendment_1 = str_extract(summary_control_data_all$amendment_1, "[^@]+")) #keep everything before the first @

temp <- summary_control_data_all %>%  distinct(amendment_1) %>% arrange(amendment_1)


summary_control_data_all <- summary_control_data_all %>% 
  mutate(amendment_2 = case_when(
    
    amendment_all ==  "30+30_Fert.banded_30" ~        "Fert",
    amendment_all ==  "30+30_Fert.banded_30_annual" ~ "Fert",
    amendment_all ==  "30+30_none" ~                  "none",
    amendment_all ==  "30+30_none_annual" ~           "none",
    amendment_all ==  "30+7.5_none" ~                  "none",
    amendment_all ==  "30+7.5_none_annual" ~           "none",
    amendment_all ==  "Cl.incorp_30.Gypsum.incorp_30" ~ "Cl+Gypsum",
    amendment_all ==  "Cl@20.incorp_20.Clay.incorp_20" ~ "Cl+Clay",
    amendment_all ==  "Cl@20.incorp_20.Fert.surface" ~ "Cl+Fert",
    amendment_all ==  "Cl@20.incorp_20.Fert.surface.Clay.incorp_20" ~ "Cl+Fert+Clay",
    amendment_all ==  "Cl@20.incorp_8.Clay.incorp_8" ~ "Cl+Clay",
    amendment_all ==  "Cl@20.incorp_8.Fert.surface" ~ "Cl+Fert",
    amendment_all ==  "Cl@20.incorp_8.Fert.surface.Clay.incorp_8" ~ "Cl+Fert+Clay",
    amendment_all ==  "Cl@3.incorp_30.Lime.incorp_8" ~ "Cl+Lime",
    amendment_all ==  "Cl@5.incorp_20.Clay.incorp_20" ~ "Cl+Clay",
    amendment_all ==  "Cl@5.incorp_20.Fert.surface" ~ "Cl+Fert",
    amendment_all ==  "Cl@5.incorp_20.Fert.surface.Clay.incorp_20" ~ "Cl+Fert+Clay",
    amendment_all ==  "Cl@5.incorp_8.Clay.incorp_8" ~ "Cl+Clay",
    amendment_all ==  "Cl@5.incorp_8.Fert.surface" ~ "Cl+Fert",
    amendment_all ==  "Cl@5.incorp_8.Fert.surface.Clay.incorp_8" ~ "Cl+Fert+Clay",
    amendment_all ==  "drill_20+20_Fert.banded_20" ~ "Fert",
    amendment_all ==  "drill_20+20_Fert.banded_20_annual" ~ "Fert",
    amendment_all ==  "drill_20+20_none" ~ "none",
    amendment_all ==  "drill_20+20_none_annual" ~ "none",
    amendment_all ==  "drill_20+7.5_none" ~ "none",
    amendment_all ==  "drill_20+7.5_none_annual" ~ "none",
    amendment_all ==  "Fert.band_30.Clay.incorp_10" ~ "Fert+Clay",
    amendment_all ==  "Fert.incorp_30.Clay.incorp_30" ~ "Fert+Clay",
    amendment_all ==  "Fert.incorp_30.K_added.incorp_30" ~ "Fert+K",
    amendment_all ==  "Fert.surface.Clay.incorp_20" ~ "Fert+Clay",
    amendment_all ==  "Fert.surface.Clay.incorp_8" ~ "Fert+Clay",
    amendment_all ==  "Fert_APP.band_45" ~ "Fert",
    amendment_all ==  "Fert_High.band_45" ~ "Fert",
    amendment_all ==  "Fert_Low.band_45" ~ "Fert",
    amendment_all ==  "K_added.surface" ~ "K",
    amendment_all ==  "Lc.incorp_30.Clay.incorp_30" ~ "Lc+Clay",
    amendment_all ==  "Lc.incorp_30.Fert.incorp_30.Clay.incorp_30" ~ "Lc+Fert+Clay",
    amendment_all ==  "Lc@1.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Lc@10.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Lc@15.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Lc@2.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Lc@20.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Lc@4.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Lc@6.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Lc@8.incorp_30.K_added.surface" ~ "Lc+K",
    amendment_all ==  "Vet_Cer_In.incorp_30" ~ "Vetch_Cer",
    
    
    TRUE ~ amendment_1
  ))

unique(summary_control_data_all$amendment_2)

summary_control_data_all <-  summary_control_data_all %>% 
  mutate(amendment_123 = amendment_2)


#tidy up working outs
summary_control_data_all <- summary_control_data_all %>%
  dplyr::select(-amendment_all, -amendment_1, -amendment_2)
names(summary_control_data_all)


################################################################################

##### DF 1 - step 2 is the amendments


# Add code for amendments classed as
# 
# - animal
# 
# - plant
# 
# - non organic
# 
# - none
# 
# - mixed
# 
# ```{r code for amendment plant and animals, message=FALSE, warning=FALSE, include=FALSE}

summary_control_data_all<- summary_control_data_all %>% 
  mutate(amendment_code1 = case_when(
    amendment_123 ==  "none" ~ "none",
    
    amendment_123 == "drill_20+7"              ~ "none",
    amendment_123 == "30+30_none_annual"       ~ "none",
    amendment_123 == "drill_20+20_none"        ~ "none",
    amendment_123 == "drill_20+20_none_annual" ~ "none",
    amendment_123 == "30+7"                    ~ "none",
    amendment_123 == "30+30_none"              ~ "none",
    
    
    amendment_123 ==  "Cl" ~ "animal",
    
    amendment_123 ==  "Lc" ~ "plant",
    amendment_123 ==  "Cereal" ~ "plant",
    amendment_123 ==  "Vetch" ~ "plant",
    amendment_123 ==  "Vet_Cer" ~ "plant",
    amendment_123 ==  "Vetch_Cer" ~ "plant",
    amendment_123 ==  "Vet_Cer_In" ~ "plant", 
    amendment_123 ==  "Com" ~ "plant",
    
    
    amendment_123 ==  "Fert" ~        "fertiliser",
    amendment_123 ==  "Fert_High"   ~ "fertiliser",
    amendment_123 ==  "Fert_Low"    ~ "fertiliser",
    amendment_123 ==  "Fert_APP"    ~ "fertiliser",
    amendment_123 ==  "K_added"     ~ "fertiliser",
    amendment_123 ==  "K"     ~ "fertiliser",
    amendment_123 ==  "FertK_added" ~ "fertiliser",
    amendment_123 ==  "Fert+K" ~ "fertiliser",
    
    amendment_123 ==  "30+30_Fert"  ~ "fertiliser",
    amendment_123 ==  "drill_20+20_Fert" ~ "fertiliser",
    
    amendment_123 ==  "Gypsum" ~ "non organic",
    amendment_123 ==  "Clay" ~ "non organic",
    amendment_123 ==  "Lime" ~ "non organic",
    amendment_123 ==  "Bi_Agra" ~ "non organic",
    amendment_123 ==  "SE14" ~ "non organic",
    
    
    amendment_123 ==  "ClFertClay" ~ "mixed",
    amendment_123 ==  "Cl+Fert+Clay" ~ "mixed",
    amendment_123 ==  "ClFert" ~ "mixed",
    amendment_123 ==  "Cl+Fert" ~ "mixed",
    amendment_123 ==  "ClClay" ~ "mixed",
    amendment_123 ==  "Cl+Clay" ~ "mixed",
    
    amendment_123 ==  "ClLime" ~ "mixed",
    amendment_123 ==  "Cl+Lime" ~ "mixed",
    amendment_123 ==  "ClGypsum" ~ "mixed",
    amendment_123 ==  "Cl+Gypsum" ~ "mixed",
    
    amendment_123 ==  "LcFert" ~ "mixed",
    amendment_123 ==  "Lc+Fert" ~ "mixed",
    amendment_123 ==  "LcClay" ~ "mixed",
    amendment_123 ==  "Lc+Clay" ~ "mixed",
    amendment_123 ==  "LcFertClay" ~ "mixed",
    amendment_123 ==  "Lc+Fert+Clay" ~ "mixed",
    amendment_123 ==  "LcK_added" ~ "mixed",
    amendment_123 ==  "Lc+K" ~ "mixed",
    
    amendment_123 ==  "FertClay" ~ "mixed",
    amendment_123 ==  "Fert+Clay" ~ "mixed",
    
    
    TRUE ~ "check"
    
    
  ))

#```


unique(summary_control_data_all$amendment_code1)



## Scatter Plots control vs treatments yields **with amendment groupings**




#```{r scatter amendment, echo=FALSE, message=FALSE, warning=FALSE}



summary_control_data_all$amendment_code1 <-
  factor(summary_control_data_all$amendment_code1,
         #levels = c("animal", "plant", "fertiliser"))
         levels = c("animal", "plant", "fertiliser","non organic", "mixed"))


summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  ggplot( mapping = aes(control_yield, yield)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  geom_smooth(method = lm, se = FALSE, na.rm = TRUE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Control yield - all data - no summary\nnote each site, treatment, year and rep is matched to control
    \nFacet wrap is filtered data amendments",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)



#################################################################################

summary_control_data_all %>%  
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  
  filter(!is.na(yield)) %>% 
  filter(!is.na(control_yield)) %>% 
  
  filter(soil_modification != "Unmodified") %>% 
  
  
  
  ggplot( mapping = aes(control_yield, yield,)) +
  geom_point(aes(colour= amendment_code1),alpha= 0.4) +
  #geom_point(data = Bute, colour='black', alpha = 0.4)+
  #geom_point(data = Younghusband, colour='black', alpha = 0.4)+ #no data for this
  geom_point(data = Ouyen, colour='black', alpha = 0.4)+
  geom_smooth(method = lm, se = FALSE) +
  stat_regline_equation(
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula = (y ~ x)
  ) +
  geom_abline(intercept = 0, slope = 1, linetype="dashed")+
  
  theme_bw()+
  theme(legend.position = "none")+
  labs(title = "Control yield - no summary. ANY soil modification \nnote each site, treatment, year and rep is matched to control
      \nFacet wrap is amendment",
       x = "control yield t/ha", y = "treatment yield t/ha")+
  facet_wrap(.~ amendment_code1)
###############################################################################

#add the sites as an overlay.

## Bute

unique(summary_control_data_all$site)
Younghusband <- summary_control_data_all %>% 
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  
  filter(site == "Younghusband" )


Bute <- summary_control_data_all %>% 
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  filter(site == "Bute_CSIRO" | site == "Bute_Trengrove")

Ouyen <- summary_control_data_all %>% 
  filter(amendment_code1 != "none") %>%  
  filter(amendment_code1 != "non organic") %>% 
  filter(amendment_code1 != "mixed" ) %>% 
  filter(site == "Ouyen_Spade" | site == "Ouyen_Placement")


Younghusband_temp <- summary_control_data_all %>% 
    filter(site == "Younghusband" )
unique(Younghusband_temp$Descriptors)

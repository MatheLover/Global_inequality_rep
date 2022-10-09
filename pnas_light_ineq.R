# install.packages("lmerTest")
# install.packages("sp")
# install.packages('MuMIn')
# install.packages("raster")
# install.packages("rasterVis")
# install.packages("rgdal")
# install.packages("mixtools")
# install.packages("RColorBrewer")
# install.packages("visreg")

# load data 
test <- load('pnas_light_ineq.RData')
library(MuMIn)
library(sp)
library(lmerTest)
library(visreg)
library(tidyverse)
library(raster)
library(rasterVis)
library(rgdal)
library(mixtools)
library(RColorBrewer)

# check NA
all(is.na(df_nat))
all(is.na(df_US_states))

# check global dataset 
summary(unique(df_nat$ISO3)) # 57 countries but we have 195 countries in the world  
(unique(df_nat$Year)) # 1995 2000 2005 2010 1990
summary(df_nat$Gini) # min 20; mean 34.2; max 58.3 -- * 100 ? 
plot(density(df_nat$Gini))
summary(df_nat$Gini_SE) # min 0.2 mean 0.75 max 1
summary(df_nat$Light_Gini) # min 24; mean 53.91; max 88
summary(df_nat$GDP)
sorted <- df_nat[order(-df_nat$GDP,-df_nat$Year),] # order is correct; yet some countries only have two-year data 
summary(df_nat$POP)
sorted_pop <- df_nat[order(-df_nat$POP,-df_nat$Year),] 

# read ctry code 
code <- readxl::read_excel("/Users/benchiang/Documents/R_Project/PNAS_light_inequality/historical-classification-of-developed-and-developing-regions.xlsx",sheet=2)
code <- code[code$`Developed / Developing regions` == "Developing",]
df_deving <- df_nat[(df_nat$ISO3 %in% code$`ISO-alpha3 Code`),]  
unique(df_deving$ISO3) # 24 developing countries  / 183 overall 

mod_1 <- lmer(Gini ~ Light_Gini + 
                (1 | Year), 
              data = df_deving)
summary(mod_1)
r.squaredGLMM(mod_1)[2]


mod_1 <- lmer(Gini ~ Light_Gini + 
                (1 | Year), 
              data = df_nat)
summary(mod_1)
r.squaredGLMM(mod_1)[2]
AIC(mod_1)
AICc(mod_1)

mod_2 <- lmer(Gini ~ Light_Gini +  
                log(POP) +
                (1 | Year), 
              data = df_nat)
summary(mod_2)
r.squaredGLMM(mod_2)[2]
AIC(mod_2)
AICc(mod_2)


mod_3 <- lmer(Gini ~ Light_Gini +  
                  log(POP) +
                  log(GDP) + 
                  (1 | Year), 
                data = df_nat)
summary(mod_3)
r.squaredGLMM(mod_3)[2]
AIC(mod_3)
AICc(mod_3)

mod_4 <- lm(Gini ~ Light_Gini +  
                log(POP) +
                log(GDP), 
              data = df_nat)
summary(mod_4)


mod_4 <- lm(Gini ~ Light_Gini +  
              log(POP) +
              log(GDP), 
            data = df_deving)
summary(mod_4)


#Fig 1
visreg(mod_3, 'Light_Gini', gg = T, overlay = T, band = F) + 
  geom_point(size = 2, colour = 'grey') + labs(x = 'Light Gini', y = 'Income Gini')

spplot(world_2010, c("gini_disp"), col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd'))(20), 
       cuts = 19, col = "grey")

spplot(world_2010, c("light_gini_lpp"), col.regions = colorRampPalette(brewer.pal(9, 'YlOrRd'))(20), 
       cuts = 19, col = "grey")


#Fig 2
ggplot(data = swiid_df, aes(income_cat, light_gini_lpp)) + 
  geom_boxplot(colour = 'darkblue', fill = 'lightblue', size = 1) +
  labs(x = 'Income Group', y = 'Light Gini') +
  theme_minimal(base_size = 20) + xlim(c('High Income', 'Low Income', 'Middle Income'))

ggplot(data = swiid_df, aes(income_cat, gini_disp)) + 
  geom_boxplot(colour = 'darkblue', fill = 'lightblue', size = 1) +
  labs(x = 'Income Group', y = 'Income Gini') +
  theme_minimal(base_size = 20) + xlim(c('High Income', 'Low Income', 'Middle Income'))

ggplot(data = swiid_df, aes(region, light_gini_lpp)) + 
  geom_boxplot(colour = 'red4', fill = 'indianred1', size = 1) +
  labs(x = 'Regions', y = 'Light Gini') +
  theme_minimal(base_size = 20) 

ggplot(data = swiid_df, aes(region, gini_disp)) + 
  geom_boxplot(colour = 'red4', fill = 'indianred1', size = 1) +
  labs(x = 'Regions', y = 'Income Gini') +
  theme_minimal(base_size = 20) 


#Fig 3
spplot(US_states_10, c("Estimate..Gini.Index"), par.settings=list(fontsize=list(text=20)), 
       col.regions = colorRampPalette(brewer.pal(9, 'Reds'))(50), cuts = 49, col = "transparent")

spplot(US_states_10, c("light_gini"), par.settings=list(fontsize=list(text=20)),
       col.regions = colorRampPalette(brewer.pal(9, 'Reds'))(50), cuts = 49, col = "transparent")

spplot(US_county, c("Estimate..Gini.Index"), par.settings=list(fontsize=list(text=20)),
       col.regions = colorRampPalette(brewer.pal(9, 'Reds'))(50), cuts = 49, col = "transparent")

cor.test(~ light_gini + Gini, df_US_states[year != 2005])

ggplot(df_US_states[year != 2005], aes(light_gini, Gini)) + geom_point() + 
  geom_smooth(method = 'lm', se = F) + labs(x = 'Light Gini', y = 'Income Gini')


# Fig 4
# Use data at https://github.com/muhusmanmirza/PNAS_light_inequality to plot the static window inequality maps for 1990, 1995, 2000, 2005 & 2010 
# Use data https://zenodo.org/record/4635734/files/gini_2010.tif?download=1 to plot the smoothed version for 2010. 


name <- "agg_gini_2010.tif"
imported_raster <- raster(name)
spplot(imported_raster)


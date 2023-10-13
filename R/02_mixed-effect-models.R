###
# Phenology Curves for Flowers
###
rm(list = ls())
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpubr)
library(lme4)
library(wiqid)

site_meta <- read.csv('./data/MW_SiteInfo_2013_2020.csv')
site_snow <- read.csv('./data/MW_SDDall.csv')

site_meta$Transect <- gsub("^([A-Z]{2}).*", "\\1", site_meta$Site_Loc) |> 
  sapply(function(x) switch (
    x,
    'RL' = "Reflection Lakes",
    "GB" = "Glacier Basin",
    ""
  ))

site_meta <- site_snow |> 
  select(Year, Site_Loc) |> 
  left_join(site_meta, by = 'Site_Loc')



pheno_calced <- read.csv('./data/MW_Phenocurves.csv')
pheno_calced <- site_meta |> 
  select(Year, Site_Loc, Elevation) |> 
  right_join(pheno_calced, by = c("Site_Loc" = 'site_code',
                                  'Year' = 'year'))


ggplot(pheno_calced)+
  geom_point(aes(x = Elevation, y = peak))+
  geom_smooth(aes(x = Elevation, y = peak),
              method = 'lm')+
  theme_classic()

ggplot(pheno_calced) +
  geom_point(aes(x = SDD, y = Elevation,
                 size = peak, color = peak)) +
  scale_color_gradient(low = 'grey', high = 'black')+
  theme_classic()




peak_mod1 <- lmer(peak ~ scale(Elevation) + scale(SDD) + (1 | species), data = pheno_calced) 
peak_mod2 <- lmer(peak ~ scale(Elevation) + (1| species), data = pheno_calced)
peak_mod3 <- lmer(peak ~ scale(Elevation) + scale(SDD) + (1 | species) + (1|Site_Loc), data = pheno_calced)
peak_mod4 <- lmer(peak ~ scale(Elevation) + scale(SDD) + scale(Elevation) * scale(SDD) + (1 | species) + (1|Site_Loc), data = pheno_calced)


standardize2match(c(104:204), pheno_calced$SDD) -> sdd_scaled
standardize2match(c(range(pheno_calced$Elevation)[1]:range(pheno_calced$Elevation)[2]), pheno_calced$Elevation) -> ele_scaled
213.47 + 17.438 * sdd_scaled -> pred_sdd_DOYpeak

ggplot() +
  geom_point(aes(x = SDD, y = peak), data = pheno_calced) +
  geom_smooth(aes(x = SDD, y = peak), data = pheno_calced,
              method = 'lm') +
  geom_smooth(aes(x = c(104:204), y = pred_sdd_DOYpeak),
              method = 'lm') +
  theme_classic()


# need to calculate the effect of sdd over the range of pisslbe values.
# 
# ggplot() +
#   geom_point(aes(x = c(range(pheno_calced$Elevation)[1]:range(pheno_calced$Elevation)[2]), 
#                  y = ) +
#   theme_classic()




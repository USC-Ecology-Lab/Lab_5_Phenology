###
# Phenology Curves for Flowers
###
library(ggplot2)
library(dplyr)
library(lubridate)
library(ggpubr)

pheno_dat <- read.csv('./data/MW_PhenoDat_2013_2019_anonymized.csv')
pheno_sum <- pheno_dat |> 
  group_by(Date = as.Date(Date, format = '%m/%d/%y'),Site_Code, Transect) |> 
  summarize(prop_flowering = sum(Flower)/length(Flower))

pheno_sum$DOY <- pheno_sum$Date |> yday()

## Initial Plot
ggplot(pheno_sum)+
  geom_point(aes(x = DOY, y = prop_flowering,
                 color = as.factor(year(Date))),
             alpha = 0.25)+
  geom_smooth(aes(x = DOY, y = prop_flowering,
                  color = as.factor(year(Date))),
              se = F)+
  scale_y_continuous(limits = c(0,1))+
  facet_wrap(~Transect)+
  theme_classic()
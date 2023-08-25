rm(list=ls())
library(ggplot2)
library(tidyr)
library(data.table)
library(openxlsx)
library(foreign)

path <- "C:\\Users\\navidehno\\OneDrive - Bill & Melinda Gates Foundation\\Nutrition\\SQ-LNS\\Compartmental-model\\data\\Niger-LiST.xlsx"

sheets <- openxlsx::getSheetNames(path)
data <- data.table(lapply(sheets, openxlsx::read.xlsx, xlsxFile=path)[[1]])

CMort <- melt(data[Indicator == "Child deaths" & Configuration %in% c("6-11 months","12-23 months"),],
id.vars=c("Configuration"),measure.vars=c("2000","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011",
"2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"),value.name = "value")

CMort[,CMort6.23 := sum(value),keyby=c("variable")]

bth <- melt(data[Indicator %in% c("Total births : Total births") & Configuration %in% c("Both sexes"),],
id.vars=c("Configuration"),measure.vars=c("2000","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011",
"2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"),value.name = "value")

life.expectancy <- melt(data[Indicator %in% c("Life expectancy (years) : Life expectancy (years)") & Configuration %in% c("Both sexes"),],
id.vars=c("Configuration"),measure.vars=c("2000","2002","2003","2004","2005","2006","2007","2008","2009","2010","2011",
"2012","2013","2014","2015","2016","2017","2018","2019","2020","2021","2022","2023"),value.name = "value")

bth[,CMort6.23:=CMort$CMort6.23[match(variable,CMort$variable)]]
bth[,CMR:= 1000*CMort6.23/value]

bth[,life.expectancy:=life.expectancy$value[match(variable,life.expectancy$variable)]]
bth <- bth[,-1]
names(bth) <- c("year","births.number","deaths.number.6.23","mortality.rate","life.expectancy")

write.csv(bth,"C:/Users/navidehno/OneDrive - Bill & Melinda Gates Foundation/Nutrition/SQ-LNS/Compartmental-model/data/Niger-bth-dth.csv")

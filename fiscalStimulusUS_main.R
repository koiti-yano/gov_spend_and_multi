# Yano, K., (2020), "Government spending and multi-category 
# treatment effects: The modified conditional independence 
# assumption," manuscript.
# Copyright (c) 2020 Koiti Yano
# 
# Noah Greifer (2020). WeightIt: Weighting for Covariate 
# Balance in Observational Studies. R package version 0.10.0.
# https://CRAN.R-project.org/package=WeightIt

rm(list=ls())
setwd("C:/Users/koiti/Dropbox/program/R/1_fiscalPolicyAndCausality/src")

require(vars) # Firstly loaded to avoid a name-collision of select 
require(WeightIt)
require(cobalt)
require(openxlsx) # For openxlsx
require(seasonal) # For seas
require(broom) # For tidy()
require(huxtable)
require(jtools) # For summ() #install.packages("flextable"); install.packages("officer")
require(fredr) # https://github.com/sboysel/fredr
fredr_set_key("d88c682c8274047b72e7e64da46a7801")

require(tidyverse) # For pipe and dplyr
# Conflicts between the tidyverse (dplyr) and other packages
# https://tidyverse.tidyverse.org/reference/tidyverse_conflicts.html
# Ref: Handling dplyr and MASS select clashes
# https://hollyemblem.medium.com/handling-dplyr-and-mass-select-clashes-7c88258fd9d0
tidyverse_conflicts()
select <- dplyr::select
flter <- dplyr::filter
lag <- dplyr::lag


#freqFlag <- "m" # Monthly
freqFlag <- "q" # Quarterly
#freqFlag <- "a" # Annual

plotFlag <- TRUE # Plot data.
# plotFlag <- FALSE 

# In FRED, the start and the end of the observation period
startDate <- "1992-01-01"
endDate <- "2019-12-31"

# Real Gross Domestic Product (GDPC1)
# https://fred.stlouisfed.org/series/GDPC1
series_ID <- "GDPC1"
dataGDP <- fredr_series_observations(
  series_id = series_ID,
  observation_start = as.Date(startDate),
  observation_end = as.Date(endDate),
  frequency = freqFlag, 
  units = "lin"
) 
dataGDP <- dataGDP %>% rename(gdp = value) 

if(plotFlag){
  ggplot(data = dataGDP, mapping = aes(x = date, y = gdp, color = series_id)) +
    geom_line() +
    labs(x = "Observation Date", y = series_ID, color = "Series")
}

# Gross Domestic Product: Implicit Price Deflator (GDPDEF)
# https://fred.stlouisfed.org/series/GDPDEF
series_ID <- "GDPDEF"
dataDef <- fredr_series_observations(
  series_id = series_ID,
  observation_start = as.Date(startDate),
  observation_end = as.Date(endDate),
  frequency = freqFlag, 
  units = "lin"
) 
dataDef <- dataDef %>% rename(price = value) 

if(plotFlag){
  ggplot(data = dataDef, mapping = aes(x = date, y = price, color = series_id)) +
    geom_line() +
    labs(x = "Observation Date", y = series_ID, color = "Series")
}

# Global Price Index of All Commodities (PALLFNFINDEXQ)
# https://fred.stlouisfed.org/series/PALLFNFINDEXQ
series_ID <- "PALLFNFINDEXQ"
dataCommo <- fredr_series_observations(
  series_id = series_ID,
  observation_start = as.Date(startDate),
  observation_end = as.Date(endDate),
  frequency = freqFlag, 
  units = "lin"
) 
dataCommo <- dataCommo %>% rename(commo = value) 


if(plotFlag){
  ggplot(data = dataCommo, mapping = aes(x = date, y = commo, color = series_id)) +
    geom_line() +
    labs(x = "Observation Date", y = series_ID, color = "Series")
}

# Government Consumption Expenditures and Gross Investment (GCE)
# https://fred.stlouisfed.org/series/GCE
series_ID <- "GCE"
dataGCE <- fredr_series_observations(
  series_id = series_ID,
  observation_start = as.Date(startDate),
  observation_end = as.Date(endDate),
  frequency = freqFlag, 
  units = "lin"
) 
dataGCE <- dataGCE %>% rename(govSpend = value) 

if(plotFlag){
  ggplot(data = dataGCE, mapping = aes(x = date, y = govSpend, color = series_id)) +
    geom_line() +
    labs(x = "Observation Date", y = series_ID, color = "Series")
}

# Unemployment Rate (UNRATE)
# https://fred.stlouisfed.org/series/UNRATE
series_ID <- "UNRATE"
dataAdd <- fredr_series_observations(
  series_id = series_ID,
  observation_start = as.Date(startDate),
  observation_end = as.Date(endDate),
  frequency = freqFlag, 
  units = "lin"
) 
dataAdd <- dataAdd %>% rename(add = value) 

if(plotFlag){
  ggplot(data = dataAdd, mapping = aes(x = date, y = add, color = series_id)) +
    geom_line() +
    labs(x = "Observation Date", y = series_ID, color = "Series")
}

# 10-Year Treasury Constant Maturity Rate (DGS10)
# https://fred.stlouisfed.org/series/DGS10
series_ID <- "TEDRATE"
#series_ID <- "T10Y3M"
dataSpread <- fredr_series_observations(
  series_id = series_ID,
  observation_start = as.Date(startDate),
  observation_end = as.Date(endDate),
  frequency = freqFlag, 
  units = "lin"
) 
dataSpread <- dataSpread %>% rename(spread = value) 

ggplot(data = dataSpread, mapping = aes(x = date, y = spread, color = series_id)) +
  geom_line() +
  labs(x = "Observation Date", y = series_ID, color = "Series")

macroData <- inner_join(dataGDP, dataDef, by="date") %>%
  inner_join(dataCommo, by="date") %>% 
  inner_join(dataAdd, by="date") %>% 
  inner_join(dataGCE, by="date") %>% 
  inner_join(dataSpread, by="date")

#??????????????
macroDataSelect <- macroData %>% select(date, gdp, price, commo, 
                                        add, govSpend, spread)

macroDataTmp <- macroDataSelect %>%
  mutate(gdpLog = log(gdp), priceLog = log(price), 
         commoLog = log(commo), addLog = log(add),
         govSpendLog = log(govSpend))

macroDataTmp %>% head()

dataStart <- c(1992,1)
dataEnd <- c(2019,4) # dataEnd is required because our data set may include NAs.

#varSelected <- c("gdpLog", "priceLog", "commoLog", "addLog")
varSelected <- c("gdpLog", "spread", "commoLog", "addLog")
macroDataTs <- ts(macroDataTmp[, varSelected], start=dataStart, end=dataEnd, freq=4)

spreadTs <- ts(dataSpread[,"spread"], start=dataStart, end=dataEnd, freq=4)

varOrder <- 1
var.mac <- VAR(macroDataTs, type="const", p=varOrder) #,exogen=spreadTs)
#var.mac <- VAR(macroDataTs, type="both") # Order selection
summary(var.mac)

sampleSize <- dim(macroDataTs)[1]
obsNum <- dim(macroDataTs)[2]

Amat <- Acoef(var.mac) # Or use Bcoef
A1 <- Amat[[1]] 
#A2 <- Amat[[2]] 
#OLD var.mac$varresult$exLog$coefficients["const"]
#OLD var.mac$varresult$exLog$coefficients["exo1"]


t(t(Bcoef(var.mac)[,"const"]))

obsHat <- array(NA, c(sampleSize,obsNum))
for (ii in 1:(sampleSize-1)){
  obsHat[ii+1, ] <- t(A1 %*% t(t(macroDataTs[ii+1,]))  + t(t(Bcoef(var.mac)[,"const"])) )
  #  obsHat[ii+1, ] <- t(A1 %*% t(t(macroDataTs[ii+1,])) + t(t(Bcoef(var.mac)[,"exo1"])) %*% spreadTs[ii+1] + t(t(Bcoef(var.mac)[,"const"])) )
#  obsHat[ii+1, ] <- t(A1 %*% t(t(macroDataTs[ii+1,])) + A2 %*% t(t(macroDataTs[ii,])) + t(t(Bcoef(var.mac)[,"exo1"])) %*% spreadTs[ii+1] + t(t(Bcoef(var.mac)[,"const"])))
  #OLD A1 %*% t(t(macroData[2,])) + A2 %*% t(t(macroData[1,])) + t(t(Bcoef(var.mac)[,"exo1"])) %*% ctaxRateTs[2] + t(t(Bcoef(var.mac)[,"const"])) 
}

if(plotFlag){
  par(mfrow=c(1,3))
  plot(macroDataTs[,"gdpLog"]) 
  lines(ts(obsHat[,1], start=dataStart, end=dataEnd, freq=4), col=2)
  
  hist(macroDataTs[,"gdpLog"]-ts(obsHat[,1], start=dataStart, end=dataEnd, freq=4))
  
  plot(macroDataTs[,"commoLog"]) 
  lines(ts(obsHat[,3], start=dataStart, end=dataEnd, freq=4), col=2)
}

macroDataFrm <- macroDataTmp %>% 
  mutate(gdpHat = obsHat[,1])

macroDataFrm %>% head()

# You cannot use diff in mutate. Use lag().
# https://notchained.hatenablog.com/entry/2015/04/24/223027
#macroDataFrm1 <- macroDataFrm %>% 
#  mutate(gdpLogLag = lag(gdpLog)) %>%
#  mutate(priceLogLag = lag(priceLog, order_by = date)) %>% 
#  mutate(commoLogLag= lag(commoLog)) %>%
#  mutate(addLogLag = lag(addLog)) %>%
#  mutate(gdpHatLag = lag(gdpHat)) %>%
#  mutate(govSpendLogLag = lag(govSpendLog)) 

macroDataFrm2 <- macroDataFrm %>% 
  mutate(gdpDiff = (gdpLog-lag(gdpLog))) %>%
  mutate(priceDiff = (priceLog-lag(priceLog, order_by = date))) %>% 
  mutate(commoDiff = (commoLog-lag(commoLog))) %>%
  mutate(addDiff = (addLog-lag(addLog))) %>%
  mutate(growthHat = (gdpLog-lag(gdpHat))) %>%
  mutate(govSpendDiff = (govSpendLog-lag(govSpendLog))) 

macroDataFrm3 <- macroDataFrm2 %>% 
  mutate(gdpDiffLag1 = lag(gdpDiff)) %>%
  mutate(priceDiffLag1 = lag(priceDiff)) %>%
  mutate(commoDiffLag1 = lag(commoDiff)) %>%
  mutate(addDiffLag1 = lag(addDiff)) %>% 
  mutate(spreadLag1 = lag(spread))

macroDataFrm4 <- macroDataFrm3 %>% 
  mutate(gdpDiffLag2 = lag(gdpDiffLag1)) %>%
  mutate(priceDiffLag2 = lag(priceDiffLag1)) %>%
  mutate(commoDiffLag2 = lag(commoDiffLag1)) %>%
  mutate(addDiffLag2 = lag(addDiffLag1))

gsMean <- macroDataFrm4 %>% summarise(mean(govSpendDiff, na.rm=T)) %>% as.numeric()
treatSd <- macroDataFrm4 %>% summarise(sd(govSpendDiff, na.rm=T)) %>% as.numeric()

# https://dplyr.tidyverse.org/reference/case_when.html
macroDataFrm5 <- macroDataFrm4 %>%
  mutate(
    fiscStmCatgry = case_when(
      ((govSpendDiff-gsMean) < - treatSd) ~ "ContLarg",
      ((govSpendDiff-gsMean) >= - treatSd & (govSpendDiff-gsMean) < 0) ~ "ContSml",
      ((govSpendDiff-gsMean) >= 0 & (govSpendDiff-gsMean) < treatSd) ~ "ExpaSml",
      ((govSpendDiff-gsMean) >= treatSd) ~ "ExpaLarg"
    )
  ) 

macroDataNaOmit <- na.omit(macroDataFrm5)

macroDataFinal <- macroDataNaOmit #%>% 
#  mutate(fiscStmCatgry = as.factor(fiscStmCatgry)) %>%
#  mutate(fiscStmCatgry = relevel(fiscStmCatgry, "ContLarg"))

macroDataFinal %>% head()
macroDataFinal %>% count(fiscStmCatgry)

weiMacro <- weightit(fiscStmCatgry ~ gdpDiffLag1 + commoDiffLag1 + addDiffLag1, # + spreadLag1,
                     data = macroDataFinal)

love.plot(weiMacro, which.treat = "ContLarg")
#love.plot(weiMacro, which.treat =  = .all)
balMacro <- bal.tab(weiMacro, un=TRUE, which.treat = "ContLarg") # Foucus on "ContLarg (0) vs."
#balMacro <- bal.tab(weiMacro, un=TRUE, which.treat = .all) #all treatment comparisons
print(balMacro)

weiMacroTrim <- trim(weiMacro, at=.95)

lmFscDiff <- lm(gdpDiff ~ fiscStmCatgry,
                 data=macroDataFinal)

lmFscDiffWei <- lm(gdpDiff ~ fiscStmCatgry,
                    data=macroDataFinal, weights = weiMacroTrim$weights)

lmFscHat <- lm(growthHat ~ fiscStmCatgry,
                data=macroDataFinal)

lmFscHatWei <- lm(growthHat ~ fiscStmCatgry,
                   data=macroDataFinal, weights = weiMacro$weights)

lmFscHatWeiTri <- lm(growthHat ~ fiscStmCatgry,
                   data=macroDataFinal, weights = weiMacroTrim$weights)

par(mfrow=c(1,3))
acf(resid(lmFscHatWei))
acf(resid(lmFscHatWeiTri))
acf(resid(lmFscHat))

hist(resid(lmFscHatWei))
hist(resid(lmFscHatWeiTri))
hist(resid(lmFscHat))

lm.hux  <- huxreg(lmFscHatWei, lmFscHat, lmFscDiffWei, 
                  number_format = "%.4f",
                  coefs=c("Large fiscal contraction (Intercept)"="(Intercept)",
                          "Small fiscal contraction"="fiscStmCatgryContSml",
                          "Small fiscal expansion"="fiscStmCatgryExpaLarg",
                          "Large fiscal expansion"="fiscStmCatgryExpaSml")
)
lm.hux 

# OLD code (jtools). Not used.
#robustFlag <- FALSE
#robustFlag <- "HC2"
#options("jtools-digits" = 4)
#lmFscHat %>% summ(robust=robustFlag) -> lmFscHat.Summ
#lmFscHatWei %>% summ(robust=robustFlag) -> lmFscHatWei.Summ

exportTo <- "word"
#exportTo <- "excel"

if(exportTo == "word"){
  quick_docx(lm.hux, file="lmFscHatUS.docx")
  
  } else if (exportTo == "excel") {
  
  lmFscHat %>% summ(robust=robustFlag, confint = TRUE) %>% tidy() -> lmFscHat.tidy
  lmFscHat %>% summ(robust=robustFlag, confint = TRUE) %>% glance() -> lmFscHat.glan
  
  lmFscHatWei %>% summ(robust=robustFlag, confint = TRUE) %>% tidy() -> lmFscHatWei.tidy
  lmFscHatWei %>% summ(robust=robustFlag, confint = TRUE) %>% glance() -> lmFscHatWei.glan
  
  wb <- createWorkbook()
  addWorksheet(wb, 'lmFscHatWei.tidy')
  addWorksheet(wb, 'lmFscHatWei.glan')
  addWorksheet(wb, 'lmFscHat.tidy')
  addWorksheet(wb, 'lmFscHat.glan')
  writeData(wb, sheet = 'lmFscHatWei.tidy', x = lmFscHatWei.tidy, withFilter=F)
  writeData(wb, sheet = 'lmFscHatWei.glan', x = lmFscHatWei.glan, withFilter=F)
  writeData(wb, sheet = 'lmFscHat.tidy', x = lmFscHat.tidy, withFilter=F)
  writeData(wb, sheet = 'lmFscHat.glan', x = lmFscHat.glan, withFilter=F)
  saveWorkbook(wb, "../output/lm_us_res.xlsx", overwrite = T)
  
}

if(0){
  balMacro$Pair.Balance$`ContSml vs. ContLarg`$Balance
  balMacro$Pair.Balance$`ContSml vs. ContLarg`$Observations
  
  balMacro$Pair.Balance$`ExpaSml vs. ContLarg`$Balance
  balMacro$Pair.Balance$`ExpaSml vs. ContLarg`$Observations
  
  balMacro$Pair.Balance$`ExpaLarg vs. ContLarg`$Balance
  balMacro$Pair.Balance$`ExpaLarg vs. ContLarg`$Observations
}

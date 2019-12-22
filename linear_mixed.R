#LME usage

library(data.table)
library(lme4)
library(tictoc)

path <- "D:/bios625data"
#this takes 3 minutes ish
#fpath <- file.path(path,"flight_weather_cleaned.csv")
#tic("fread 6gb data import")
#flight_data <- data.table::fread(fpath)
#toc()

#this takes 40 minutes lol
#tic("test for read.csv")
#x_test <- read.csv("D:/bios625data/flight_weather_cleaned.csv")
#toc()

model_test <- lmer(Reaction ~ Days + (Days | Subject), sleepstudy)
summary(model_test)
summary(bigreg_slr)
PRESS = function(fit){
  res = residuals(fit)
  H = hatvalues(fit)
  sigma = summary(fit)$sigm
  sres = sapply(1:length(res), function(i) res[[i]]/(sigma*sqrt(1-H[[i]])))
  PRESS = sum(sres^2)
  return(PRESS)
}
SSY = (fitted(flight_model) - mean(dat$DepDelay))^2
R_PRESS = 1 - PRESS(flight_model)/SSY

model_test$fitted
fitted(model_test)
StudentResid(model_test)


#change parameters as needed
tic("LMM model runtime")

flight_model <- lmer(DepDelay ~ Year + Fog.y + Rain.y + Snow.y + Hail.y + Thunder.y + 
                       TEMP.y + (1 | Year), flight_data)
toc()
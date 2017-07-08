rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
source("./scripts/graphing/graph-elemental.R")
source("./scripts/graphing/graph-presets.R") # pre-sets and options for graphing

# ---- load-packages -----------------------------------------------------------
library(magrittr) #Pipes
library(ggplot2)
library(MplusAutomation)
requireNamespace("readr")
requireNamespace("knitr")
requireNamespace("dplyr")
requireNamespace("tidyr")


# ---- declare-globals ---------------------------------------------------------
path_out <- "./data-unshared/raw/model-final-2017-07-05.out"

# ---- load-data ---------------------------------------------------------------
model_result <- MplusAutomation::readModels(path_out)
names(model_result)
# assign alias for faster reference
mr <- model_result
mr$parameters$unstandardized

model_result$savedata_info
ds <- model_result$savedata

ds %>% dplyr::glimpse()

# genrate predicted value from the generated factor scores
ds <- ds %>% 
  dplyr::mutate(
    predicted  = B_FAT + LINEAR*MO + QUAD*MOSQ + SED*SEDWP + MVPA*MVPAWP +
      CAL*CALWP100 + HT*HEIGHT_W ,
    residual  =  FAT - predicted
  ) 


# ---- tweak-data ----------------------------------------
ds %>% elemental_line(
  variable_name = "predicted",
  time_metric = "MO",
  color_name="black",
  line_alpha=1,
  line_size =.5, 
  smoothed = TRUE,
  rounded_digits = 0L
)

# Plot residuals against observed or modeled 
ds %>% 
  # ggplot2::ggplot(aes(x=FAT, y=residual))+
  ggplot2::ggplot(aes(x=predicted, y=residual))+
  geom_abline(slope = 0, intercept = 0)+
  geom_point()+
  theme_bw()

# histogram of the residuals
ds %>% TabularManifest::histogram_continuous(
  variable_name = "residual" )



# ---- basic-graph --------------------------

# ---- define-utility-functions ---------------


# ---- examine-created-output ----------------
source("./scripts/mplus/mplus.R") # downloaded from http://www.statmodel.com/mplus-R/mplus.R
path_gh5 <- "./data-unshared/raw/model-final-2017-07-05.gh5"

# view options: https://www.statmodel.com/mplus-R/GH5_R.shtml

mplus.list.variables(path_gh5) # variables in the gh5 file
mplus.view.plots(path_gh5)  # available graphs for this type of gh5 file
# histograms
mplus.plot.histogram(path_gh5, "LINEAR") #
mplus.plot.histogram(path_gh5, "QUAD") #
# scatterplots
mplus.plot.scatterplot(path_gh5, "B_FAT", "LINEAR") 
# mplus.plot.scatterplot(path_gh5, "SA", "SB")
# mplus.plot.scatterplot(path_gh5, "IA", "SA") 
# mplus.plot.scatterplot(path_gh5, "IB", "SB") 

# ds <- mplus.get.data(path_gh5, "SA")

summary(ds)
head(ds)

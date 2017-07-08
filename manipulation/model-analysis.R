rm(list=ls(all=TRUE)) #Clear the memory of variables from previous run. This is not called by knitr, because it's above the first chunk.

# ---- load-sources ------------------------------------------------------------
source("./scripts/functions-graphs.R")
source("./scripts/functions-tables.R")
source("./scripts/graph-presets.R") # pre-sets and options for graphing

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

# ---- tweak-data ----------------------------------------



# ---- define-utility-functions ---------------


# ---- examine-created-output ----------------
source("./scripts/mplus/mplus.R") # downloaded from http://www.statmodel.com/mplus-R/mplus.R
path_gh5 <- "./data-unshared/raw/model-final-2017-07-05.gh5"

# view options: https://www.statmodel.com/mplus-R/GH5_R.shtml

mplus.list.variables(path_gh5) # variables in the gh5 file
mplus.view.plots(path_gh5)  # available graphs for this type of gh5 file
# histograms
# mplus.plot.histogram(path_gh5, "SA") # 
# scatterplots
# mplus.plot.scatterplot(path_gh5, "IA", "IB") # intercepts
# mplus.plot.scatterplot(path_gh5, "SA", "SB") # slopes
# mplus.plot.scatterplot(path_gh5, "IA", "SA") # physical
# mplus.plot.scatterplot(path_gh5, "IB", "SB") # cognitive

# ds <- mplus.get.data(path_gh5, "SA")

summary(ds)
head(ds)

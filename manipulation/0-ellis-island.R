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
path_folder <- "./data-unshared/raw"
path_stencil      <- "./data-public/raw/table-stencil.csv"
# baseSize <- 12

# ---- load-data ---------------------------------------------------------------
path_outputs <- list.files(path_folder,pattern = ".out$",full.names = T, recursive = T)
stencil <- readr::read_csv(path_stencil) # shorter names


## ---- define-utility-functions ---------------
# formatting functions to remove leading zero
numformat <- function(val) { sub("^(-?)0.", "\\1.", sprintf("%.2f", val)) }

#extract table of parameters from the output of  MplusAutomation::readModels()
get_estimate_table <- function(
  lst # list object, product of MplusAutomation::readModels()
  ,stencil # a stencil used for this parsing (might change with study)
){
  # lst <- model_result
  d1 <- lst[["parameters"]][["unstandardized"]]
  # d2 <- stencil %>% 
  d2 <- d1 %>% 
    # dplyr::left_join(d1,by=c("paramHeader","param")) %>% 
    dplyr::mutate(
      est_pretty  = numformat( est),
      se_pretty   = numformat( se),
      pval_pretty = ifelse(pval<.001,"<.001",numformat(pval)),
      dense = sprintf("%4s(%4s), %5s",est_pretty, se_pretty, pval_pretty),
      dense = ifelse(is.na(est),"",dense)
    )
  return(d2)
}

parse_outputs <- function(
  paths
  ,stencil
){
  # Values for testing and development
  # paths <- path_outputs[1]
  # stencil <- stencil
  
  ls_catalog <- list()
  # regex_1 <- "(u0|u1|u2|b0|b1|b2)_(\\w+)_(\\w+)_(\\w+)_(\\w+)"
  for(i in seq_along(paths)){
    # i <- 1
    model_name <- gsub(".out$","",basename(paths[i]))
    model_result <- MplusAutomation::readModels(paths[i])
    if(length(model_result$errors)==0L){
      ls_temp <- list(
        # "model_number" =  gsub(regex_1, "\\1", model_name),
        # "subgroup"     =  gsub(regex_1, "\\2", model_name),
        # "model_type"   =  gsub(regex_1, "\\3", model_name),
        # "process_a"    =  gsub(regex_1, "\\4", model_name),
        # "process_b"    =  gsub(regex_1, "\\5", model_name),
        "table"        =  get_estimate_table(model_result, stencil),
        "N"            = model_result$summaries$Observations,
        "parameters"   = model_result$summaries$Parameters,
        "AIC"          = model_result$summaries$AIC,
        "BIC"          = model_result$summaries$BIC,
        "path"         = paths[i]
      )
    } else{
      ls_temp <- list(
        # "model_number" =  gsub(regex_1, "\\1", model_name),
        # "subgroup"     =  gsub(regex_1, "\\2", model_name),
        # "model_type"   =  gsub(regex_1, "\\3", model_name),
        # "process_a"    =  gsub(regex_1, "\\4", model_name),
        # "process_b"    =  gsub(regex_1, "\\5", model_name),
        "table"        = NA,
        "N"            = NA,
        "parameters"   = NA,
        "AIC"          = NA,
        "BIC"          = NA,
        "path"         = NA
      )
    }
    ls_catalog[[model_name]] <- ls_temp
  }
  return(ls_catalog)
}

# ---- assemble-catalog -------------------------------
# assemble the list catalog by running parsing functions
ls_catalog <- parse_outputs(path_outputs, stencil_octo)
# after parsing all outputs, you might want to save the catalog for faster access later
saveRDS(ls_catalog,"./data-public/derived/ls_catalog.rds")
# ls_catalog <- readRDS("./data-public/derived/ls_catalog.rds")

# Explore catalog
names(ls_catalog)
# View the names of the components in the first element of the list
names(ls_catalog[[1]])
# print the contents of the first element
ls_catalog[["model-final-2017-07-05"]]

# ---- flatten-catalog -------------------------
# convert list object into a single flat dataframe
ds_catalog <- plyr::ldply(ls_catalog, data.frame, .id = "model_name")
names(ds_catalog) <- gsub("^table.","",names(ds_catalog))

# ---- save-to-disk ----------------------------
# save for faster recall later
saveRDS(ds_catalog,"./data-public/derived/ds_catalog.rds")
# ds_catalog <- readRDS("./data-public/derived/catalog.rds")

model_result <- MplusAutomation::readModels(path_outputs[1])
saveRDS(model_result, "./data-unshared/derived/model_result.rds")



# ---- examine-created-output ----------------
source("./scripts/mplus/mplus.R") # downloaded from http://www.statmodel.com/mplus-R/mplus.R
path_gh5 <- "./sandbox/syntax-creator/outputs/grip-mmse/male_5.gh5"

# view options: https://www.statmodel.com/mplus-R/GH5_R.shtml

mplus.list.variables(path_gh5) # variables in the gh5 file
mplus.view.plots(path_gh5)  # available graphs for this type of gh5 file
# histograms
mplus.plot.histogram(path_gh5, "SA") # slope of process A
mplus.plot.histogram(path_gh5, "SB") # slope of process B
# scatterplots
mplus.plot.scatterplot(path_gh5, "IA", "IB") # intercepts
mplus.plot.scatterplot(path_gh5, "SA", "SB") # slopes
mplus.plot.scatterplot(path_gh5, "IA", "SA") # physical
mplus.plot.scatterplot(path_gh5, "IB", "SB") # cognitive

ds <- mplus.get.data(path_gh5, "SA")

summary(ds)
head(ds)

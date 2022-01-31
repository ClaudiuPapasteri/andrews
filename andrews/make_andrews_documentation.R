if (!require(document)) install.packages("document")
if (!require(here)) install.packages("here")
library(document)
library(here)

folder <- here::here()
sourcefile <- "andrews.R"
setwd(folder)

document::document(file.path(folder, sourcefile), working_directory = folder, output_directory = folder)

name <- sub(".R", "", sourcefile) 
rdfile <- paste0(name, ".Rd")

docpath <- file.path(folder, name, "man")  
rstudioapi::previewRd(file.path(docpath, rdfile)) # to preview 

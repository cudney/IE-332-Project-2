# Read the file 'README.md' before you start

# LOAD PACKAGES
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(rstudioapi)

# SET WORKING DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ADD SCRIPTS
source("./script.R")

# RESET TEST PHOTOS FOR ALGORITHM TESTING
reset_test_images()

# --- EDIT BELOW THIS LINE ---




# --- EDIT ABOVE THIS LINE ---

# CHECK TEST PHOTOS
check_test_images()
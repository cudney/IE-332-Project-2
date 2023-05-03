# Read the file 'README.md' before you start

# LOAD PACKAGES
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(rstudioapi)
library(knitr)
library(imager)
library(graphics)
library(repr)

# SET WORKING DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# ADD SCRIPTS
source("./script.R")

# RESET TEST PHOTOS FOR ALGORITHM TESTING
reset_test_images()

# --- EDIT BELOW THIS LINE ---
FGSM <- function(type, file_name, epsilon){
  
  #load model
  model2 <- keras::application_mobilenet_v2(weights = "imagenet")
  knitr::knit_engines$set(python = reticulate::eng_python) 
  
  # Preprocess input data
  x <- image_load(paste("./test_photos/",type,"/",file_name,".jpg",sep = ""), target_size = c(224, 224))
  x <- image_to_array(x)
  x <- array_reshape(x, c(1, dim(x)))
  x <- tf$convert_to_tensor(x, dtype = "float32") #change x to tensor
  
  # Predict first using the model
  pred <- model2(x)
  pred_shape <- tf$shape(pred)
  pred <- tf$cast(pred, "float32")
  
  #label
  labrador_retriever_index <- 208
  label <- k_one_hot(as.integer(labrador_retriever_index), tf$shape(pred)[-1])
  label <- tf$reshape(label, shape = c(as.integer(1), tf$shape(pred)[-1]))
  
  #get the gradient
  with(tf$GradientTape() %as% tape, {
    tape$watch(x)
    pred <- model2(x)
    loss <- k_categorical_crossentropy(label, pred)
    grads <- tape$gradient(loss,x)
  })
  
  #get the sign of gradient
  signed <- tf$sign(grads)
  
  #let's get the perturbations
  perturbations <- signed
  
  #set the epsilons and add the perturbations
  epsilon <- epsilon
  adv_x <- x + epsilon * perturbations
  
  #plot image
  adv_x_matrix <- matrix(adv_x, nrow=dim(adv_x)[1]*dim(adv_x)[2], ncol=dim(adv_x)[3])
  adv_x_2d <- (adv_x_matrix * 0.5) + 0.5
  #to change the maragin
  par(mar=c(1,1,1,1))
  a <- image(adv_x_2d)

}

# --- EDIT ABOVE THIS LINE ---

# CHECK TEST PHOTOS
check_test_images()

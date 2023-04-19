# DO NOT MODIFY THIS FILE

# Load packages
library(tidyverse)
library(keras)
library(tensorflow) 
library(reticulate)

# function to reset test images
reset_test_images <- function() {
    source_folder <- "./original_photos"
    destination_folder <- "./test_photos"

    # Create destination folder if it doesn't exist
    if (!dir.exists(destination_folder)) {
        dir.create(destination_folder)
    }

    # Copy files from source to destination
    for (f in list.files(source_folder)) {
        file.copy(from = file.path(source_folder, f), to = destination_folder, overwrite = TRUE, recursive = TRUE)
    }
}

# initialize model
model <- load_model_tf("./dandelion_model")

# function to check if an image is a dandelion or grass
check_image <- function(type, file_name) {
    test_image <- image_load(paste("./test_photos/", type, "/", file_name, sep = ""), target_size = c(224, 224))
    x <- image_to_array(test_image)
    x <- array_reshape(x, c(1, dim(x)))
    x <- x / 255
    pred <- model %>% predict(x)
    print(pred)

    print(paste("File Name:", file_name))
    print(paste("Actual Type:", type))
    if (pred[1, 1] < 0.50) {
        print("Predicted Type: 'grass'")
        print("With Confidence:")
        print(pred[1, 2])
    } else if (pred[1, 2] < 0.50) {
        print("Predicted Type: 'dandelion'")
        print("With Confidence:")
        print(pred[1, 1])
    } else {
        print("ERROR")
    }
}

check_test_images <- function() {
    for (type in c("grass", "dandelions")) {
        for (file in list.files(paste("./test_photos/", type, sep = ""))) {
            check_image(type, file)
        }
    }
}
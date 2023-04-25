# DO NOT MODIFY THIS FILE

# Load packages
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(R6)

# initialize model
model <- load_model_tf("./dandelion_model")

# initialize vector of all test images
all_images <- c()
for (type in c("grass", "dandelions")) {
    for (file in list.files(paste("./test_photos/", type, sep = ""))) {
        all_images <- append(all_images, Image$new(type, file))
    }
}


# Image class
Image <- R6Class("Image",
    public = list(
        type = NULL,
        file_name = NULL,
        file_path = NULL,
        img = NULL,

        # constructor
        initialize = function(type, file_name) {
            self$type <- type
            self$file_name <- file_name
            self$file_path <- paste("./test_photos/", type, "/", file_name, sep = "")
            self$img <- image_load(self$file_path)
        },

        # function to check if an image is a dandelion or grass
        check = function() {
            test_image <- image_load(self$file_path, target_size = c(224, 224))
            x <- image_to_array(test_image)
            x <- array_reshape(x, c(1, dim(x)))
            x <- x / 255
            pred <- model %>% predict(x)
            print(pred)

            print(paste("File Name:", self$file_name))
            print(paste("Actual Type:", self$type))
            if (pred[1, 1] < 0.50) {
                print("Predicted Type: 'grass'")
                print("With Confidence:")
                print(pred[1, 2])
            } else if (pred[1, 2] < 0.50) {
                print("Predicted Type: dandelions")
                print("With Confidence:")
                print(pred[1, 1])
            } else {
                print("ERROR")
            }
        }
    )
)

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

# function to check test images
check_test_images <- function() {
    for (image in all_images) {
        image$check()
    }
}
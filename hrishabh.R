# Load packages
library(tidyverse)
library(keras)
library(tensorflow)
library(reticulate)
library(R6)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Image class
Image <- R6Class("Image",
    public = list(
        type = NULL,
        file_name = NULL,
        original_path = NULL,
        test_path = NULL,
        img = NULL,

        # constructor
        initialize = function(type, file_name) {
            self$type <- type
            self$file_name <- file_name
            self$original_path <- paste("./original_photos/", type, "/", file_name, sep = "")
            self$test_path <- paste("./test_photos/", type, "/", file_name, sep = "")
        },

        # open test image
        open_test = function() {
            system(paste("open 'test_photos/", self$type, "/", self$file_name, "'", sep = ""))
        },
        open_original = function() {
            system(paste("open 'original_photos/", self$type, "/", self$file_name, "'", sep = ""))
        },

        # function to check if an image is a dandelion or grass
        check = function() {
            test_image <- image_load(self$test_path, target_size = c(224, 224))
            x <- image_to_array(test_image)
            x <- array_reshape(x, c(1, dim(x)))
            x <- x / 255
            pred <- model %>% predict(x)

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

            csv_data <- c(paste('"', self$file_name, '"', sep = ""), self$type)

            if (pred[1, 1] < 0.5) {
                csv_data <- append(csv_data, "grass")
                csv_data <- append(csv_data, pred[1, 2])
                if (self$type == "grass") {
                    csv_data <- append(csv_data, "FALSE")
                } else {
                    csv_data <- append(csv_data, "TRUE")
                }
            } else if (pred[1, 2] < 0.5) {
                csv_data <- append(csv_data, "dandelions")
                csv_data <- append(csv_data, pred[1, 1])
                if (self$type == "dandelions") {
                    csv_data <- append(csv_data, "FALSE")
                } else {
                    csv_data <- append(csv_data, "TRUE")
                }
            } else {
                csv_data <- append(csv_data, "ERROR")
                csv_data <- append(csv_data, 0)
                csv_data <- append(csv_data, "FALSE")
            }

            csv_data <- append(csv_data, "\n")
            csv_data <- paste(csv_data, collapse = ",")

            return(csv_data)
        }
    )
)

# initialize model
model <- load_model_tf("./dandelion_model")

# initialize vector of all test images
all_images <- c()
for (type in c("grass", "dandelions")) {
    for (file in list.files(paste("./test_photos/", type, sep = ""))) {
        all_images <- append(all_images, Image$new(type, file))
    }
}

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
    csv_string <- "file_name,actual_type,predicted_type,confidence,succesful_attack\n"

    for (image in all_images) {
        csv_string <- paste(csv_string, image$check(), sep = "")
    }

    write(csv_string, file = "./results.csv")
}

# script
reset_test_images()

# algorithm
library(magick)

launch_attack <- function(weights = c(0.2, 0.2, 0.2, 0.2, 0.2)) {
    for (image in all_images) {
        img <- image_read(image$original_path)

        # add color filter
        img <- image_colorize(img, weights[1] * 50, "red")

        # add grayscale filter
        img <- image_colorize(img, weights[2] * 50, "gray")

        # add noise
        img <- image_noise(img, "gaussian")

        # add blur
        img <- image_blur(img, 25, 2)

        # add emboss
        img <- image_emboss(img, 25, 2)

        # save image
        image_write(img, image$test_path)
        print(paste("Modified Image:", image$file_name))
    }
}

launch_attack()

# check results
check_test_images()
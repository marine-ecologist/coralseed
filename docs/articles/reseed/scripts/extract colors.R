# Load required libraries
library(imager)
library(dplyr)

# Load the image
image_path <- "/Users/rof011/Downloads/ct5km_dhw_v3.1_20240427.png" # Replace with the path to your image
image <- load.image(image_path)

# Convert the image to a data frame
image_df <- as.data.frame(image, wide = "c") # wide = "c" extracts RGB channels

# Rename RGB columns for convenience
colnames(image_df)[colnames(image_df) %in% c("c.1", "c.2", "c.3")] <- c("R", "G", "B")

# Count unique colors
unique_colors <- image_df %>%
  select(R, G, B) %>%
  distinct() %>%
  count()

# Print the result
cat("Number of unique colors in the image:", unique_colors$n, "\n")


html_colors <- image_df %>%
  select(R, G, B) %>%
  distinct() %>%
  mutate(
    R = round(R * 255),  # Scale RGB values from 0-1 to 0-255
    G = round(G * 255),
    B = round(B * 255),
    HTML = sprintf("#%02X%02X%02X", R, G, B)  # Convert to hex color code
  )

# View the HTML color codes
head(html_colors$HTML)


c(NA, 1, NA,
[1] "#FFFFFF" "#C8FAFA" "#505050" "#780078" "#320032" "#463278" "#C800C8" "#645096" "#FFB900" "#A00000" "#D20000" "#826EB4" "#552D14" "#A000A0" "#FFDC00" "#E67D46" "#A08CD2" "#B45A28" "#FFFF00"
[20] "#FF9600" "#F000F0" "#7D3C1E" "#6E0000" "#FF0000"


# Load the image
image_path <- "/Users/rof011/Downloads/coraltemp_v3.1_19980103.png" # Replace with the path to your image
image <- load.image(image_path)

# Convert the image to a data frame
image_df <- as.data.frame(image, wide = "c") # wide = "c" extracts RGB channels

# Rename RGB columns for convenience
colnames(image_df)[colnames(image_df) %in% c("c.1", "c.2", "c.3")] <- c("R", "G", "B")

# Count unique colors
unique_colors <- image_df %>%
  select(R, G, B) %>%
  distinct() %>%
  count()

# Print the result
cat("Number of unique colors in the image:", unique_colors$n, "\n")


html_colors <- image_df %>%
  select(R, G, B) %>%
  distinct() %>%
  mutate(
    R = round(R * 255),  # Scale RGB values from 0-1 to 0-255
    G = round(G * 255),
    B = round(B * 255),
    HTML = sprintf("#%02X%02X%02X", R, G, B)  # Convert to hex color code
  )

# View the HTML color codes
head(html_colors)

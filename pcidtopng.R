# Ensure necessary libraries are loaded
library(httr)
library(magick)

# Function to download and format PNG
download_and_format_png <- function(cid, output_dir, width = NULL, height = NULL) {
  url <- paste0("https://pubchem.ncbi.nlm.nih.gov/rest/pug/compound/cid/", cid, "/PNG")

  response <- GET(url)

  if (status_code(response) == 200) {
    file_path <- file.path(output_dir, paste0(cid, ".png"))
    writeBin(content(response, "raw"), file_path)

    # Read the image
    img <- image_read(file_path)

    # Resize the image if width and height are provided
    if (!is.null(width) && !is.null(height)) {
      img <- image_resize(img, paste0(width, "x", height))
    }

    # Add border and set background color
    img <- image_border(img, color = "black", geometry = "3x3", operator = "copy")
    img <- image_background(img, color = "white", flatten = TRUE)
    img <- image_transparent(img, color = "white", fuzz = 0)


     # Save the formatted image
    image_write(img, path = file_path, format = "png")

    print(paste("Downloaded and formatted PNG for CID", cid))
  } else {
    print(paste("Failed to download PNG for CID", cid, ", status code:", status_code(response)))
  }
}

# Function to handle multiple CIDs
download_and_format_pngs <- function(cids, output_dir, width = NULL, height = NULL) {
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE)  # Create the directory if it doesn't exist
  }

  for (cid in cids) {
    download_and_format_png(cid, output_dir, width, height)
  }
}

# Define your CIDs and output directory

# selected =  "sertraline hydrochloride"
# welke=which(pelke$cmp%in% c(selected,"tetrabenazine" , "zardaverine","ritanserin","parthenolide" ))
welke=c(60, 63, 74, 82, 86)
# cids <- pelke$pcid[welke]

cids <- readRDS("24Jan8/pcids.RDS")
#cids=cids[welke]

output_dir <- "/Users/gurigiaever/Dropbox/2024_July7_COVID19/www/images"
width <- 200  # Desired width (optional)
height <- 200  # Desired height (optional)

# Download and format PNGs
download_and_format_pngs(cids, output_dir, width, height)

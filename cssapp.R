library(shiny)
library(DT)

# Helper function to generate img src tag
generate_img_tag <- function(path, border = "3px solid black", width = "200px", height = "200px") {
  img_tag <- sprintf(
    '<img src="%s" style="border: %s; width: %s; height: %s;">',
    path, border, width, height
  )
  return(img_tag)
}

# Define UI
ui <- fluidPage(
  titlePanel("Chemical Structure Images"),
  DTOutput("datatable")
)

# Define server logic
server <- function(input, output) {
  # Create a data frame with CIDs and corresponding image paths
  cids <- c(443939, 65348, 62770, 36462, 23663976)
  image_paths <- sapply(cids, function(cid) {
    paste0("hipimg/", cid, ".png")
  })

  # Use the helper function to generate the img tags
  img_tags <- sapply(image_paths, function(path) {
    generate_img_tag(path)
  })

  # Print the generated img tags for debugging
  print(img_tags)

  data <- data.frame(
    CID = cids,
    Image = img_tags,
    stringsAsFactors = FALSE
  )

  # Render the DataTable
  output$datatable <- renderDT({
    datatable(data, escape = FALSE)
     })
      #         , options = list(
      # columnDefs = list(list(targets = 1, render = JS(
      #   "function(data, type, row, meta) {",
      #   "return type === 'display' ? data : data;",
      #   "}")
#       ))
#     ))
#   })
 }

# Run the application
shinyApp(ui = ui, server = server)

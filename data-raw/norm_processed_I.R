norm_data <- read.csv(file = 'data-raw/norm_data.csv', header = TRUE, stringsAsFactors = FALSE)
devtools::use_data(norm_data)

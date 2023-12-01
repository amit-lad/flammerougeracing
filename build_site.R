library(here)



rmarkdown::render_supporting_files(from = "www", files_dir = "_site")
rmarkdown::render("s3_tour_france.Rmd", output_file = "_site/s3_France.html")

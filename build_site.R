library(here)



rmarkdown::render_supporting_files(from = "www", files_dir = "_site")
rmarkdown::render("s3_tour_france.Rmd", output_file = "_site/s3_tour_france.html")
rmarkdown::render("s3_tour_britannia.Rmd", output_file = "_site/s3_tour_britannia.html")
rmarkdown::render("s3_tour_watopia.Rmd", output_file = "_site/s3_tour_watopia.html")

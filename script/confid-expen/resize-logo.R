library(magick)

path <- "image/logo/gov"

dirs <- list.files(path, full.names = TRUE, recursive = TRUE)

file.size(dirs)

lapply(dirs, function(imgdir){
  new_dir <- sub("gov", "gov_res", dirname(imgdir))
  if(!dir.exists(new_dir)) dir.create(new_dir, recursive = TRUE)
  image_read(imgdir) |> 
    image_scale("100") |>  # width: 100px
    image_write(paste0(new_dir, "/", basename(imgdir)))
})


tic()
savetest <- mapdeck() %>% 
  add_polygon( data = seascape |> sf::st_transform(4326), fill_colour = "class") |> 
  add_scatterplot( data = particles |> sf::st_transform(4326), fill_colour = "dispersaltime",  palette = "plasma")
toc()


htmlwidgets::saveWidget(savetest, "savetest.html")

tic()
library(tmap)
savetestmap <- tm_shape(seascape) +
  tm_fill("class") +
tm_shape(particles) +
  tm_dots("dispersaltime")
toc()


htmlwidgets::saveWidget(savetestmap, "savetestmap.html")

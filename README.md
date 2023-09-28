# Malla-Bricks-
Este repositorio promueve el uso de información geográfica. Proporcionamos una malla espacial referente a marcos geoestadísticos, permitiendo generar mallas a medida. Fomentamos la transparencia, colaboración y uso efectivo de datos geográficos. ¡Contribuye y comparte tus hallazgos!

library(sf)
library(raster)

### Cargar la capa poligonal
capa_poligono <- st_read("tu_direccion/poligono.shp")  # Reemplaza con la ruta de tu archivo

### Obtener los límites de la capa poligonal
limites <- st_bbox(capa_poligono)

### Definir la resolución de la malla
resolucion <- 1000  # 1 km = 1000 m

### Crear una cuadrícula regular con la resolución deseada
malla <- raster::rasterToPolygons(raster::raster(extent(limites), resolution = resolucion))
malla <- st_as_sf(malla)

### Agregamos el sistema de referencia a la malla
malla <- st_set_crs(malla, 32717)
centroides <- st_centroid(malla)
coords <- st_coordinates(centroides) %>% as.data.frame()
### Crear las columnas separadas de latitud y longitud
malla$X <- coords[, "Y"]
malla$Y <- coords[, "X"]
num_letras <- length(unique(malla$Y)) 

### Función para convertir un número en letras según el sistema de numeración de Excel
num_to_letras <- function(num) {
  div <- num %/% 26
  mod <- num %% 26
  if (mod == 0) {
    mod <- 26
    div <- div - 1
  }
  if (div > 0) {
    return(paste0(num_to_letras(div), LETTERS[mod]))
  } else {
    return(LETTERS[mod])
  }
}

### Crear el vector de letras siguiendo el sistema de numeración de Excel
codigo_x <- sapply(1:num_letras, num_to_letras)
codigo_x <- rep(codigo_x, times = length(unique(malla$X)))
num_y=length(unique(malla$X))
rm(codigo_y,codigo_y1)
while (num_y>0) {
  codigo_y1 <- rep(num_y,length(unique(malla$Y)))
  num_y=num_y-1
  if(exists("codigo_y")){
    codigo_y=c(codigo_y,codigo_y1)
  }else{
    codigo_y=codigo_y1
  }
}

### Agregar los códigos únicos a la malla
### Editar en caso de generar una malla para la zona insular 
malla$COD <- paste0("C+",codigo_x, codigo_y)

### Guardar la capa 
### cambiar la direccion de salida
st_write(malla,"direccion_de_salida/Malla_salida.shp")


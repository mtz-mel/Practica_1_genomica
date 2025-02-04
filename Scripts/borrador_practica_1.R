################ Abundancia de subsecuencias en Genes ##########################

## Cargar la librería necesaria

library(Biostrings)

## Los datos de la práctica se transformaron a formato fasta para que fuera más
## facil su trabajo en biostrings.

genes <- readDNAStringSet("Datos/genes.FASTA")
print(genes)

subsecuencias <- readDNAStringSet("Datos/subsecuencias.FASTA")
print(subsecuencias)

################################################################################

## Recopilamos en una base de datos la cantidad de subsecuencias diferentes que 
## había en un gen, sin importar la cantidad de veces que aparece una misma subsecuencia
## en el mismo, es decir si aparece 1 vez o 10 veces las coincidencias serán igual a 1.

## 1. Crear un data frame para almacenar los resultados

abundancia_subsecuencias_unicas <- data.frame(
  Gen = character(), 
  Subsecuencias_unicas_por_gen = integer()
)


## 2. Ciclo for para recorrer cada gen, en busca de la subsecuencia:

for (nombre_gen in names(genes)) {
  contador_subsecuencias_unicas <- 0  ## Contador iniciado en 0
  
  ## Recorrer cada subsecuencia
  for (nombre_subsecuencia in names(subsecuencias)) {
    ## Conteo de cuántas veces aparece la subsecuencia en el gen
    conteo <- countPattern(subsecuencias[[nombre_subsecuencia]], genes[[nombre_gen]])
    
    ## Si la subsecuencia aparece al menos una vez, aumentar el contador (solo se
    ## toma en cuenta si aparece en el gen, no cuantas veces lo hace)
    if (conteo > 0) {  
      contador_subsecuencias_unicas <- contador_subsecuencias_unicas + 1  
    }
  }
  
## 3. Guardar los resultados del conteo en el data frame
  abundancia_subsecuencias_unicas <- rbind(
    abundancia_subsecuencias_unicas, 
    data.frame(Gen = nombre_gen, Subsecuencias_unicas_por_gen = contador_subsecuencias_unicas)
  )
}

################################################################################

## Gráfica

################################################################################

## Después recopilamos en otra base de datos la cantidad de subsecuencias diferentes que 
## había en un gen, tomando en cuenta el numero de veces que aparece una misma subsecuencia
## en el mismo.


# 1. Crear otro data frame para almacenar los resultados
abundancia_subsecuencias_totales <- data.frame(
  Gen = character(), 
  Total_subsecuencias_por_gen = integer()
)

## 2. Ciclo for para recorrer cada gen, en busca de la subsecuencia:
for (nombre_gen in names(genes)) {
  contador_subsecuencias_totales <- 0  
  
  ## Recorrer cada subsecuencia
  for (nombre_subsecuencia in names(subsecuencias)) {
    ## Conteo de cuántas veces aparece la subsecuencia en el gen
    conteo <- countPattern(subsecuencias[[nombre_subsecuencia]], genes[[nombre_gen]])
    
    ## Sumar el total de coincedencias, aunque sea una misma subsecuencia
    contador_subsecuencias_totales <- contador_subsecuencias_totales + conteo  
  }
  
## Guardar el resultado de la suma del conteo en el data frame
  abundancia_subsecuencias_totales <- rbind(
    abundancia_subsecuencias_totales, 
    data.frame(Gen = nombre_gen, Total_subsecuencias_por_gen = contador_subsecuencias_totales)
  )
}

################################################################################

## Gráfica

################################################################################
## Finalmente decidimos hacer otro data frame, para identificar que genes contiene
## cada subsecuencia y de este modo saber, la diferencia entre una subsecuencia larga 
## y una subsecuencia más corta.

## 1. Crear un data frame para almacenar los resultados

resultado_subsecuencias_genes <- data.frame(
  Subsecuencia = character(), 
  Genes = character()
)

## Repetimos los mismos pasos 
for (nombre_subsecuencia in names(subsecuencias)) {
  genes_con_subsecuencia <- c()  ## En vez de un contador, creamos una lista, la cual
  ## va a contener los genes que tienen esa subsecuencia.
  
  for (nombre_gen in names(genes)) {
    conteo <- countPattern(subsecuencias[[nombre_subsecuencia]], genes[[nombre_gen]])
    
    ## Si hay al menos una coincidencia de la subsecuencia en el gen, el gen será
    ## agregado a la lista de la subsecuencia.
    
    if (conteo > 0) {  
      genes_con_subsecuencia <- c(genes_con_subsecuencia, nombre_gen)
    }
  }
  
  ## Guardar el resultado de la lista en en el data frame
  resultado_subsecuencias_genes <- rbind(
    resultado_subsecuencias_genes, 
    data.frame(
      Subsecuencia = nombre_subsecuencia, 
      Genes = paste(genes_con_subsecuencia, collapse = ", ")
    )
  )
}

################################################################################

## No se si de este también poner gráfica

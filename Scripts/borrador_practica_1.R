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
## en el mismo.

## 1. Crear un data frame para almacenar los resultados

abundancia_subsecuencias_unicas <- data.frame(
  Gen = character(), 
  Subsecuencia_Unica = integer()
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
    data.frame(Gen = nombre_gen, Subsecuencia_Unica = contador_subsecuencias_unicas)
  )
}


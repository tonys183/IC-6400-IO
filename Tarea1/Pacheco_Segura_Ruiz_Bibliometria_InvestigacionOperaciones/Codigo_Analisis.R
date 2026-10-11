# -------------------------------
# INSTALACIÓN DE PAQUETES
# -------------------------------
# install.packages(c("remotes","igraph","ggplot2","ggrepel","stringr"))
# remotes::install_version("bibliometrix", version = "4.3.0", dependencies = TRUE, upgrade = "never")


# ===============================
# 1. CONFIGURACIÓN GENERAL
# ===============================
CONFIG <- list(
  semilla = 2026L,
  fecha_consulta = "2026-09-10",
  anio_inicio = 2000L,
  anio_final = 2017L,
  registros_pubmed_esperados = 133L,
  documentos_analisis_esperados = 77L,
  frecuencia_minima_mesh = 5L,
  minimo_coocurrencias_visibles = 3L
)
RUTA_FIGURAS <- "Figuras"
RUTA_ANEXOS <- "Anexos"
ARCHIVO_PUBMED <- file.path(RUTA_ANEXOS, "PubMed_Original.txt")
ARCHIVO_CSV <- "Base_Datos_Bibliografica.csv"
ARCHIVO_REGISTRO <- file.path(RUTA_ANEXOS, "Registro_Ejecucion.txt")
COLOR_BASE <- "#163B5C"
set.seed(CONFIG$semilla)


# ===============================
# 2. CARGA DE PAQUETES
# ===============================
suppressPackageStartupMessages({
  library(bibliometrix)
  library(igraph)
  library(ggplot2)
})


# ===============================
# 3. FUNCIONES AUXILIARES
# ===============================
# Separa términos almacenados en una misma celda con punto y coma.
# También elimina espacios, valores vacíos y términos repetidos.
separar_terminos <- function(vector) {
  lapply(vector, function(valor) {
    if (length(valor) == 0L || is.na(valor) || !nzchar(trimws(valor))) {
      return(character(0))
    }

    terminos <- trimws(strsplit(valor, split = ";", fixed = TRUE)[[1]])
    terminos <- terminos[nzchar(terminos)]

    unique(terminos)
  })
}

# Calcula la cantidad de documentos en los que aparece cada elemento.
# Un término o autor se cuenta una sola vez por documento.
crear_tabla_frecuencias <- function(vector) {
  frecuencias <- sort(table(unlist(separar_terminos(vector))), decreasing = TRUE)

  data.frame(
    Elemento = names(frecuencias),
    Documentos = as.integer(frecuencias),
    row.names = NULL
  )
}

# Guarda tablas CSV utilizando codificación UTF-8.
guardar_csv <- function(datos, archivo, incluir_filas = FALSE) {
  write.csv(datos, file = archivo, row.names = incluir_filas, na = "", fileEncoding = "UTF-8")
}


# ===============================
# 4. PROGRAMA PRINCIPAL
# ===============================
ejecutar_analisis <- function() {
  # -------------------------------
  # 4.1. Crear carpetas de resultados
  # -------------------------------
  dir.create(RUTA_FIGURAS, showWarnings = FALSE, recursive = TRUE)
  dir.create(RUTA_ANEXOS, showWarnings = FALSE, recursive = TRUE)

  # -------------------------------
  # 4.2. Iniciar el registro de ejecución
  # -------------------------------
  sink(ARCHIVO_REGISTRO, split = TRUE)
  registro_abierto <- TRUE

  on.exit({
    if (registro_abierto && sink.number(type = "output") > 0L) {
      sink(type = "output")
    }
  }, add = TRUE)
  cat("Fecha de consulta de datos:", CONFIG$fecha_consulta, "\n")
  cat("Inicio del análisis:", format(Sys.time()), "\n")

  # -------------------------------
  # 4.3. Importar los registros originales de PubMed
  # -------------------------------
  M0 <- convert2df(ARCHIVO_PUBMED, dbsource = "pubmed", format = "pubmed")

  # -------------------------------
  # 4.4. Importar la base depurada del análisis
  # -------------------------------
  D <- read.csv(
    ARCHIVO_CSV, stringsAsFactors = FALSE, check.names = FALSE,
    na.strings = c("", "NA"), fileEncoding = "UTF-8"
  )
  D$PY <- suppressWarnings(as.integer(D$PY))

  # -------------------------------
  # 4.5. Relacionar el CSV depurado con la descarga original
  # -------------------------------
  indice <- match(as.character(D$PMID), as.character(M0$PMID))
  M <- M0[indice, , drop = FALSE]

  # -------------------------------
  # 4.6. Incorporar los campos depurados del CSV
  # -------------------------------
  for (nombre_columna in names(D)) { M[[nombre_columna]] <- D[[nombre_columna]] }
  M$TC <- NA_real_

  # Crear una identificación única para cada referencia.
  M$SR <- make.unique(paste(sub(";.*", "", M$AU), M$PY, M$J9, sep = ", "))
  rownames(M) <- M$SR
  guardar_csv(M, file.path(RUTA_ANEXOS, "Matriz_Bibliometrix.csv"))

  # -------------------------------
  # 4.7. Ejecutar el análisis bibliométrico general
  # -------------------------------
  BA <- biblioAnalysis(M, sep = ";")
  saveRDS(BA, file.path(RUTA_ANEXOS, "Analisis_Bibliometrix.rds"))

  # -------------------------------
  # 4.8. Crear tablas descriptivas
  # -------------------------------
  annual <- data.frame(
    Anio = CONFIG$anio_inicio:CONFIG$anio_final,
    Documentos = as.integer(table(factor(M$PY, levels = CONFIG$anio_inicio:CONFIG$anio_final)))
  )

  sources <- crear_tabla_frecuencias(M$SO)
  authors <- crear_tabla_frecuencias(M$AU)
  keywords <- crear_tabla_frecuencias(M$ID)

  tablas_descriptivas <- list( annual = annual, sources = sources, authors = authors, keywords = keywords )
  for (nombre_tabla in names(tablas_descriptivas)) {
    guardar_csv(
      tablas_descriptivas[[nombre_tabla]],
      file.path(RUTA_ANEXOS, paste0("Tabla_", nombre_tabla, ".csv"))
    )
  }
  print(annual)
  print(head(sources, 10))
  print(head(authors, 25))
  print(head(keywords, 20))

  # -------------------------------
  # 4.9. Crear la figura de producción anual
  # -------------------------------
  theme_set(theme_minimal(base_size = 14))
  limite_y <- max(9L, max(annual$Documentos, na.rm = TRUE) + 1L)
  figura_anual <- ggplot(annual, aes(x = Anio, y = Documentos)) +
    geom_col(fill = COLOR_BASE, width = 0.7) +
    geom_text(aes(label = Documentos), vjust = -0.4, size = 4) +
    scale_x_continuous(breaks = CONFIG$anio_inicio:CONFIG$anio_final) +
    scale_y_continuous(breaks = 0:limite_y, limits = c(0, limite_y)) +
    labs(x = "Año de publicación", y = "Documentos") +
    theme(
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )

  ggsave(
    filename = file.path(RUTA_FIGURAS, "Figura_01_Produccion_Anual.png"),
    plot = figura_anual, width = 10, height = 4.5, dpi = 300, bg = "white"
  )

  # -------------------------------
  # 4.10. Limpiar los términos MeSH
  # -------------------------------
  # Se eliminan etiquetas demográficas y geográficas generales.
  # No se crean palabras clave nuevas a partir de títulos o resúmenes.
  terminos_excluidos <- c(
    "HUMANS", "FEMALE", "MALE", "ADULT", "AGED", "MIDDLE AGED", "CHILD",
    "ADOLESCENT", "YOUNG ADULT", "AGED, 80 AND OVER", "CHILD, PRESCHOOL",
    "INFANT", "INFANT, NEWBORN", "UNITED STATES", "UNITED KINGDOM", "ENGLAND",
    "CANADA", "BRAZIL", "IRAN", "NETHERLANDS", "NIGERIA", "DENMARK", "SPAIN",
    "TAIWAN", "CHINA", "AUSTRALIA", "NEW YORK CITY"
  )

  M$ID <- vapply(
    separar_terminos(toupper(M$ID)),
    function(terminos) paste(setdiff(terminos, terminos_excluidos), collapse = ";"),
    FUN.VALUE = character(1)
  )

  writeLines(terminos_excluidos, file.path(RUTA_ANEXOS, "Terminos_Excluidos.txt"))
  keywords_limpias <- crear_tabla_frecuencias(M$ID)
  guardar_csv(keywords_limpias, file.path(RUTA_ANEXOS, "Tabla_keywords_limpias.csv"))

  # -------------------------------
  # 4.11. Conservar términos presentes en cinco o más documentos
  # -------------------------------
  terminos_conservar <- keywords_limpias$Elemento[ keywords_limpias$Documentos >= CONFIG$frecuencia_minima_mesh ]
  M$ID <- vapply(
    separar_terminos(M$ID),
    function(terminos) paste(intersect(terminos, terminos_conservar), collapse = ";"),
    FUN.VALUE = character(1)
  )

  # -------------------------------
  # 4.12. Crear la matriz de coocurrencia de palabras clave
  # -------------------------------
  K <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")
  K <- as.matrix(K)
  write.csv(K, file.path(RUTA_ANEXOS, "Matriz_Coocurrencia.csv"))
  cat("\nTérminos utilizados en los mapas:", length(terminos_conservar), "\n")

  # -------------------------------
  # 4.13. Crear el mapa temático
  # -------------------------------
  set.seed(CONFIG$semilla)
  TM <- thematicMap(
    M, field = "ID", n = length(terminos_conservar), minfreq = 0,
    stemming = FALSE, n.labels = 1, repel = TRUE, cluster = "walktrap"
  )

  saveRDS(TM, file.path(RUTA_ANEXOS, "Mapa_Tematico.rds"))
  guardar_csv(TM$clusters, file.path(RUTA_ANEXOS, "Clusters_Tematicos.csv"))
  guardar_csv(TM$words, file.path(RUTA_ANEXOS, "Palabras_Clusters.csv"))
  print(TM$clusters)
  print(TM$words)
  clusters_tematicos <- TM$clusters
  paleta_tematica <- setNames( as.character(clusters_tematicos$color), as.character(clusters_tematicos$name) )
  figura_tematica <- ggplot(clusters_tematicos, aes(x = rcentrality, y = rdensity)) +
    geom_vline(xintercept = mean(clusters_tematicos$rcentrality), linetype = 2, color = "gray55") +
    geom_hline(yintercept = mean(clusters_tematicos$rdensity), linetype = 2, color = "gray55") +
    geom_point(aes(size = freq, color = name), alpha = 0.75) +
    ggrepel::geom_label_repel(
      aes(label = name), size = 4.2, seed = CONFIG$semilla,
      box.padding = 0.6, show.legend = FALSE
    ) +
    scale_color_manual(values = paleta_tematica, guide = "none") +
    scale_size_area(max_size = 20, name = "Suma de frecuencias") +
    scale_x_continuous(limits = c(0.4, 4.6), breaks = 1:4) +
    scale_y_continuous(limits = c(0.4, 4.6), breaks = 1:4) +
    annotate(
      "text", x = c(0.5, 4.5, 0.5, 4.5), y = c(4.5, 4.5, 0.5, 0.5),
      label = c("Nicho", "Motores", "Emergentes o en declive", "Básicos"),
      hjust = c(0, 1, 0, 1), size = 4, color = "gray40"
    ) +
    labs(x = "Centralidad (rango)", y = "Densidad (rango)") +
    theme(panel.grid.minor = element_blank(), legend.position = "bottom")

  ggsave(
    filename = file.path(RUTA_FIGURAS, "Figura_03_Mapa_Tematico.png"),
    plot = figura_tematica, width = 10, height = 6.2, dpi = 300, bg = "white"
  )

  # -------------------------------
  # 4.14. Construir la red de coocurrencia
  # -------------------------------
  # La posición de los nodos utiliza normalización por asociación.
  # El grosor de las líneas representa la cantidad de documentos compartidos.
  # Solo se dibujan enlaces con tres o más documentos para facilitar la lectura.
  # Las matrices guardadas conservan todos los enlaces.
  set.seed(CONFIG$semilla)
  NP <- networkPlot(
    K, normalize = "association", type = "fruchterman", n = nrow(K),
    cluster = "walktrap", remove.isolates = TRUE, weighted = TRUE,
    noloops = TRUE, verbose = FALSE
  )

  saveRDS(NP, file.path(RUTA_ANEXOS, "Red_Coocurrencia.rds"))
  red_palabras <- NP$graph

  # -------------------------------
  # 4.15. Calcular indicadores de la red de palabras
  # -------------------------------
  grafo_palabras <- graph_from_adjacency_matrix( K, mode = "undirected", weighted = TRUE, diag = FALSE )
  frecuencia_palabras <- diag(K)
  grado_palabras <- degree(grafo_palabras)
  fuerza_palabras <- strength(grafo_palabras)

  indicadores_palabras <- data.frame(
    Termino = names(grado_palabras),
    Frecuencia = as.numeric(frecuencia_palabras[names(grado_palabras)]),
    Grado = as.numeric(grado_palabras),
    Fuerza = as.numeric(fuerza_palabras)
  )

  guardar_csv(indicadores_palabras, file.path(RUTA_ANEXOS, "Indicadores_Red_Palabras.csv"))
  enlaces_palabras <- as_data_frame(grafo_palabras, what = "edges")
  enlaces_palabras <- enlaces_palabras[order(-enlaces_palabras$weight), , drop = FALSE]
  guardar_csv(enlaces_palabras, file.path(RUTA_ANEXOS, "Enlaces_Palabras.csv"))
  print(head(enlaces_palabras, 15))

  # -------------------------------
  # 4.16. Preparar los datos para dibujar la red de palabras
  # -------------------------------
  matriz_cruda <- as.matrix(K)
  rownames(matriz_cruda) <- tolower(rownames(matriz_cruda))
  colnames(matriz_cruda) <- tolower(colnames(matriz_cruda))
  V(red_palabras)$name <- tolower(V(red_palabras)$name)
  frecuencia_nodos <- diag(matriz_cruda)[V(red_palabras)$name]
  extremos_red <- ends(red_palabras, E(red_palabras), names = TRUE)
  peso_crudo <- matriz_cruda[cbind(extremos_red[, 1], extremos_red[, 2])]
  set.seed(CONFIG$semilla)
  coordenadas_palabras <- layout_with_fr(red_palabras)
  colores_nodos <- V(red_palabras)$color

  if (is.null(colores_nodos) || length(colores_nodos) != vcount(red_palabras)) {
    colores_nodos <- rep(COLOR_BASE, vcount(red_palabras))
  }

  nodos_palabras <- data.frame(
    name = V(red_palabras)$name,
    x = coordenadas_palabras[, 1],
    y = coordenadas_palabras[, 2],
    freq = as.numeric(frecuencia_nodos),
    color = colores_nodos
  )

  aristas_palabras <- data.frame(
    x = coordenadas_palabras[match(extremos_red[, 1], nodos_palabras$name), 1],
    y = coordenadas_palabras[match(extremos_red[, 1], nodos_palabras$name), 2],
    xend = coordenadas_palabras[match(extremos_red[, 2], nodos_palabras$name), 1],
    yend = coordenadas_palabras[match(extremos_red[, 2], nodos_palabras$name), 2],
    weight = as.numeric(peso_crudo)
  )
  aristas_visibles <- aristas_palabras[ aristas_palabras$weight >= CONFIG$minimo_coocurrencias_visibles, , drop = FALSE ]

  # -------------------------------
  # 4.17. Dibujar la red de coocurrencia
  # -------------------------------
  figura_coocurrencia <- ggplot() +
    geom_segment(
      data = aristas_visibles,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = weight),
      color = "#9CA9B8", alpha = 0.6
    ) +
    geom_point(data = nodos_palabras, aes(x = x, y = y, size = freq, color = color)) +
    ggrepel::geom_label_repel(
      data = nodos_palabras,
      aes(x = x, y = y, label = stringr::str_wrap(name, width = 22)),
      size = 3.8, seed = CONFIG$semilla, box.padding = 0.55, point.padding = 0.8,
      min.segment.length = 0, max.overlaps = Inf, label.size = 0.1
    ) +
    scale_color_identity() +
    scale_size_area(max_size = 17, name = "Documentos") +
    scale_linewidth_continuous(range = c(0.4, 1.8), name = "Coocurrencias") +
    theme_void() +
    theme(legend.position = "bottom", plot.margin = margin(15, 25, 15, 25))

  ggsave(
    filename = file.path(RUTA_FIGURAS, "Figura_02_Coocurrencia.png"),
    plot = figura_coocurrencia, width = 11, height = 7.5, dpi = 300, bg = "white"
  )

  # -------------------------------
  # 4.18. Construir la red de colaboración entre autores
  # -------------------------------
  # Las firmas formadas por apellido e iniciales se analizan tal como aparecen.
  # El programa no realiza una desambiguación de identidades.
  red_autores_matriz <- biblioNetwork(M, analysis = "collaboration", network = "authors", sep = ";")
  red_autores_matriz <- as.matrix(red_autores_matriz)
  grafo_autores <- graph_from_adjacency_matrix( red_autores_matriz, mode = "undirected", weighted = TRUE, diag = FALSE )
  componentes_autores <- components(grafo_autores)
  indice_componente_mayor <- which.max(componentes_autores$csize)
  componente_mayor <- induced_subgraph(
    grafo_autores, which(componentes_autores$membership == indice_componente_mayor)
  )

  # -------------------------------
  # 4.19. Detectar comunidades dentro de la mayor componente
  # -------------------------------
  set.seed(CONFIG$semilla)
  if (ecount(componente_mayor) > 0L) {
    comunidades_autores <- cluster_walktrap(
      componente_mayor, weights = E(componente_mayor)$weight
    )$membership
  } else { comunidades_autores <- rep(1L, vcount(componente_mayor)) }
  colores_base_autores <- c("#163B5C", "#A54D42", "#3F7E73", "#9D7F36", "#765B8E")
  cantidad_grupos <- length(unique(comunidades_autores))
  paleta_autores <- colorRampPalette(colores_base_autores)(cantidad_grupos)
  frecuencia_autores <- diag(red_autores_matriz)[V(componente_mayor)$name]
  set.seed(CONFIG$semilla)
  coordenadas_autores <- layout_with_fr(componente_mayor)

  nodos_autores <- data.frame(
    name = V(componente_mayor)$name,
    x = coordenadas_autores[, 1],
    y = coordenadas_autores[, 2],
    freq = as.numeric(frecuencia_autores),
    grupo = factor(comunidades_autores)
  )
  enlaces_autores_originales <- as_data_frame(componente_mayor, what = "edges")

  aristas_autores <- data.frame(
    x = coordenadas_autores[match(enlaces_autores_originales$from, nodos_autores$name), 1],
    y = coordenadas_autores[match(enlaces_autores_originales$from, nodos_autores$name), 2],
    xend = coordenadas_autores[match(enlaces_autores_originales$to, nodos_autores$name), 1],
    yend = coordenadas_autores[match(enlaces_autores_originales$to, nodos_autores$name), 2],
    weight = enlaces_autores_originales$weight
  )

  # -------------------------------
  # 4.20. Dibujar la red de colaboración entre autores
  # -------------------------------
  figura_autores <- ggplot() +
    geom_segment(
      data = aristas_autores,
      aes(x = x, y = y, xend = xend, yend = yend, linewidth = weight),
      color = "#9CA9B8"
    ) +
    geom_point(data = nodos_autores, aes(x = x, y = y, size = freq, color = grupo)) +
    ggrepel::geom_label_repel(
      data = nodos_autores, aes(x = x, y = y, label = name), seed = CONFIG$semilla,
      size = 4.2, box.padding = 0.7, point.padding = 0.9, max.overlaps = Inf,
      min.segment.length = 0, label.size = 0.1
    ) +
    scale_color_manual(values = paleta_autores, guide = "none") +
    scale_size_area(max_size = 15, breaks = 1:2, name = "Documentos") +
    scale_linewidth_continuous(range = c(0.5, 1.4), breaks = 1:2, name = "Artículos compartidos") +
    theme_void() +
    theme(legend.position = "bottom", plot.margin = margin(15, 25, 15, 25))

  ggsave(
    filename = file.path(RUTA_FIGURAS, "Figura_04_Colaboracion_Autores.png"),
    plot = figura_autores, width = 10, height = 5.5, dpi = 300, bg = "white"
  )

  # -------------------------------
  # 4.21. Guardar información de la red de autores
  # -------------------------------
  guardar_csv(
    as_data_frame(grafo_autores, what = "edges"),
    file.path(RUTA_ANEXOS, "Enlaces_Autores.csv")
  )

  tabla_componente_autores <- data.frame(
    Autor = V(componente_mayor)$name,
    Documentos = as.numeric(frecuencia_autores),
    Grado = as.numeric(degree(componente_mayor)),
    Grupo = comunidades_autores
  )
  guardar_csv(tabla_componente_autores, file.path(RUTA_ANEXOS, "Mayor_Componente_Autores.csv"))

  # -------------------------------
  # 4.22. Calcular indicadores generales
  # -------------------------------
  documentos_con_mesh <- sum(!is.na(D$ID) & nzchar(trimws(D$ID)))
  documentos_con_palabras_autor <- sum(!is.na(D$DE) & nzchar(trimws(D$DE)))
  metricas <- data.frame(
    Indicador = c(
      "Documentos", "Fuentes", "Firmas_autor", "Con_MeSH", "Con_palabras_autor",
      "Autores_por_documento", "Autores_recurrentes", "Nodos_coautoria",
      "Enlaces_coautoria", "Componentes_coautoria", "Aislados_coautoria",
      "Mayor_componente", "Nodos_palabras", "Enlaces_palabras"
    ),
    Valor = c(
      nrow(M), nrow(sources), nrow(authors), documentos_con_mesh,
      documentos_con_palabras_autor, mean(lengths(separar_terminos(M$AU))),
      sum(authors$Documentos > 1), vcount(grafo_autores), ecount(grafo_autores),
      componentes_autores$no, sum(degree(grafo_autores) == 0),
      vcount(componente_mayor), vcount(grafo_palabras), ecount(grafo_palabras)
    )
  )

  print(metricas)
  print(data.frame(Autor = V(componente_mayor)$name, Grado = degree(componente_mayor)))
  guardar_csv(metricas, file.path(RUTA_ANEXOS, "Indicadores_Generales.csv"))

  # -------------------------------
  # 4.23. Guardar todos los resultados en un único archivo de R
  # -------------------------------
  resultados_completos <- list(M = M, BA = BA, TM = TM, K = K, A = red_autores_matriz)
  saveRDS(resultados_completos, file.path(RUTA_ANEXOS, "Resultados_Completos.rds"))

  # -------------------------------
  # 4.24. Guardar información de R y de los paquetes utilizados
  # -------------------------------
  capture.output(sessionInfo(), file = file.path(RUTA_ANEXOS, "SessionInfo.txt"))
  cat("\nAnálisis completado correctamente.\n")
  cat("Las figuras se guardaron en:", RUTA_FIGURAS, "\n")
  cat("Las tablas y resultados se guardaron en:", RUTA_ANEXOS, "\n")

  sink()
  registro_abierto <- FALSE
}


# ===============================
# 5. EJECUTAR EL PROGRAMA
# ===============================
ejecutar_analisis()

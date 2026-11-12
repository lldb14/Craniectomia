# ===============================
# ANALISIS DE CORRESPONDENCIAS MULTIPLES (ACM)
# Hoja: "laura"
# ===============================

# 1. Instalar y cargar paquetes
install.packages(c("readxl", "FactoMineR", "factoextra"))
library(readxl)
library(FactoMineR)
library(factoextra)

setwd("C:/LAURA LUCIA/imbanaco/adrian/articulos/2")

# 2. Leer el archivo Excel (hoja "laura")
datos <- read_excel(
  path = "Archivo arreglado 17 01.xlsx",
  sheet = "LAURA"
)

# 3. Revisar estructura de los datos
str(datos)

# 4. Convertir variables a factores
# (usa todas las columnas; si no, cambia los índices)
datos_acm <- data.frame(lapply(datos, as.factor))

datos_acm$Edad<-as.numeric(datos_acm$Edad)
datos_acm$Comorbilidades<-as.numeric(datos_acm$Comorbilidades)

datos_acm$Tiempo.quirurgico..horas.<-as.numeric(datos_acm$Tiempo.quirurgico..horas.)

datos_acm$Dias.de.estancia.antes.del.procedimiento<-as.numeric(datos_acm$Dias.de.estancia.antes.del.procedimiento)

datos_acm$Dias.de.estancia.despues.del.procedimiento<-as.numeric(datos_acm$Dias.de.estancia.despues.del.procedimiento)

datos_acm$Tiempo.de.espera.valoración.neurocirugía..horas.<-as.numeric(datos_acm$Tiempo.de.espera.valoración.neurocirugía..horas.)

str(datos_acm)
summary(datos_acm)

#categorizar-----------------
datos_mca<-datos_acm
# Categorizar Edad (ejemplo con rangos)
datos_mca$Years <- cut(datos_acm$Edad, 
                          breaks = c(0, 10, 20, 30, 40),
                          labels = c("Edad 0-10", "Edad 11-20", "Edad 21-30", "Edad 31-40"))

# Categorizar comorbilidades (ejemplo con rangos)
datos_mca$Comorbilidades_cat <- cut(datos_acm$Comorbilidades,
                                    breaks = c(0, 1, 2, 3, 10),
                                    labels = c("Com-1", "Com-2", "Com-3", "Com >=4"),
                                    include.lowest = TRUE,
                                    right = TRUE)

# Categorizar Tiempo quirúrgico
datos_mca$T_qx <- cut(datos_acm$Tiempo.quirurgico..horas.,
                                 breaks = c(0, 2, 4, 10),
                                 labels = c("T_qx 0-2h", "T_qx 2-4h", "T_qx >4h"))

# Categorizar Tiempo de espera
datos_mca$Espera <- cut(datos_acm$Tiempo.de.espera.valoración.neurocirugía..horas.,
                            breaks = c(0, 5, 10, 25),
                            labels = c("Espera 0-5h", "Espera 5-10h", "Espera >10h"))

# Categorizar Días de estancia antes
datos_mca$Estancia_a <- cut(datos_acm$Dias.de.estancia.antes.del.procedimiento,
                                    breaks = c(0, 3, 5, 10),
                                    labels = c("Estancia_a 0-3d", "Estancia_a 4-5d", "Estancia_a >5d"))

# Categorizar Días de estancia después
datos_mca$Estancia_despues<- cut(datos_acm$Dias.de.estancia.despues.del.procedimiento,
                                      breaks = c(0, 7, 10, 25),
                                      labels = c("Estancia_d 0-7d", "Estancia_d 8-10d", "Estancia_d >11d"))

# Mantener variables categóricas originales
# Comorbilidades (aunque es numérica, parece tener pocos valores únicos)
datos_mca$Comorbilidades <- as.factor(datos_acm$Comorbilidades)

# Verificar todas las variables
str(datos_mca)

names(datos_mca)
datos_mca <- datos_mca[,-1]

resumen<-datos_mca[,-c(1,3,4,5,9,10,11,18)]
summary(resumen)
names(datos_mca)
# acm----------------
colnames(datos_mca)[7] <- "Complicación"

# 5. Ajustar el modelo ACM
acm <- MCA(datos_mca[,-c(1,3,4,5,6,9,10,11,18)], graph = FALSE)

# 6. Autovalores (varianza explicada)
acm$eig

# 7. Gráfico de individuos
fviz_mca_ind(acm,
             repel = TRUE,
             ggtheme = theme_minimal())

# 8. Gráfico de variables
fviz_mca_var(acm,
             repel = TRUE,
             ggtheme = theme_minimal())

# 9. Biplot (individuos + variables)
fviz_mca_biplot(acm,
                repel = TRUE,
                ggtheme = theme_minimal())

library(ggrepel)
options(ggrepel.max.overlaps = Inf)
fviz_mca_biplot(acm, repel=TRUE,cex=0.6,geom.ind = c("point"),geom.var =  c("point", "text"),col.ind  ="#C1CDCD", col.var="#8B1A1A")


# 10. Resultados numéricos
# Contribución de las variables
acm$var$contrib

# Coordenadas de los individuos
head(acm$ind$coord)

#-------------------cluster laura----------------------------
library(FactoMineR)
cluster.votaciones<-HCPC(acm,nb.clust=3,consol=FALSE,graph=FALSE)
library(ggrepel)
options(ggrepel.max.overlaps = Inf)
plot.HCPC(cluster.votaciones,choice='map',draw.tree=FALSE,title = '',axes = c(1,2),ylim=c(-1.7,1.7),ind.names = FALSE)
##P values
summary(cluster.votaciones)
cluster.votaciones$desc.var #valores test
#con todos los ejes del acm

cluster.votaciones$data.clust #conjunto de datos con la clasificacion

library(Factoshiny)

HCPCshiny(acm)
#--------------cluster--------------------

library(FactoMineR)
library(factoextra)
library(cluster)

# 1. Verificar y extraer coordenadas correctamente
n_dim_reales <- ncol(acm$ind$coord)
varianza_acumulada <- cumsum(acm$eig[1:n_dim_reales, 2])
ndim <- min(which(varianza_acumulada > 80)[1], n_dim_reales)

# Si no alcanza el 80%, usar las que tengamos
if(is.na(ndim)) {
  ndim <- min(5, n_dim_reales)  # Usar primeras 5 o las disponibles
  cat("No se alcanza 80% de varianza. Usando", ndim, "dimensiones\n")
}

coordenadas_ind <- acm$ind$coord[, 1:ndim]
cat("Dimensiones para clustering:", ncol(coordenadas_ind), "\n")

# 2. Determinar número óptimo de clusters
# Método del codo
fviz_nbclust(coordenadas_ind, 
             kmeans, 
             method = "wss",
             k.max = min(6, nrow(coordenadas_ind)-1)) +
  labs(title = "Método del Codo")

# Método de la silueta
fviz_nbclust(coordenadas_ind, 
             kmeans, 
             method = "silhouette",
             k.max = min(8, nrow(coordenadas_ind)-1)) +
  labs(title = "Método de la Silueta")

# 3. Elegir número de clusters (ajusta según los gráficos)
set.seed(123)
n_clusters <- 3  # Puedes cambiar según lo que muestren los gráficos

# Verificar que n_clusters sea válido
if(n_clusters >= nrow(coordenadas_ind)) {
  n_clusters <- min(3, nrow(coordenadas_ind)-1)
  cat("Ajustando número de clusters a:", n_clusters, "\n")
}

# 4. Aplicar K-means
kmeans_result <- kmeans(coordenadas_ind, 
                        centers = n_clusters, 
                        nstart = 25)

# 5. Añadir clusters a los datos
datos_mca$Cluster <- as.factor(kmeans_result$cluster)

# 6. Visualización
# Gráfico de individuos coloreados por cluster
fviz_mca_ind(acm,
             geom.ind = "point",
             col.ind = datos_mca$Cluster,
             palette = c("#00AFBB", "#E7B800", "#FC4E07", "#4DAF4A","blue"),
             addEllipses = TRUE,
             ellipse.type = "confidence",
             legend.title = "Clusters",
             title = "Clusters en espacio ACM")

# Gráfico alternativo con las coordenadas reducidas
fviz_cluster(list(data = coordenadas_ind, 
                  cluster = kmeans_result$cluster),
             ellipse.type = "norm",
             geom = "point",
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             ggtheme = theme_minimal(),
             main = "Visualización de Clusters")

# 7. Caracterización básica de clusters
cat("\n=== DISTRIBUCIÓN DE CLUSTERS ===\n")
print(table(datos_mca$Cluster))
print(round(prop.table(table(datos_mca$Cluster)) * 100, 1))

# 8. Relación con variables clave
# Función para analizar cada variable
analizar_variable <- function(var_name) {
  cat("\n---", var_name, "---\n")
  tab <- table(datos_mca$Cluster, datos_mca[,var_name])
  print(tab)
  cat("\nPorcentajes por fila:\n")
  print(round(prop.table(tab, 1) * 100, 1))
}

# Analizar variables principales
analizar_variable("Complicacion.morbilidad.intrahospitalaria")
analizar_variable("Defunción")
analizar_variable("Sexo")
analizar_variable("Comorbilidades_cat")
analizar_variable("Edad_cat")

# 9. Visualización de perfiles
library(ggplot2)
library(tidyr)

# Preparar datos para gráfico

library(tidyr)
library(ggplot2)

# 1. IDENTIFICAR LAS VARIABLES CATEGÓRICAS EN datos_mca
# Ver todas las variables disponibles
names(datos_mca)

# Seleccionar SOLO las variables categóricas (factores)
vars_categoricas <- c("Sexo", 
                      "Comorbilidades_cat", 
                      "Sala.de.atención", 
                      "Complicacion.morbilidad.intrahospitalaria", 
                      "Defunción", 
                      "Lateralidad.del.Abordaje", 
                      "Edad_cat", 
                      "Tiempo_quir_cat", 
                      "Espera_cat", 
                      "Estancia_antes_cat", 
                      "Estancia_despues_cat",
                      "Cluster")  # Incluir Cluster

# Crear dataframe solo con variables categóricas
datos_categoricos <- datos_mca[, vars_categoricas]

# Verificar que todas sean factores
str(datos_categoricos)

# 2. AHORA SÍ, hacer pivot_longer con datos homogéneos
datos_long <- datos_categoricos %>%
  pivot_longer(cols = -Cluster, 
               names_to = "Variable", 
               values_to = "Categoria")

# Verificar la estructura
head(datos_long)
str(datos_long)

# 3. Gráfico de perfiles
ggplot(datos_long, aes(x = Cluster, fill = Categoria)) +
  geom_bar(position = "fill") +
  facet_wrap(~Variable, scales = "free_x", ncol = 3) +
  labs(y = "Proporción", 
       title = "Perfil de los Clusters",
       x = "Cluster") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        strip.text = element_text(size = 8)) +
  scale_fill_viridis_d()  # Paleta de colores accesible

# 4. Alternativa: Gráfico de barras agrupado para variables clave
vars_importantes <- c("Complicacion.morbilidad.intrahospitalaria", 
                      "Defunción", 
                      "Sexo", 
                      "Comorbilidades_cat")

for(var in vars_importantes) {
  p <- ggplot(datos_categoricos, aes_string(x = "Cluster", fill = var)) +
    geom_bar(position = "fill") +
    labs(y = "Proporción", 
         title = paste("Cluster vs", var),
         x = "Cluster") +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2")
  
  print(p)
}

# 5. Gráfico de calor de frecuencias
# Crear tabla de contingencia para cada variable
library(reshape2)

# Función para crear matriz de perfiles
crear_matriz_perfiles <- function(datos, variable) {
  tab <- prop.table(table(datos$Cluster, datos[[variable]]), 1)
  return(tab)
}

# Crear heatmap para complicaciones
matriz_comp <- crear_matriz_perfiles(datos_categoricos, "Complicacion.morbilidad.intrahospitalaria")
melted_comp <- melt(matriz_comp)

ggplot(melted_comp, aes(x = Var2, y = Var1, fill = value)) +
  geom_tile() +
  geom_text(aes(label = sprintf("%.1f%%", value*100)), size = 3) +
  scale_fill_gradient(low = "white", high = "steelblue", 
                      labels = scales::percent) +
  labs(x = "", y = "Cluster", 
       title = "Proporción de Complicaciones por Cluster",
       fill = "Proporción") +
  theme_minimal()

# 6. Análisis estadístico de diferencias entre clusters
for(var in vars_categoricas[-length(vars_categoricas)]) {  # Excluir Cluster
  cat("\n", paste(rep("=", 50), collapse = ""))
  cat("\nVariable:", var)
  cat("\n", paste(rep("=", 50), collapse = ""), "\n")
  
  # Tabla de contingencia
  tab <- table(datos_categoricos$Cluster, datos_categoricos[[var]])
  print(tab)
  
  # Chi-cuadrado
  test <- chisq.test(tab)
  cat("\nChi-cuadrado: p-value =", round(test$p.value, 4))
  if(test$p.value < 0.05) {
    cat(" - DIFERENCIA SIGNIFICATIVA\n")
  } else {
    cat(" - No significativo\n")
  }
}




#----------car cluster-----------

# 1. FUNCIÓN PARA DESCRIBIR CADA CLUSTER DETALLADAMENTE

describir_clusters_detallado <- function(datos, var_cluster = "Cluster") {
  
  # Número de clusters
  n_clusters <- length(unique(datos[[var_cluster]]))
  
  # Resultados globales
  cat("\n", paste(rep("=", 80), collapse = ""))
  cat("\n ANÁLISIS DE CARACTERÍSTICAS POR CLUSTER")
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  
  # Estadísticas globales
  cat("\n📊 DISTRIBUCIÓN GLOBAL:\n")
  print(table(datos[[var_cluster]]))
  print(round(prop.table(table(datos[[var_cluster]])) * 100, 1))
  
  # Análisis por cluster
  for(i in 1:n_clusters) {
    cluster_data <- datos[datos[[var_cluster]] == i, ]
    otros_data <- datos[datos[[var_cluster]] != i, ]
    
    cat("\n", paste(rep("🔷", 20), collapse = ""))
    cat("\n🔷 CLUSTER", i, ":", nrow(cluster_data), "PACIENTES", 
        "(", round(nrow(cluster_data)/nrow(datos)*100, 1), "% DEL TOTAL)")
    cat("\n", paste(rep("🔷", 20), collapse = ""), "\n")
    
    # Identificar variables categóricas (excluyendo Cluster)
    vars_cat <- names(datos)[sapply(datos, is.factor)]
    vars_cat <- vars_cat[vars_cat != var_cluster]
    
    for(var in vars_cat) {
      cat("\n📌", var, ":\n")
      
      # Distribución en el cluster
      tab_cluster <- table(cluster_data[[var]])
      prop_cluster <- prop.table(tab_cluster) * 100
      
      # Distribución global
      tab_global <- table(datos[[var]])
      prop_global <- prop.table(tab_global) * 100
      
      # Distribución en otros clusters
      prop_otros <- prop.table(table(otros_data[[var]])) * 100
      
      # Mostrar todas las categorías
      for(cat in names(tab_cluster)) {
        diff_con_global <- prop_cluster[cat] - prop_global[cat]
        diff_con_otros <- prop_cluster[cat] - prop_otros[cat]
        
        # Formato según diferencia
        if(abs(diff_con_global) > 10) {
          flecha <- ifelse(diff_con_global > 0, "🔺", "🔻")
          cat(sprintf("  %s %s: %.1f%% (%s%.1f%% vs global)\n", 
                      flecha, cat, prop_cluster[cat], 
                      ifelse(diff_con_global > 0, "+", ""), diff_con_global))
        } else {
          cat(sprintf("     %s: %.1f%%\n", cat, prop_cluster[cat]))
        }
      }
      
      # Categoría predominante
      cat_pred <- names(sort(prop_cluster, decreasing = TRUE))[1]
      cat(sprintf("  ▶️  Predominante: %s (%.1f%%)\n", 
                  cat_pred, max(prop_cluster)))
    }
    
    # Resumen ejecutivo del cluster
    cat("\n📋 RESUMEN DEL CLUSTER", i, ":\n")
    cat("   Pacientes:", nrow(cluster_data), "\n")
    
    # Características distintivas (diferencia > 15% con otros clusters)
    cat("   Características distintivas:\n")
    for(var in vars_cat) {
      prop_cluster <- prop.table(table(cluster_data[[var]])) * 100
      prop_otros <- prop.table(table(otros_data[[var]])) * 100
      
      for(cat in names(prop_cluster)) {
        if(cat %in% names(prop_otros)) {
          diff <- prop_cluster[cat] - prop_otros[cat]
          if(abs(diff) > 15) {
            if(diff > 0) {
              cat(sprintf("     ✓ Mayor %s - %s: +%.1f%%\n", 
                          var, cat, diff))
            } else {
              cat(sprintf("     ✗ Menor %s - %s: %.1f%%\n", 
                          var, cat, diff))
            }
          }
        }
      }
    }
  }
}

# Aplicar la función
describir_clusters_detallado(datos_categoricos)








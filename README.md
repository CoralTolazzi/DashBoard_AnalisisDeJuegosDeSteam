# Trabajo Práctico N.º 5 - Visualización Interactiva: Juegos de Steam

**Nombre:** Coral Tolazzi
**Materia:** Ciencia de Datos
**Tema:** DashBoard
**Profesor:** Alejandro Luis Bonavita 
**Cuatrimestre:** 1.º Cuatrimestre del 2025
**Instituto:** Instituto Tecnológico Beltrán

## Descripción

Este trabajo práctico implementa una aplicación web interactiva con **R y Shiny** para analizar un conjunto de datos de más de **2300 videojuegos populares de Steam**. La app permite explorar los juegos en profundidad, descubrir patrones de consumo, comparar ratings, visualizar etiquetas frecuentes y analizar características como dificultad, duración y edad recomendada.


## Funcionalidades

* **Análisis de juegos individuales**

  * Nube de palabras con las etiquetas más comunes
  * Top 15 juegos más descargados
  * Juegos con mejor rating
  * Relación entre precio y descargas (gráfico interactivo)
  * Tabla interactiva de juegos seleccionables

* **Análisis de compañías**

  * Comparación de desarrolladoras según rating promedio
  * Filtro por cantidad mínima de juegos publicados
  * Tabla con métricas de desempeño por compañía

* **Comparaciones generales**

  * Distribución de juegos por dificultad
  * Duración estimada (en horas)
  * Recomendación por edad
  * Compatibilidad con sistemas operativos (Windows, Mac, Linux)


## Componentes principales

* `ui`: define la estructura visual de la aplicación con pestañas temáticas
* `server`: contiene la lógica para procesamiento, filtrado y visualización
* `prepared_data`: preprocesamiento del dataset original
* `tags_freq`, `os_data`: procesamiento específico de etiquetas y sistemas operativos
* Gráficos: `ggplot2`, `plotly` y `wordcloud`
* Tablas interactivas: `DT`


## Requisitos

Antes de ejecutar el código, asegurate de tener instalados los siguientes paquetes en R:

```r
install.packages(c("shiny", "tm", "wordcloud", "stringr", "RColorBrewer", 
                   "dplyr", "ggplot2", "DT", "plotly"))
```

Además, colocá el archivo **`bestSelling_games.csv`** en el mismo directorio que el script de la app.
Link De Descarga del Dataset:  https://www.kaggle.com/datasets/hbugrae/best-selling-steam-games-of-all-time?resource=download



![Steam Dashboard Screenshot](https://upload.wikimedia.org/wikipedia/commons/thumb/8/83/Steam_icon_logo.svg/512px-Steam_icon_logo.svg.png)

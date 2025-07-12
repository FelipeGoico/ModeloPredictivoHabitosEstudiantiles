# Examen R - Análisis de Hábitos Estudiantiles

Este repositorio contiene el desarrollo del examen final del curso **Programación con R** del Máster en Data Science (UDLA, 2025). El objetivo del proyecto fue construir una función de complejidad media o alta aplicada a un dataset real, evaluando hábitos estudiantiles y su relación con el rendimiento académico.

## Contenidos del repositorio

- `ModeloPredictivoHabitosEstudiantiles`: Informe Quarto con todo el desarrollo, explicación, código y resultados.
- `student_habits_performance.csv`: Dataset utilizado, con registros de hábitos y notas de estudiantes.
- `Funciones.R`: Código fuente de la función `analiza_habitos_estudio()`.
- `video_explicativo.mp4`: Video explicando el trabajo.

## Descripción del problema

Se busca analizar el impacto de variables como:
- Horas de estudio, sueño, redes sociales, ejercicio, salud mental.

Sobre el rendimiento académico medido mediante `exam_score`.

## Función desarrollada

La función `analiza_habitos_estudio()`:
- Permite modelar de forma flexible el rendimiento académico.
- Acepta distintos conjuntos de variables predictoras.
- Entrega resultados resumidos o el modelo completo (`lm` o `glm`).
- Maneja errores y filas incompletas.

## Resultados

Los modelos muestran que variables como `study_hours_per_day`, `sleep_hours` y `mental_health_rating` tienen una correlación significativa con el rendimiento académico.

## Requisitos

- R
- tidyverse (analisis y manipúlación de datos)
- tidymodels (modelado y analisis estadistico)
- GGally (visualizaciones complejas)
- testthat (pruebas unitarias de codigo)

## Ejecución

## Importar dataset

```{r}
rendimiento_estudiantes_inmutable=read.csv("student_habits_performance.csv")
```

```{r , include=FALSE}
#Librerias
library(tidyverse) #analisis y manipúlación de datos
library(tidymodels) #modelado y analisis estadistico
library(GGally) #visualizaciones complejas
library(testthat) # pruebas unitarias de codigo
```

## Visualización del dataset

### Primeras 6 filas del dataset

```{r}
head(rendimiento_estudiantes_inmutable,5)
```

\newpage

### Variables del dataset

Variables numéricas continuas:

-   age – Edad del estudiante

-   study_hours_per_day – Horas de estudio por día

-   social_media_hours – Horas en redes sociales por día

-   netflix_hours – Horas viendo Netflix por día

-   attendance_percentage – Porcentaje de asistencia a clases

-   sleep_hours – Horas de sueño por día

-   exercise_frequency – Frecuencia de ejercicio (veces por semana)

-   mental_health_rating – Valoración del bienestar mental (escala 1 a 10)

-   exam_score – Puntaje en el examen final (0 a 100)

Variables categóricas:

-   student_id – ID único del estudiante (no se analiza, sirve para identificación)

-   gender – Género (Male, Female, Other)

-   part_time_job – Tiene trabajo de medio tiempo (Yes/No)

-   diet_quality – Calidad de la dieta (Poor, Fair, Good)

-   parental_education_level – Nivel educativo de los padres (por ejemplo: High School, Bachelor, etc.)

-   internet_quality – Calidad del internet (Poor, Average, Good)

-   extracurricular_participation – Participación en actividades extracurriculares (Yes/No)

\newpage

## Analisis exploratorio de datos

```{r}
summary(rendimiento_estudiantes_inmutable)
str(rendimiento_estudiantes_inmutable)
```

\newpage

## Uso de funciones creadas

Se preparan los datos utilizando la funcion 'preparacion_data' y se guardan en 'rendimiento_estudiantes_preparado'

```{r}

source("Funciones.R") #carga de funciones
rendimiento_estudiantes_preparado <- preparacion_data(rendimiento_estudiantes_inmutable)
head(rendimiento_estudiantes_preparado)
```

\newpage

Se utilizan los datos preparados en la funcion 'coeficientes_correlacion' y se imprimen los resultados

```{r}
corpearson <- coeficientes_correlacion(rendimiento_estudiantes_preparado,
                                       metodo = "pearson")
print(corpearson)
```

\newpage

Visualización dataset para el modelo.

```{r}
# Se extrae y se visualiza los datos para el modelo. 
data_modelo <- corpearson$data_modelo
head(data_modelo)
```

## Desarrollo de la función

```{r}
analiza_habitos_estudio <- function(data, 
                                    variable_objetivo = "exam_score", 
                                    variables_predictoras = c("study_hours_per_day",
                                                              "sleep_hours"), 
                                    modelo = "lm", 
                                    resumen = TRUE) {
  # Validaciones
  if (!is.data.frame(data)) 
    stop("El objeto ingresado no es un data.frame")
  if (!all(c(variable_objetivo, variables_predictoras) %in% colnames(data))) 
    stop("Variables no encontradas en el dataset")

  # Eliminar filas incompletas
  data <- data %>% drop_na(all_of(c(variable_objetivo, 
                                    variables_predictoras)))

  # Formula dinámica
  formula_str <- paste(variable_objetivo, 
                       "~", 
                       paste(variables_predictoras, 
                             collapse = " + "))
  fmla <- as.formula(formula_str)

  # Selección de modelo
  if (modelo == "lm") {
    fit <- lm(fmla, data = data)
  } else if (modelo == "glm") {
    fit <- glm(fmla, data = data)
  } else {
    stop("Modelo no soportado. Usa 'lm' o 'glm'.")
  }

  if (resumen) {
    return(summary(fit))
  } else {
    return(fit)
  }
}
```

## Aplicación de la función

### variables predictoras

```{r}
variables_predictoras <- corpearson$variables_predictoras
```

### Modelo con variables predictoras de data_modelo

```{r}
# Modelo con variables predictoras de data_modelo
analiza_habitos_estudio(data_modelo, 
                        variables_predictoras = variables_predictoras, 
                        resumen = T)
```

### Modelo con 2 variables predictoras (study_hours_per_day & sleep_hours)

```{r}
# Modelo con dos variables predictoras
analiza_habitos_estudio(data_modelo, 
                        variables_predictoras = c("study_hours_per_day", 
                                                  "sleep_hours"), 
                        resumen = T)
```

## Validación con testthat

```{r}


test_that("El input debe ser un data.frame", {
  expect_error(analiza_habitos_estudio("texto"))
})

test_that("Devuelve un modelo lm", {
  df <- data_modelo %>% select(exam_score, 
                               study_hours_per_day, 
                               sleep_hours) %>% drop_na()
  fit <- analiza_habitos_estudio(df, 
                                 resumen = FALSE)
  expect_s3_class(fit, "lm")
})

test_that("Lanza error si las variables no existen", {
  expect_error(analiza_habitos_estudio(df_test, 
                                       variables_predictoras = c("inexistente")))
})

test_that("Funciona correctamente con resumen", {
  resultado <- analiza_habitos_estudio(data_modelo)
  expect_type(resultado, "list")
})
```


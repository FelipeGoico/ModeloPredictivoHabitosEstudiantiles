---
title: "Modelo Predictivo de hábitos estudiantiles y rendimiento académico"
format: pdf
editor: visual
---

# Modelo Predictivo de hábitos estudiantiles y rendimiento académico

## Descripción general del dataset

El dataset contiene **1000 registros de estudiantes**, cada uno con información sobre hábitos diarios, condiciones personales y rendimiento académico. En total, hay **16 variables**, tanto numéricas como categóricas. Estas variables incluyen aspectos como edad, género, horas de estudio, uso de redes sociales, calidad del sueño, ejercicio, salud mental y nota obtenida en el examen final.

\newpage

## Objetivos

### General

Desarrollar un modelo predictivo capaz de estimar el rendimiento académico de los estudiantes, medido a través de la nota del examen final, en función de sus hábitos de estudio, estilo de vida y condiciones personales.

### Especificos

-   Explorar y analizar los datos disponibles sobre hábitos estudiantiles, bienestar y rendimiento académico.

-   Identificar las variables que tienen mayor correlación con el puntaje en el examen, como horas de estudio, sueño, uso de redes sociales, entre otras.

-   Preprocesar los datos para su uso en modelos predictivos (tratamiento de valores nulos, codificación de variables categóricas, normalización, etc.).λ

-   Interpretar los resultados del modelo para comprender la importancia relativa de cada hábito o variable en la predicción del rendimiento académico.

-   ¿Que función propongo? Una función que tome un dataframe, realice una imputacion, saque valores extremos, codificar a numeros ("encoder") y dejar en la clase que requiere el ML.

-   Pensar para el examen la función que da la predección final.

\newpage

## Importar dataset

```{r}
rendimiento_estudiantes_inmutable=read.csv("student_habits_performance.csv")
```

### Cargar librerias

```{r}
#Librerias
library(tidyverse)
library(tidymodels)
library(GGally)
```

\newpage

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

```{r}
# Se preparan los datos utilizando la funcion 'preparacion_data' y se guardan en 'rendimiento_estudiantes_preparado'
source("Funciones.R")
rendimiento_estudiantes_preparado <- preparacion_data(rendimiento_estudiantes_inmutable)
head(rendimiento_estudiantes_preparado)
```

\newpage

```{r}
# Se utilizan los datos preparados en la funcion 'coeficientes_correlacion' y se imprimen los resultados
corpearson <- coeficientes_correlacion(rendimiento_estudiantes_preparado,metodo = "pearson")
print(corpearson)
```

\newpage

```{r}
data_modelo <- corpearson$data_modelo
head(data_modelo)
```

```{r}
# 2. Especificacion del modelo (Funcional)
especificacion_modelo <- linear_reg() %>% 
  set_engine("lm")
```

```{r}
# 3. Ajuste del Modelo
ajuste_modelo <- especificacion_modelo %>% 
  fit(exam_score ~ study_hours_per_day + social_media_hours + netflix_hours  + attendance_percentage + sleep_hours  + exercise_frequency   + mental_health_rating  , data = rendimiento_estudiantes_preparado)



```

```{r}
#Se visualiza el resumen del modelo generado
resumen_modelo <- ajuste_modelo %>% 
  pluck("fit") %>% 
  anova() %>% 
  tidy()
print(resumen_modelo)

```

```{r}
# se realizan test de la funcion creada con el metodo spearman
corspearman <- coeficientes_correlacion(rendimiento_estudiantes_preparado,metodo = "spearman")
print(corspearman)
```

```{r}
# se realizan test de la funcion creada con el metodo kendall
corkendall <- coeficientes_correlacion(rendimiento_estudiantes_preparado,metodo = "kendall")
print(corkendall)
```

```{r}
# se realizan test de la funcion creada con el metodo lala
cor1 <- coeficientes_correlacion(rendimiento_estudiantes_preparado,metodo = "lala")
```

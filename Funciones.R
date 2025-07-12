# 1. Preparación de Datos (funcional e inmutables)
#Funcion para preparar datos para el modelo

preparacion_data <- function(data){
  data %>% 
    select(-student_id) %>% 
    mutate(gender = factor(gender),
           part_time_job = factor(part_time_job),
           diet_quality = factor(diet_quality),
           parental_education_level = factor(parental_education_level),
           internet_quality = factor(internet_quality),
           extracurricular_participation = factor(extracurricular_participation),
           gender = as.numeric(gender),
           part_time_job = as.numeric(part_time_job),
           diet_quality = as.numeric(diet_quality),
           parental_education_level = as.numeric(parental_education_level),
           internet_quality = as.numeric(internet_quality),
           extracurricular_participation = as.numeric(extracurricular_participation),
    )
}


# Funcion para calcular los coeficientes de correlacions de la variable 
# dependiente con respecto a las variables independientes

coeficientes_correlacion <- function(data, metodo = "pearson"){
  if(!metodo %in% c("kendall", "pearson", "spearman") ){
    cat("Metodo no es el correcto, debe escoger entre 'kendall', 'spearman' o 'pearson' ")
    
  }else{
    i <- 1
    nombre_columnas <- names(data)
    columnas_qty <- ncol(data)
    Puntaje_examen <- data[,columnas_qty]
    columnas_p_value <- character(0)
    value_p <- c()
    correlacion <- c()
    while (i<columnas_qty){
      columna_sel <- data[,i]
      c1 <- cor.test(columna_sel,Puntaje_examen)
      corre_value <- cor(columna_sel,Puntaje_examen,method = metodo)
      p <-c1$p.value
      
      if(p<0.05){
        columnas_p_value <- append(columnas_p_value,nombre_columnas[i])
        value_p <- append(value_p, p)
        correlacion <- append(correlacion, corre_value)
        
      }
      i <- i+1
    }
    
    
    datanew <- data %>% 
      select(all_of(columnas_p_value),nombre_columnas[[columnas_qty]])
    
    
    cor_y_p_value <- list(
      variables_predictoras = columnas_p_value,
      valor_p = value_p,
      correlación = correlacion,
      metodo = metodo,
      cantidad_columnas = length(columnas_p_value),
      data_modelo = datanew
    )
    class(cor_y_p_value) <- "coeficientes"
    
    return(cor_y_p_value)
  }
}


#Con esta función se da formato al imprimir el resultado de la clase coeficientes

print.coeficientes <- function(x, ...) {
  qty_col <- length(x$data_modelo)
  col_p <- x$variables_predictoras
  i <- 1 
  cat("--- Resultado del calculo de las correlaciones con el Metodo:", x$metodo ," --- ")
  while (i < qty_col) {
    gra <- ggpairs(x$data_modelo[,c(i,qty_col)])
    print(gra)
    cat("La correlación entre",col_p[i],"y exam_score es:",x$correlación[[i]]*100 , "%")
    i <- i+1
    
    
    
  }
  
  
  analiza_habitos_estudio <- function(data, variable_objetivo = "exam_score", variables_predictoras = c("study_hours_per_day", "sleep_hours"), modelo = "lm", resumen = TRUE) {
    # Validaciones
    if (!is.data.frame(data)) stop("El objeto ingresado no es un data.frame")
    if (!all(c(variable_objetivo, variables_predictoras) %in% colnames(data))) stop("Variables no encontradas en el dataset")
    
    # Eliminar filas incompletas
    data <- data %>% drop_na(all_of(c(variable_objetivo, variables_predictoras)))
    
    # Formula dinámica
    formula_str <- paste(variable_objetivo, "~", paste(variables_predictoras, collapse = " + "))
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
  
  

  
}

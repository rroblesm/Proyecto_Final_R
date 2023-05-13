# @autor : ruben, mariana, santiago

# Importamos las librerias para graficar las distribuciones
library(ggplot2)
library(stats)
# fitdistrplus nos permite identificar de manera especifica las distribuciones
library(fitdistrplus)
library(patchwork)

# ---------------------------------------- PREGUNTA 1 ---------------------------------------- #

# Define una función para simular las variables aleatorias X y Y y calcular T:
simulate_T <- function(n, m) {
  X <- rexp(n, rate = 1/5)
  Y <- rexp(m, rate = 1/15)
  
  mean_X <- mean(X)
  mean_Y <- mean(Y)
  
  T <- mean_X / mean_Y
  return(T)
}

# Ahora, realiza las simulaciones para cada caso y guarda los resultados en listas:
# Caso (a)
a_results <- list()
for (n in c(10, 100, 1000, 10000)) {
  a_results[[as.character(n)]] <- replicate(1000, simulate_T(n, m = 20))
}

# Caso (b)
b_results <- list()
for (m in c(10, 100, 1000, 10000)) {
  b_results[[as.character(m)]] <- replicate(1000, simulate_T(n = 20, m))
}

# Crea gráficas de densidad para analizar las distribuciones resultantes:
# Función para crear gráficas de densidad
plot_density <- function(data, title) {
  df <- data.frame(value = data)
  p <- ggplot(df, aes(x = value)) +
    geom_density(fill = "blue", alpha = 0.5) +
    labs(title = title, x = "Valor de T", y = "Densidad")
  return(p)
}

# Gráficas de densidad para caso (a)
a_plots <- lapply(names(a_results), function(n) {
  plot_density(a_results[[n]], paste("Caso a, n =", n))
})

# Gráficas de densidad para caso (b)
b_plots <- lapply(names(b_results), function(m) {
  plot_density(b_results[[m]], paste("Caso b, m =", m))
})

# Muestra las gráficas y analiza las distribuciones resultantes:
# Mostrar gráficas caso a
for (p in a_plots) {
  print(p)
}

# Mostrar gráficas caso b
for (p in b_plots) {
  print(p)
}

# ---------------------------------------- PREGUNTA 1, CON COMPARACION DE GAMMAS A UN LADO ---------------------------------------- #

# Función para crear gráficas de densidad con la función de densidad Gamma ajustada
plot_density_with_fitted_gamma <- function(data, title) {
  df <- data.frame(value = data)
  
  # Ajustar una distribución Gamma a los datos
  gamma_fit <- fitdist(data, "gamma")
  shape <- gamma_fit$estimate["shape"]
  rate <- gamma_fit$estimate["rate"]
  
  p <- ggplot(df, aes(x = value)) +
    geom_density(fill = "blue", alpha = 0.5) +
    stat_function(fun = dgamma, args = list(shape = shape, rate = rate),
                  geom = "line", color = "red", size = 1.5) +
    labs(title = title, x = "Valor de T", y = "Densidad")
  return(p)
}

# Gráficas de densidad con Gamma ajustada para caso a
a_plots_fitted_gamma <- lapply(names(a_results), function(n) {
  plot_density_with_fitted_gamma(a_results[[n]], paste("Caso a, n =", n))
})

# Gráficas de densidad con Gamma ajustada para caso b
b_plots_fitted_gamma <- lapply(names(b_results), function(m) {
  plot_density_with_fitted_gamma(b_results[[m]], paste("Caso b, m =", m))
})

# Mostrar gráficas con Gamma ajustada para caso a
for (p in a_plots_fitted_gamma) {
  print(p)
}

# Mostrar gráficas con Gamma ajustada para caso b
for (p in b_plots_fitted_gamma) {
  print(p)
}

# ---------------------------------------- PREGUNTA 2 ---------------------------------------- #

# Define una función para simular las variables aleatorias X y calcular U:
simulate_U <- function(n) {
  X <- rnorm(n, mean = 0, sd = 1)
  
  sum_X <- sum(X)
  sum_X_squared <- sum(X^2)
  
  U <- (sqrt(n) * sum_X) / sum_X_squared
  return(U)
}

#realiza las simulaciones para cada valor de n y guarda los resultados en una lista
u_results <- list()
for (n in c(10, 100, 1000, 10000)) {
  u_results[[as.character(n)]] <- replicate(1000, simulate_U(n))
}

# Gráficas de densidad para analizar las distribuciones resultantes
# Gráficas de densidad para U
u_plots <- lapply(names(u_results), function(n) {
  plot_density(u_results[[n]], paste("U con n =", n))
})

# Mostrar gráficas para U
for (p in u_plots) {
  print(p)
}

# Ajustando la distribucion con una t-student

# Función para crear gráficas de densidad con la función de densidad t-Student
plot_density_with_t <- function(data, title, df) {
  df_data <- data.frame(value = data)
  p <- ggplot(df_data, aes(x = value)) +
    geom_density(fill = "blue", alpha = 0.5) +
    stat_function(fun = dt, args = list(df = df),
                  geom = "line", color = "red", size = 1.5) +
    labs(title = title, x = "Valor de U", y = "Densidad")
  return(p)
}

# Gráficas de densidad con t-Student para U
u_plots_t <- lapply(names(u_results), function(n) {
  df_t <- as.numeric(n) - 1 # Grados de libertad para la distribución t-Student
  plot_density_with_t(u_results[[n]], paste("U con n =", n), df_t)
})

# Mostrar gráficas con t-Student para U
for (p in u_plots_t) {
  print(p)
}

# ---------------------------------------- PREGUNTA 3 ---------------------------------------- #

# Pensamos que se distribuye Beta, y comprobamos con la libreria fitdistrplus

simulate_U2 <- function(n) {
  X <- runif(n, min = 0, max = 5)
  
  U <- max(X) - 5
  return(U)
}

u2_results <- list()
for (n in c(10, 100, 1000, 10000)) {
  u2_results[[as.character(n)]] <- replicate(1000, simulate_U2(n))
}

# Gráficas de densidad para U2.
u2_plots <- lapply(names(u2_results), function(n) {
  plot_density(u2_results[[n]], paste("U2 con n =", n))
})

# Mostrar gráficas para U2
for (p in u2_plots) {
  print(p)
}

# ---------------------------------------- PREGUNTA 4 ---------------------------------------- #

#Función para simular el lanzamiento de tres dados y sumar sus caras
simular_tres_dados <- function() {
  dado1 <- sample(1:6, 1)
  dado2 <- sample(1:6, 1)
  dado3 <- sample(1:6, 1)
  
  X <- dado1 + dado2 + dado3
  return(X)
}
# (a) Realiza las simulaciones y calcula la función de masa de probabilidad aproximada para cada valor de n
n_values <- c(100, 1000, 10000, 100000)

# Función para calcular la frecuencia relativa
frecuencia_relativa <- function(x) {
  return(table(x) / length(x))
}

# funcion de densidad
pmf_results <- lapply(n_values, function(n) {
  Xs <- replicate(n, simular_tres_dados())
  pmf <- frecuencia_relativa(Xs)
  return(pmf)
})

#( b) Utiliza las librerías 'patchwork' y 'ggplot2' para mostrar las gráficas de las funciones de masa en un solo gráfico
# Función para crear una gráfica de barras de la función de masa de probabilidad
crear_grafica_pmf <- function(pmf, n) {
  data_pmf <- data.frame(x = as.numeric(names(pmf)), y = as.vector(pmf))
  ggplot(data_pmf, aes(x = x, y = y)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    ggtitle(paste("Función de masa de probabilidad para n =", n)) +
    xlab("X") +
    ylab("Probabilidad") +
    theme_minimal()
}

# Crea las gráficas
pmf_plots <- mapply(crear_grafica_pmf, pmf_results, n_values, SIMPLIFY = FALSE)

# Combina las gráficas en un solo gráfico
combined_plot <- (pmf_plots[[1]] / pmf_plots[[2]]) | (pmf_plots[[3]] / pmf_plots[[4]])
print(combined_plot)

# (c) Realiza las simulaciones para estimar la probabilidad P(X ≤ 3)
n_values_c <- 10:5000

probabilidad_X_leq_3 <- sapply(n_values_c, function(n) {
  Xs <- replicate(n, simular_tres_dados())
  probabilidad <- sum(Xs <= 3) / n
  return(probabilidad)
})

# Crea un gráfico de la probabilidad en función de n
probabilidad_plot <- ggplot(data.frame(n = n_values_c, probabilidad = probabilidad_X_leq_3), aes(x = n, y = probabilidad)) +
  geom_line(color = "steelblue") +
  ggtitle("Probabilidad estimada de P(X ≤ 3)") +
  xlab("n") +
  ylab("Probabilidad") +
  theme_minimal()
print(probabilidad_plot)

# ---------------------------------------- PREGUNTA 5 ---------------------------------------- #

#Función para simular la extracción de 2 bolas sin reemplazo y calcular la suma de los números en las bolas
simular_extraccion_bolas <- function() {
  bolas <- sample(1:7, 2, replace = FALSE)
  X <- sum(bolas)
  return(X)
}

# (a) Realiza las simulaciones y calcula la función de masa de probabilidad aproximada para cada valor de n
n_values <- c(100, 1000, 10000, 100000)

pmf_results <- lapply(n_values, function(n) {
  Xs <- replicate(n, simular_extraccion_bolas())
  pmf <- frecuencia_relativa(Xs)
  return(pmf)
})

# (b) Utiliza las librerías 'patchwork' y 'ggplot2' para mostrar las gráficas de las funciones de masa en un solo gráfico
pmf_plots <- mapply(crear_grafica_pmf, pmf_results, n_values, SIMPLIFY = FALSE)

# Combinamos
combined_plot <- (pmf_plots[[1]] / pmf_plots[[2]]) | (pmf_plots[[3]] / pmf_plots[[4]])
print(combined_plot)

#(c) Realiza las simulaciones para estimar la probabilidad P(X ≤ 10)
n_values_c <- 10:100000
probabilidad_X_leq_10 <- sapply(n_values_c, function(n) {
  Xs <- replicate(n, simular_extraccion_bolas())
  probabilidad <- sum(Xs <= 10) / n
  return(probabilidad)
})

# Graficamos la probabilidad estimada
probabilidad_plot <- ggplot(data.frame(n = n_values_c, probabilidad = probabilidad_X_leq_10), aes(x = n, y = probabilidad)) +
  geom_line(color = "steelblue") +
  ggtitle("Probabilidad estimada de P(X ≤ 10)") +
  xlab("n") +
  ylab("Probabilidad") +
  theme_minimal()
print(probabilidad_plot)

# ---------------------------------------- PREGUNTA 6 ---------------------------------------- #
# Simulamos la extraccion de las 7 bolas, experimento con reemplazo
simular_extraccion_bolas_con_reemplazo <- function() {
  bolas <- sample(1:7, 2, replace = TRUE)
  X <- sum(bolas)
  return(X)
}

n_values <- c(100, 1000, 10000, 100000)

# funcion de densidad
pmf_results_con_reemplazo <- lapply(n_values, function(n) {
  Xs <- replicate(n, simular_extraccion_bolas_con_reemplazo())
  pmf <- frecuencia_relativa(Xs)
  return(pmf)
})
pmf_plots_con_reemplazo <- mapply(crear_grafica_pmf, pmf_results_con_reemplazo, n_values, SIMPLIFY = FALSE)

# combinamos las graficas
combined_plot_con_reemplazo <- (pmf_plots_con_reemplazo[[1]] / pmf_plots_con_reemplazo[[2]]) | (pmf_plots_con_reemplazo[[3]] / pmf_plots_con_reemplazo[[4]])
print(combined_plot_con_reemplazo)

n_values_c <- 10:100000

# Calculamos la probabilidad
probabilidad_X_leq_10_con_reemplazo <- sapply(n_values_c, function(n) {
  Xs <- replicate(n, simular_extraccion_bolas_con_reemplazo())
  probabilidad <- sum(Xs <= 10) / n
  return(probabilidad)
})

# graficamos la probabilidad estimada
probabilidad_plot_con_reemplazo <- ggplot(data.frame(n = n_values_c, probabilidad = probabilidad_X_leq_10_con_reemplazo), aes(x = n, y = probabilidad)) +
  geom_line(color = "steelblue") +
  ggtitle("Probabilidad estimada de P(X ≤ 10) con reemplazo") +
  xlab("n") +
  ylab("Probabilidad") +
  theme_minimal()
print(probabilidad_plot_con_reemplazo)

# ---------------------------------------- PREGUNTA 7 ---------------------------------------- #
# Experimento de la urna con claves unicas de los alumnos
simular_clave_urna <- function(num_estudiantes) {
  claves <- 1:num_estudiantes
  claves_tomadas <- sample(claves, num_estudiantes, replace = FALSE)
  coincidencias <- sum(claves_tomadas == claves)
  return(coincidencias)
}

# (a) Realiza las simulaciones y calcula la función de masa de probabilidad aproximada para cada valor de n
num_estudiantes <- 50
n_values <- c(100, 1000, 10000, 100000)

# funcion de densidad
pmf_results_claves <- lapply(n_values, function(n) {
  Xs <- replicate(n, simular_clave_urna(num_estudiantes))
  pmf <- frecuencia_relativa(Xs)
  return(pmf)
})

# (b) Utiliza las librerías 'patchwork' y 'ggplot2' para mostrar las gráficas de las funciones de masa en un solo gráfico
pmf_plots_claves <- mapply(crear_grafica_pmf, pmf_results_claves, n_values, SIMPLIFY = FALSE)

# combinamos graficas
combined_plot_claves <- (pmf_plots_claves[[1]] / pmf_plots_claves[[2]]) | (pmf_plots_claves[[3]] / pmf_plots_claves[[4]])
print(combined_plot_claves)

# (c) Realiza las simulaciones para estimar la probabilidad P(X ≤ 15)
n_values_c <- 10:100000
probabilidad_X_leq_15 <- sapply(n_values_c, function(n) {
  Xs <- replicate(n, simular_clave_urna(num_estudiantes))
  probabilidad <- sum(Xs <= 15) / n
  return(probabilidad)
})

# graficamos la probabilidad estimada
probabilidad_plot_15 <- ggplot(data.frame(n = n_values_c, probabilidad = probabilidad_X_leq_15), aes(x = n, y = probabilidad)) +
  geom_line(color = "steelblue") +
  ggtitle("Probabilidad estimada de P(X ≤ 15)") +
  xlab("n") +
  ylab("Probabilidad") +
  theme_minimal()
print(probabilidad_plot_15)

# ---------------------------------------- PREGUNTA 8 ---------------------------------------- #
# Suma de numeros uniformes (0,1), hasta que la suma sea >= 1
sumar_uniformes_hasta_1 <- function() {
  suma <- 0
  N <- 0
  while (suma < 1) {
    suma <- suma + runif(1)
    N <- N + 1
  }
  return(N)
}

# (a) Realiza las simulaciones y calcula la función de masa de probabilidad aproximada para cada valor de n
n_values <- c(100, 1000, 10000, 100000)

pmf_results_N <- lapply(n_values, function(n) {
  Ns <- replicate(n, sumar_uniformes_hasta_1())
  pmf <- frecuencia_relativa(Ns)
  return(pmf)
})

# (b) Utiliza las librerías 'patchwork' y 'ggplot2' para mostrar las gráficas de las funciones de masa en un solo gráfico
pmf_plots_N <- mapply(crear_grafica_pmf, pmf_results_N, n_values, SIMPLIFY = FALSE)

combined_plot_N <- (pmf_plots_N[[1]] / pmf_plots_N[[2]]) | (pmf_plots_N[[3]] / pmf_plots_N[[4]])
print(combined_plot_N)

# (c) Realiza las simulaciones para estimar el valor esperado E(N)
n_values_c <- 10:100000
valor_esperado_N <- sapply(n_values_c, function(n) {
  Ns <- replicate(n, sumar_uniformes_hasta_1())
  valor_esperado <- mean(Ns)
  return(valor_esperado)
})

valor_esperado_plot <- ggplot(data.frame(n = n_values_c, valor_esperado = valor_esperado_N), aes(x = n, y = valor_esperado)) +
  geom_line(color = "steelblue") +
  ggtitle("Valor esperado estimado de E(N)") +
  xlab("n") +
  ylab("Valor esperado") +
  theme_minimal()
print(valor_esperado_plot)

# ---------------------------------------- PREGUNTA 9 ---------------------------------------- #
# Estadistico de orden 2, dado que Xi se distribuye uniforme (0,1)
segundo_menor <- function(n) {
  x <- runif(n)
  return(sort(x)[2])
}

# (a) Realiza las simulaciones y calcula la función de densidad de probabilidad aproximada para cada valor de n
n_values <- c(100, 1000, 10000, 100000)
simulations <- 10000

pdf_results_X2 <- lapply(n_values, function(n) {
  X2s <- replicate(simulations, segundo_menor(n))
  pdf <- density(X2s)
  return(pdf)
})

# (b) Utiliza la librería 'patchwork' para mostrar las gráficas de las funciones de densidad en un solo gráfico
crear_grafica_pdf <- function(pdf, n) {
  df <- data.frame(x = pdf$x, y = pdf$y)
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    labs(title = paste0("n = ", n),
         x = "X(2)",
         y = "Densidad")
  return(p)
}

pdf_plots_X2 <- mapply(crear_grafica_pdf, pdf_results_X2, n_values, SIMPLIFY = FALSE)

combined_plot_X2 <- (pdf_plots_X2[[1]] / pdf_plots_X2[[2]]) | (pdf_plots_X2[[3]] / pdf_plots_X2[[4]])
print(combined_plot_X2)

# (c) 
# Observando las gráficas, podrías notar que la densidad de X(2) se parece a la densidad de una distribución Beta con parámetros α = 2 y β = n - 1.
# Además por lo visto en clase podemos saber que el estadistico de orden j se distribuye beta con parámetros:
# Esperanza = j/(n+1)     Varianza : [j(n-j+1)]/[(n+1)^2 (n+2)]

# (d) Este resultado es consistente con el ejemplo teórico que se vio en clase. 
# La densidad de probabilidad de la k-ésima orden estadística de una muestra de tamaño n de la distribución
# uniforme (0, 1) sigue una distribución Beta con parámetros α = k y β = n - k + 1. En este caso, k = 2, 
# por lo que la densidad de probabilidad de X(2) sigue una distribución Beta con parámetros α = 2 y β = n - 1.

# ---------------------------------------- PREGUNTA 10 ---------------------------------------- #

# (a) Simulaciones para obtener la funcion de densidad de probabilidad aproximada de Xmed.
n_values <- c(100, 1000, 10000, 100000)
n_simulations <- 10000

# Calculamos la media
calculate_median <- function(x) {
  n <- length(x)
  if (n %% 2 == 1) {
    return(x[(n + 1) / 2])
  } else {
    return(0.5 * (x[n / 2] + x[(n + 1) / 2]))
  }
}

# Aplicamos la simulacion y ordenamos las variables aleatorias para obtener los estadisticos de orden
simulate_Xmed <- function(n) {
  X <- runif(n, min = -1, max = 1)
  X_sorted <- sort(X)
  return(calculate_median(X_sorted))
}

simulate_Xmed_n_times <- function(n) {
  sapply(1:n_simulations, function(x) simulate_Xmed(n))
}

pdf_results_Xmed <- lapply(n_values, function(n) {
  simulations <- simulate_Xmed_n_times(n)
  density(simulations)
})

# (b) Graficar
crear_grafica_pdf_Xmed <- function(pdf, n) {
  df <- data.frame(x = pdf$x, y = pdf$y)
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    labs(title = paste0("n = ", n),
         x = "X_med",
         y = "Densidad")
  return(p)
}

pdf_plots_Xmed <- mapply(crear_grafica_pdf_Xmed, pdf_results_Xmed, n_values, SIMPLIFY = FALSE)

combined_plot_Xmed <- (pdf_plots_Xmed[[1]] / pdf_plots_Xmed[[2]]) | (pdf_plots_Xmed[[3]] / pdf_plots_Xmed[[4]])
print(combined_plot_Xmed)

# (c) ¿Que densidad diria que tiene X_med?
# Observando las gráficas generadas en el inciso b, podemos ver que la densidad de X_med tiende a ser más 
# concentrada alrededor de 0. Esto es consistente con la intuición, ya que las Xi's siguen una distribución uniforme en el intervalo (-1, 1), 
# y su mediana debería estar cerca del centro del intervalo. 

# ---------------------------------------- PREGUNTA 11 ---------------------------------------- #

# (a) Obtenga la función de densidad de probabilidad aproximada de X_bar, tenemos 999 v.a.
n_values <- c(100, 1000, 10000, 100000)
n_simulations <- 10000

simulate_X <- function() {
  X <- c(runif(999, min = -1, max = 1), runif(1, min = 200, max = 300))
  return(X)
}

simulate_Xbar_n_times <- function(n) {
  sapply(1:n_simulations, function(x) mean(simulate_X()))
}

pdf_results_Xbar <- lapply(n_values, function(n) {
  simulations <- simulate_Xbar_n_times(n)
  density(simulations)
})


# (b) Usando las funciones de la librería 'patchwork', ponga en un mismo gráfico las cuatro gráficas de las funciones de masa del inicio (a):
crear_grafica_pdf_Xbar <- function(pdf, n) {
  df <- data.frame(x = pdf$x, y = pdf$y)
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    labs(title = paste0("n = ", n),
         x = "X_bar",
         y = "Densidad")
  return(p)
}

pdf_plots_Xbar <- mapply(crear_grafica_pdf_Xbar, pdf_results_Xbar, n_values, SIMPLIFY = FALSE)

combined_plot_Xbar <- (pdf_plots_Xbar[[1]] / pdf_plots_Xbar[[2]]) | (pdf_plots_Xbar[[3]] / pdf_plots_Xbar[[4]])
print(combined_plot_Xbar)

# (c) Observando las gráficas generadas en el inciso b, 
# podemos ver que la densidad de X_bar parece no seguir una distribución normal. 
# La presencia de una variable aleatoria con una distribución muy diferente (X1000 ∼ Unif[200, 300]) 
# parece estar afectando la distribución de la media muestral.


# (d) Sí, se podría decir que se está violando el supuesto del Teorema del Límite Central en este caso. 
# El teorema establece que la media muestral tiende a una distribución normal cuando el tamaño de la muestra es suficientemente grande, pero esto se aplica 
# cuando las variables aleatorias son idénticamente distribuidas. En este caso, la presencia de X1000 con una distribución muy diferente está afectando 
# la distribución de la media muestral y, por lo tanto, el Teorema del Límite Central no se aplica de manera directa.

# ---------------------------------------- PREGUNTA 12 ---------------------------------------- #

# (a) Realice las simulaciones y obtenga la función de densidad de probabilidad aproximada de X_med:

n_values <- c(100, 1000, 10000, 100000)
n_simulations <- 10000

simulate_X_med <- function(n) {
  X <- rexp(n, rate = 1)
  X_sorted <- sort(X)
  
  if (n %% 2 == 1) {
    X_med <- X_sorted[(n + 1) / 2]
  } else {
    X_med <- 0.5 * (X_sorted[n / 2] + X_sorted[(n / 2) + 1])
  }
  
  return(X_med)
}

simulate_X_med_n_times <- function(n) {
  sapply(1:n_simulations, function(x) simulate_X_med(n))
}

pdf_results_X_med <- lapply(n_values, function(n) {
  simulations <- simulate_X_med_n_times(n)
  density(simulations)
})

# (b) Usando las funciones de la librería 'patchwork', ponga en un mismo gráfico las cuatro gráficas de las funciones de masa del inicio (a):
crear_grafica_pdf_X_med <- function(pdf, n) {
  df <- data.frame(x = pdf$x, y = pdf$y)
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    labs(title = paste0("n = ", n),
         x = "X_med",
         y = "Densidad")
  return(p)
}

pdf_plots_X_med <- mapply(crear_grafica_pdf_X_med, pdf_results_X_med, n_values, SIMPLIFY = FALSE)

combined_plot_X_med <- (pdf_plots_X_med[[1]] / pdf_plots_X_med[[2]]) | (pdf_plots_X_med[[3]] / pdf_plots_X_med[[4]])
print(combined_plot_X_med)

# (c) Observando las gráficas generadas en el inciso b, podemos ver que la densidad de X_med parece seguir una distribución diferente a la distribución exponencial original. 
# A medida que n aumenta, la densidad de X_med parece ser más concentrada cerca de cero y tiene una forma más estrecha. Sin embargo, no es fácil determinar exactamente qué distribución sigue.

# ---------------------------------------- PREGUNTA 13 ---------------------------------------- #

# (a) La distribución t-Student con 1 grado de libertad es una distribución de Cauchy. 
# Dado que la distribución de Cauchy no tiene media finita, no podemos calcular teóricamente E(X).

# (b) Realice las simulaciones y obtenga la función de densidad de probabilidad aproximada de ¯X:
n_values <- c(100, 1000, 10000, 100000)
n_simulations <- 10000

simulate_mean_t <- function(n) {
  X <- rt(n, df = 1)
  mean_X <- mean(X)
  return(mean_X)
}

simulate_mean_t_n_times <- function(n) {
  sapply(1:n_simulations, function(x) simulate_mean_t(n))
}

pdf_results_mean_t <- lapply(n_values, function(n) {
  simulations <- simulate_mean_t_n_times(n)
  density(simulations)
})

# (c) Usando las funciones de la librería 'patchwork', ponga en un mismo gráfico las cuatro gráficas de las funciones de masa del inicio (a):
crear_grafica_pdf_mean_t <- function(pdf, n) {
  df <- data.frame(x = pdf$x, y = pdf$y)
  p <- ggplot(df, aes(x = x, y = y)) +
    geom_line() +
    labs(title = paste0("n = ", n),
         x = "¯X",
         y = "Densidad")
  return(p)
}

pdf_plots_mean_t <- mapply(crear_grafica_pdf_mean_t, pdf_results_mean_t, n_values, SIMPLIFY = FALSE)

combined_plot_mean_t <- (pdf_plots_mean_t[[1]] / pdf_plots_mean_t[[2]]) | (pdf_plots_mean_t[[3]] / pdf_plots_mean_t[[4]])
print(combined_plot_mean_t)

# (d) Observando las gráficas generadas en el inciso c, podemos ver que la densidad de ¯X varía dependiendo del tamaño de n. 
# A medida que n aumenta, la densidad de ¯X parece converger hacia una distribución centrada en 0, pero sigue siendo diferente a la distribución normal.

# (e) Sí, se podría decir que se viola el Teorema del Límite Central en este caso. 
# La distribución t-Student con 1 grado de libertad (distribución de Cauchy) no cumple con los supuestos del Teorema del Límite Central, 
# que requiere que la distribución tenga media y varianza finitas. Como resultado, no observamos una convergencia hacia la distribución normal a medida que n aumenta.

# ---------------------------------------- PREGUNTA 14 ---------------------------------------- #

# (a) Simulación del lanzamiento de 2 dados regulares y obtención de la función de masa de X:
simular_dado_regular <- function() {
  return(sample(1:6, 1, replace = TRUE))
}

simulacion_X <- function(n) {
  sapply(1:n, function(x) {
    dado1 <- simular_dado_regular()
    dado2 <- simular_dado_regular()
    return(dado1 + dado2)
  })
}

n_simulaciones <- 100000
resultados_X <- simulacion_X(n_simulaciones)
fmp_X <- table(resultados_X) / n_simulaciones


# (b) Simulación del lanzamiento de 2 dados especiales y obtención de la función de masa de Y:
simular_dado_especial1 <- function() {
  return(5)
}

simular_dado_especial2 <- function() {
  return(sample(c(2, 2, 2, 6, 6, 6), 1, replace = TRUE))
}

simulacion_Y <- function(n) {
  sapply(1:n, function(x) {
    dado1 <- simular_dado_especial1()
    dado2 <- simular_dado_especial2()
    return(dado1 + dado2)
  })
}

resultados_Y <- simulacion_Y(n_simulaciones)
fmp_Y <- table(resultados_Y) / n_simulaciones

# (c) Simulación del lanzamiento de 2 dados especiales y obtención de la función de masa de Z:
simular_dado_especial3 <- function() {
  return(sample(c(1, 2, 2, 3, 3, 4), 1, replace = TRUE))
}

simular_dado_especial4 <- function() {
  return(sample(c(1, 3, 4, 5, 6, 8), 1, replace = TRUE))
}

simulacion_Z <- function(n) {
  sapply(1:n, function(x) {
    dado1 <- simular_dado_especial3()
    dado2 <- simular_dado_especial4()
    return(dado1 + dado2)
  })
}

resultados_Z <- simulacion_Z(n_simulaciones)
fmp_Z <- table(resultados_Z) / n_simulaciones

# (d) Al comparar las densidades de X, Y y Z, podemos ver que:

# La densidad de X, correspondiente a la suma de dos dados regulares, presenta una distribución triangular en el rango [2, 12]. 
# Los valores centrales tienen una mayor probabilidad, mientras que los valores extremos tienen una menor probabilidad.

# La densidad de Y, correspondiente a la suma de un dado con todas las caras marcadas con "5" y otro dado con 3 marcas de "2" y 3 marcas de "6", 
# presenta una distribución discreta y asimétrica en el rango [7, 11]. En este caso, la suma "7" tiene la mayor probabilidad.

# La densidad de Z, correspondiente a la suma de dos dados con marcas específicas en sus caras, presenta una distribución discreta en el rango [2, 12]. 
# En este caso, no hay un patrón obvio en la distribución.

# Cada una de estas densidades representa el comportamiento de la suma de los valores de los dados bajo diferentes condiciones de lanzamiento y tipos de dados.

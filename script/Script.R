# Integrantes: Saúl Pacheco Cubillo, Gael Ruiz Ñurinda, Anthony Segura Paniagua
library(readxl)
library(dplyr)
library(lubridate)
library(ggplot2)
library(queuecomputer)

# 8. Importación y Preparación de datos
ruta_archivo <- "Datos_IO.xlsx"
datos <- read_excel(ruta_archivo, sheet = "Datos_seleccionados")

datos_preparados <- datos %>%
    arrange(fecha, hora_llegada) %>%
    mutate(
      interarribo_min = as.numeric(difftime(hora_llegada, lag(hora_llegada), units = "mins")),
      tiempo_espera_min = as.numeric(difftime(hora_inicio_servicio, hora_llegada, units = "mins")),
      tiempo_servicio_min = as.numeric(difftime(hora_fin_servicio, hora_inicio_servicio, units = "mins")),
      tiempo_sistema_min = as.numeric(difftime(hora_fin_servicio, hora_llegada, units = "mins"))
    )

datos_limpios <- datos_preparados %>% filter(!is.na(interarribo_min) | id_observacion == 1)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# 11. Análisis descriptivo y visualización
cat("\n--- Resumen de tiempos (minutos) ---\n")
#summary(datos_limpios[, c("tiempo_espera_min", "tiempo_servicio_min", "tiempo_sistema_min", "interarribo_min")])

# Histograma del tiempo de servicio
p1 <- ggplot(datos_limpios, aes(x = tiempo_servicio_min)) +
      geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.8) +
      theme_minimal() +
      labs(title = "Distribución de los Tiempos de Servicio", x = "Tiempo de Servicio (minutos)", y = "Frecuencia (Cantidad de vehículos)")
#ggsave("hist_servicio.png", plot = p1, width = 6, height = 4)

# Llegadas por intervalo
datos_intervalos <- datos_limpios %>% select(intervalo_observacion, numero_llegadas_intervalo) %>% distinct()

p2 <- ggplot(datos_intervalos, aes(x = numero_llegadas_intervalo)) +
      geom_bar(fill = "darkorange", color = "black", alpha = 0.8) +
      theme_minimal() +
      labs(title = "Frecuencia de Llegadas por Intervalo de 10 minutos", x = "Número de Llegadas en el Intervalo", y = "Frecuencia")
#ggsave("bar_llegadas.png", plot = p2, width = 6, height = 4)


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# 12. Bondad de ajuste para Poisson
llegadas <- datos_intervalos$numero_llegadas_intervalo
n <- length(llegadas)
lambda_est <- mean(llegadas)

x_vals <- 0:max(llegadas)
O_i <- as.numeric(table(factor(llegadas, levels = x_vals)))

P_i <- dpois(x_vals, lambda_est)
P_i[length(P_i)] <- 1 - sum(dpois(0:(length(P_i)-2), lambda_est))
E_i <- n * P_i

O_agrupado <- c(sum(O_i[1:4]), sum(O_i[5:6]), sum(O_i[7:length(O_i)]))
E_agrupado <- c(sum(E_i[1:4]), sum(E_i[5:6]), sum(E_i[7:length(E_i)]))

tabla_chi <- data.frame(Grupo = c("<= 3 llegadas", "4 a 5 llegadas", ">= 6 llegadas"), Observado = O_agrupado,Esperado = E_agrupado)
print(tabla_chi)

chi2_calc <- sum((O_agrupado - E_agrupado)^2 / E_agrupado)
df_chi <- length(O_agrupado) - 1 - 1
chi2_crit <- qchisq(0.95, df_chi)
p_val <- 1 - pchisq(chi2_calc, df_chi)

cat(sprintf("Lambda estimado: %.4f\n", lambda_est))
cat(sprintf("Chi^2 Calculado: %.4f\n", chi2_calc))
cat(sprintf("Chi^2 Critico: %.4f\n", chi2_crit))
cat(sprintf("Valor p: %.4f\n", p_val))
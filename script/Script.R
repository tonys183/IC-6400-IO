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

O_agrupado <- c(sum(O_i[1:3]), sum(O_i[4:5]), sum(O_i[6:length(O_i)]))
E_agrupado <- c(sum(E_i[1:3]), sum(E_i[4:5]), sum(E_i[6:length(E_i)]))

tabla_chi <- data.frame(Grupo = c("<= 2 llegadas", "3 a 4 llegadas", ">= 5 llegadas"), Observado = O_agrupado, Esperado = E_agrupado)
print(tabla_chi)

chi2_calc <- sum((O_agrupado - E_agrupado)^2 / E_agrupado)
df_chi <- length(O_agrupado) - 1 - 1
chi2_crit <- qchisq(0.95, df_chi)
p_val <- 1 - pchisq(chi2_calc, df_chi)

cat(sprintf("Lambda estimado: %.4f\n", lambda_est))
cat(sprintf("Chi^2 Calculado: %.4f\n", chi2_calc))
cat(sprintf("Chi^2 Critico: %.4f\n", chi2_crit))
cat(sprintf("Valor p: %.4f\n", p_val))


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


# 13. Bondad de ajuste para Exponencial
tiempos_servicio <- datos_limpios$tiempo_servicio_min
n_servicios <- length(tiempos_servicio)
media_servicio <- mean(tiempos_servicio)
mu_est <- 1 / media_servicio
limites <- c(0, 1, 2, 3, 4, 6, Inf)
etiquetas <- c("[0, 1) min", "[1, 2) min", "[2, 3) min", "[3, 4) min", "[4, 6) min", "[6, +inf) min")

intervalos_cortados <- cut(tiempos_servicio, breaks = limites, labels = etiquetas, right = FALSE)
O_exp <- as.numeric(table(intervalos_cortados))

P_exp <- pexp(limites[-1], rate = mu_est) - pexp(limites[-length(limites)], rate = mu_est)
E_exp <- n_servicios * P_exp

tabla_chi_exp <- data.frame(Grupo = etiquetas, Observado = O_exp, Esperado = E_exp)
print(tabla_chi_exp)

chi2_calc_exp <- sum((O_exp - E_exp)^2 / E_exp)
df_chi_exp <- length(O_exp) - 1 - 1
chi2_crit_exp <- qchisq(0.95, df_chi_exp)
p_val_exp <- 1 - pchisq(chi2_calc_exp, df_chi_exp)

cat(sprintf("Tasa estimada (mu): %.4f\n", mu_est))
cat(sprintf("Chi2 Calculado: %.4f\n", chi2_calc_exp))
cat(sprintf("Chi2 Critico: %.4f\n", chi2_crit_exp))
cat(sprintf("Valor p: %.4f\n", p_val_exp))
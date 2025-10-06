#MarceloBianchini_rm567412_Fase2_cap7

# install.packages(c("readxl", "ggplot2", "dplyr", "modeest"), dependencies = TRUE)
library(readxl)
library(ggplot2)
library(dplyr)
library(modeest)
library(scales)

# Ler o arquivo Excel
dados <- read_excel("C:\\users\\marce\\Documents\\Fiap\\MeuProjeto\\soja_municipios_2022.xlsx")

dados$produção_soja_ton <- as.numeric(gsub("[^0-9]", "", dados$produção_soja_ton))
dados$rendimento_kg_ha <- as.numeric(gsub("[^0-9]", "", dados$rendimento_kg_ha))

# Medidas de tendência central
media <- mean(dados$produção_soja_ton, na.rm = TRUE)
mediana <- median(dados$produção_soja_ton, na.rm = TRUE)
moda <- mfv(dados$produção_soja_ton)

cat("=== Medidas de Tendência Central ===\n")
cat("Média:", media, "\n")
cat("Mediana:", mediana, "\n")

tab <- table(dados$produção_soja_ton)
frequencia_max <- max(tab)
if (frequencia_max == 1) {
  cat("Moda: não existe (todos os valores são únicos)\n\n")
} else {
  valores_mais_repetidos <- names(tab[tab == frequencia_max])
  cat("Moda(s):", paste(valores_mais_repetidos, collapse = ", "), "\n\n")
}


# Medidas de dispersão
desvio <- sd(dados$produção_soja_ton, na.rm = TRUE)
variancia <- var(dados$produção_soja_ton, na.rm = TRUE)
amplitude <- max(dados$produção_soja_ton, na.rm = TRUE) - min(dados$produção_soja_ton, na.rm = TRUE)

cat("=== Medidas de Dispersão ===\n")
cat("Desvio padrão:", desvio, "\n")
cat("Variância:", variancia, "\n")
cat("Amplitude:", amplitude, "\n\n")

# Medidas de separatrizes
quartis <- quantile(dados$produção_soja_ton, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)
decis <- quantile(dados$produção_soja_ton, probs = seq(0.1, 0.9, 0.1), na.rm = TRUE)
percentil_90 <- quantile(dados$produção_soja_ton, probs = 0.9, na.rm = TRUE)

cat("=== Separatrizes ===\n")
cat("Quartis:\n"); print(quartis)
cat("Decis:\n"); print(decis)
cat("Percentil 90:", percentil_90, "\n\n")

library(ggplot2)
library(scales)

# === Gráficos da variável quantitativa ===

# Histograma
ggplot(dados, aes(x = produção_soja_ton)) +
  geom_histogram(binwidth = 100000, fill = "steelblue", color = "white", alpha = 0.7) +
  scale_x_continuous(labels = comma) +  # formata o eixo X
  labs(title = "Histograma da Produção de Soja por Município (2022)",
       x = "Produção (ton)", y = "Frequência")

# Boxplot
ggplot(dados, aes(y = produção_soja_ton)) +
  geom_boxplot(fill = "lightgreen") +
  scale_y_continuous(labels = comma) +  # formata o eixo Y
  labs(title = "Boxplot da Produção de Soja (2022)",
       y = "Produção (ton)")

# Gráfico de barras por município
ggplot(dados, aes(x = reorder(municipio, -produção_soja_ton), y = produção_soja_ton)) +
  geom_bar(stat = "identity", fill = "orange") +
  scale_y_continuous(labels = comma) +  # formata o eixo Y
  coord_flip() +
  labs(title = "Produção de Soja por Município (2022)",
       x = "Município", y = "Produção (ton)")

# Gráfico de barras da variável qualitativa
ggplot(dados, aes(x = faixa_produção)) +
  geom_bar(fill = "purple") +
  scale_y_continuous(labels = comma) +  # formata o eixo Y
  labs(title = "Distribuição da Faixa de Produção",
       x = "Faixa de Produção", y = "Número de Municípios")

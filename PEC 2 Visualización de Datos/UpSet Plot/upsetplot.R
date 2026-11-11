# Apagar warnings

oldw <- getOption("warn")
options(warn = -1)

# Carga Librerías

suppressMessages(library(UpSetR))
suppressMessages(library(tidyverse))

dataset2526 <- read.csv("C:/Users/Didac/Desktop/MÁSTER UOC/Viz2/PEC 2/season-2526.csv")
dataset2425 <- read.csv("C:/Users/Didac/Desktop/MÁSTER UOC/Viz2/PEC 2/season-2425.csv")
dataset2324 <- read.csv("C:/Users/Didac/Desktop/MÁSTER UOC/Viz2/PEC 2/season-2324.csv")
dataset2223 <- read.csv("C:/Users/Didac/Desktop/MÁSTER UOC/Viz2/PEC 2/season-2223.csv")
dataset2122 <- read.csv("C:/Users/Didac/Desktop/MÁSTER UOC/Viz2/PEC 2/season-2122.csv")

# Juntar los datos
dataset <- rbind(dataset2526,dataset2425,dataset2324,dataset2223,dataset2122)

# Modificar las etiquetas
dataset <- dataset %>%
  mutate(
    HTR = case_when(
      HTR == "H" ~ "Local",
      HTR == "A" ~ "Visitante",
      HTR == "D" ~ "Empate",
      TRUE ~ HTR
    ),
    FTR = case_when(
      FTR == "H" ~ "Local",
      FTR == "A" ~ "Visitante",
      FTR == "D" ~ "Empate",
      TRUE ~ FTR
    )
  )
# Variables binarias
df_upset <- dataset %>%
  mutate(
    HT_Local     = if_else(HTR == "Local", 1, 0),
    HT_Visitante = if_else(HTR == "Visitante", 1, 0),
    HT_Empate    = if_else(HTR == "Empate", 1, 0),
    
    FT_Local     = if_else(FTR == "Local", 1, 0),
    FT_Visitante = if_else(FTR == "Visitante", 1, 0),
    FT_Empate    = if_else(FTR == "Empate", 1, 0)
  )

# Gráfico UpSet
upset(
  df_upset,
  sets = c("HT_Local", "HT_Visitante", "HT_Empate", "FT_Local", "FT_Visitante", "FT_Empate"),
  sets.bar.color = "#56B4E9",
  order.by = "freq",
  main.bar.color = "#E69F00",
  matrix.color = "gray23",
  keep.order = TRUE,
  text.scale = 1.1,
  mb.ratio = c(0.6, 0.4)
)

### EXTRA

# Frecuencias y probabilidades
df_parejas <- dataset %>%
  filter(!is.na(HTR) & !is.na(FTR)) %>% # Limpiar posibles nulos si los hubiera
  count(HTR, FTR) %>%
  mutate(
    # Ordenar explícitamente los niveles de los factores
    HTR = factor(HTR, levels = c("Local", "Empate", "Visitante")),
    FTR = factor(FTR, levels = c("Local", "Empate", "Visitante")),
    probability = n / sum(n),
    percentage = round(probability * 100, 1)
  )
  
# Matriz
ggplot(df_parejas, aes(x = HTR, y = FTR, fill = probability)) +
  geom_tile(color = "white", linewidth = 1.5) +
  geom_text(aes(label = paste0(percentage, "%\n(n=", n, ")")), color = "black", fontface = "bold", size = 4) +
  scale_fill_gradient(low = "#f0fdf4", high = "#22c55e") + # Gradiente de color según probabilidad
  theme_minimal() +
  labs(
    title = "Matriz de Probabilidad: Descanso (HTR) vs Final (FTR)",
    subtitle = "Porcentaje de partidos y conteo absoluto para cada pareja de resultados",
    x = "Resultado a Media Parte (HTR)",
    y = "Resultado al Final del Partido (FTR)",
    fill = "Probabilidad"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 10, face = "bold"),
    panel.grid = element_blank()
  )

# Encender warnings
options(warn = oldw)

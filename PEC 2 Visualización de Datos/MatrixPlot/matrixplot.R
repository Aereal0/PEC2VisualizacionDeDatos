# Apagar warnings

oldw <- getOption("warn")
options(warn = -1)

# Carga datos
dataset <- read.csv("C:/Users/Didac/Desktop/MÁSTER UOC/Viz2/PEC 2/datasetmatrix.csv")

# Carga librerías
suppressMessages(library(tidyverse))
suppressMessages(library(GGally))

# Seleccionar las variables
df_matrix <- dataset %>%
  select(
    gender,
    final_grade_pct,
    study_hours_per_week,
    attendance_rate_pct,
    parental_involvement_score,  
    distance_to_school_km        
  ) %>%
  mutate(gender = as.factor(gender))

# Matrix Plot
ggpairs(
  df_matrix,
  aes(color = gender, alpha = 0.15),
  upper = list(continuous = wrap("cor", size = 3)), 
  lower = list(continuous = wrap("points", size = 0.4, alpha = 0.15)), 
  diag = list(continuous = wrap("densityDiag", alpha = 0.4)) 
) +
  theme_minimal() +
  labs(
    title = "Matriz de Rendimiento Estudiantil (Dataset Completo)",
    subtitle = "Segregado por género (Male / Female)"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 12),
    axis.text = element_text(size = 7),
    strip.text = element_text(size = 8, face = "bold") 
  )

# Encender warnings
options(warn = oldw)

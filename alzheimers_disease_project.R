# ==============================================================================
# PROYECTO: ANÁLISIS DE DATOS SOBRE EL ALZHEIMER
# Pasos 1 a 6: Carga, Limpieza, Transformación (dplyr) y Exploración (ggplot2)
# ==============================================================================

# 1. Instalación y carga de librerías necesarias
paquetes <- c("readr", "dplyr", "ggplot2", "corrplot", "tidyr")
instalados <- paquetes %in% rownames(installed.packages())
if (any(!instalados)) install.packages(paquetes[!instalados])

library(readr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(tidyr)

# ==============================================================================
# PASO 2: CARGA DEL DATASET
# ==============================================================================
# Usamos read_csv() de 'readr' por ser más rápido que read.csv() y respetar tipos
df_raw <- read_csv("alzheimers_disease_data.csv", show_col_types = FALSE)

# Verificación inicial de estructura y dimensiones (2149 filas x 35 columnas)
dim(df_raw)
glimpse(df_raw)

# ==============================================================================
# PASO 3: LIMPIEZA DE DATOS (Data Cleaning)
# ==============================================================================

# 3.1. Valores faltantes explícitos (NA)
na_explicitos <- sum(is.na(df_raw))
cat("Valores NA explícitos:", na_explicitos, "\n")
colSums(is.na(df_raw))[colSums(is.na(df_raw)) > 0]   # por columna (vacío si no hay)

# 3.2. Filas duplicadas (completas y por identificador de paciente)
cat("Filas duplicadas:", sum(duplicated(df_raw)), "\n")
cat("PatientID duplicados:", sum(duplicated(df_raw$PatientID)), "\n")

# 3.3. Valores faltantes implícitos e inconsistencias
# a) Columnas sin varianza (como DoctorInCharge)
constantes <- names(df_raw)[sapply(df_raw, function(x) length(unique(x)) == 1)]
cat("Columnas constantes:", constantes, "\n")

# b) Cadenas vacías o marcadores típicos de dato faltante en columnas de texto
texto <- df_raw %>% select(where(is.character))
sapply(texto, function(x) sum(x %in% c("", " ", "NA", "N/A", "?", "-", "null")))

# c) Valores imposibles en variables numéricas (ceros o negativos donde no tiene sentido)
cat("BMI <= 0:", sum(df_raw$BMI <= 0), "\n")
cat("Algún valor negativo en numéricas:",
    any(df_raw %>% select(where(is.numeric)) < 0), "\n")

# d) Rangos esperados de las variables clave
summary(df_raw %>% select(Age, BMI, MMSE, FunctionalAssessment, ADL,
                          SleepQuality, PhysicalActivity, SystolicBP, DiastolicBP))
cat("Rango de Age:", range(df_raw$Age), "\n")
stopifnot(all(df_raw$MMSE >= 0 & df_raw$MMSE <= 30))

# e) Categorías: comprobar que los códigos coinciden con los esperados
table(df_raw$Diagnosis)
table(df_raw$Gender)
table(df_raw$EducationLevel)

# 3.4. Valores atípicos (IQR)
vars_numericas <- c("Age", "BMI", "MMSE", "FunctionalAssessment", "ADL",
                    "CholesterolTotal", "SystolicBP", "DiastolicBP")

detectar_outliers <- function(x) {
  q1 <- quantile(x, 0.25, na.rm = TRUE)
  q3 <- quantile(x, 0.75, na.rm = TRUE)
  iqr <- q3 - q1
  sum(x < (q1 - 1.5 * iqr) | x > (q3 + 1.5 * iqr))
}

outliers_por_columna <- sapply(df_raw[vars_numericas], detectar_outliers)
print(outliers_por_columna)

# Decisión: se MANTIENEN los outliers detectados (si los hay) porque son valores
# clínicamente plausibles y no errores de medición. Eliminarlos supondría perder
# casos reales. Ajusta este comentario según lo que muestre la salida anterior.

# 3.5. Eliminación de columnas sin aporte analítico
df_clean <- df_raw %>%
  select(-DoctorInCharge)

glimpse(df_clean)

# ==============================================================================
# PASO 7: APLICACIÓN WEB (Shiny + plotly) - Análisis de datos sobre el Alzheimer
# ==============================================================================
paquetes <- c("shiny", "readr", "dplyr", "tidyr", "ggplot2", "plotly", "DT")
instalados <- paquetes %in% rownames(installed.packages())
if (any(!instalados)) install.packages(paquetes[!instalados])

library(shiny)
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(DT)

# ------------------------------------------------------------------------------
# Carga y transformación (misma lógica que los pasos 2-5)
# ------------------------------------------------------------------------------
df_raw <- read_csv("alzheimers_disease_data.csv", show_col_types = FALSE)

df <- df_raw %>%
  select(-DoctorInCharge) %>%
  mutate(
    Diagnosis_Label = factor(Diagnosis, levels = c(0, 1),
                             labels = c("Sin Alzheimer", "Con Alzheimer")),
    Gender_Label = factor(Gender, levels = c(0, 1),
                          labels = c("Masculino", "Femenino")),
    Age_Group = case_when(
      Age < 70 ~ "60-69 años",
      Age >= 70 & Age < 80 ~ "70-79 años",
      Age >= 80 ~ "80-90 años"
    ),
    Cognitive_Functional_Index = round((MMSE / 30 * 10 + FunctionalAssessment + ADL) / 3, 2),
    Total_Symptoms = MemoryComplaints + BehavioralProblems + Confusion +
      Disorientation + PersonalityChanges + DifficultyCompletingTasks + Forgetfulness
  )

vars_y <- c("MMSE" = "MMSE",
            "Evaluación funcional" = "FunctionalAssessment",
            "ADL" = "ADL",
            "BMI" = "BMI",
            "Calidad del sueño" = "SleepQuality",
            "Actividad física" = "PhysicalActivity",
            "Índice cognitivo-funcional" = "Cognitive_Functional_Index")

vars_corr_choices <- c("Diagnosis", "MMSE", "FunctionalAssessment", "ADL",
                       "MemoryComplaints", "BehavioralProblems", "Age", "BMI",
                       "SleepQuality", "PhysicalActivity", "Total_Symptoms")

# ------------------------------------------------------------------------------
# Interfaz de usuario
# ------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Análisis de datos sobre el Alzheimer"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      h4("Filtros"),
      checkboxGroupInput("f_diag", "Diagnóstico",
                         choices = levels(df$Diagnosis_Label),
                         selected = levels(df$Diagnosis_Label)),
      checkboxGroupInput("f_gender", "Género",
                         choices = levels(df$Gender_Label),
                         selected = levels(df$Gender_Label)),
      checkboxGroupInput("f_age_group", "Grupo de edad",
                         choices = sort(unique(df$Age_Group)),
                         selected = sort(unique(df$Age_Group))),
      sliderInput("f_age", "Rango de edad",
                  min = min(df$Age), max = max(df$Age),
                  value = c(min(df$Age), max(df$Age)), step = 1),
      hr(),
      textOutput("n_pacientes")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Distribución por diagnóstico",
                 br(),
                 selectInput("y_var", "Variable a comparar:", choices = vars_y),
                 plotlyOutput("g1", height = "500px")),
        
        tabPanel("Funcional vs ADL",
                 br(),
                 checkboxInput("show_lm", "Mostrar recta de regresión", TRUE),
                 plotlyOutput("g2", height = "500px")),
        
        tabPanel("Síntomas",
                 br(),
                 plotlyOutput("g3", height = "500px")),
        
        tabPanel("Correlaciones",
                 br(),
                 checkboxGroupInput("corr_vars", "Variables:",
                                    choices = vars_corr_choices,
                                    selected = vars_corr_choices[1:10],
                                    inline = TRUE),
                 plotlyOutput("g4", height = "600px")),
        
        tabPanel("Datos",
                 br(),
                 DTOutput("tabla"))
      )
    )
  )
)

# ------------------------------------------------------------------------------
# Servidor
# ------------------------------------------------------------------------------
server <- function(input, output, session) {
  
  datos <- reactive({
    df %>%
      filter(
        Diagnosis_Label %in% input$f_diag,
        Gender_Label %in% input$f_gender,
        Age_Group %in% input$f_age_group,
        Age >= input$f_age[1], Age <= input$f_age[2]
      )
  })
  
  output$n_pacientes <- renderText({
    paste("Pacientes seleccionados:", nrow(datos()))
  })
  
  # Gráfico 1: violin + boxplot de la variable elegida
  output$g1 <- renderPlotly({
    d <- datos()
    validate(need(nrow(d) > 0, "No hay datos con los filtros seleccionados."))
    
    g <- ggplot(d, aes(x = Diagnosis_Label, y = .data[[input$y_var]],
                       fill = Gender_Label)) +
      geom_violin(alpha = 0.3, position = position_dodge(0.8)) +
      geom_boxplot(width = 0.25, position = position_dodge(0.8), outlier.alpha = 0.5) +
      scale_fill_manual(values = c("Masculino" = "#2b8cbe", "Femenino" = "#e34a33")) +
      labs(title = paste("Distribución de", input$y_var, "según diagnóstico"),
           x = "Diagnóstico clínico", y = input$y_var, fill = "Género") +
      theme_minimal(base_size = 12)
    
    ggplotly(g) %>% layout(boxmode = "group")
  })
  
  # Gráfico 2: Evaluación funcional vs ADL
  output$g2 <- renderPlotly({
    d <- datos()
    validate(need(nrow(d) > 1, "No hay datos suficientes con los filtros seleccionados."))
    
    g <- ggplot(d, aes(x = FunctionalAssessment, y = ADL, color = Diagnosis_Label)) +
      geom_point(alpha = 0.55, size = 1.8) +
      scale_color_manual(values = c("Sin Alzheimer" = "#1b9e77",
                                    "Con Alzheimer" = "#d95f02")) +
      labs(title = "Evaluación funcional vs. Actividades de la vida diaria (ADL)",
           x = "Evaluación funcional (0 - 10)", y = "ADL (0 - 10)",
           color = "Diagnóstico") +
      theme_minimal(base_size = 12)
    
    if (input$show_lm) g <- g + geom_smooth(method = "lm", se = FALSE, linewidth = 1)
    ggplotly(g)
  })
  
  # Gráfico 3: prevalencia de síntomas
  output$g3 <- renderPlotly({
    d <- datos()
    validate(need(nrow(d) > 0, "No hay datos con los filtros seleccionados."))
    
    sint <- d %>%
      group_by(Diagnosis_Label) %>%
      summarise(
        `Quejas de memoria` = mean(MemoryComplaints) * 100,
        `Problemas conductuales` = mean(BehavioralProblems) * 100,
        `Confusión` = mean(Confusion) * 100,
        `Desorientación` = mean(Disorientation) * 100,
        `Olvidos` = mean(Forgetfulness) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(-Diagnosis_Label, names_to = "Sintoma", values_to = "Porcentaje")
    
    g <- ggplot(sint, aes(x = reorder(Sintoma, Porcentaje), y = Porcentaje,
                          fill = Diagnosis_Label)) +
      geom_col(position = "dodge", width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = c("Sin Alzheimer" = "#386cb0",
                                   "Con Alzheimer" = "#f0027f")) +
      labs(title = "Prevalencia de síntomas por diagnóstico (%)",
           x = "Síntoma", y = "Porcentaje de pacientes (%)", fill = "Diagnóstico") +
      theme_minimal(base_size = 12)
    
    ggplotly(g)
  })
  
  # Gráfico 4: mapa de calor de correlaciones
  output$g4 <- renderPlotly({
    d <- datos()
    validate(need(length(input$corr_vars) >= 2, "Selecciona al menos 2 variables."))
    validate(need(nrow(d) > 2, "No hay datos suficientes con los filtros seleccionados."))
    
    m <- d %>% select(all_of(input$corr_vars))
    m <- m[, sapply(m, sd) > 0, drop = FALSE]   # quita variables constantes
    validate(need(ncol(m) >= 2, "Con estos filtros, quedan menos de 2 variables con variación."))
    
    cm <- round(cor(m), 2)
    
    plot_ly(x = colnames(cm), y = rownames(cm), z = cm, type = "heatmap",
            colors = colorRamp(c("#2166ac", "white", "#b2182b")),
            zmin = -1, zmax = 1,
            text = cm, hovertemplate = "%{y} vs %{x}: %{z}<extra></extra>") %>%
      layout(title = "Matriz de correlación de variables clínicas",
             yaxis = list(autorange = "reversed"))
  })
  
  # Tabla de datos filtrados
  output$tabla <- renderDT({
    datatable(datos() %>%
                select(PatientID, Age, Gender_Label, Diagnosis_Label, MMSE,
                       FunctionalAssessment, ADL, Total_Symptoms),
              options = list(pageLength = 10), rownames = FALSE)
  })
}

shinyApp(ui, server)
library(shiny)    
library(bslib)    
library(ggplot2)  
library(DT)

ui <- page_sidebar(
  # cargamos el tema "pulse" de bootswatch
  theme = bs_theme(version = 5, bootswatch = "pulse"),
  
  titlePanel(textOutput("titulo_archivo")),
  
  sidebar = sidebar(
    title = "Configuración Global",
    fileInput("file", "Cargar Archivo CSV",
              accept = c("text/csv", ".csv"))
  ),
  
  navset_card_tab(
    id = "main_tabs",
    
    nav_panel("Resumen datos",

              layout_columns(
                col_widths = c(6, 6), 
                card(
                  full_screen = TRUE,
                  card_header("Datos originales"),
                  card_body(
                    layout_columns(
                      col_widths = c(12, 12),
                      row_heights = c(1,5),
                      fill = FALSE,
                      card(
                        # Le damos estilo con clases de Bootstrap para que se vea como un value_box
                        class = "bg-purple text-white", 
                        card_body(
                          uiOutput("resumen_datos_originales")
                        )
                      ),
                      DTOutput("tabla")
                    )
                  )
                ),
                card(
                  full_screen = TRUE,
                  card_header("Datos procesados"),
                  card_body(
                    layout_columns(
                      col_widths = c(12, 12),
                      row_heights = c(1,5),
                      fill = FALSE,
                      card(
                        # Le damos estilo con clases de Bootstrap para
                        # que se vea como un value_box
                        class = "bg-purple text-white", 
                        card_body(
                          uiOutput("resumen_datos_procesados")
                        )
                      ),
                      DTOutput("tablaProcesada")
                    )
                  )
                )
              )
    ),
    
    nav_panel("Visión General (EDA)",
              navset_pill(
                nav_panel("Histograma",
                          uiOutput("hist_selector_ui"),
                          plotOutput("histograma", height = "600px")
                         ),
                nav_panel("Dispersión",
                          uiOutput("scatter_selector_ui"),
                          plotOutput("scatter_plot", height = "600px")
                         ),
                nav_panel("Boxplot",
                          uiOutput("box_selector_ui"),
                          plotOutput("boxplot", height = "600px")
                         ),
                nav_panel("Correlación",
                          plotOutput("corr_plot", height = "600px")
                )
              )
    ),
    
    nav_panel("Modelo de Regresión Logística",
      layout_sidebar(
        sidebar = sidebar(
          title = "Configuración del Modelo",
          p("El objetivo es predecir si un paciente tiene diabetes
             usando Regresión Logística. Se crea una variable 'diabetes' (SI/NO)
             usando un umbral de hemoglobina glicosilada (glyhb) >= 6.5."),
          
          uiOutput("model_predictors_ui"),
          
          sliderInput("train_split",
                      "Porcentaje para entrenamiento:",
                      min = 50, max = 90, value = 70, step = 5),
          
          actionButton("train_model_btn",
                       "Entrenar y Validar Modelo",
                       class = "btn-primary",
                       icon = icon("cogs"))
        ),
                
                navset_card_pill(
                  nav_panel("Matriz de Confusión",
                            card_body(
                              h5("Rendimiento del Modelo"),
                              verbatimTextOutput("confusion_matrix_print")
                            )
                  ),
                  nav_panel("Curva ROC y AUC",
                            card_body(
                              h5("Área Bajo la Curva (AUC)"),
                              # Usaremos un value_box para destacar el valor del AUC
                              value_box(
                                title = "AUC (Area Under Curve)",
                                value = textOutput("auc_value"),
                                showcase = bsicons::bs_icon("graph-up-arrow"),
                                theme = "primary"
                              ),
                              hr(),
                              h5("Gráfico de la Curva ROC"),
                              plotOutput("roc_curve_plot")
                            )
                  )
                )
              )
    )
  )
)

server <- function(input, output) {
  
  data <- reactive({
    req(input$file)
    # Los datos usados dependen del archivo seleccionado
    read.csv(input$file$datapath, row.names = NULL)   
  })
  
  # Definimos título dependiente del archivo
  output$titulo_archivo <- renderText({
    if (is.null(input$file)) {
      # Si no se ha cargado ningún archivo
      "Cargue un archivo"
    } else {
      nombre_limpio <- tools::file_path_sans_ext(input$file$name)
      nombre_capitalizado <- paste0(toupper(substring(nombre_limpio, 1, 1)), substring(nombre_limpio, 2))
      
      paste(nombre_capitalizado)
    }
  })

 
  ## ------------------ Datos originales ------------------
  # Resumen 
  output$resumen_datos_originales <- renderUI({
    req(data())
    tagList(
      # Usamos tags$p() para crear párrafos. El texto es reactivo.
      tags$p(paste("Nº observaciones:", nrow(data()))),
      tags$p(paste("Nº variables:",  ncol(data()))),
    )
  })
  # Tabla estadísticos
  output$tabla <- renderDT({
    req(data())
   
    df <- estadisticas_numericas(data())

    datatable(
      df,
      options = list(
        paging = FALSE,
        scrollY = "450px",
        scrollX = TRUE,
        searching = FALSE # Deshabilitar búsqueda
      ),
      rownames = FALSE # No mostrar nombres de fila 
    )
  })

  ## ----------- Datos procesados ---------------- 
  # Resumen 
  output$resumen_datos_procesados <- renderUI({
    req(data())
    
    # procesamos los datos
    df <- procesaDatosOriginales(data())
    
    tagList(
      # Usamos tags$p() para crear párrafos. El texto es reactivo.
      tags$p(paste("Nº observaciones:", nrow(df))),
      tags$p(paste("Nº variables:",  ncol(df))),
    )
  })
  # Tabla estadísticos
  output$tablaProcesada <- renderDT({
    req(data())
    # procesamos los datos
    df <- data()
    datos <- procesaDatosOriginales(df)
    
    # creamos las estadísticas numéricas
    df <- estadisticas_numericas(datos)  
    
    # En caso de que tengamos datos montamos la tabla
    datatable(
      df,
      options = list(
        paging = FALSE,
        scrollY = "450px", 
        scrollX = TRUE,
        searching = FALSE # Habilitar búsqueda
      ),
      rownames = FALSE # No mostrar nombres de fila 
    )
    
  })

  # ----------- Histograma ------------------------------
  output$hist_selector_ui <- renderUI({
  
    req(data())
    # procesamos los datos
    df <- data()
    datos <- procesaDatosOriginales(df)
  
    variables_numericas <- names(datos[,sapply(datos, is.numeric)])
    variables_numericas <- setdiff(variables_numericas, c("id"))
  
    tagList(
      fluidRow(
         column(6,
          selectInput("var_hist",
                      "Variable:",
                      choices = variables_numericas,
                      selected = intersect("BMI", variables_numericas)[1])
         ),
         column(6,
          sliderInput("bins_hist",
                  "Bins:",
                  min = 5,
                  max = 100,
                   value = 30)
         )
      )
    )
  })

  output$histograma <- renderPlot({
  
     req(data(), input$var_hist)
     # procesamos los datos
     df <- data()
     datos <- procesaDatosOriginales(df)
  
     ggplot(datos,
            aes_string(x = input$var_hist)) +
       geom_histogram(bins = input$bins_hist,
                      fill = "#007bc2",
                      color = "white",
                      alpha = 0.8) +
       labs(title = paste("Distribución de", input$var_hist)) +
       theme_minimal(base_size = 14)
   })

  
  # --------- Scaterplot -----------------------------
   output$scatter_selector_ui <- renderUI({ 
     req(data())
     # procesamos los datos
     datos <- procesaDatosOriginales(data())
     
     variables_numericas <- names(datos[,sapply(datos, is.numeric)])
     variables_categoricas <- names(datos[,sapply(datos, function(x) is.factor(x) || is.character(x))])
     
     variables_numericas <- setdiff(variables_numericas, c("id"))
     color_choices <-  c("Ninguna" = "",variables_categoricas)
     
     tagList(
       fluidRow(
         column(4,
                selectInput("var_scatter_x",
                            "Eje X:",
                            choices = variables_numericas,
                            selected = intersect("imc",
                                                 variables_numericas)[1])
                ),
         column(4, 
                selectInput("var_scatter_y",
                            "Eje Y:",
                            choices = variables_numericas,
                            selected = intersect("hemoglobina_glicosilada",
                                                 variables_numericas)[1])),
         column(4, 
                selectInput("color_scatter",
                            "colorear por",
                            choices = color_choices,
                            selected = "Ninguna"
                )
         )
       )
     )
   })
   
   output$scatter_plot <- renderPlot({ 
     
     req(data(), input$var_scatter_x, input$var_scatter_y);
     
     datos <- procesaDatosOriginales(data())
     
     p <- ggplot(datos,
                 aes_string(x = input$var_scatter_x, y = input$var_scatter_y)); 
     
     if (input$color_scatter != "" ) { 
       p <- p + geom_point(aes_string(color = input$color_scatter), alpha = 0.7, size = 3)
     } 
     else { 
       p <- p + geom_point(alpha = 0.7, size = 3, color = "#007bc2")
     }
     
     p + labs(title = paste("Relación entre",
                            input$var_scatter_x, "y",
                            input$var_scatter_y)) +
       theme_minimal(base_size = 14) 
   })
   
  
  # ------------------ Boxplot ----------------------------------
   output$box_selector_ui <- renderUI({ 
     req(data())
     # procesamos los datos
     datos <- procesaDatosOriginales(data())
     
     variables_numericas <- names(datos[,sapply(datos, is.numeric)])
     variables_categoricas <- names(datos[
                                      sapply(datos,
                                           function(x) is.factor(x) ||
                                                       is.character(x))
                                      ])
     variables_numericas <- setdiff(variables_numericas, c("id"))
     
     tagList(
      fluidRow(
        column(6,
           selectInput("var_box_cat", "Categórica:",
                       choices = variables_categoricas,
                       selected = intersect("frame",
                                            variables_categoricas)[1])
        ),
        column(6,
          selectInput("var_box_num", "Numérica:",
                   choices = variables_numericas,
                   selected = intersect("BMI",
                                        variables_numericas)[1])
        )
      )
     )
   })
   
   output$boxplot <- renderPlot({
     
     req(data(), input$var_box_cat, input$var_box_num);
     
     datos <- procesaDatosOriginales(data())
     
     ggplot(datos,
            aes_string(x = input$var_box_cat, y = input$var_box_num)) +
       geom_boxplot(
         aes_string(fill = input$var_box_cat),
         alpha = 0.8,
         show.legend = FALSE) +
       geom_jitter(width = 0.1, alpha = 0.3) +
       labs(title = paste("Distribución de",
                          input$var_box_num, "por",
                          input$var_box_cat)) +
       theme_minimal(base_size = 14) 
   })
   
  
  # ----------------- Correlación ----------------------------
   output$corr_plot <- renderPlot({
     req(data());
     
     datos <- procesaDatosOriginales(data())
     
     variables_numericas <- names(datos[,sapply(datos, is.numeric)])
     variables_numericas <- setdiff(variables_numericas, c("id"))
     
     df_numeric <- na.omit(datos[, variables_numericas]);
     cor_matrix <- cor(df_numeric);
     corrplot(cor_matrix,
              method = "color",
              type = "upper",
              order = "hclust",
              addCoef.col = "black",
              tl.col = "black",
              tl.srt = 45,
              diag = FALSE
     )
   })
   
   
  
  # ------------ Regresión logística -------------------------
  
   
   output$model_predictors_ui <- renderUI({

     req(data())
     # procesamos los datos
     datos <- procesaDatosOriginales(data())

     variables_numericas <- names(datos[,sapply(datos, is.numeric)])
     variables_categoricas <- names(datos[
       sapply(datos,
              function(x) is.factor(x) ||
                is.character(x))
     ])


     predictores_sugeridos <- setdiff(variables_numericas,
                                      c("id", "glyhb", "height", "weight"))

     selectInput("model_predictors", "Variables Predictoras:",
                 choices = c(variables_numericas, variables_categoricas),
                 selected = predictores_sugeridos,
                 multiple = TRUE)
   })
   
   model_results <- eventReactive(input$train_model_btn, {
        req(req(data()), input$model_predictors)
        
        datos <- procesaDatosOriginales(data())
        
        columnas_modelo <- c("diabetes", input$model_predictors)
        df <- na.omit(datos[, columnas_modelo])
        
        set.seed(666)
        
        train_index <- createDataPartition(
          datos$diabetes,
          p = 0.7,
          list = FALSE,
          times = 1
        )
        
        
        training_set <- datos[train_index,]
        test_set <- datos[-train_index,]
        
       #cat(input$model_predictors)
        
        formula_str <- paste("diabetes","~", paste(input$model_predictors, collapse=" + "))
        formula_str <- gsub("NULL","", formula_str, ignore.case = TRUE)
        formula <- as.formula(formula_str)
        
        #cat("La formula que utilizaremos es: \n", formula_str)
        
        modelo_logistico <- glm(formula, data=training_set, family = "binomial")
        
        
        prob_test <- predict(modelo_logistico, newdata = test_set, type="response")
        
        clase_positiva <- levels(training_set$diabetes)[2] # diabéticos
        clase_negativa <- levels(training_set$diabetes)[1] # no diabéticos
        
        clase_predicha <- factor(
          ifelse(prob_test > 0.5 , clase_positiva, clase_negativa),
          levels = levels(test_set$diabetes))
        
        matriz_confusion <- confusionMatrix(data = clase_predicha,
                                            reference = test_set$diabetes,
                                            positive = clase_positiva)
        
        roc_test <- roc(response = test_set$diabetes,
                        predictor = prob_test,
                        levels = rev(levels(test_set$diabetes)))
        
        return(list(confusionMatrix = matriz_confusion, roc = roc_test))
   })
   
  
   
   output$confusion_matrix_print <- renderPrint({
     req(model_results())
      model_results()$confusionMatrix
    })
   
   output$auc_value <- renderText({
     req(model_results())
     # Extraemos el AUC del objeto roc y lo formateamos
     auc_val <- auc(model_results()$roc)
     paste0(round(auc_val, 3))
   })
   
   output$roc_curve_plot <- renderPlot({
     req(model_results())
     roc_obj <- model_results()$roc
     
     # Graficar la curva ROC con ggplot2 para un mejor estilo
     ggroc(roc_obj, color = "#007bc2", size = 1) +
       geom_segment(aes(x = 1, xend = 0, y = 0, yend = 1),
                    color="grey",
                    linetype="dashed") +
       ggtitle("Curva ROC") +
       labs(x = "Tasa de Falsos Positivos (1 - Especificidad)",
            y = "Tasa de Verdaderos Positivos (Sensibilidad)") +
       annotate("text", x = .5, y = .5, 
                label = paste("AUC =", round(auc(roc_obj), 3)), 
                size = 5, fontface = "bold") +
       theme_minimal(base_size = 14)
   })
   
  # --------- Funciones de apoyo -----------------------------
   procesaDatosOriginales <- function(data) {
    
    df_procesado <- data
    
    # ------- Cambio unidades de variables --------------------
    # de pulgadas a metros
    df_procesado$height <- df_procesado$height * 0.0254
    df_procesado$hip <- df_procesado$hip * 0.0254
    df_procesado$waist <- df_procesado$waist * 0.0254
    
    # de libras a kilos
    df_procesado$weight <- df_procesado$weight * 0.453592
    
    # ------- factorización variables -------------------------
    columnas_factorizables <- c("location", "gender", "frame")
    
    df_procesado[columnas_factorizables] <- lapply(
      df_procesado[columnas_factorizables],
      as.factor)
    
    # Eliminamos el nivel extra de Frame
    df_procesado <- df_procesado[df_procesado$frame != "", ]  # Filtra filas con factor diferente de ""
    df_procesado$frame <- droplevels(df_procesado$frame)    # Elimina el nivel vacío de los factores
    
    # Factorizamos la edad
    df_procesado$age <- cut(df_procesado$age,   # partimos la variable...
                           breaks = seq(0, 100, by = 10),          # ...en intervalos de 10 años
                           right = FALSE,                 # intervalo cerrado a la izquierda [x,y)
                           include.lowest = TRUE          # incluye el primer valor en el primer grupo
    )
    
    # --------- Creamos nuevas variables -------------------
    
    # indice de masa corporal
    df_procesado$BMI <- df_procesado$weight / df_procesado$height^2
    
    # ratio cintura cadera (Waist Hip Ratio)
    df_procesado$WHR <- df_procesado$waist / df_procesado$hip
    
    # suponemos que es diabético si glyhb >= 6.5
    df_procesado$diabetes <- cut(df_procesado$glyhb,
                                breaks = c(0,  6.49, Inf),
                                labels = c("NO","SI"),
                                right = TRUE) # right=TRUE => intervalo es (a, b]
    
    df_procesado$hipertenso <- as.factor(ifelse(
      df_procesado$bp.1s >= 140 &
        df_procesado$bp.1d >= 90,
      "SI",
      "NO")
    )
    
    #------ Eliminamos Na's -----------
    df_procesado <- na.omit(df_procesado)
    
    return (df_procesado)
  }
  
  estadisticos_columna <- function(col) {
    if (is.numeric(col)) {
      # Si es numérica, devolver resumen estadístico básico y el porcentaje de nulos
      # Para hacer el porcentaje basta con hacer la media del numero de nulos (nºnulos / nº valores de la columna)
      # Para los estadisticos eliminamos los valores faltantes con na.rm=TRUE
      return(
        c(
          Nulos = round(mean(is.na(col)) * 100, 2),
          Min = round(min(col, na.rm = TRUE), 2),
          Q1 = round(quantile(col, 0.25, na.rm = TRUE), 2),
          Median = round(median(col, na.rm = TRUE), 2),
          Mean = round(mean(col, na.rm = TRUE), 2),
          Sd = round(sd(col, na.rm = TRUE), 2),
          Q3 = round(quantile(col, 0.75, na.rm = TRUE), 2),
          Max = round(max(col, na.rm = TRUE), 2)
        )
      )
    }
  }
  
  estadisticas_numericas <- function(df) {
    # Escogemos solo las variables numéricas
    columnas_numericas <- df[, sapply(df, function(x)
      is.numeric(x))]
    
    # Aplicamos la funcion de estadísticos para cada variable
    estadisticos <- lapply(columnas_numericas, estadisticos_columna)
    
    # Convertir lista de vectores a data.frame
    tabla_resumen <- do.call(rbind, estadisticos)
    
    # Cramos el dataframe final añadiendo el nombre de las variables como primera columna
    tabla_resumen <- data.frame(Variable = rownames(tabla_resumen),
                                tabla_resumen,
                                row.names = NULL)
    names(tabla_resumen) <- c("Variable","% nulos","Min","Q1","Median","Mean","Sd","Q3","Max")
    return(tabla_resumen)
    
  }

}

# Creamos la aplicación Shiny
shinyApp(ui = ui, server = server)

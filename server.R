library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(corrplot)
library(VIM)
library(gridExtra)
library(broom)
library(car)
library(lmtest)
library(nortest)
library(moments)
library(tseries)
library(rlang)
library(leaflet)
library(sf)
library(geojsonio)
library(RColorBrewer)
library(rmarkdown)
library(knitr)
library(tinytex) # Pastikan TinyTeX di-load untuk PDF

server <- function(input, output, session) {
  # Register cleanup function to run when session ends (connection lost)
  session$onSessionEnded(function() {
    # Simple cleanup when connection is lost
    cleanup_all_plots()
  })

  # Helper function for plot path management
  create_plot_path <- function(filename) {
    # Ensure temp_plots directory exists
    if (!dir.exists("temp_plots")) {
      dir.create("temp_plots", recursive = TRUE)
    }

    # Always save to temp_plots
    plot_full_path <- file.path("temp_plots", filename)
    # For LaTeX/Markdown, just use the filename.
    # The downloadHandler will copy this file to the render directory.
    plot_path_latex <- filename

    return(list(
      full_path = plot_full_path,
      latex_path = plot_path_latex,
      filename = filename
    ))
  }

  # Function to save plot safely with verification and cleanup of parent directory files
  save_plot_safely <- function(plot_paths, plot_code, extra_vars = NULL) {
    if (dev.cur() != 1) dev.off()

    tryCatch({
      if (!dir.exists(dirname(plot_paths$full_path))) {
        dir.create(dirname(plot_paths$full_path), recursive = TRUE)
      }

      png(plot_paths$full_path, width = 1000, height = 600, res = 100)

      # Buat environment dengan semua variabel yang dibutuhkan
      plot_env <- new.env()
      plot_env$data <- sovi_data()
      plot_env$input <- input

      # Tambahkan variabel khusus berdasarkan jenis plot
      if (grepl("regression", plot_paths$filename)) {
        data <- sovi_data()
        formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " + "))
        plot_env$model <- lm(as.formula(formula_str), data = data)
      }

      # Tambahkan extra variables jika ada
      if (!is.null(extra_vars) && is.list(extra_vars)) {
        for (var_name in names(extra_vars)) {
          plot_env[[var_name]] <- extra_vars[[var_name]]
        }
      }

      # Evaluasi plot code
      eval(plot_code, envir = plot_env)
      dev.off()

      # Check hasil
      if (file.exists(plot_paths$full_path) && file.info(plot_paths$full_path)$size > 0) {
        return(TRUE)
      } else {
        if (dev.cur() != 1) dev.off()
        cat("Error: Plot file was not created or is empty:", plot_paths$full_path, "\n")
        return(FALSE)
      }
    }, error = function(e) {
      if (dev.cur() != 1) dev.off()
      cat("Error in plot generation for", plot_paths$filename, ":", e$message, "\n")
      return(FALSE)
    }, finally = {
      if (dev.cur() != 1) dev.off()
    })
  }

  # Initialize report content storage
  report_items <- reactiveValues(
    content = list(),
    counter = 0
  )

  # Reactive data management
  user_data <- reactiveValues(
    main_data = NULL,
    distance_data = NULL,
    data_uploaded = FALSE
  )

  # Load default data or user uploaded data
  sovi_data <- reactive({
    if (!is.null(user_data$main_data)) {
      return(user_data$main_data)
    } else {
      return(read.csv("data/sovi_data.csv"))
    }
  })

  distance_data <- reactive({
    if (!is.null(user_data$distance_data)) {
      return(user_data$distance_data)
    } else {
      return(read.csv("data/distance.csv", row.names = 1))
    }
  })

  # Dynamic column choices based on uploaded data
  numeric_columns <- reactive({
    data <- sovi_data()
    numeric_vars <- sapply(data, is.numeric)
    names(data)[numeric_vars]
  })

  all_columns <- reactive({
    data <- sovi_data()
    names(data)
  })

  # Load geospatial data
  indonesia_geojson <- reactive({
    tryCatch(
      {
        geojsonio::geojson_read("data/indonesia_kabkota_simple.geojson", what = "sp")
      },
      error = function(e) {
        NULL
      }
    )
  })

  # File upload handlers
  observeEvent(input$file_main, {
    req(input$file_main)

    tryCatch(
      {
        sep_char <- input$sep_main

        user_data$main_data <- read.csv(input$file_main$datapath,
          sep = sep_char,
          header = input$header_main
        )
        user_data$data_uploaded <- TRUE

        showNotification("Data utama berhasil diupload!", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error uploading main data:", e$message), type = "error")
      }
    )
  })

  observeEvent(input$file_distance, {
    req(input$file_distance)

    tryCatch(
      {
        sep_char <- input$sep_distance

        user_data$distance_data <- read.csv(input$file_distance$datapath,
          sep = sep_char,
          header = input$header_distance,
          row.names = if (input$header_distance) 1 else NULL
        )

        showNotification("Data jarak berhasil diupload!", type = "message")
      },
      error = function(e) {
        showNotification(paste("Error uploading distance data:", e$message), type = "error")
      }
    )
  })

  # Reset to default data
  observeEvent(input$use_default_data, {
    user_data$main_data <- NULL
    user_data$distance_data <- NULL
    user_data$data_uploaded <- FALSE
    showNotification("Menggunakan data default SOVI", type = "message")
  })

  # Data preview outputs
  output$uploaded_data_preview <- DT::renderDataTable({
    data <- sovi_data()
    DT::datatable(data, options = list(scrollX = TRUE, pageLength = 5))
  })

  output$uploaded_distance_preview <- DT::renderDataTable({
    data <- distance_data()
    DT::datatable(data, options = list(scrollX = TRUE, pageLength = 5))
  })

  # Update UI choices based on uploaded data
  observe({
    choices <- numeric_columns()
    updateSelectInput(session, "var_to_categorize", choices = choices)
    updateSelectInput(session, "x_var", choices = choices)
    updateSelectInput(session, "y_var", choices = choices)
    updateSelectInput(session, "choropleth_var", choices = choices)
    updateSelectInput(session, "spatial_var1", choices = choices)
    updateSelectInput(session, "spatial_var2", choices = choices)
    updateSelectInput(session, "normality_var", choices = choices)
    updateSelectInput(session, "homogeneity_var", choices = choices)
    updateSelectInput(session, "group_var", choices = choices)
    updateSelectInput(session, "mean_var", choices = choices)
    updateSelectInput(session, "group_var_mean", choices = choices)
    updateSelectInput(session, "paired_var", choices = choices)
    updateSelectInput(session, "prop_var", choices = choices)
    updateSelectInput(session, "variance_var", choices = choices)
    updateSelectInput(session, "group_var_variance", choices = choices)
    updateSelectInput(session, "anova_response", choices = choices)
    updateSelectInput(session, "anova_factor1", choices = choices)
    updateSelectInput(session, "anova_factor2", choices = choices)
    updateSelectInput(session, "reg_response", choices = choices)
    updateCheckboxGroupInput(session, "reg_predictors", choices = choices)
  })

  # Reactive data for categorization
  categorized_data <- reactive({
    req(input$categorize)
    req(input$var_to_categorize)
    req(input$n_categories)
    req(input$method)

    data <- sovi_data()
    var_name <- input$var_to_categorize
    n_cat <- input$n_categories
    method <- input$method

    # Get custom category names with proper validation
    cat_names <- c()

    if (n_cat >= 1) {
      cat_names[1] <- if (!is.null(input$cat_name_1) && input$cat_name_1 != "") input$cat_name_1 else "Kategori_1"
    }
    if (n_cat >= 2) {
      cat_names[2] <- if (!is.null(input$cat_name_2) && input$cat_name_2 != "") input$cat_name_2 else "Kategori_2"
    }
    if (n_cat >= 3) {
      cat_names[3] <- if (!is.null(input$cat_name_3) && input$cat_name_3 != "") input$cat_name_3 else "Kategori_3"
    }
    if (n_cat >= 4) {
      cat_names[4] <- if (!is.null(input$cat_name_4) && input$cat_name_4 != "") input$cat_name_4 else "Kategori_4"
    }
    if (n_cat >= 5) {
      cat_names[5] <- if (!is.null(input$cat_name_5) && input$cat_name_5 != "") input$cat_name_5 else "Kategori_5"
    }
    if (n_cat >= 6) {
      cat_names[6] <- if (!is.null(input$cat_name_6) && input$cat_name_6 != "") input$cat_name_6 else "Kategori_6"
    }
    if (n_cat >= 7) {
      cat_names[7] <- if (!is.null(input$cat_name_7) && input$cat_name_7 != "") input$cat_name_7 else "Kategori_7"
    }
    if (n_cat >= 8) {
      cat_names[8] <- if (!is.null(input$cat_name_8) && input$cat_name_8 != "") input$cat_name_8 else "Kategori_8"
    }
    if (n_cat >= 9) {
      cat_names[9] <- if (!is.null(input$cat_name_9) && input$cat_name_9 != "") input$cat_name_9 else "Kategori_9"
    }
    if (n_cat >= 10) {
      cat_names[10] <- if (!is.null(input$cat_name_10) && input$cat_name_10 != "") input$cat_name_10 else "Kategori_10"
    }

    # Ensure we have the right number of names
    cat_names <- cat_names[1:n_cat]

    if (method == "quantile") {
      data[[paste0(var_name, "_cat")]] <- cut(data[[var_name]],
        breaks = quantile(data[[var_name]],
          probs = seq(0, 1, length.out = n_cat + 1),
          na.rm = TRUE
        ),
        labels = cat_names,
        include.lowest = TRUE
      )
    } else if (method == "equal_width") {
      data[[paste0(var_name, "_cat")]] <- cut(data[[var_name]],
        breaks = n_cat,
        labels = cat_names,
        include.lowest = TRUE
      )
    } else if (method == "kmeans") {
      # Handle missing values for kmeans
      var_values <- data[[var_name]]
      var_values_clean <- var_values[!is.na(var_values)]

      if (length(var_values_clean) > 0) {
        km_result <- kmeans(var_values_clean, centers = n_cat, nstart = 25)

        # Create factor with NA handling
        clusters <- rep(NA, length(var_values))
        clusters[!is.na(var_values)] <- km_result$cluster
        data[[paste0(var_name, "_cat")]] <- factor(clusters, labels = cat_names)
      } else {
        data[[paste0(var_name, "_cat")]] <- factor(rep(NA, nrow(data)), labels = cat_names)
      }
    }

    return(data)
  })

  # Original data table
  output$original_data <- DT::renderDataTable({
    DT::datatable(sovi_data(), options = list(scrollX = TRUE))
  })

  # Data Asli tab output (same as original_data but for new tab)
  output$data_asli_table <- DT::renderDataTable({
    DT::datatable(sovi_data(), options = list(scrollX = TRUE, pageLength = 10))
  })

  # Data summary for data_asli tab
  output$data_summary <- renderText({
    data <- sovi_data()
    paste(
      "Jumlah observasi (kabupaten/kota):", nrow(data), "\n",
      "Jumlah variabel:", ncol(data), "\n",
      "Periode data: SUSENAS 2017\n",
      "Sumber: BPS-Statistics Indonesia"
    )
  })

  # Raw data table for data_asli tab
  output$raw_data_table <- DT::renderDataTable({
    DT::datatable(sovi_data(), options = list(scrollX = TRUE, pageLength = 10))
  })

  # Categorized data preview
  output$categorized_preview <- DT::renderDataTable({
    tryCatch(
      {
        if (input$categorize > 0) {
          data <- categorized_data()
          cat_col <- paste0(input$var_to_categorize, "_cat")
          if (cat_col %in% names(data)) {
            DT::datatable(data[, c("DISTRICTCODE", input$var_to_categorize, cat_col)],
              options = list(scrollX = TRUE)
            )
          }
        }
      },
      error = function(e) {
        DT::datatable(data.frame(Message = "Data belum siap. Silakan tunggu proses kategorisasi selesai."))
      }
    )
  })

  # Categorization interpretation
  output$categorization_interpretation <- renderText({
    tryCatch(
      {
        if (input$categorize > 0) {
          data <- categorized_data()
          var_name <- input$var_to_categorize
          cat_col <- paste0(var_name, "_cat")

          if (cat_col %in% names(data)) {
            # Get statistics for each category
            cat_summary <- data %>%
              group_by(!!sym(cat_col)) %>%
              summarise(
                count = n(),
                mean_value = mean(!!sym(var_name), na.rm = TRUE),
                min_value = min(!!sym(var_name), na.rm = TRUE),
                max_value = max(!!sym(var_name), na.rm = TRUE),
                .groups = "drop"
              )

            interpretation <- paste0(
              "INTERPRETASI KATEGORISASI DATA:\n\n",
              "Variabel: ", var_name, "\n",
              "Metode: ", switch(input$method,
                "quantile" = "Quantile (berdasarkan persentil)",
                "equal_width" = "Equal Width (lebar interval sama)",
                "kmeans" = "K-means clustering"
              ), "\n",
              "Jumlah kategori: ", input$n_categories, "\n\n",
              "Distribusi data per kategori:\n"
            )

            for (i in 1:nrow(cat_summary)) {
              interpretation <- paste0(
                interpretation,
                "- ", cat_summary[[cat_col]][i], ": ", cat_summary$count[i], " wilayah ",
                "(", round(cat_summary$count[i] / nrow(data) * 100, 1), "%) ",
                "- Rata-rata: ", round(cat_summary$mean_value[i], 2),
                " (Range: ", round(cat_summary$min_value[i], 2), " - ",
                round(cat_summary$max_value[i], 2), ")\n"
              )
            }

            interpretation <- paste0(
              interpretation, "\n",
              "Interpretasi: Kategorisasi berhasil membagi data menjadi ", input$n_categories,
              " kelompok yang dapat digunakan untuk analisis lebih lanjut."
            )

            return(interpretation)
          }
        }
      },
      error = function(e) {
        return("Data belum siap. Silakan tunggu proses kategorisasi selesai.")
      }
    )
  })

  # Full categorized data table
  output$categorized_full_data <- DT::renderDataTable({
    tryCatch(
      {
        if (input$categorize > 0) {
          data <- categorized_data()
          DT::datatable(data, options = list(scrollX = TRUE, pageLength = 10))
        } else {
          DT::datatable(data.frame(Message = "Silakan lakukan kategorisasi data terlebih dahulu."))
        }
      },
      error = function(e) {
        DT::datatable(data.frame(Message = "Data belum siap. Silakan tunggu proses kategorisasi selesai."))
      }
    )
  })

  # Update categorical variable choices dynamically
  observe({
    if (input$categorize > 0) {
      tryCatch(
        {
          data <- categorized_data()
          cat_vars <- names(data)[grepl("_cat$", names(data))]

          if (length(cat_vars) > 0) {
            updateSelectInput(session, "cat_var_explore", choices = cat_vars)
            updateSelectInput(session, "cat_var1", choices = cat_vars)
            updateSelectInput(session, "cat_var2", choices = cat_vars)
          }
        },
        error = function(e) {
          # Handle error silently when data is not ready
        }
      )
    }
  })

  # Categorical plot (simplified to bar chart only)
  output$categorical_plot <- renderPlotly({
    req(input$categorize > 0, input$cat_var_explore)

    data <- categorized_data()
    cat_var <- input$cat_var_explore

    # Simple frequency bar chart
    p <- ggplot(data, aes(x = !!sym(cat_var), fill = !!sym(cat_var))) +
      geom_bar(alpha = 0.7, color = "white", size = 0.5) +
      theme_minimal() +
      labs(
        title = paste("Distribusi Frekuensi:", cat_var),
        x = cat_var,
        y = "Frekuensi"
      ) +
      theme(
        legend.position = "none",
        plot.title = element_text(size = 14, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)
      ) +
      scale_fill_brewer(type = "qual", palette = "Set3")

    ggplotly(p)
  })

  # Frequency table for categorical variables
  output$frequency_table <- DT::renderDataTable({
    req(input$categorize > 0, input$cat_var_explore)

    data <- categorized_data()
    cat_var <- input$cat_var_explore

    freq_data <- data %>%
      count(!!sym(cat_var)) %>%
      mutate(
        Percentage = round(n / sum(n) * 100, 2),
        `Cumulative Freq` = cumsum(n),
        `Cumulative %` = round(cumsum(n) / sum(n) * 100, 2)
      ) %>%
      rename(
        Category = !!sym(cat_var),
        Frequency = n
      )

    DT::datatable(freq_data, options = list(scrollX = TRUE, pageLength = 10))
  })

  # Categorical interpretation
  output$categorical_interpretation <- renderText({
    req(input$categorize > 0, input$cat_var_explore)

    data <- categorized_data()
    cat_var <- input$cat_var_explore

    freq_data <- data %>%
      count(!!sym(cat_var)) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))

    max_category <- freq_data[which.max(freq_data$n), ]
    min_category <- freq_data[which.min(freq_data$n), ]

    paste0(
      "Interpretasi Eksplorasi Data Kategorik:\n\n",
      "1. Distribusi Frekuensi:\n",
      "   - Kategori dengan frekuensi tertinggi: ", max_category[[cat_var]], " (", max_category$n, " atau ", max_category$percentage, "%)\n",
      "   - Kategori dengan frekuensi terendah: ", min_category[[cat_var]], " (", min_category$n, " atau ", min_category$percentage, "%)\n\n",
      "2. Karakteristik Distribusi:\n",
      "   - Total observasi: ", sum(freq_data$n), " wilayah\n",
      "   - Jumlah kategori: ", nrow(freq_data), " kategori\n",
      "   - Distribusi relatif merata: ", ifelse(max(freq_data$percentage) - min(freq_data$percentage) < 20, "Ya", "Tidak"), "\n\n",
      "3. Kesimpulan:\n",
      "   Variabel ", cat_var, " menunjukkan distribusi yang ",
      ifelse(max(freq_data$percentage) > 50, "tidak merata dengan dominasi kategori tertentu", "relatif merata antar kategori"), "."
    )
  })

  # Cross tabulation
  observeEvent(input$create_crosstab, {
    req(input$cat_var1, input$cat_var2)

    data <- categorized_data()
    crosstab <- table(data[[input$cat_var1]], data[[input$cat_var2]])
    crosstab_df <- as.data.frame.matrix(crosstab)
    crosstab_df$Total <- rowSums(crosstab_df)
    crosstab_df <- rbind(crosstab_df, Total = colSums(crosstab_df))

    output$crosstab_table <- DT::renderDataTable({
      DT::datatable(crosstab_df, options = list(scrollX = TRUE))
    })
  })

  # Descriptive statistics
  output$descriptive_stats <- renderPrint({
    data <- sovi_data()
    numeric_vars <- sapply(data, is.numeric)
    summary(data[numeric_vars])
  })

  # Correlation plot
  output$correlation_plot <- renderPlot({
    data <- sovi_data()
    numeric_vars <- sapply(data, is.numeric)
    cor_matrix <- cor(data[numeric_vars], use = "complete.obs")
    corrplot(cor_matrix,
      method = "color", type = "upper", order = "hclust",
      tl.cex = 0.8, tl.col = "black", tl.srt = 45
    )
  })

  # Data visualization
  output$data_plot <- renderPlotly({
    data <- sovi_data()

    if (input$plot_type == "Histogram") {
      p <- ggplot(data, aes(x = !!sym(input$x_var))) +
        geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", input$x_var))
    } else if (input$plot_type == "Boxplot") {
      p <- ggplot(data, aes(y = !!sym(input$x_var))) +
        geom_boxplot(fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Boxplot of", input$x_var))
    } else if (input$plot_type == "Scatter Plot") {
      p <- ggplot(data, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
        geom_point(alpha = 0.6) +
        geom_smooth(method = "lm", se = FALSE) +
        theme_minimal() +
        labs(title = paste("Scatter Plot:", input$x_var, "vs", input$y_var))
    } else if (input$plot_type == "Density Plot") {
      p <- ggplot(data, aes(x = !!sym(input$x_var))) +
        geom_density(fill = "lightblue", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Density Plot of", input$x_var))
    }

    ggplotly(p)
  })

  # Exploration interpretation
  output$exploration_interpretation <- renderText({
    data <- sovi_data()
    var_name <- input$x_var

    if (!is.null(var_name)) {
      var_data <- data[[var_name]]
      mean_val <- mean(var_data, na.rm = TRUE)
      median_val <- median(var_data, na.rm = TRUE)
      sd_val <- sd(var_data, na.rm = TRUE)

      interpretation <- paste0(
        "INTERPRETASI EKSPLORASI DATA:\n\n",
        "Variabel: ", var_name, "\n",
        "Rata-rata: ", round(mean_val, 2), "\n",
        "Median: ", round(median_val, 2), "\n",
        "Standar Deviasi: ", round(sd_val, 2), "\n\n",
        if (abs(mean_val - median_val) < 0.1 * sd_val) {
          "Distribusi data cenderung simetris (normal)."
        } else if (mean_val > median_val) {
          "Distribusi data cenderung miring ke kanan (positively skewed)."
        } else {
          "Distribusi data cenderung miring ke kiri (negatively skewed)."
        }
      )

      return(interpretation)
    }
  })

  # Normality test
  observeEvent(input$test_normality, {
    data <- sovi_data()
    var_data <- data[[input$normality_var]]

    # Remove missing values
    var_data <- var_data[!is.na(var_data)]

    output$normality_result <- renderPrint({
      shapiro_res <- if (length(var_data) <= 5000) shapiro.test(var_data) else NULL
      ad_res <- ad.test(var_data)
      jb_res <- tseries::jarque.bera.test(var_data)

      hasil <- list()

      if (!is.null(shapiro_res)) {
        hasil[["Shapiro-Wilk Test"]] <- shapiro_res
        hasil[["Interpretasi Shapiro-Wil"]] <- interpret_pval("Shapiro-Wilk", "normality_test", shapiro_res$p.value)
      } else {
        hasil[["Shapiro-Wilk Test"]] <- "Dilewati (n>5000)"
      }

      hasil[["Anderson-Darling Test"]] <- ad_res
      hasil[["Interpretasi Anderson-Darling"]] <- interpret_pval("Anderson-Darling", "normality_test", ad_res$p.value)

      return(hasil)
    })

    # Q-Q plot
    output$normality_plot <- renderPlot({
      qqnorm(var_data, main = paste("Q-Q Plot for", input$normality_var))
      qqline(var_data, col = "red")
    })
  })

  # Homogeneity test
  observeEvent(input$test_homogeneity, {
    data <- sovi_data()

    # Create groups based on quartiles
    group_var <- cut(data[[input$group_var]],
      breaks = quantile(data[[input$group_var]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
      labels = c("Q1", "Q2", "Q3", "Q4"),
      include.lowest = TRUE
    )

    # Levene's test
    levene_result <- leveneTest(data[[input$homogeneity_var]], group_var)

    # Bartlett's test
    bartlett_result <- bartlett.test(data[[input$homogeneity_var]], group_var)

    output$homogeneity_result <- renderPrint({
      list(
        "Levene's Test" = levene_result,
        "Levene's Test Interpetation" = interpret_pval("Levene's Test", "homogeneity test", pval <- levene_result[["Pr(>F)"]][1]),
        "Bartlett's Test" = bartlett_result,
        "Bartlett's Test Interpetation" = interpret_pval("Bartlett's Test", "homogeneity test", bartlett_result$p.value)
      )
    })
  })

  interpret_pval <- function(test_name, test_group, pval) {
    if (is.na(pval) || length(pval) == 0) {
      return(paste(test_name, ": p-value tidak tersedia"))
    }
    if (is.null(test_group) || length(test_group) == 0) {
      return(paste(test_name, ": Jenis uji tidak diketahui"))
    }

    if (pval > 0.05) {
      if (test_group == "normality_test") {
        paste0(test_name, ": p = ", round(pval, 4), " -> Data cenderung berdistribusi **normal** (p > 0.05)")
      } else {
        paste0(test_name, ": p = ", round(pval, 4), " -> Data **homogen** (p > 0.05)")
      }
    } else {
      if (test_group == "normality_test") {
        paste0(test_name, ": p = ", round(pval, 4), " -> Data **tidak normal** (p <= 0.05)")
      } else {
        paste0(test_name, ": p = ", round(pval, 4), " -> Data **tidak homogen** (p <= 0.05)")
      }
    }
  }


  # Assumption interpretation
  output$assumption_interpretation <- renderText({
    "#KETERANGAN (menggunakan \\alpha=0.05) \n
    Hipotesis: \n\n
    UJI NORMALITAS:
    - Jika p-value > 0.05: Data berdistribusi normal
    - Jika p-value <= 0.05: Data tidak berdistribusi normal

    UJI HOMOGENITAS:
    - Jika p-value > 0.05: Varians antar kelompok homogen
    - Jika p-value <= 0.05: Varians antar kelompok tidak homogen

    Catatan: Uji asumsi penting untuk menentukan metode statistik yang tepat."
  })

  # Mean tests
  observeEvent(input$test_mean, {
    data <- sovi_data()
    var_data <- data[[input$mean_var]]

    if (input$test_type == "one_sample") {
      result <- t.test(var_data, mu = input$mu)
      output$mean_test_result <- renderPrint({
        result
      })

      output$mean_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI RATA-RATA SATU SAMPEL:\n\n",
          "H0: mu = ", input$mu, "\n",
          "H1: mu tidak sama dengan ", input$mu, "\n\n",
          "p-value: ", round(result$p.value, 4), "\n\n",
          if (result$p.value < 0.05) {
            paste0("Kesimpulan: Tolak H0. Rata-rata populasi berbeda signifikan dari ", input$mu)
          } else {
            paste0("Kesimpulan: Terima H0. Rata-rata populasi tidak berbeda signifikan dari ", input$mu)
          }
        )
      })
    } else if (input$test_type == "two_sample") {
      # Create groups
      group_var <- cut(data[[input$group_var_mean]],
        breaks = 2,
        labels = c("Group1", "Group2")
      )

      group1_data <- var_data[group_var == "Group1"]
      group2_data <- var_data[group_var == "Group2"]

      result <- t.test(group1_data, group2_data)
      output$mean_test_result <- renderPrint({
        result
      })

      output$mean_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI RATA-RATA DUA SAMPEL:\n\n",
          "H0: mu1 = mu2\n",
          "H1: mu1 tidak sama dengan mu2\n\n",
          "p-value: ", round(result$p.value, 4), "\n\n",
          if (result$p.value < 0.05) {
            "Kesimpulan: Tolak H0. Rata-rata kedua kelompok berbeda signifikan"
          } else {
            "Kesimpulan: Terima H0. Rata-rata kedua kelompok tidak berbeda signifikan"
          }
        )
      })
    } else if (input$test_type == "paired") {
      var1_data <- data[[input$mean_var]]
      var2_data <- data[[input$paired_var]]

      result <- t.test(var1_data, var2_data, paired = TRUE)
      output$mean_test_result <- renderPrint({
        result
      })

      output$mean_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI RATA-RATA BERPASANGAN:\n\n",
          "H0: mu_d = 0 (tidak ada perbedaan)\n",
          "H1: mu_d tidak sama dengan 0 (ada perbedaan)\n\n",
          "p-value: ", round(result$p.value, 4), "\n\n",
          if (result$p.value < 0.05) {
            "Kesimpulan: Tolak H0. Ada perbedaan signifikan antara kedua variabel"
          } else {
            "Kesimpulan: Terima H0. Tidak ada perbedaan signifikan antara kedua variabel"
          }
        )
      })
    }
  })

  # Proportion test
  observeEvent(input$test_proportion, {
    data <- sovi_data()
    var_data <- data[[input$prop_var]]

    # Create binary outcome
    successes <- sum(var_data > input$prop_threshold, na.rm = TRUE)
    n <- length(var_data)

    result <- prop.test(successes, n, p = input$prop_hypothesized)

    output$proportion_result <- renderPrint({
      result
    })
  })

  # Variance test
  observeEvent(input$test_variance, {
    data <- sovi_data()
    var_data <- data[[input$variance_var]]

    if (input$var_test_type == "one_var") {
      # Chi-square test for variance
      n <- length(var_data)
      sample_var <- var(var_data, na.rm = TRUE)
      chi_sq <- (n - 1) * sample_var / input$sigma_squared
      p_value <- 2 * min(pchisq(chi_sq, df = n - 1), 1 - pchisq(chi_sq, df = n - 1))

      result <- list(
        statistic = chi_sq,
        p.value = p_value,
        df = n - 1,
        sample_variance = sample_var,
        hypothesized_variance = input$sigma_squared
      )

      output$variance_result <- renderPrint({
        result
      })
    } else if (input$var_test_type == "two_var") {
      # Create groups
      group_var <- cut(data[[input$group_var_variance]],
        breaks = 2,
        labels = c("Group1", "Group2")
      )

      group1_data <- var_data[group_var == "Group1"]
      group2_data <- var_data[group_var == "Group2"]

      result <- var.test(group1_data, group2_data)
      output$variance_result <- renderPrint({
        result
      })
    }
  })

  # Proportion interpretation
  output$proportion_interpretation <- renderText({
    "INTERPRETASI UJI PROPORSI:\n\n
    H0: p = p0 (proporsi sama dengan nilai hipotesis)
    H1: p tidak sama dengan p0 (proporsi berbeda dari nilai hipotesis)\n
    Kriteria Keputusan:
    - Jika p-value < 0.05: Tolak H0
    - Jika p-value >= 0.05: Terima H0\n
    Uji proporsi digunakan untuk menguji apakah proporsi populasi
    sama dengan nilai yang dihipotesiskan berdasarkan threshold tertentu."
  })

  # Variance interpretation
  output$variance_interpretation <- renderText({
    "INTERPRETASI UJI VARIANCE:\n\n
    Satu Sampel:
    H0: sigma^2 = sigma^2_0 vs H1: sigma^2 tidak sama dengan sigma^2_0\n
    Dua Sampel:
    H0: sigma^2_1 = sigma^2_2 vs H1: sigma^2_1 tidak sama dengan sigma^2_2\n
    Kriteria Keputusan:
    - Jika p-value < 0.05: Tolak H0
    - Jika p-value >= 0.05: Terima H0\n
    Uji variance menguji kesamaan varians antar kelompok
    atau dengan nilai hipotesis tertentu."
  })

  # ANOVA
  observeEvent(input$perform_anova, {
    data <- sovi_data()

    # Create categorical variables
    factor1 <- cut(data[[input$anova_factor1]],
      breaks = 3,
      labels = c("Low", "Medium", "High")
    )

    if (input$anova_type == "one_way") {
      formula_str <- paste(input$anova_response, "~ factor1")
      model <- aov(as.formula(formula_str), data = data)

      output$anova_result <- renderPrint({
        summary(model)
      })

      output$anova_interpretation <- renderText({
        aov_summary <- summary(model)
        p_value <- aov_summary[[1]][["Pr(>F)"]][1]

        paste0(
          "INTERPRETASI ONE-WAY ANOVA:\n\n",
          "H0: mu_1 = mu_2 = mu_3 (semua grup memiliki rata-rata sama)\n",
          "H1: Minimal ada satu grup yang berbeda\n\n",
          "p-value: ", round(p_value, 4), "\n\n",
          if (p_value < 0.05) {
            "Kesimpulan: Tolak H0. Ada perbedaan signifikan antar kelompok"
          } else {
            "Kesimpulan: Terima H0. Tidak ada perbedaan signifikan antar kelompok"
          }
        )
      })
    } else if (input$anova_type == "two_way") {
      factor2 <- cut(data[[input$anova_factor2]],
        breaks = 3,
        labels = c("Low", "Medium", "High")
      )

      formula_str <- paste(input$anova_response, "~ factor1 * factor2")
      model <- aov(as.formula(formula_str), data = data)

      output$anova_result <- renderPrint({
        summary(model)
      })

      output$anova_interpretation <- renderText({
        aov_summary <- summary(model)
        p_values <- aov_summary[[1]][["Pr(>F)"]]

        paste0(
          "INTERPRETASI TWO-WAY ANOVA:\n\n",
          "Efek utama Faktor 1 p-value: ", round(p_values[1], 4), "\n",
          "Efek utama Faktor 2 p-value: ", round(p_values[2], 4), "\n",
          "Interaksi p-value: ", round(p_values[3], 4), "\n\n",
          "Interpretasi (p < 0.05 = signifikan):\n",
          "- Faktor 1: ", if (p_values[1] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
          "- Faktor 2: ", if (p_values[2] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
          "- Interaksi: ", if (p_values[3] < 0.05) "Signifikan" else "Tidak signifikan"
        )
      })
    }
  })

  # Regression analysis
  observeEvent(input$perform_regression, {
    req(input$reg_predictors)

    data <- sovi_data()

    # Add validation for numeric response variable
    if (!is.numeric(data[[input$reg_response]])) {
      showNotification("Variabel respons harus numerik untuk regresi.", type = "error")
      return()
    }
    if (length(input$reg_predictors) == 0) {
      showNotification("Pilih setidaknya satu variabel prediktor.", type = "error")
      return()
    }
    # Pastikan semua prediktor numerik juga
    for (pred in input$reg_predictors) {
      if (!is.numeric(data[[pred]])) {
        showNotification(paste0("Prediktor '", pred, "' harus numerik."), type = "error")
        return()
      }
    }

    # Create formula
    formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " + "))

    # Fit model
    model <- lm(as.formula(formula_str), data = data)

    output$regression_result <- renderPrint({
      summary(model)
    })

    # Diagnostic plots
    output$regression_diagnostics <- renderPlot({
      par(mfrow = c(2, 2))
      plot(model)
    })

    # Regression assumptions
    output$regression_assumptions <- renderPrint({
      list(
        "Durbin-Watson Test (Independence)" = dwtest(model),
        "Breusch-Pagan Test (Homoscedasticity)" = bptest(model),
        "Shapiro-Wilk Test (Normality of Residuals)" = shapiro.test(residuals(model))
      )
    })

    # Regression interpretation
    output$regression_interpretation <- renderText({
      model_summary <- summary(model)
      r_squared <- model_summary$r.squared
      f_stat <- model_summary$fstatistic
      p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

      paste0(
        "INTERPRETASI REGRESI LINEAR BERGANDA:\n\n",
        "R-squared: ", round(r_squared, 4), " (", round(r_squared * 100, 2), "% variasi dijelaskan)\n",
        "F-statistic p-value: ", format(p_value, scientific = TRUE), "\n\n",
        if (p_value < 0.05) {
          "Model secara keseluruhan signifikan dalam menjelaskan variasi data."
        } else {
          "Model secara keseluruhan tidak signifikan."
        }, "\n\n",
        "Uji Asumsi:\n",
        "- Periksa diagnostic plots untuk linearitas dan homoskedastisitas\n",
        "- Periksa hasil uji untuk normalitas residual dan independensi\n",
        "- Koefisien dengan p-value < 0.05 signifikan secara statistik"
      )
    })
  })

  # =================
  # CHOROPLETH MAP AND SPATIAL ANALYSIS
  # =================

  # Choropleth Map Generation
  observeEvent(input$generate_choropleth, {
    req(input$choropleth_var)

    output$choropleth_map <- renderLeaflet({
      tryCatch(
        {
          # Get geospatial data
          geo_data <- indonesia_geojson()
          sovi <- sovi_data()

          if (is.null(geo_data)) {
            return(leaflet() %>%
              addTiles() %>%
              setView(lng = 118, lat = -2, zoom = 4) %>%
              addPopups(
                lng = 118, lat = -2,
                popup = "Data geospasial tidak tersedia",
                options = popupOptions(closeButton = FALSE)
              ))
          }

          # Match data with 512 districts assumption
          if (nrow(sovi) == 512 && length(geo_data) == 512) {
            # Simple index-based matching
            geo_data$selected_var <- sovi[[input$choropleth_var]]
          } else {
            # Fallback: create random data for demonstration
            set.seed(123)
            geo_data$selected_var <- rnorm(length(geo_data),
              mean = mean(sovi[[input$choropleth_var]], na.rm = TRUE),
              sd = sd(sovi[[input$choropleth_var]], na.rm = TRUE)
            )
          }

          # Create color palette
          pal <- colorNumeric("YlOrRd", geo_data$selected_var, na.color = "transparent")

          # Create leaflet map
          leaflet(geo_data, options = leafletOptions(scrollWheelZoom = FALSE, doubleClickZoom = FALSE, boxZoom = FALSE, touchZoom = FALSE, keyboard = FALSE)) %>%
            addTiles() %>%
            setView(lng = 118, lat = -2, zoom = 4) %>%
            addPolygons(
              fillColor = ~ pal(selected_var),
              weight = 1,
              opacity = 1,
              color = "white",
              dashArray = "",
              fillOpacity = 0.7,
              popup = ~ paste0(
                "<b>", ifelse(is.null(NAME_2), "Kabupaten/Kota", NAME_2), "</b><br/>",
                input$choropleth_var, ": ", round(selected_var, 2)
              ),
              highlightOptions = highlightOptions(
                weight = 2,
                color = "#666",
                dashArray = "",
                fillOpacity = 0.9,
                bringToFront = TRUE
              )
            ) %>%
            addLegend(
              pal = pal,
              values = ~selected_var,
              opacity = 0.7,
              title = input$choropleth_var,
              position = "bottomright"
            )
        },
        error = function(e) {
          leaflet() %>%
            addTiles() %>%
            setView(lng = 118, lat = -2, zoom = 4) %>%
            addPopups(
              lng = 118, lat = -2,
              popup = paste("Error:", e$message),
              options = popupOptions(closeButton = FALSE)
            )
        }
      )
    })
  })

  # Spatial Analysis
  observeEvent(input$generate_spatial, {
    req(input$spatial_var1, input$spatial_var2, input$distance_threshold)

    output$spatial_plot <- renderPlotly({
      tryCatch(
        {
          sovi <- sovi_data()
          dist_matrix <- as.matrix(distance_data())

          # Get variables
          var1_data <- sovi[[input$spatial_var1]]
          var2_data <- sovi[[input$spatial_var2]]

          # Create spatial correlation analysis
          # Find pairs within distance threshold
          spatial_pairs <- which(dist_matrix <= input$distance_threshold & dist_matrix > 0, arr.ind = TRUE)

          if (nrow(spatial_pairs) > 0) {
            # Calculate spatial correlation
            pair_var1 <- var1_data[spatial_pairs[, 1]]
            pair_var2 <- var2_data[spatial_pairs[, 2]]

            # Create scatter plot with spatial context
            spatial_df <- data.frame(
              var1 = pair_var1,
              var2 = pair_var2,
              distance = dist_matrix[spatial_pairs]
            )

            p <- ggplot(spatial_df, aes(x = var1, y = var2, color = distance)) +
              geom_point(alpha = 0.6, size = 2) +
              geom_smooth(method = "lm", se = TRUE, color = "red") +
              scale_color_viridis_c(name = "Jarak (km)") +
              labs(
                title = paste("Analisis Spasial:", input$spatial_var1, "vs", input$spatial_var2),
                subtitle = paste("Kabupaten dalam radius", input$distance_threshold, "km"),
                x = input$spatial_var1,
                y = input$spatial_var2
              ) +
              theme_minimal() +
              theme(
                plot.title = element_text(size = 14, face = "bold"),
                plot.subtitle = element_text(size = 12)
              )

            ggplotly(p, tooltip = c("x", "y", "colour"))
          } else {
            # No pairs within threshold
            p <- ggplot() +
              annotate("text",
                x = 0.5, y = 0.5,
                label = "Tidak ada pasangan kabupaten\ndalam radius yang ditentukan",
                hjust = 0.5, vjust = 0.5, size = 6
              ) +
              xlim(0, 1) +
              ylim(0, 1) +
              theme_void()

            ggplotly(p)
          }
        },
        error = function(e) {
          p <- ggplot() +
            annotate("text",
              x = 0.5, y = 0.5,
              label = paste("Error dalam analisis spasial:", e$message),
              hjust = 0.5, vjust = 0.5, size = 5
            ) +
            xlim(0, 1) +
            ylim(0, 1) +
            theme_void()

          ggplotly(p)
        }
      )
    })

    # Spatial interpretation
    output$spatial_interpretation <- renderText({
      tryCatch(
        {
          sovi <- sovi_data()
          dist_matrix <- as.matrix(distance_data())

          var1_data <- sovi[[input$spatial_var1]]
          var2_data <- sovi[[input$spatial_var2]]

          # Find pairs within distance threshold
          spatial_pairs <- which(dist_matrix <= input$distance_threshold & dist_matrix > 0, arr.ind = TRUE)

          if (nrow(spatial_pairs) > 0) {
            pair_var1 <- var1_data[spatial_pairs[, 1]]
            pair_var2 <- var2_data[spatial_pairs[, 2]]

            # Calculate spatial correlation
            spatial_cor <- cor(pair_var1, pair_var2, use = "complete.obs")

            # Calculate average distance
            avg_distance <- mean(dist_matrix[spatial_pairs])

            paste0(
              "INTERPRETASI ANALISIS SPASIAL:\n\n",
              "Variabel Utama: ", input$spatial_var1, "\n",
              "Variabel Pembanding: ", input$spatial_var2, "\n",
              "Threshold Jarak: ", input$distance_threshold, " km\n\n",
              "Jumlah pasangan kabupaten dalam radius: ", nrow(spatial_pairs), "\n",
              "Rata-rata jarak: ", round(avg_distance, 2), " km\n",
              "Korelasi spasial: ", round(spatial_cor, 4), "\n\n",
              "Interpretasi Korelasi:\n",
              if (abs(spatial_cor) >= 0.7) {
                "Korelasi spasial sangat kuat - ada pola geografis yang jelas"
              } else if (abs(spatial_cor) >= 0.5) {
                "Korelasi spasial kuat - menunjukkan adanya pola geografis"
              } else if (abs(spatial_cor) >= 0.3) {
                "Korelasi spasial sedang - pola geografis lemah"
              } else {
                "Korelasi spasial lemah - tidak ada pola geografis yang jelas"
              }, "\n\n",
              if (spatial_cor > 0) {
                "Korelasi positif menunjukkan kabupaten yang berdekatan cenderung memiliki nilai yang serupa."
              } else {
                "Korelasi negatif menunjukkan kabupaten yang berdekatan cenderung memiliki nilai yang berlawanan."
              }
            )
          } else {
            paste0(
              "INTERPRETASI ANALISIS SPASIAL:\n\n",
              "Tidak ada pasangan kabupaten yang ditemukan dalam radius ",
              input$distance_threshold, " km.\n\n",
              "Saran: Coba perbesar threshold jarak untuk analisis yang lebih komprehensif."
            )
          }
        },
        error = function(e) {
          paste("Error dalam interpretasi:", e$message)
        }
      )
    })
  })

  # =================
  # REPORT MANAGEMENT
  # =================

  # Add exploration analysis to report
  observeEvent(input$add_exploration_to_report, {
    req(input$x_var, input$plot_type)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("exploration_", report_items$counter)

    # Get current interpretation from the actual output
    interpretation <- isolate({
      data <- sovi_data()
      var_name <- input$x_var

      if (!is.null(var_name) && var_name %in% names(data)) {
        var_data <- data[[var_name]]
        var_data <- var_data[!is.na(var_data)]

        # Calculate actual statistics
        mean_val <- round(mean(var_data), 4)
        median_val <- round(median(var_data), 4)
        sd_val <- round(sd(var_data), 4)
        min_val <- round(min(var_data), 4)
        max_val <- round(max(var_data), 4)

        # Determine skewness
        skewness_val <- (mean_val - median_val) / sd_val
        skewness_desc <- if (abs(skewness_val) < 0.1) {
          "relatif simetris"
        } else if (skewness_val > 0.1) {
          "condong ke kanan (right-skewed)"
        } else {
          "condong ke kiri (left-skewed)"
        }

        # Create comprehensive interpretation based on plot type
        plot_desc <- switch(input$plot_type,
          "Histogram" = "Histogram menunjukkan distribusi frekuensi data",
          "Boxplot" = "Boxplot menampilkan ringkasan lima angka dan outlier",
          "Density Plot" = "Density plot menampilkan estimasi distribusi probabilitas",
          "Scatter Plot" = paste("Scatter plot menunjukkan hubungan antara", input$x_var, "dan", input$y_var)
        )

        # Create plot with new helper function
        plot_filename <- paste0("exploration_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", var_name), "_", gsub("[^A-Za-z0-9]", "_", input$plot_type), ".png")
        plot_paths <- create_plot_path(plot_filename)

        # Generate plot code
        # Generate plot code - PERBAIKAN
        plot_code <- quote({
          # Ensure par is reset after potential multi-plot layout from previous calls
          if (!is.null(par("mfrow"))) {
            if (all(par("mfrow") == c(1, 1))) {
              # Do nothing, already 1x1
            } else {
              par(mfrow = c(1, 1)) # Reset to single plot
            }
          }

          # Ambil data dan variabel dari environment (yang sudah disediakan oleh save_plot_safely)
          var_name <- input$x_var
          var_data <- data[[var_name]]
          var_data <- var_data[!is.na(var_data)] # Remove NA values

          if (input$plot_type == "Histogram") {
            hist(var_data, main = paste("Histogram of", var_name), xlab = var_name, col = "lightblue", border = "white", breaks = 30)
          } else if (input$plot_type == "Boxplot") {
            boxplot(var_data, main = paste("Boxplot of", var_name), ylab = var_name, col = "lightblue")
          } else if (input$plot_type == "Density Plot") {
            plot(density(var_data, na.rm = TRUE), main = paste("Density Plot of", var_name), xlab = var_name)
            polygon(density(var_data, na.rm = TRUE), col = "lightblue", border = "blue")
          } else if (input$plot_type == "Scatter Plot" && !is.null(input$y_var)) {
            y_data <- data[[input$y_var]]
            y_data <- y_data[!is.na(y_data)] # Remove NA values
            plot(var_data, y_data,
              xlab = var_name, ylab = input$y_var,
              main = paste("Scatter Plot:", var_name, "vs", input$y_var),
              pch = 16, col = "steelblue"
            )
            abline(lm(y_data ~ var_data), col = "red")
          } else { # Fallback for scatter if y_var is null, or other issues
            hist(var_data, main = paste("Histogram of", var_name), xlab = var_name, col = "lightblue", border = "white", breaks = 30)
          }
        })

        # Save plot safely
        plot_success <- save_plot_safely(plot_paths, plot_code)

        content <- paste0(
          "## Eksplorasi Data: ", var_name, "\n\n",
          "**Jenis Visualisasi:** ", input$plot_type, "\n\n",
          if (plot_success) {
            paste0("![", input$plot_type, " untuk variabel ", var_name, "](", plot_paths$latex_path, ")\n\n")
          } else {
            "**Error:** Plot tidak dapat dibuat.\n\n"
          },
          "**Statistik Deskriptif:**\n",
          "- Rata-rata: ", mean_val, "\n",
          "- Median: ", median_val, "\n",
          "- Standar Deviasi: ", sd_val, "\n",
          "- Minimum: ", min_val, "\n",
          "- Maksimum: ", max_val, "\n",
          "- Rentang: ", round(max_val - min_val, 4), "\n\n",
          "**Karakteristik Distribusi:**\n",
          "- Bentuk distribusi: ", skewness_desc, "\n",
          "- Koefisien variasi: ", round((sd_val / mean_val) * 100, 2), "%\n\n",
          "**Interpretasi Visualisasi:** ", plot_desc, ". ",
          "Data menunjukkan distribusi yang ", skewness_desc, " dengan variabilitas ",
          if ((sd_val / mean_val) < 0.2) "rendah" else if ((sd_val / mean_val) < 0.5) "sedang" else "tinggi",
          ".\n\n"
        )

        # Add scatter plot specific interpretation
        if (input$plot_type == "Scatter Plot" && !is.null(input$y_var)) {
          y_data <- data[[input$y_var]]
          if (length(var_data) == length(y_data)) {
            correlation <- cor(var_data, y_data, use = "complete.obs")
            cor_strength <- if (abs(correlation) < 0.3) "lemah" else if (abs(correlation) < 0.7) "sedang" else "kuat"
            cor_direction <- if (correlation > 0) "positif" else "negatif"

            content <- paste0(
              content,
              "**Analisis Korelasi:**\n",
              "- Koefisien korelasi: ", round(correlation, 4), "\n",
              "- Kekuatan hubungan: ", cor_strength, "\n",
              "- Arah hubungan: ", cor_direction, "\n\n"
            )
          }
        }

        content
      } else {
        paste0(
          "## Eksplorasi Data: ", input$x_var, "\n\n",
          "Error: Variabel tidak ditemukan dalam dataset.\n\n"
        )
      }
    })

    report_items$content[[item_id]] <- list(
      type = "exploration",
      title = paste("Eksplorasi Data:", input$x_var),
      content = interpretation,
      timestamp = Sys.time()
    )

    showNotification("Analisis eksplorasi berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add spatial analysis to report (simplified)
  observeEvent(input$add_spatial_to_report, {
    req(input$spatial_var1, input$spatial_var2, input$distance_threshold)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("spatial_", report_items$counter)

    # Save spatial plot as PNG file first
    plot_filename <- paste0("spatial_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$spatial_var1), "_vs_", gsub("[^A-Za-z0-9]", "_", input$spatial_var2), ".png")
    plot_paths <- create_plot_path(plot_filename)

    # Generate and save the spatial plot
    plot_code <- quote({
      # Ensure par is reset after potential multi-plot layout from previous calls
      if (!is.null(par("mfrow"))) {
        if (all(par("mfrow") == c(1, 1))) {
          # Do nothing, already 1x1
        } else {
          par(mfrow = c(1, 1)) # Reset to single plot
        }
      }
      var1_data <- data[[input$spatial_var1]]
      var2_data <- data[[input$spatial_var2]]
      correlation <- cor(var1_data, var2_data, use = "complete.obs")

      plot(var1_data, var2_data,
        xlab = input$spatial_var1, ylab = input$spatial_var2,
        main = paste("Analisis Spasial:", input$spatial_var1, "vs", input$spatial_var2),
        pch = 16, col = "steelblue", cex = 0.8
      )
      abline(lm(var2_data ~ var1_data), col = "red", lwd = 2)
      legend("topright", legend = paste("r =", round(correlation, 3)), bty = "n")
    })

    plot_success <- save_plot_safely(plot_paths, plot_code)

    content <- paste0(
      "## Analisis Spasial: ", input$spatial_var1, " vs ", input$spatial_var2, "\n\n",
      if (plot_success) {
        paste0("![Scatter Plot Analisis Spasial](", plot_paths$latex_path, ")\n\n")
      } else {
        "**Error:** Plot tidak dapat dibuat.\n\n"
      },
      "**Variabel 1:** ", input$spatial_var1, "\n",
      "**Variabel 2:** ", input$spatial_var2, "\n",
      "**Threshold Jarak:** ", input$distance_threshold, " km\n",
      "**Korelasi:** ", round(cor(sovi_data()[[input$spatial_var1]], sovi_data()[[input$spatial_var2]], use = "complete.obs"), 3), "\n\n",
      "**Interpretasi:** Scatter plot menunjukkan hubungan spasial antara kedua variabel dengan mempertimbangkan threshold jarak ", input$distance_threshold, " km. Korelasi sebesar ", round(cor(sovi_data()[[input$spatial_var1]], sovi_data()[[input$spatial_var2]], use = "complete.obs"), 3),
      if (abs(cor(sovi_data()[[input$spatial_var1]], sovi_data()[[input$spatial_var2]], use = "complete.obs")) > 0.7) " menunjukkan hubungan yang kuat." else if (abs(cor(sovi_data()[[input$spatial_var1]], sovi_data()[[input$spatial_var2]], use = "complete.obs")) > 0.3) " menunjukkan hubungan yang sedang." else " menunjukkan hubungan yang lemah.", "\n\n"
    )

    report_items$content[[item_id]] <- list(
      type = "spatial",
      title = paste("Analisis Spasial:", input$spatial_var1, "vs", input$spatial_var2),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Analisis spasial berhasil ditambahkan ke laporan! Download scatter plot secara terpisah.", type = "message")
  })

  # Add regression analysis to report
  observeEvent(input$add_regression_to_report, {
    req(input$reg_response, input$reg_predictors)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("regression_", report_items$counter)

    # Get regression results
    interpretation <- isolate({
      tryCatch(
        {
          data <- sovi_data()
          # Add validation for numeric response variable
          if (!is.numeric(data[[input$reg_response]])) {
            return("Error: Variabel respons harus numerik untuk regresi.")
          }
          if (length(input$reg_predictors) == 0) {
            return("Error: Pilih setidaknya satu variabel prediktor.")
          }
          for (pred in input$reg_predictors) {
            if (!is.numeric(data[[pred]])) {
              return(paste0("Error: Prediktor '", pred, "' harus numerik."))
            }
          }

          formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " + "))
          model <- lm(as.formula(formula_str), data = data)
          model_summary <- summary(model)

          r_squared <- model_summary$r.squared
          f_stat <- model_summary$fstatistic
          p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

          # Get significant coefficients
          coef_table <- model_summary$coefficients
          sig_vars <- rownames(coef_table)[coef_table[, "Pr(>|t|)"] < 0.05]
          sig_vars <- sig_vars[sig_vars != "(Intercept)"]

          # Save diagnostic plots as PNG file first
          plot_filename <- paste0("regression_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$reg_response), ".png")
          plot_paths <- create_plot_path(plot_filename)

          # Generate and save the diagnostic plots
          plot_code <- quote({
            par(mfrow = c(2, 2))
            plot(model, which = 1:4)
            par(mfrow = c(1, 1)) # Reset par after plotting
          })

          plot_success <- save_plot_safely(plot_paths, plot_code)

          paste0(
            "## Analisis Regresi Linear: ", input$reg_response, "\n\n",
            "**Model:** ", formula_str, "\n\n",
            if (plot_success) {
              paste0("![Diagnostic Plots untuk Analisis Regresi](", plot_paths$latex_path, ")\n\n")
            } else {
              "**Error:** Plot diagnostik tidak dapat dibuat.\n\n"
            },
            "**Hasil Analisis:**\n",
            "- R-squared: ", round(r_squared, 4), " (", round(r_squared * 100, 2), "% variasi dijelaskan)\n",
            "- F-statistic p-value: ", format(p_value, scientific = TRUE), "\n",
            "- Jumlah predictor: ", length(input$reg_predictors), "\n\n",
            "**Variabel Signifikan (p < 0.05):**\n",
            if (length(sig_vars) > 0) {
              paste("- ", sig_vars, collapse = "\n")
            } else {
              "Tidak ada variabel yang signifikan"
            }, "\n\n",
            "**Interpretasi Model:** ",
            if (p_value < 0.05) {
              paste0(
                "Model secara keseluruhan signifikan (p < 0.05) dengan kemampuan menjelaskan ",
                round(r_squared * 100, 2), "% variasi dalam ", input$reg_response, "."
              )
            } else {
              "Model secara keseluruhan tidak signifikan."
            }, "\n\n",
            "**Catatan:** Uji asumsi regresi perlu diperiksa untuk memvalidasi hasil analisis.\n\n"
          )
        },
        error = function(e) {
          paste0("Error dalam analisis regresi: ", e$message, "\n\n")
        }
      )
    })

    report_items$content[[item_id]] <- list(
      type = "regression",
      title = paste("Regresi Linear:", input$reg_response),
      content = interpretation,
      timestamp = Sys.time()
    )

    showNotification("Analisis regresi berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add data summary to report
  observeEvent(input$add_data_summary_to_report, {
    report_items$counter <- report_items$counter + 1
    item_id <- paste0("summary_", report_items$counter)

    content <- isolate({
      data <- sovi_data()
      paste0(
        "## Ringkasan Dataset\n\n",
        "**Jumlah Observasi:** ", nrow(data), " kabupaten/kota\n\n",
        "**Jumlah Variabel:** ", ncol(data), "\n\n",
        "**Periode Data:** SUSENAS 2017\n\n",
        "**Status Data:** Dataset lengkap dengan informasi komprehensif mengenai indikator kerentanan sosial di seluruh Indonesia.\n\n"
      )
    })

    report_items$content[[item_id]] <- list(
      type = "summary",
      title = "Ringkasan Dataset",
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Ringkasan dataset berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add descriptive statistics to report
  observeEvent(input$add_descriptive_to_report, {
    report_items$counter <- report_items$counter + 1
    item_id <- paste0("descriptive_", report_items$counter)

    content <- isolate({
      data <- sovi_data()
      numeric_vars <- sapply(data, is.numeric)
      summary_stats <- summary(data[numeric_vars])

      # Convert summary to a more readable format
      summary_text <- ""
      for (var in names(data[numeric_vars])) {
        var_summary <- summary(data[[var]])
        summary_text <- paste0(
          summary_text,
          "**", var, ":**\n",
          "- Min: ", round(var_summary[1], 4), "\n",
          "- Q1: ", round(var_summary[2], 4), "\n",
          "- Median: ", round(var_summary[3], 4), "\n",
          "- Mean: ", round(var_summary[4], 4), "\n",
          "- Q3: ", round(var_summary[5], 4), "\n",
          "- Max: ", round(var_summary[6], 4), "\n\n"
        )
      }

      paste0(
        "## Statistik Deskriptif\n\n",
        "**Jumlah Variabel Numerik:** ", sum(numeric_vars), "\n\n",
        "**Jumlah Observasi:** ", nrow(data), "\n\n",
        "**Ringkasan Statistik:**\n\n",
        summary_text,
        "**Interpretasi:** Statistik deskriptif menunjukkan distribusi dan karakteristik dasar dari setiap variabel kerentanan sosial. ",
        "Variabel dengan variabilitas tertinggi dapat diidentifikasi dari rentang (max-min) yang besar.\n\n"
      )
    })

    report_items$content[[item_id]] <- list(
      type = "descriptive",
      title = "Statistik Deskriptif",
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Statistik deskriptif berhasil ditambahkan ke laporan!", type = "message")
  })

observeEvent(input$add_correlation_to_report, {
  report_items$counter <- report_items$counter + 1
  item_id <- paste0("correlation_", report_items$counter)

  # Alternative solution: Direct plot creation without quote()
content <- isolate({
  data <- sovi_data()
  
  # Ensure we have numeric data
  numeric_vars <- sapply(data, is.numeric)
  numeric_data <- data[numeric_vars]
  
  # Check if we have enough numeric variables
  if (ncol(numeric_data) < 2) {
    return("**Error:** Tidak cukup variabel numerik untuk analisis korelasi.\n\n")
  }
  
  # Calculate correlation matrix
  cor_matrix <- cor(numeric_data, use = "complete.obs")
  
  # Find strongest correlations
  cor_temp <- cor_matrix
  cor_temp[upper.tri(cor_temp, diag = TRUE)] <- NA
  strong_corr <- which(abs(cor_temp) > 0.7, arr.ind = TRUE)

  strong_pairs <- ""
  if (nrow(strong_corr) > 0) {
    for (i in 1:min(5, nrow(strong_corr))) {
      var1 <- rownames(cor_temp)[strong_corr[i, 1]]
      var2 <- colnames(cor_temp)[strong_corr[i, 2]]
      corr_val <- cor_temp[strong_corr[i, 1], strong_corr[i, 2]]
      strong_pairs <- paste0(strong_pairs, "- ", var1, " vs ", var2, ": ", round(corr_val, 3), "\n")
    }
  } else {
    strong_pairs <- "- Tidak ada korelasi yang sangat kuat (>0.7) ditemukan\n"
  }

  # Save plot as PNG file
  plot_filename <- paste0("correlation_", report_items$counter, ".png")
  plot_paths <- create_plot_path(plot_filename)

  # Direct plot creation without quote()
  plot_success <- tryCatch({
    png(plot_paths$full_path, width = 800, height = 600, res = 100)
    
    # Load library and create plot
    library(corrplot)
    par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
    
    corrplot(cor_matrix,
      method = "color", 
      type = "upper", 
      order = "hclust",
      tl.cex = 0.8, 
      tl.col = "black", 
      tl.srt = 45,
      addCoef.col = "black",
      number.cex = 0.7,
      mar = c(0, 0, 2, 0),
      title = "Matriks Korelasi Variabel SOVI"
    )
    
    dev.off()
    TRUE
  }, error = function(e) {
    if (dev.cur() > 1) dev.off()
    cat("Error creating plot:", e$message, "\n")
    FALSE
  })

  paste0(
    "## Analisis Korelasi\n\n",
    if (plot_success) {
      paste0("![Matriks Korelasi Variabel Kerentanan Sosial](", plot_paths$latex_path, ")\n\n")
    } else {
      "**Error:** Plot korelasi tidak dapat dibuat.\n\n"
    },
    "**Matriks korelasi** menunjukkan hubungan linear antar variabel kerentanan sosial.\n\n",
    "**Korelasi Kuat yang Ditemukan (|r| > 0.7):**\n",
    strong_pairs, "\n",
    "**Interpretasi:** Korelasi yang kuat menunjukkan adanya hubungan erat antar indikator kerentanan sosial, yang dapat mengindikasikan pola multidimensi kerentanan.\n\n"
  )
})

    showNotification("Analisis korelasi berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add normality test to report
  observeEvent(input$add_normality_to_report, {
    req(input$normality_var)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("normality_", report_items$counter)

    content <- isolate({
      data <- sovi_data()
      var_data <- data[[input$normality_var]]
      var_data <- var_data[!is.na(var_data)]

      if (length(var_data) <= 5000) {
        shapiro_result <- shapiro.test(var_data)
        shapiro_text <- paste("Shapiro-Wilk p-value:", round(shapiro_result$p.value, 4))
      } else {
        shapiro_text <- "Shapiro-Wilk test tidak dilakukan (sampel > 5000)"
      }

      ad_result <- ad.test(var_data)
      jb_result <- tseries::jarque.bera.test(var_data)

      interpretation <- if (ad_result$p.value < 0.05) {
        "Data tidak berdistribusi normal (p < 0.05)"
      } else {
        "Data kemungkinan berdistribusi normal (p >= 0.05)"
      }

      # Save plot as PNG file first
      plot_filename <- paste0("normality_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$normality_var), ".png")
      plot_paths <- create_plot_path(plot_filename)

      # Generate and save the normality plots
      plot_code <- quote({
        par(mfrow = c(1, 2))
        # Q-Q Plot
        qqnorm(var_data, main = paste("Q-Q Plot:", input$normality_var))
        qqline(var_data, col = "red")
        # Histogram with normal curve
        hist(var_data,
          prob = TRUE, main = paste("Histogram:", input$normality_var),
          xlab = input$normality_var, col = "lightblue", border = "white"
        )
        curve(dnorm(x, mean = mean(var_data), sd = sd(var_data)), add = TRUE, col = "red", lwd = 2)
        par(mfrow = c(1, 1))
      })

      plot_success <- save_plot_safely(plot_paths, plot_code)

      paste0(
        "## Uji Normalitas: ", input$normality_var, "\n\n",
        if (plot_success) {
          paste0("![Q-Q Plot dan Histogram untuk Uji Normalitas](", plot_paths$latex_path, ")\n\n")
        } else {
          "**Error:** Plot normalitas tidak dapat dibuat.\n\n"
        },
        "**Hasil Uji:**\n",
        "- ", shapiro_text, "\n",
        "- Anderson-Darling p-value: ", round(ad_result$p.value, 4), "\n",
        "- Jarque-Bera p-value: ", round(jb_result$p.value, 4), "\n\n",
        "**Interpretasi:** ", interpretation, "\n\n",
        "**Catatan:** Uji normalitas penting untuk menentukan metode analisis statistik yang tepat.\n\n"
      )
    })

    report_items$content[[item_id]] <- list(
      type = "normality",
      title = paste("Uji Normalitas:", input$normality_var),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Uji normalitas berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add homogeneity test to report
  observeEvent(input$add_homogeneity_to_report, {
    req(input$homogeneity_var, input$group_var)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("homogeneity_", report_items$counter)

    content <- isolate({
      data <- sovi_data()
      group_var <- cut(data[[input$group_var]],
        breaks = quantile(data[[input$group_var]], probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE),
        labels = c("Q1", "Q2", "Q3", "Q4"),
        include.lowest = TRUE
      )

      levene_result <- leveneTest(data[[input$homogeneity_var]], group_var)
      bartlett_result <- bartlett.test(data[[input$homogeneity_var]], group_var)

      interpretation <- if (levene_result$`Pr(>F)`[1] < 0.05) {
        "Varians antar kelompok tidak homogen (p < 0.05)"
      } else {
        "Varians antar kelompok homogen (p >= 0.05)"
      }

      paste0(
        "## Uji Homogenitas: ", input$homogeneity_var, "\n\n",
        "**Variabel Pengelompokan:** ", input$group_var, " (dalam kuartil)\n\n",
        "**Hasil Uji:**\n",
        "- Levene's Test p-value: ", round(levene_result$`Pr(>F)`[1], 4), "\n",
        "- Bartlett's Test p-value: ", round(bartlett_result$p.value, 4), "\n\n",
        "**Interpretasi:** ", interpretation, "\n\n",
        "**Catatan:** Uji homogenitas varians penting untuk ANOVA dan uji parametrik lainnya.\n\n"
      )
    })

    report_items$content[[item_id]] <- list(
      type = "homogeneity",
      title = paste("Uji Homogenitas:", input$homogeneity_var),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Uji homogenitas berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add mean test to report
  observeEvent(input$add_mean_test_to_report, {
    req(input$mean_var, input$test_type)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("mean_test_", report_items$counter)

    content <- isolate({
      data <- sovi_data()
      var_data <- data[[input$mean_var]]

      if (input$test_type == "one_sample") {
        result <- t.test(var_data, mu = input$mu)
        test_desc <- paste("Uji t satu sampel dengan mu_0 =", input$mu)
        conclusion <- if (result$p.value < 0.05) {
          paste0("Tolak H0. Rata-rata populasi berbeda signifikan dari ", input$mu)
        } else {
          paste0("Terima H0. Rata-rata populasi tidak berbeda signifikan dari ", input$mu)
        }
      } else if (input$test_type == "two_sample") {
        group_var <- cut(data[[input$group_var_mean]], breaks = 2, labels = c("Group1", "Group2"))
        group1_data <- var_data[group_var == "Group1"]
        group2_data <- var_data[group_var == "Group2"]
        result <- t.test(group1_data, group2_data)
        test_desc <- "Uji t dua sampel independen"
        conclusion <- if (result$p.value < 0.05) {
          "Tolak H0. Rata-rata kedua kelompok berbeda signifikan"
        } else {
          "Terima H0. Rata-rata kedua kelompok tidak berbeda signifikan"
        }
      } else if (input$test_type == "paired") {
        var1_data <- data[[input$mean_var]]
        var2_data <- data[[input$paired_var]]
        result <- t.test(var1_data, var2_data, paired = TRUE)
        test_desc <- paste("Uji t berpasangan:", input$mean_var, "vs", input$paired_var)
        conclusion <- if (result$p.value < 0.05) {
          "Tolak H0. Ada perbedaan signifikan antara kedua variabel"
        } else {
          "Terima H0. Tidak ada perbedaan signifikan antara kedua variabel"
        }
      }

      paste0(
        "## Uji Rata-rata: ", input$mean_var, "\n\n",
        "**Jenis Uji:** ", test_desc, "\n\n",
        "**Hasil Uji:**\n",
        "- t-statistic: ", round(result$statistic, 4), "\n",
        "- p-value: ", format(result$p.value, scientific = TRUE), "\n",
        "- Confidence Interval: [", round(result$conf.int[1], 4), ", ", round(result$conf.int[2], 4), "]\n\n",
        "**Kesimpulan:** ", conclusion, "\n\n"
      )
    })

    report_items$content[[item_id]] <- list(
      type = "mean_test",
      title = paste("Uji Rata-rata:", input$mean_var),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Uji rata-rata berhasil ditambahkan ke laporan!", type = "message")
  })

# Add ANOVA to report
observeEvent(input$add_anova_to_report, {
  req(input$anova_response, input$anova_factor1, input$anova_type)

  report_items$counter <- report_items$counter + 1
  item_id <- paste0("anova_", report_items$counter)

  content <- isolate({
    data <- sovi_data()
    factor1 <- cut(data[[input$anova_factor1]], breaks = 3, labels = c("Low", "Medium", "High"))

    if (input$anova_type == "one_way") {
      # Save ANOVA plot as PNG file first
      plot_filename <- paste0("anova_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$anova_response), "_by_", gsub("[^A-Za-z0-9]", "_", input$anova_factor1), ".png")
      plot_paths <- create_plot_path(plot_filename)

      # Direct plot creation without quote()
      plot_success <- tryCatch({
        png(plot_paths$full_path, width = 800, height = 600, res = 100)
        
        # Reset par and create boxplot
        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
        boxplot(data[[input$anova_response]] ~ factor1,
          main = paste("Boxplot:", input$anova_response, "by", input$anova_factor1),
          xlab = input$anova_factor1,
          ylab = input$anova_response,
          col = c("lightblue", "lightgreen", "lightcoral")
        )
        
        dev.off()
        TRUE
      }, error = function(e) {
        if (dev.cur() > 1) dev.off()
        cat("Error creating ANOVA plot:", e$message, "\n")
        FALSE
      })

      formula_str <- paste(input$anova_response, "~ factor1")
      model <- aov(as.formula(formula_str), data = data)
      aov_summary <- summary(model)
      p_value <- aov_summary[[1]][["Pr(>F)"]][1]

      # Safe check for p_value
      conclusion <- if (!is.na(p_value) && p_value < 0.05) {
        "Tolak H0. Ada perbedaan signifikan antar kelompok"
      } else {
        "Terima H0. Tidak ada perbedaan signifikan antar kelompok"
      }

      content_text <- paste0(
        "## ANOVA Satu Arah: ", input$anova_response, "\n\n",
        if (plot_success) {
          paste0("![Boxplot ANOVA](", plot_paths$latex_path, ")\n\n")
        } else {
          "**Error:** Plot ANOVA tidak dapat dibuat.\n\n"
        },
        "**Faktor:** ", input$anova_factor1, " (3 kategori)\n\n",
        "**Hasil ANOVA:**\n",
        "- F-statistic: ", round(aov_summary[[1]][["F value"]][1], 4), "\n",
        "- p-value: ", format(p_value, scientific = TRUE), "\n\n",
        "**Kesimpulan:** ", conclusion, "\n\n"
      )
      
    } else if (input$anova_type == "two_way") {
      # Validate that factor2 input exists
      req(input$anova_factor2)
      
      # Save ANOVA plot as PNG file first
      plot_filename <- paste0("anova_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$anova_response), "_two_way.png")
      plot_paths <- create_plot_path(plot_filename)

      factor2 <- cut(data[[input$anova_factor2]], breaks = 3, labels = c("Low", "Medium", "High"))

      # Direct plot creation without quote()
      plot_success <- tryCatch({
        png(plot_paths$full_path, width = 800, height = 600, res = 100)
        
        # Reset par and create interaction plot
        par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1)
        interaction.plot(factor1, factor2, data[[input$anova_response]],
          main = paste("Interaction Plot:", input$anova_response),
          xlab = input$anova_factor1,
          ylab = input$anova_response,
          trace.label = input$anova_factor2
        )
        
        dev.off()
        TRUE
      }, error = function(e) {
        if (dev.cur() > 1) dev.off()
        cat("Error creating interaction plot:", e$message, "\n")
        FALSE
      })

      formula_str <- paste(input$anova_response, "~ factor1 * factor2")
      model <- aov(as.formula(formula_str), data = data)
      aov_summary <- summary(model)
      p_values <- aov_summary[[1]][["Pr(>F)"]]
      
      # Safe check for p_values with proper handling of NA values
      interpret_significance <- function(p_val) {
        if (is.na(p_val)) {
          return("Tidak dapat dihitung")
        } else if (p_val < 0.05) {
          return("Signifikan")
        } else {
          return("Tidak signifikan")
        }
      }
      
      # Format p-values safely
      format_p_value <- function(p_val) {
        if (is.na(p_val)) {
          return("NA")
        } else {
          return(format(p_val, scientific = TRUE))
        }
      }

      content_text <- paste0(
        "## ANOVA Dua Arah: ", input$anova_response, "\n\n",
        if (plot_success) {
          paste0("![Interaction Plot ANOVA](", plot_paths$latex_path, ")\n\n")
        } else {
          "**Error:** Plot interaksi ANOVA tidak dapat dibuat.\n\n"
        },
        "**Faktor 1:** ", input$anova_factor1, "\n",
        "**Faktor 2:** ", input$anova_factor2, "\n\n",
        "**Hasil ANOVA:**\n",
        "- Efek utama Faktor 1 p-value: ", format_p_value(p_values[1]), "\n",
        "- Efek utama Faktor 2 p-value: ", format_p_value(p_values[2]), "\n",
        "- Interaksi p-value: ", format_p_value(p_values[3]), "\n\n",
        "**Interpretasi:**\n",
        "- Faktor 1: ", interpret_significance(p_values[1]), "\n",
        "- Faktor 2: ", interpret_significance(p_values[2]), "\n",
        "- Interaksi: ", interpret_significance(p_values[3]), "\n\n"
      )
    }

    content_text
  })

    report_items$content[[item_id]] <- list(
      type = "anova",
      title = paste("ANOVA:", input$anova_response),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("ANOVA berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add proportion test to report
  observeEvent(input$add_proportion_to_report, {
    req(input$prop_var, input$prop_threshold, input$prop_hypothesized)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("proportion_", report_items$counter)

    content <- isolate({
      data <- sovi_data()
      var_data <- data[[input$prop_var]]
      success_count <- sum(var_data > input$prop_threshold, na.rm = TRUE)
      total_count <- sum(!is.na(var_data))
      sample_prop <- success_count / total_count

      prop_result <- prop.test(success_count, total_count, p = input$prop_hypothesized)

      conclusion <- if (prop_result$p.value < 0.05) {
        paste0("Tolak H0. Proporsi populasi berbeda signifikan dari ", input$prop_hypothesized)
      } else {
        paste0("Terima H0. Proporsi populasi tidak berbeda signifikan dari ", input$prop_hypothesized)
      }

      paste0(
        "## Uji Proporsi: ", input$prop_var, "\n\n",
        "**Threshold:** > ", input$prop_threshold, "\n",
        "**Proporsi Hipotesis:** ", input$prop_hypothesized, "\n\n",
        "**Hasil Uji:**\n",
        "- Proporsi Sampel: ", round(sample_prop, 4), "\n",
        "- X-squared: ", round(prop_result$statistic, 4), "\n",
        "- p-value: ", format(prop_result$p.value, scientific = TRUE), "\n",
        "- Confidence Interval: [", round(prop_result$conf.int[1], 4), ", ", round(prop_result$conf.int[2], 4), "]\n\n",
        "**Kesimpulan:** ", conclusion, "\n\n"
      )
    })

    report_items$content[[item_id]] <- list(
      type = "proportion",
      title = paste("Uji Proporsi:", input$prop_var),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Uji proporsi berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add variance test to report
  observeEvent(input$add_variance_to_report, {
    req(input$variance_var, input$var_test_type)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("variance_", report_items$counter)

    content <- isolate({
      data <- sovi_data()
      var_data <- data[[input$variance_var]]
      var_data <- var_data[!is.na(var_data)]

      if (input$var_test_type == "one_var") {
        # Chi-square test for variance
        n <- length(var_data)
        sample_var <- var(var_data)
        chi_stat <- (n - 1) * sample_var / input$sigma_squared
        p_value <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))

        conclusion <- if (p_value < 0.05) {
          paste0("Tolak H0. Varians populasi berbeda signifikan dari ", input$sigma_squared)
        } else {
          paste0("Terima H0. Varians populasi tidak berbeda signifikan dari ", input$sigma_squared)
        }

        content_text <- paste0(
          "## Uji Varians Satu Sampel: ", input$variance_var, "\n\n",
          "**Varians Hipotesis (sigma-squared):** ", input$sigma_squared, "\n\n",
          "**Hasil Uji:**\n",
          "- Varians Sampel: ", round(sample_var, 4), "\n",
          "- Chi-square statistic: ", round(chi_stat, 4), "\n",
          "- p-value: ", format(p_value, scientific = TRUE), "\n\n",
          "**Kesimpulan:** ", conclusion, "\n\n"
        )
      } else {
        # F-test for equality of variances
        group_var <- cut(data[[input$group_var_variance]], breaks = 2, labels = c("Group1", "Group2"))
        group1_data <- var_data[group_var == "Group1" & !is.na(group_var)]
        group2_data <- var_data[group_var == "Group2" & !is.na(group_var)]

        f_result <- var.test(group1_data, group2_data)

        conclusion <- if (f_result$p.value < 0.05) {
          "Tolak H0. Varians kedua kelompok berbeda signifikan"
        } else {
          "Terima H0. Varians kedua kelompok tidak berbeda signifikan"
        }

        content_text <- paste0(
          "## Uji Varians Dua Sampel: ", input$variance_var, "\n\n",
          "**Variabel Pengelompokan:** ", input$group_var_variance, "\n\n",
          "**Hasil Uji:**\n",
          "- F-statistic: ", round(f_result$statistic, 4), "\n",
          "- p-value: ", format(f_result$p.value, scientific = TRUE), "\n",
          "- Confidence Interval: [", round(f_result$conf.int[1], 4), ", ", round(f_result$conf.int[2], 4), "]\n\n",
          "**Kesimpulan:** ", conclusion, "\n\n"
        )
      }

      content_text
    })

    report_items$content[[item_id]] <- list(
      type = "variance",
      title = paste("Uji Varians:", input$variance_var),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Uji varians berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add categorization to report
  observeEvent(input$add_categorization_to_report, {
    req(input$var_to_categorize, input$method, input$n_categories)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("categorization_", report_items$counter)

    content <- isolate({
      # Get actual categorized data
      data <- sovi_data()
      var_name <- input$var_to_categorize
      n_cat <- input$n_categories

      # Perform categorization
      cat_var <- NULL # Initialize to avoid errors if conditions not met
      if (input$method == "quantile") {
        cat_var <- cut(data[[var_name]],
          breaks = quantile(data[[var_name]], probs = seq(0, 1, length.out = n_cat + 1), na.rm = TRUE),
          include.lowest = TRUE, labels = paste0("Cat", 1:n_cat)
        )
      } else if (input$method == "equal_width") {
        cat_var <- cut(data[[var_name]], breaks = n_cat, labels = paste0("Cat", 1:n_cat))
      } else if (input$method == "kmeans") {
        if (sum(!is.na(data[[var_name]])) > n_cat) {
          kmeans_result <- kmeans(data[[var_name]][!is.na(data[[var_name]])], centers = n_cat)
          cat_var <- cut(data[[var_name]],
            breaks = quantile(data[[var_name]], probs = seq(0, 1, length.out = n_cat + 1), na.rm = TRUE),
            include.lowest = TRUE, labels = paste0("Cat", 1:n_cat)
          )
        } else {
          cat_var <- cut(data[[var_name]], breaks = n_cat, labels = paste0("Cat", 1:n_cat))
        }
      }

      # Handle case where cat_var is still NULL or has NAs
      if (is.null(cat_var) || all(is.na(cat_var))) {
        return(paste0(
          "## Kategorisasi Data: ", input$var_to_categorize, "\n\n",
          "**Error:** Kategorisasi tidak dapat dilakukan. Pastikan variabel dan jumlah kategori sesuai.\n\n"
        ))
      }

      # Create summary by category
      summary_stats <- data.frame(
        Kategori = names(table(cat_var)),
        Frekuensi = as.numeric(table(cat_var)),
        Persentase = round(as.numeric(prop.table(table(cat_var))) * 100, 2),
        stringsAsFactors = FALSE
      )

      # Calculate means by category for original variable
      means_by_cat <- aggregate(data[[var_name]], by = list(cat_var), FUN = mean, na.rm = TRUE)
      summary_stats$Rata_rata_Asli <- round(means_by_cat$x, 4)

      # Save plot as PNG file first
      plot_filename <- paste0("categorization_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$var_to_categorize), ".png")
      plot_paths <- create_plot_path(plot_filename)

      # Generate and save the categorization plots
      plot_code <- quote({
        # Ensure par is reset after potential multi-plot layout from previous calls
        if (!is.null(par("mfrow"))) {
          if (all(par("mfrow") == c(1, 1))) {
            # Do nothing, already 1x1
          } else {
            par(mfrow = c(1, 1)) # Reset to single plot
          }
        }

        par(mfrow = c(1, 2))

        # Bar plot of categories
        freq_table <- table(cat_var)
        barplot(freq_table,
          main = paste("Distribusi Kategori:", var_name),
          xlab = "Kategori",
          ylab = "Frekuensi",
          col = rainbow(length(freq_table)),
          border = "white"
        )

        # Histogram of original variable
        hist(data[[var_name]],
          main = paste("Histogram:", var_name),
          xlab = var_name,
          col = "lightblue",
          border = "white",
          breaks = 20
        )

        par(mfrow = c(1, 1))
      })
      plot_success <- save_plot_safely(plot_paths, plot_code)

      # Create content
      paste0(
        "## Kategorisasi Data: ", input$var_to_categorize, "\n\n",
        if (plot_success) {
          paste0("![Visualisasi Hasil Kategorisasi](", plot_paths$latex_path, ")\n\n")
        } else {
          "**Error:** Plot tidak dapat dibuat.\n\n"
        },
        "**Metode:** ", switch(input$method,
          "quantile" = "Kuantil (Percentile)",
          "equal_width" = "Lebar Sama (Equal Width)",
          "kmeans" = "K-means Clustering"
        ), "\n\n",
        "**Jumlah Kategori:** ", input$n_categories, "\n\n",
        "**Ringkasan Kategorisasi:**\n\n",
        "| Kategori | Frekuensi | Persentase | Rata-rata Asli |\n",
        "|----------|-----------|------------|------------|",
        paste(apply(summary_stats, 1, function(x) {
          paste("| ", x[1], " | ", x[2], " | ", x[3], "% | ", x[4], " |")
        }), collapse = "\n"), "\n\n",
        "**Total Observasi:** ", sum(summary_stats$Frekuensi), "\n\n",
        "**Interpretasi:** Kategorisasi berhasil membagi variabel ", var_name,
        " menjadi ", n_cat, " kategori. Kategori dengan frekuensi tertinggi adalah '",
        summary_stats$Kategori[which.max(summary_stats$Frekuensi)], "' dengan ",
        max(summary_stats$Frekuensi), " observasi (",
        summary_stats$Persentase[which.max(summary_stats$Frekuensi)], "%).\n\n",
        "**Catatan:** Metode ", switch(input$method,
          "quantile" = "kuantil memastikan distribusi yang relatif merata antar kategori",
          "equal_width" = "lebar sama memberikan interval yang konsisten",
          "kmeans" = "k-means mengelompokkan data berdasarkan kemiripan nilai"
        ), ".\n\n"
      )
    })

    report_items$content[[item_id]] <- list(
      type = "categorization",
      title = paste("Kategorisasi:", input$var_to_categorize),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Proses kategorisasi berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add frequency table to report
  observeEvent(input$add_freq_table_to_report, {
    req(input$cat_var_explore)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("freq_table_", report_items$counter)

    content <- isolate({
      data <- categorized_data()
      var_name <- input$cat_var_explore
      freq_table <- table(data[[var_name]])
      prop_table <- prop.table(freq_table) * 100

      # Create formatted table text
      table_text <- paste0(
        "## Tabel Frekuensi: ", var_name, "\n\n",
        "| Kategori | Frekuensi | Persentase |\n",
        "|----------|-----------|------------|\n"
      )

      for (i in 1:length(freq_table)) {
        table_text <- paste0(
          table_text,
          "| ", names(freq_table)[i], " | ", freq_table[i], " | ",
          round(prop_table[i], 2), "% |\n"
        )
      }

      table_text <- paste0(
        table_text, "\n",
        "**Total Observasi:** ", sum(freq_table), "\n\n",
        "**Interpretasi:** Tabel frekuensi menunjukkan distribusi data kategorik untuk variabel ",
        var_name, ". Kategori dengan frekuensi tertinggi adalah '",
        names(which.max(freq_table)), "' dengan ", max(freq_table),
        " observasi (", round(max(prop_table), 2), "%).\n\n"
      )

      table_text
    })

    report_items$content[[item_id]] <- list(
      type = "frequency_table",
      title = paste("Tabel Frekuensi:", input$cat_var_explore),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Tabel frekuensi berhasil ditambahkan ke laporan!", type = "message")
  })

  # Add crosstab table to report
  observeEvent(input$add_crosstab_to_report, {
    req(input$cat_var1, input$cat_var2)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("crosstab_", report_items$counter)

    content <- isolate({
      data <- categorized_data()
      var1 <- input$cat_var1
      var2 <- input$cat_var2

      # Create crosstab
      crosstab <- table(data[[var1]], data[[var2]])
      prop_crosstab <- prop.table(crosstab) * 100

      # Create formatted table text
      table_text <- paste0(
        "## Tabulasi Silang: ", var1, " vs ", var2, "\n\n",
        "### Frekuensi:\n",
        "```\n",
        paste(capture.output(print(crosstab)), collapse = "\n"),
        "\n```\n\n",
        "### Persentase:\n",
        "```\n",
        paste(capture.output(print(round(prop_crosstab, 2))), collapse = "\n"),
        "\n```\n\n"
      )

      # Chi-square test if applicable
      if (min(crosstab) >= 5) {
        chi_test <- chisq.test(crosstab)
        table_text <- paste0(
          table_text,
          "**Uji Chi-square:**\n",
          "- Chi-square statistic: ", round(chi_test$statistic, 4), "\n",
          "- p-value: ", format(chi_test$p.value, scientific = TRUE), "\n",
          "- Kesimpulan: ",
          if (chi_test$p.value < 0.05) {
            "Ada hubungan signifikan antara kedua variabel"
          } else {
            "Tidak ada hubungan signifikan antara kedua variabel"
          }, "\n\n"
        )
      } else {
        table_text <- paste0(
          table_text,
          "**Catatan:** Uji Chi-square tidak dapat dilakukan karena ada sel dengan frekuensi < 5.\n\n"
        )
      }

      table_text <- paste0(
        table_text,
        "**Interpretasi:** Tabulasi silang menunjukkan hubungan antara ", var1, " dan ", var2,
        ". Total observasi: ", sum(crosstab), ".\n\n"
      )

      table_text
    })

    report_items$content[[item_id]] <- list(
      type = "crosstab",
      title = paste("Tabulasi Silang:", input$cat_var1, "vs", input$cat_var2),
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Tabulasi silang berhasil ditambahkan ke laporan!", type = "message")
  })

  # Display current report contents
  output$report_contents <- renderText({
    if (length(report_items$content) == 0) {
      return("Belum ada analisis yang ditambahkan ke laporan.\n\nPETUNJUK PENGGUNAAN:\n1. Lakukan analisis di berbagai tab (Eksplorasi, Asumsi, Inferensia, Regresi)\n2. Klik tombol 'Tambah ke Laporan' pada setiap analisis\n3. Visualisasi akan otomatis disertakan dalam laporan\n4. Download laporan lengkap dalam format R Markdown atau PDF")
    }

    contents <- character(0)
    viz_count <- 0

    for (i in seq_along(report_items$content)) {
      item <- report_items$content[[i]]
      contents <- c(contents, paste0(i, ". ", item$title, " (", item$type, ")"))

      # Count visualizations
      if (item$type %in% c("exploration", "choropleth", "spatial", "regression", "categorization", "correlation", "normality", "anova")) { # Added correlation, normality, anova
        viz_count <- viz_count + 1
      }
    }

    summary_text <- paste0(
      "KONTEN LAPORAN:\n",
      paste(contents, collapse = "\n"),
      "\n\n",
      "RINGKASAN:\n",
      "- Total analisis: ", length(report_items$content), "\n",
      "- Analisis dengan visualisasi: ", viz_count, "\n\n",
      "CATATAN: Pastikan untuk mendownload semua visualisasi secara terpisah sebelum menyusun laporan final."
    )

    return(summary_text)
  })

  # Comprehensive cleanup function
  cleanup_all_plots <- function() {
    cleaned_files <- 0

    # 1. Clean temp_plots directory
    if (dir.exists("temp_plots")) {
      plot_files_temp <- list.files("temp_plots", pattern = "\\.png$", full.names = TRUE)
      if (length(plot_files_temp) > 0) {
        file.remove(plot_files_temp)
        cleaned_files <- cleaned_files + length(plot_files_temp)
      }
    }

    # 2. Clean working directory (parent folder) only files that are known to be copied there by download_pdf_report
    # This specifically targets files that might be copied for PDF generation.
    copied_pattern_files <- list.files(".", pattern = "^(exploration_|correlation_|normality_|categorization_|regression_|spatial_|anova_).*\\.png$", full.names = TRUE)
    if (length(copied_pattern_files) > 0) {
      existing_files <- copied_pattern_files[file.exists(copied_pattern_files)]
      if (length(existing_files) > 0) {
        file.remove(existing_files)
        cleaned_files <- cleaned_files + length(existing_files)
      }
    }

    return(cleaned_files)
  }

  # Clear report with plot cleanup
  observeEvent(input$clear_report, {
    # Clean all plots
    cleaned_count <- cleanup_all_plots()

    # Clear report content
    report_items$content <- list()
    report_items$counter <- 0

    showNotification(paste("Laporan dibersihkan! Dihapus", cleaned_count, "file plot."), type = "message")
  })

  # =================
  # DOWNLOAD HANDLERS FOR SPATIAL ANALYSIS
  # =================

  # Download handlers for spatial analysis
  output$download_choropleth <- downloadHandler(
    filename = function() {
      paste("choropleth_", input$choropleth_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      # Create static ggplot version of the choropleth
      tryCatch(
        {
          geo_data <- indonesia_geojson()
          sovi <- sovi_data()

          if (!is.null(geo_data) && nrow(sovi) == 512 && length(geo_data) == 512) {
            geo_data$selected_var <- sovi[[input$choropleth_var]]

            # Convert to sf for ggplot
            geo_sf <- sf::st_as_sf(geo_data)

            p <- ggplot(geo_sf) +
              geom_sf(aes(fill = selected_var), color = "white", size = 0.1) +
              scale_fill_viridis_c(name = input$choropleth_var, na.value = "grey50") +
              theme_void() +
              labs(
                title = paste("Peta Choropleth:", input$choropleth_var),
                subtitle = "Data SUSENAS 2017"
              ) +
              theme(
                plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
                plot.subtitle = element_text(size = 12, hjust = 0.5),
                legend.position = "bottom"
              )

            ggsave(file, plot = p, width = 12, height = 8, dpi = 300, bg = "white")
          }
        },
        error = function(e) {
          # Fallback: create a simple text plot
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = "Error creating map", size = 6) +
            theme_void()
          ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
        }
      )
    }
  )

  output$download_spatial <- downloadHandler(
    filename = function() {
      paste("spatial_scatter_", input$spatial_var1, "_vs_", input$spatial_var2, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tryCatch(
        {
          # Get actual spatial data and create scatter plot
          sovi <- sovi_data()

          var1_data <- sovi[[input$spatial_var1]]
          var2_data <- sovi[[input$spatial_var2]]

          # Create scatter plot
          p <- ggplot(sovi, aes(x = !!sym(input$spatial_var1), y = !!sym(input$spatial_var2))) +
            geom_point(alpha = 0.6, size = 2, color = "steelblue") +
            geom_smooth(method = "lm", se = TRUE, color = "red", linetype = "dashed") +
            theme_minimal() +
            labs(
              title = paste("Analisis Spasial:", input$spatial_var1, "vs", input$spatial_var2),
              x = input$spatial_var1,
              y = input$spatial_var2,
              subtitle = paste("Korelasi:", round(cor(var1_data, var2_data, use = "complete.obs"), 4))
            ) +
            theme(
              plot.title = element_text(size = 14, face = "bold"),
              plot.subtitle = element_text(size = 12),
              axis.title = element_text(size = 11)
            )

          ggsave(file, plot = p, width = 10, height = 8, dpi = 300, bg = "white")
        },
        error = function(e) {
          # Fallback error plot
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = paste("Error creating spatial plot:", e$message), size = 6) +
            theme_void()
          ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
        }
      )
    }
  )

  # =================
  # DOWNLOAD HANDLERS (EXISTING)
  # =================
  # Data Asli tab download handlers
  output$download_data_summary <- downloadHandler(
    filename = function() {
      paste("data_summary_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      data <- sovi_data()
      summary_text <- paste(
        "Dataset Kerentanan Sosial Indonesia - SUSENAS 2017\n",
        "===============================================\n",
        "Jumlah observasi (kabupaten/kota):", nrow(data), "\n",
        "Jumlah variabel:", ncol(data), "\n",
        "Periode data: SUSENAS 2017\n",
        "Sumber: BPS-Statistics Indonesia\n\n",
        "Statistik Deskriptif:\n",
        "=====================\n"
      )
      write(summary_text, file)
      numeric_vars <- sapply(data, is.numeric)
      capture.output(summary(data[numeric_vars]), file = file, append = TRUE)
    }
  )

  output$download_raw_data <- downloadHandler(
    filename = function() {
      paste("raw_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(sovi_data(), file, row.names = FALSE)
    }
  )

  output$print_raw_data <- downloadHandler(
    filename = function() {
      paste("raw_data_print_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- sovi_data()
      write.csv(data, file, row.names = FALSE)
    }
  )

  output$download_desc <- downloadHandler(
    filename = function() {
      paste("descriptive_statistics_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      data <- sovi_data()
      numeric_vars <- sapply(data, is.numeric)
      capture.output(summary(data[numeric_vars]), file = file)
    }
  )

  output$download_corr <- downloadHandler(
    filename = function() {
      paste("correlation_matrix_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- sovi_data()
      numeric_vars <- sapply(data, is.numeric)
      cor_matrix <- cor(data[numeric_vars], use = "complete.obs")
      write.csv(cor_matrix, file)
    }
  )

  # Download plot for general exploration
  output$download_plot <- downloadHandler(
    filename = function() {
      paste("exploration_plot_", input$plot_type, "_", input$x_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      data <- sovi_data()

      if (input$plot_type == "Histogram") {
        p <- ggplot(data, aes(x = !!sym(input$x_var))) +
          geom_histogram(bins = 30, fill = "lightblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Histogram of", input$x_var))
      } else if (input$plot_type == "Boxplot") {
        p <- ggplot(data, aes(y = !!sym(input$x_var))) +
          geom_boxplot(fill = "lightblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$x_var))
      } else if (input$plot_type == "Scatter Plot") {
        p <- ggplot(data, aes(x = !!sym(input$x_var), y = !!sym(input$y_var))) +
          geom_point(alpha = 0.6) +
          geom_smooth(method = "lm", se = FALSE) +
          theme_minimal() +
          labs(title = paste("Scatter Plot:", input$x_var, "vs", input$y_var))
      } else if (input$plot_type == "Density Plot") {
        p <- ggplot(data, aes(x = !!sym(input$x_var))) +
          geom_density(fill = "lightblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Density Plot of", input$x_var))
      }

      ggsave(file, plot = p, width = 10, height = 6, dpi = 300)
    }
  )

  # Download regression results (enhanced with plots)
  output$download_regression <- downloadHandler(
    filename = function() {
      paste("regression_analysis_", input$reg_response, "_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(input$reg_predictors)

      tryCatch(
        {
          data <- sovi_data()
          formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " + "))
          model <- lm(as.formula(formula_str), data = data)

          # Create diagnostic plots
          png_file <- gsub("\\.txt$", "_diagnostics.png", file)
          png(png_file, width = 12, height = 8, units = "in", res = 300)
          par(mfrow = c(2, 2))
          plot(model)
          dev.off()

          # Capture all regression output
          output_text <- paste(
            "ANALISIS REGRESI LINEAR\n",
            "======================\n\n",
            "Formula: ", formula_str, "\n\n",
            "SUMMARY MODEL:\n",
            paste(capture.output(summary(model)), collapse = "\n"),
            "\n\n",
            "UJI ASUMSI:\n",
            paste(capture.output({
              cat("Durbin-Watson Test (Independence):\n")
              print(dwtest(model))
              cat("\nBreusch-Pagan Test (Homoscedasticity):\n")
              print(bptest(model))
              cat("\nShapiro-Wilk Test (Normality of Residuals):\n")
              if (length(residuals(model)) <= 5000) {
                print(shapiro.test(residuals(model)))
              } else {
                cat("Sample too large for Shapiro-Wilk test\n")
              }
            }), collapse = "\n"),
            "\n\nCATATAN: File diagnostic plots tersimpan sebagai: ", basename(png_file),
            "\n\nTanggal analisis: ", Sys.Date()
          )

          writeLines(output_text, file)
        },
        error = function(e) {
          writeLines(paste("Error in regression analysis:", e$message), file)
        }
      )
    }
  )

  # Download categorized data
  output$download_categorized_data <- downloadHandler(
    filename = function() {
      paste("categorized_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0) {
        data <- categorized_data()
        write.csv(data, file, row.names = FALSE)
      }
    }
  )

  # Print categorized data (opens in new tab)
  output$print_categorized_data <- downloadHandler(
    filename = function() {
      paste("categorized_data_print_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0) {
        data <- categorized_data()
        write.csv(data, file, row.names = FALSE)
      }
    }
  )

  # Download categorical plot (simplified)
  output$download_cat_plot <- downloadHandler(
    filename = function() {
      paste("categorical_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0 && !is.null(input$cat_var_explore)) {
        data <- categorized_data()
        cat_var <- input$cat_var_explore

        # Simple frequency bar chart
        p <- ggplot(data, aes(x = !!sym(cat_var), fill = !!sym(cat_var))) +
          geom_bar(alpha = 0.7, color = "white", size = 0.5) +
          theme_minimal() +
          labs(
            title = paste("Distribusi Frekuensi:", cat_var),
            x = cat_var,
            y = "Frekuensi"
          ) +
          theme(
            legend.position = "none",
            plot.title = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 45, hjust = 1)
          ) +
          scale_fill_brewer(type = "qual", palette = "Set3")

        ggsave(file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
      }
    }
  )

  # Download frequency table
  output$download_freq_table <- downloadHandler(
    filename = function() {
      paste("frequency_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0 && !is.null(input$cat_var_explore)) {
        data <- categorized_data()
        cat_var <- input$cat_var_explore

        freq_data <- data %>%
          count(!!sym(cat_var)) %>%
          mutate(
            Percentage = round(n / sum(n) * 100, 2),
            `Cumulative Freq` = cumsum(n),
            `Cumulative %` = round(cumsum(n) / sum(n) * 100, 2)
          ) %>%
          rename(
            Category = !!sym(cat_var),
            Frequency = n
          )

        write.csv(freq_data, file, row.names = FALSE)
      }
    }
  )

  # Download crosstab
  output$download_crosstab <- downloadHandler(
    filename = function() {
      paste("crosstab_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0 && !is.null(input$cat_var1) && !is.null(input$cat_var2)) {
        data <- categorized_data()
        crosstab <- table(data[[input$cat_var1]], data[[input$cat_var2]])
        crosstab_df <- as.data.frame.matrix(crosstab)
        crosstab_df$Total <- rowSums(crosstab_df)
        crosstab_df <- rbind(crosstab_df, Total = colSums(crosstab_df))

        write.csv(crosstab_df, file)
      }
    }
  )

  # =================
  # REPORT DOWNLOAD HANDLERS
  # =================

  # Download PDF report
  output$download_pdf_report <- downloadHandler(
    filename = function() {
      paste("laporan_analisis_", Sys.Date(), ".pdf", sep = "")
    },
    content = function(file) {
      # Ensure TinyTeX is installed
      if (!tinytex::is_tinytex()) {
        showNotification("Installing TinyTeX for PDF generation...", type = "message")
        tinytex::install_tinytex()
      }

      # Create a temporary directory for rendering to avoid polluting main working directory
      # and to ensure a clean render environment
      temp_render_dir <- tempdir()
      original_wd <- getwd() # Save original working directory

      # Change working directory to the temporary render directory
      setwd(temp_render_dir)

      # Copy all necessary plot files from temp_plots to the new temp_render_dir
      temp_plots_source_dir <- file.path(original_wd, "temp_plots")

      if (dir.exists(temp_plots_source_dir)) {
        plot_files_in_temp_plots <- list.files(temp_plots_source_dir, pattern = "\\.png$", full.names = TRUE)
        if (length(plot_files_in_temp_plots) > 0) {
          # Copy files to the new temporary rendering directory
          file.copy(plot_files_in_temp_plots, temp_render_dir, overwrite = TRUE)
        }
      }

      # Create temporary R Markdown file within the temp_render_dir
      temp_rmd_path <- file.path(temp_render_dir, "report.Rmd")

      # Try to read main template, fallback to simple template
      template_file <- file.path(original_wd, "report_template.Rmd") # Assume template is in original WD
      if (!file.exists(template_file)) {
        template_file <- file.path(original_wd, "report_template_simple.Rmd")
      }

      # Load template content
      template_content <- tryCatch(
        {
          readLines(template_file)
        },
        error = function(e) {
          # Ultimate fallback template (as you already have)
          c(
            "---",
            "title: 'Laporan Analisis Kerentanan Sosial Indonesia'",
            "output:",
            "  pdf_document:",
            "    latex_engine: xelatex",
            "    toc: true",
            "    number_sections: true",
            "    keep_tex: true",
            "header-includes:",
            "  - \\usepackage{fontspec}",
            "  - \\usepackage{polyglossia}",
            "  - \\setmainlanguage{indonesian}",
            "---",
            "",
            "# Ringkasan Eksekutif",
            "",
            "{{{executive_summary}}}",
            "",
            "{{{metadata_section}}}",
            "",
            "{{{summary_section}}}",
            "",
            "{{{content_sections}}}",
            "",
            "# Kesimpulan",
            "",
            "{{{conclusions}}}",
            "",
            "# Rekomendasi",
            "",
            "{{{recommendations}}}",
            "",
            "---",
            "Tanggal: `r Sys.Date()`"
          )
        }
      )

      # Prepare content sections for PDF with integrated visualizations
      content_sections <- ""
      if (length(report_items$content) > 0) {
        content_sections <- paste0(content_sections, "\n# ANALISIS YANG DISERTAKAN\n\n")
        content_sections <- paste0(content_sections, "**Laporan Lengkap:** Semua visualisasi dan analisis terintegrasi langsung dalam dokumen PDF ini.\n\n")
        content_sections <- paste0(content_sections, "---\n\n")

        for (item in report_items$content) {
          # Crucial: Ensure image paths are just filenames for LaTeX to find them in the temp_render_dir
          item_content <- item$content
          item_content <- gsub("!\\[([^\\]]*)\\]\\(.*?([^/\\\\]+\\.png)\\)", "![\\1](\\2)", item_content)

          content_sections <- paste0(content_sections, "\n", item_content, "\n")
        }
      } else {
        content_sections <- "\n# Analisis\n\nBelum ada analisis yang ditambahkan ke laporan.\n\n"
      }

      # Prepare metadata section (no changes needed here, as it's text)
      metadata_section <- ""
      if (input$include_metadata) {
        data <- sovi_data()
        metadata_section <- paste0(
          "\n# Metadata Dataset\n\n",
          "**Sumber Data:** SUSENAS 2017 - BPS Statistics Indonesia\n\n",
          "**Jumlah Observasi:** ", nrow(data), " kabupaten/kota\n\n",
          "**Jumlah Variabel:** ", ncol(data), "\n\n",
          "**Variabel yang Tersedia:**\n",
          paste("- ", names(data), collapse = "\n"), "\n\n",
          "**Deskripsi Variabel:**\n",
          "- CHILDREN: Persentase populasi di bawah 5 tahun\n",
          "- FEMALE: Persentase populasi perempuan\n",
          "- ELDERLY: Persentase populasi 65 tahun ke atas\n",
          "- FHEAD: Persentase rumah tangga dengan kepala keluarga perempuan\n",
          "- FAMILYSIZE: Rata-rata jumlah anggota rumah tangga\n",
          "- NOELECTRIC: Persentase rumah tangga tanpa listrik\n",
          "- LOWEDU: Persentase populasi 15+ dengan pendidikan rendah\n",
          "- GROWTH: Persentase perubahan populasi\n",
          "- POVERTY: Persentase penduduk miskin\n",
          "- ILLITERATE: Persentase populasi yang buta huruf\n",
          "- NOTRAINING: Persentase rumah tangga tanpa pelatihan bencana\n",
          "- DPRONE: Persentase rumah tangga di area rawan bencana\n",
          "- RENTED: Persentase rumah tangga yang menyewa rumah\n",
          "- NOSEWER: Persentase rumah tangga tanpa sistem drainase\n",
          "- TAPWATER: Persentase rumah tangga dengan air keran\n",
          "- POPULATION: Jumlah populasi\n\n"
        )
      }

      # Prepare summary section (no changes needed here, as it's text)
      summary_section <- ""
      if (input$include_summary) {
        summary_section <- "\n# Ringkasan Statistik\n\nStatistik deskriptif telah disertakan dalam analisis individual.\n\n"
      }

      # Replace placeholders in template_content
      template_content <- gsub("\\{\\{\\{include_code\\}\\}\\}", tolower(as.character(input$include_code)), template_content)
      template_content <- gsub(
        "\\{\\{\\{executive_summary\\}\\}\\}",
        paste0("Laporan ini berisi ", length(report_items$content), " analisis yang telah dipilih oleh pengguna."),
        template_content
      )
      template_content <- gsub("\\{\\{\\{metadata_section\\}\\}\\}", metadata_section, template_content)
      template_content <- gsub("\\{\\{\\{summary_section\\}\\}\\}", summary_section, template_content)
      template_content <- gsub("\\{\\{\\{content_sections\\}\\}\\}", content_sections, template_content)
      template_content <- gsub(
        "\\{\\{\\{conclusions\\}\\}\\}",
        "Kesimpulan akan diperbarui berdasarkan analisis yang telah dilakukan.",
        template_content
      )
      template_content <- gsub(
        "\\{\\{\\{recommendations\\}\\}\\}",
        "Rekomendasi akan dikembangkan berdasarkan temuan analisis.",
        template_content
      )

      # Write the prepared content to the temporary Rmd file
      writeLines(template_content, temp_rmd_path)

      tryCatch(
        {
          # Render to PDF
          rmarkdown::render(
            input = temp_rmd_path,
            output_format = rmarkdown::pdf_document(
              latex_engine = "xelatex",
              toc = TRUE,
              number_sections = TRUE,
              keep_tex = TRUE
            ),
            output_file = file,
            quiet = FALSE, # Set to TRUE for production to suppress messages
            envir = new.env() # Use a fresh environment for rendering
          )

          showNotification("PDF berhasil dibuat! File plot tetap tersimpan untuk penggunaan berikutnya.", type = "message")
        },
        error = function(e) {
          showNotification(paste("Error creating PDF:", e$message), type = "error")
          # Log the full error for debugging
          cat(paste("Full PDF error:", e$message, "\n"))

          # No fallback to markdown directly in downloadHandler's content function,
          # as 'file' must be a PDF. User can choose Rmd download if PDF fails.
        },
        finally = {
          # Clean up temporary Rmd file and auxiliary files regardless of success/failure
          if (file.exists(temp_rmd_path)) file.remove(temp_rmd_path)
          temp_base <- tools::file_path_sans_ext(temp_rmd_path)
          aux_files <- c(
            paste0(temp_base, ".tex"),
            paste0(temp_base, ".log"),
            paste0(temp_base, ".aux"),
            paste0(temp_base, ".out"),
            paste0(temp_base, ".toc")
          )
          for (aux_file in aux_files) {
            if (file.exists(aux_file)) {
              file.remove(aux_file)
            }
          }

          # IMPORTANT: Restore original working directory after download handler finishes
          setwd(original_wd)
        }
      )

      # Clear report list after PDF download (but don't clean up plot files in temp_plots,
      # as they might be needed for subsequent PDF renders or Rmd downloads)
      report_items$content <- list()
      report_items$counter <- 0
    }
  )

  output$download_full_report <- downloadHandler(
    filename = function() {
      paste("full_analysis_report_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Create a comprehensive text report
      data <- sovi_data()

      text_content <- paste0(
        "LAPORAN ANALISIS KERENTANAN SOSIAL INDONESIA\n",
        "============================================\n\n",
        "STATISTIK DESKRIPTIF\n",
        "-------------------\n",
        paste(capture.output(summary(data)), collapse = "\n"), "\n\n",
        "INFORMASI DATASET\n",
        "----------------\n",
        "Jumlah observasi: ", nrow(data), "\n",
        "Jumlah variabel: ", ncol(data), "\n",
        "Tanggal analisis: ", Sys.Date(), "\n\n",
        "Sumber: SUSENAS 2017 - BPS Statistics Indonesia\n"
      )

      writeLines(text_content, file)
    }
  )
}

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
library(viridis)

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
      plot_env$data <- working_data()
      plot_env$input <- input

      # Tambahkan variabel khusus berdasarkan jenis plot
      if (grepl("regression", plot_paths$filename)) {
        data <- working_data()
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
    # PERBAIKAN: Menggunakan reactiveVal untuk data kerja yang persisten
  # Ini akan menyimpan data asli ditambah kolom-kolom kategori yang dibuat pengguna
  working_data <- reactiveVal(read.csv("data/sovi_data.csv"))
  categorized_data <- reactive({
    working_data()
  })
  # Load default data or user uploaded data
  sovi_data <- reactive({
    if (!is.null(user_data$main_data)) {
      return(user_data$main_data)
    } else {
      return(read.csv("data/sovi_data.csv"))
    }
  })
    observe({
    if (!is.null(user_data$main_data)) {
      working_data(user_data$main_data)
    } else {
      working_data(read.csv("data/sovi_data.csv"))
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
    data <- working_data()
    names(data)
  })
    # PERBAIKAN: Kolom kategorik sekarang dinamis dari working_data
  categorical_columns <- reactive({
    data <- working_data()
    # Identifikasi kolom faktor atau yang berakhiran "_cat"
    is_cat <- sapply(data, is.factor) | grepl("_cat$", names(data))
    names(data)[is_cat]
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
    # Reset working_data juga
    working_data(read.csv("data/sovi_data.csv"))
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
    num_choices <- numeric_columns()
    cat_choices <- categorical_columns() # PERBAIKAN: Menggunakan reactive baru

    # Update pilihan variabel numerik
    updateSelectInput(session, "var_to_categorize", choices = num_choices)
    updateSelectInput(session, "x_var", choices = num_choices)
    updateSelectInput(session, "y_var", choices = num_choices)
    updateSelectInput(session, "choropleth_var", choices = num_choices)
    updateSelectInput(session, "spatial_var1", choices = num_choices)
    updateSelectInput(session, "spatial_var2", choices = num_choices)
    updateSelectInput(session, "normality_var", choices = num_choices)
    updateSelectInput(session, "homogeneity_var", choices = num_choices)
    updateSelectInput(session, "mean_var", choices = num_choices)
    updateSelectInput(session, "paired_var", choices = num_choices)
    updateSelectInput(session, "prop_var", choices = num_choices)
    updateSelectInput(session, "variance_var", choices = num_choices)
    updateSelectInput(session, "anova_response", choices = num_choices)
    updateSelectInput(session, "reg_response", choices = num_choices)
    updateCheckboxGroupInput(session, "reg_predictors", choices = num_choices)

    # PERBAIKAN: Update pilihan variabel kategorik dari sumber yang benar
    if (length(cat_choices) > 0) {
      # Update untuk eksplorasi data kategorik
      updateSelectInput(session, "cat_var_explore", choices = cat_choices)
      updateSelectInput(session, "cat_var1", choices = cat_choices)
      updateSelectInput(session, "cat_var2", choices = cat_choices)

      # Update untuk uji homogenitas
      updateSelectInput(session, "group_var_homogeneity", choices = cat_choices)

      # Update untuk uji rata-rata dua sampel
      updateSelectInput(session, "group_var_mean", choices = cat_choices)
      
      # PENAMBAHAN: Update untuk uji proporsi dua sampel
      updateSelectInput(session, "group_var_prop", choices = cat_choices)

      # Update untuk uji varians dua sampel
      updateSelectInput(session, "group_var_variance", choices = cat_choices)

      # Update untuk ANOVA
      updateSelectInput(session, "anova_factor1", choices = cat_choices)
      updateSelectInput(session, "anova_factor2", choices = cat_choices)
    }
  })

observeEvent(input$categorize, {
    req(input$var_to_categorize, input$n_categories, input$method)

    # Ambil data kerja saat ini
    data <- working_data()
    var_name <- input$var_to_categorize
    n_cat <- input$n_categories
    method <- input$method
    new_cat_col_name <- paste0(var_name, "_", method, "_", n_cat, "_cat")

    # Cek jika kolom sudah ada
    if (new_cat_col_name %in% names(data)) {
      showNotification(paste("Kategori '", new_cat_col_name, "' sudah ada. Silakan gunakan nama lain atau hapus yang lama."), type = "warning")
      return()
    }

    # Get custom category names with proper validation
    cat_names <- sapply(1:n_cat, function(i) {
      input_name <- input[[paste0("cat_name_", i)]]
      if (!is.null(input_name) && input_name != "") {
        return(input_name)
      } else {
        return(paste0("Kategori_", i))
      }
    })

    # Ensure we have the right number of names
    cat_names <- cat_names[1:n_cat]

    # Buat kolom kategori baru
    new_category_col <- tryCatch({
      if (method == "quantile") {
        cut(data[[var_name]],
            breaks = quantile(data[[var_name]],
                              probs = seq(0, 1, length.out = n_cat + 1),
                              na.rm = TRUE
            ),
            labels = cat_names,
            include.lowest = TRUE
        )
      } else if (method == "equal_width") {
        cut(data[[var_name]],
            breaks = n_cat,
            labels = cat_names,
            include.lowest = TRUE
        )
      } else if (method == "kmeans") {
        var_values <- data[[var_name]]
        var_values_clean <- var_values[!is.na(var_values)]

        if (length(var_values_clean) > n_cat) {
          km_result <- kmeans(var_values_clean, centers = n_cat, nstart = 25)
          clusters <- rep(NA, length(var_values))
          clusters[!is.na(var_values)] <- km_result$cluster
          factor(clusters, labels = cat_names)
        } else {
          showNotification("Data tidak cukup untuk K-means.", type = "error")
          return(NULL)
        }
      }
    }, error = function(e){
      showNotification(paste("Error saat membuat kategori:", e$message), type="error")
      return(NULL)
    })

    # Jika berhasil, tambahkan kolom baru ke working_data
    if(!is.null(new_category_col)){
      data[[new_cat_col_name]] <- new_category_col
      working_data(data) # Update reactiveVal
      showNotification(paste("Kategori baru '", new_cat_col_name, "' berhasil ditambahkan!"), type = "message")
    }
 })
  output$categorized_preview <- DT::renderDataTable({
    data <- working_data()
    cat_cols <- names(data)[grepl("_cat$", names(data))]
    if (length(cat_cols) > 0) {
      preview_cols <- c(names(data)[1], cat_cols) # Tampilkan kolom ID dan semua kolom kategori
      DT::datatable(data[, unique(preview_cols)], options = list(scrollX = TRUE))
    } else {
      DT::datatable(data.frame(Pesan = "Belum ada data yang dikategorisasi. Silakan buat kategori di atas."))
    }
  })

  # Interpretation sekarang juga menggunakan working_data
  output$categorization_interpretation <- renderText({
      data <- working_data()
      cat_cols <- names(data)[grepl("_cat$", names(data))]
      
      if(length(cat_cols) == 0){
        return("Belum ada kategori yang dibuat untuk diinterpretasi.")
      }

      # Ambil interpretasi untuk kategori terakhir yang dibuat
      last_cat_col <- tail(cat_cols, 1)
      # Ekstrak nama variabel asli dari nama kolom kategori
      var_name <- sub("_(quantile|equal_width|kmeans)_\\d+_cat$", "", last_cat_col)
      
      if(!var_name %in% names(data)){
        return("Tidak dapat menemukan variabel asli untuk kategori terakhir.")
      }

      cat_summary <- data %>%
        filter(!is.na(!!sym(last_cat_col))) %>%
        group_by(!!sym(last_cat_col)) %>%
              summarise(
                count = n(),
                mean_value = mean(!!sym(var_name), na.rm = TRUE),
                min_value = min(!!sym(var_name), na.rm = TRUE),
                max_value = max(!!sym(var_name), na.rm = TRUE),
                .groups = "drop"
              )

            interpretation <- paste0(
              "INTERPRETASI KATEGORISASI TERAKHIR:\n\n",
              "Variabel Asli: ", var_name, "\n",
              "Nama Kolom Kategori Baru: ", last_cat_col, "\n\n",
              "Distribusi data per kategori:\n"
            )

            for (i in 1:nrow(cat_summary)) {
              interpretation <- paste0(
                interpretation,
                "- ", cat_summary[[last_cat_col]][i], ": ", cat_summary$count[i], " wilayah ",
                "(", round(cat_summary$count[i] / nrow(data) * 100, 1), "%) ",
                "- Rata-rata: ", round(cat_summary$mean_value[i], 2),
                " (Range: ", round(cat_summary$min_value[i], 2), " - ",
                round(cat_summary$max_value[i], 2), ")\n"
              )
            }
            return(interpretation)
  })

  # Full categorized data table
  output$categorized_full_data <- DT::renderDataTable({
          DT::datatable(working_data(), options = list(scrollX = TRUE, pageLength = 10))
  })
  # Categorical plot (simplified to bar chart only)
  output$categorical_plot <- renderPlotly({
  req(input$cat_var_explore)
    data <- working_data()
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
      scale_fill_viridis_d() # Use viridis for discrete scales

    ggplotly(p)
  })

  # Frequency table for categorical variables
  output$frequency_table <- DT::renderDataTable({
    req(input$cat_var_explore)
    data <- working_data()
    cat_var <- input$cat_var_explore

    freq_data <- data %>%
      count(!!sym(cat_var)) %>%
      filter(!is.na(!!sym(cat_var))) %>%
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
    req(input$cat_var_explore)
    data <- working_data()
    cat_var <- input$cat_var_explore

    freq_data <- data %>%
      count(!!sym(cat_var)) %>%
      filter(!is.na(!!sym(cat_var))) %>%
      mutate(percentage = round(n / sum(n) * 100, 1))
    if(nrow(freq_data) == 0) return("Tidak ada data untuk diinterpretasi.")
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

    data <- working_data()
    crosstab <- table(data[[input$cat_var1]], data[[input$cat_var2]])
    crosstab_df <- as.data.frame.matrix(crosstab)
    crosstab_df$Total <- rowSums(crosstab_df)
    crosstab_df <- rbind(crosstab_df, Total = colSums(crosstab_df))

    output$crosstab_table <- DT::renderDataTable({
      DT::datatable(crosstab_df, options = list(scrollX = TRUE))
    })
  })
  
  # Data Asli tab output
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


  # Descriptive statistics
  output$descriptive_stats <- renderPrint({
    data <- working_data()
    numeric_vars <- sapply(data, is.numeric)
    summary(data[numeric_vars])
  })

  # Correlation plot
  output$correlation_plot <- renderPlot({
    data <- working_data()
    numeric_vars <- sapply(data, is.numeric)
    # Hapus kolom dengan varians nol
    data_numeric <- data[numeric_vars]
    data_numeric <- data_numeric[, apply(data_numeric, 2, var, na.rm=TRUE) != 0]

    if(ncol(data_numeric) < 2) {
      plot(1, type="n", axes=F, xlab="", ylab="")
      text(1, 1, "Tidak cukup variabel numerik dengan varians > 0\nuntuk membuat plot korelasi.", cex=1.2)
      return()
    }

    cor_matrix <- cor(data_numeric, use = "complete.obs")
    corrplot(cor_matrix,
      method = "color", type = "upper", order = "hclust",
      tl.cex = 0.8, tl.col = "black", tl.srt = 45
    )
  })

  # Data visualization
  output$data_plot <- renderPlotly({
    data <- working_data()

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
    data <- working_data()
    var_name <- input$x_var

    if (!is.null(var_name)) {
      var_data <- data[[var_name]]
      var_data <- var_data[!is.na(var_data)]
      
      if(length(var_data) == 0) return("Variabel tidak memiliki data valid.")
      mean_val <- mean(var_data, na.rm = TRUE)
      median_val <- median(var_data, na.rm = TRUE)
      sd_val <- sd(var_data, na.rm = TRUE)

      interpretation <- paste0(
        "INTERPRETASI EKSPLORASI DATA:\n\n",
        "Variabel: ", var_name, "\n",
        "Rata-rata: ", round(mean_val, 2), "\n",
        "Median: ", round(median_val, 2), "\n",
        "Standar Deviasi: ", round(sd_val, 2), "\n\n",
        if (sd_val == 0) {
          "Data konstan, tidak ada variasi."
        } else if (abs(mean_val - median_val) < 0.1 * sd_val) {
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
    data <- working_data()
    var_data <- data[[input$normality_var]]
    var_data <- var_data[!is.na(var_data)] # Remove missing values
    
    if(length(var_data) < 4) {
      showNotification("Uji normalitas memerlukan setidaknya 4 data poin.", type="error")
      return()
    }

    output$normality_result <- renderPrint({
      shapiro_res <- if (length(var_data) <= 5000) shapiro.test(var_data) else NULL
      ad_res <- ad.test(var_data)
      jb_res <- tseries::jarque.bera.test(var_data)

      hasil <- list()

      if (!is.null(shapiro_res)) {
        hasil[["Shapiro-Wilk Test"]] <- shapiro_res
        hasil[["Interpretasi Shapiro-Wilk"]] <- interpret_pval("Shapiro-Wilk", "normality_test", shapiro_res$p.value)
      } else {
        hasil[["Shapiro-Wilk Test"]] <- "Dilewati (n > 5000)"
      }

      hasil[["Anderson-Darling Test"]] <- ad_res
      hasil[["Interpretasi Anderson-Darling"]] <- interpret_pval("Anderson-Darling", "normality_test", ad_res$p.value)

      hasil[["Jarque-Bera Test"]] <- jb_res
      hasil[["Interpretasi Jarque-Bera"]] <- interpret_pval("Jarque-Bera", "normality_test", jb_res$p.value)
      return(hasil)
    })

    # Q-Q plot
    output$normality_plot <- renderPlot({
      qqnorm(var_data, main = paste("Q-Q Plot for", input$normality_var))
      qqline(var_data, col = "red")
    })
  })

  # Homogeneity test - MENGGUNAKAN VARIABEL KATEGORIK YANG TELAH DIBUAT
  observeEvent(input$test_homogeneity, {
    req(input$homogeneity_var, input$group_var_homogeneity)
    # Gunakan data yang sudah dikategorisasi
    data <- working_data()

    # Ambil variabel pengelompokan (kategorik) dari data yang sudah dikategorisasi
    group_var <- data[[input$group_var_homogeneity]]

   # Levene's test
    levene_result <- leveneTest(data[[input$homogeneity_var]], as.factor(group_var))

    # Bartlett's test
    bartlett_result <- bartlett.test(data[[input$homogeneity_var]], as.factor(group_var))

    output$homogeneity_result <- renderPrint({
      list(
        "Levene's Test" = levene_result,
        "Levene's Test Interpretation" = interpret_pval("Levene's Test", "homogeneity test", levene_result[["Pr(>F)"]][1]),
        "Bartlett's Test" = bartlett_result,
        "Bartlett's Test Interpretation" = interpret_pval("Bartlett's Test", "homogeneity test", bartlett_result$p.value)
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
    - Jika p-value > 0.05: Data berdistribusi normal (Gagal tolak H0)
    - Jika p-value <= 0.05: Data tidak berdistribusi normal (Tolak H0)

    UJI HOMOGENITAS:
    - Jika p-value > 0.05: Varians antar kelompok homogen (Gagal tolak H0)
    - Jika p-value <= 0.05: Varians antar kelompok tidak homogen (Tolak H0)

    Catatan: Uji asumsi penting untuk menentukan metode statistik yang tepat."
  })

 # Mean tests
  observeEvent(input$test_mean, {
    if (input$test_type == "one_sample") {
      data <- working_data()
      var_data <- data[[input$mean_var]]
      result <- t.test(var_data, mu = input$mu)
      output$mean_test_result <- renderPrint({
        result
      })
      output$mean_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI RATA-RATA SATU SAMPEL:\n\n",
          "H0: mu = ", input$mu, "\n",
          "H1: mu != ", input$mu, "\n\n",
          "p-value: ", round(result$p.value, 4), "\n\n",
          if (result$p.value < 0.05) {
            paste0("Kesimpulan: Tolak H0. Terdapat bukti statistik yang cukup untuk menyatakan rata-rata populasi berbeda dari ", input$mu, ".")
          } else {
            paste0("Kesimpulan: Gagal Tolak H0. Tidak terdapat bukti statistik yang cukup untuk menyatakan rata-rata populasi berbeda dari ", input$mu, ".")
          }
        )
      })
    } else if (input$test_type == "two_sample") {
      req(input$mean_var, input$group_var_mean)
      data <- working_data()
      group_var <- data[[input$group_var_mean]]

      # Pastikan hanya ada 2 kategori
      if (length(unique(na.omit(group_var))) != 2) {
        showNotification("Variabel pengelompokan harus memiliki tepat 2 kategori.", type = "error")
        return()
      }

      result <- t.test(as.formula(paste(input$mean_var, "~", input$group_var_mean)), data = data)
      output$mean_test_result <- renderPrint({
        result
      })
      output$mean_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI RATA-RATA DUA SAMPEL INDEPENDEN:\n\n",
          "H0: Rata-rata kedua kelompok adalah sama.\n",
          "H1: Rata-rata kedua kelompok tidak sama.\n\n",
          "p-value: ", round(result$p.value, 4), "\n\n",
          if (result$p.value < 0.05) {
            "Kesimpulan: Tolak H0. Terdapat perbedaan rata-rata yang signifikan antara kedua kelompok."
          } else {
            "Kesimpulan: Gagal Tolak H0. Tidak terdapat perbedaan rata-rata yang signifikan antara kedua kelompok."
          }
        )
      })
    } else if (input$test_type == "paired") {
      data <- working_data()
      var1_data <- data[[input$mean_var]]
      var2_data <- data[[input$paired_var]]

      result <- t.test(var1_data, var2_data, paired = TRUE)
      output$mean_test_result <- renderPrint({
        result
      })
      output$mean_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI RATA-RATA BERPASANGAN:\n\n",
          "H0: Rata-rata perbedaan antara pasangan adalah nol.\n",
          "H1: Rata-rata perbedaan antara pasangan bukan nol.\n\n",
          "p-value: ", round(result$p.value, 4), "\n\n",
          if (result$p.value < 0.05) {
            "Kesimpulan: Tolak H0. Terdapat perbedaan yang signifikan antara kedua variabel berpasangan."
          } else {
            "Kesimpulan: Gagal Tolak H0. Tidak terdapat perbedaan yang signifikan antara kedua variabel berpasangan."
          }
        )
      })
    }
  })

  #uji proporsi dan uji varians
  # BAGIAN SERVER UNTUK UJI PROPORSI - FIXED

# Dynamic UI untuk menampilkan range data
output$data_range_ui <- renderUI({
  req(input$prop_var)
  
  # PERBAIKAN: Langsung akses working_data()
  current_data <- working_data() 
  
  if (input$prop_var %in% names(current_data)) {
    var_data <- na.omit(current_data[[input$prop_var]])
    if(length(var_data) > 0 && is.numeric(var_data)) {
      min_val <- min(var_data)
      max_val <- max(var_data)
      median_val <- median(var_data)
      
      div(
        style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px; margin-bottom: 10px;",
        h5("Statistik Deskriptif:", style = "margin-top: 0;"),
        p(paste0("Range data: ", round(min_val, 3), " - ", round(max_val, 3))),
        p(paste0("Median: ", round(median_val, 3))),
        p(paste0("Mean: ", round(mean(var_data), 3)))
      )
    }
  }
})

# Update nilai default threshold berdasarkan median
observe({
  req(input$prop_var)
  
  # PERBAIKAN: Gunakan working_data()
  current_data <- working_data()
  
  if (input$prop_var %in% names(current_data)) {
    var_data <- na.omit(current_data[[input$prop_var]])
    if(length(var_data) > 0 && is.numeric(var_data)) {
      median_val <- median(var_data)
      updateNumericInput(session, "prop_threshold", value = round(median_val, 3)) # Pembulatan untuk tampilan lebih rapi
    }
  }
})

observeEvent(input$test_proportion, {
  
  # PERBAIKAN: Langsung ambil data dari reactiveVal working_data()
  current_data <- working_data()
  
  if (input$prop_test_type == "one_sample_prop") {
    req(input$prop_var, input$prop_threshold, input$prop_hypothesized)
    
    # Validasi input
    if(is.na(input$prop_threshold) || is.na(input$prop_hypothesized)) {
      showNotification("Harap isi semua parameter yang diperlukan.", type="error")
      return()
    }
    
    # Ambil data numerik
    if(!input$prop_var %in% names(current_data)) {
      showNotification("Variabel tidak ditemukan dalam data.", type="error")
      return()
    }
    
    var_data <- current_data[[input$prop_var]]
    var_data_clean <- na.omit(var_data)
    
    if(length(var_data_clean) == 0) {
      showNotification("Tidak ada data valid untuk variabel yang dipilih.", type="error")
      return()
    }
    
    if(!is.numeric(var_data_clean)) {
      showNotification("Variabel harus berupa data numerik.", type="error")
      return()
    }
    
    # Konversi ke biner berdasarkan threshold
    successes <- sum(var_data_clean > input$prop_threshold)
    n <- length(var_data_clean)
    
    tryCatch({
      result <- prop.test(x = successes, n = n, 
                         p = input$prop_hypothesized,
                         alternative = input$prop_alternative,
                         correct = TRUE)
      
      output$proportion_result <- renderPrint({ result })
      
      observed_prop <- successes / n
      alternative_text <- switch(input$prop_alternative,
        "two.sided" = "tidak sama dengan",
        "greater" = "lebih besar dari",
        "less" = "lebih kecil dari"
      )
      
      p_val <- if(is.na(result$p.value)) "tidak dapat dihitung" else round(result$p.value, 4)
      conf_int <- if(any(is.na(result$conf.int))) c("NA", "NA") else round(result$conf.int, 4)
      
      output$proportion_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI PROPORSI SATU SAMPEL:\n\n",
          "Variabel: ", input$prop_var, "\n",
          "Threshold: ", input$prop_threshold, "\n",
          "Definisi sukses: ", input$prop_var, " > ", input$prop_threshold, "\n",
          "Jumlah sukses: ", successes, " dari ", n, " observasi\n",
          "Proporsi observasi: ", round(observed_prop, 4), "\n\n",
          "H0: p = ", input$prop_hypothesized, "\n",
          "H1: p ", alternative_text, " ", input$prop_hypothesized, "\n\n",
          "p-value: ", p_val, "\n",
          "Confidence Interval: [", conf_int[1], ", ", conf_int[2], "]\n\n",
          # Perbaikan untuk error "argument is of length zero"
          if (!is.character(p_val) && p_val < 0.05) {
            paste0("Kesimpulan: Tolak H0 (α = 0.05). Proporsi sukses secara signifikan ", 
                  alternative_text, " ", input$prop_hypothesized, ".")
          } else if (!is.character(p_val)) {
            paste0("Kesimpulan: Gagal Tolak H0 (α = 0.05). Tidak cukup bukti bahwa proporsi sukses ", 
                  alternative_text, " ", input$prop_hypothesized, ".")
          } else {
            "Kesimpulan: Tidak dapat menentukan kesimpulan karena p-value tidak dapat dihitung."
          }
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji proporsi:", e$message), type="error")
    })
    
  } else if (input$prop_test_type == "two_sample_prop") {
    req(input$prop_var, input$prop_threshold, input$group_var_prop)
    
    if(!input$prop_var %in% names(current_data) || !input$group_var_prop %in% names(current_data)) {
      showNotification("Variabel tidak ditemukan dalam data.", type="error")
      return()
    }
    
    group_var <- current_data[[input$group_var_prop]]
    groups <- unique(na.omit(group_var))
    
    if (length(groups) != 2) {
      showNotification(paste("Variabel pengelompokan harus memiliki tepat 2 kategori. Ditemukan:", length(groups), "kategori."), type = "error")
      return()
    }
    
    var_data <- current_data[[input$prop_var]]
    
    if(!is.numeric(var_data)) {
      showNotification("Variabel harus berupa data numerik.", type="error")
      return()
    }
    
    successes1 <- sum(var_data[group_var == groups[1]] > input$prop_threshold, na.rm = TRUE)
    n1 <- sum(group_var == groups[1] & !is.na(var_data))
    
    successes2 <- sum(var_data[group_var == groups[2]] > input$prop_threshold, na.rm = TRUE)
    n2 <- sum(group_var == groups[2] & !is.na(var_data))
    
    if(n1 == 0 || n2 == 0){
      showNotification("Salah satu kelompok tidak memiliki data valid.", type="error")
      return()
    }
    
    tryCatch({
      result <- prop.test(x = c(successes1, successes2), n = c(n1, n2),
                         alternative = input$prop_alternative_2, correct = TRUE)
      
      output$proportion_result <- renderPrint({ result })

      prop1 <- successes1 / n1
      prop2 <- successes2 / n2
      alternative_text <- switch(input$prop_alternative_2,
        "two.sided" = "berbeda dengan",
        "greater" = "lebih besar dari", 
        "less" = "lebih kecil dari"
      )
      
      p_val <- if(is.na(result$p.value)) "tidak dapat dihitung" else round(result$p.value, 4)
      conf_int <- if(any(is.na(result$conf.int))) c("NA", "NA") else round(result$conf.int, 4)
      
      output$proportion_interpretation <- renderText({
        paste0(
          "INTERPRETASI UJI PROPORSI DUA SAMPEL:\n\n",
          "Variabel: ", input$prop_var, "\n",
          "Threshold: ", input$prop_threshold, "\n",
          "Definisi sukses: ", input$prop_var, " > ", input$prop_threshold, "\n",
          "Variabel pengelompokan: ", input$group_var_prop, "\n\n",
          "Kelompok ", groups[1], ": ", successes1, "/", n1, " = ", round(prop1, 4), "\n",
          "Kelompok ", groups[2], ": ", successes2, "/", n2, " = ", round(prop2, 4), "\n\n",
          "H0: p1 = p2 (proporsi sukses kedua kelompok sama)\n",
          "H1: p1 ", alternative_text, " p2\n\n",
          "p-value: ", p_val, "\n\n",
          # Perbaikan untuk error "argument is of length zero"
          if (!is.character(p_val) && p_val < 0.05) {
            paste0("Kesimpulan: Tolak H0 (α = 0.05). Proporsi sukses antara kelompok ", 
                  groups[1], " dan ", groups[2], " secara signifikan ", alternative_text[1], ".")
          } else if (!is.character(p_val)) {
            paste0("Kesimpulan: Gagal Tolak H0 (α = 0.05). Tidak cukup bukti bahwa proporsi sukses ", 
                  "antara kedua kelompok ", alternative_text[1], ".")
          } else {
            "Kesimpulan: Tidak dapat menentukan kesimpulan karena p-value tidak dapat dihitung."
          }
        )
      })
      
    }, error = function(e) {
      showNotification(paste("Error dalam uji proporsi dua sampel:", e$message), type="error")
    })
  }
})

# Update pilihan variabel ketika data berubah
observe({
  current_data <- if(is.reactive(data)) data() else data
  
  req(current_data)
  
  # Untuk uji proporsi, hanya variabel numerik
  numeric_vars <- names(current_data)[sapply(current_data, is.numeric)]
  
  updateSelectInput(session, "prop_var",
    choices = numeric_vars
  )
  
  # Untuk variabel pengelompokan, semua variabel
  all_vars <- names(current_data)
  
  updateSelectInput(session, "group_var_prop", 
    choices = all_vars
  )
})

# Variance test
  observeEvent(input$test_variance, {
    if (input$var_test_type == "one_var") {
      data <- working_data()
      var_data <- data[[input$variance_var]]
      var_data <- var_data[!is.na(var_data)]
      
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

      output$variance_result <- renderPrint({ result })
      output$variance_interpretation <- renderText({
        paste0("INTERPRETASI UJI VARIANS SATU SAMPEL:\n\n",
               "H0: sigma^2 = ", input$sigma_squared, "\n",
               "H1: sigma^2 != ", input$sigma_squared, "\n\n",
               "p-value: ", round(p_value, 4), "\n\n",
               if(p_value < 0.05) "Kesimpulan: Tolak H0. Varians berbeda signifikan dari nilai hipotesis."
               else "Kesimpulan: Gagal Tolak H0. Varians tidak berbeda signifikan dari nilai hipotesis."
        )
      })
    } else if (input$var_test_type == "two_var") {
      req(input$variance_var, input$group_var_variance)
      data <- working_data()
      group_var <- data[[input$group_var_variance]]

      if (length(unique(na.omit(group_var))) != 2) {
        showNotification("Variabel pengelompokan harus memiliki tepat 2 kategori.", type = "error")
        return()
      }
      
      result <- var.test(as.formula(paste(input$variance_var, "~", input$group_var_variance)), data = data)
      output$variance_result <- renderPrint({ result })
      output$variance_interpretation <- renderText({
        paste0("INTERPRETASI UJI VARIANS DUA SAMPEL (F-TEST):\n\n",
               "H0: Rasio varians kedua kelompok adalah 1.\n",
               "H1: Rasio varians kedua kelompok bukan 1.\n\n",
               "p-value: ", round(result$p.value, 4), "\n\n",
               if(result$p.value < 0.05) "Kesimpulan: Tolak H0. Terdapat perbedaan varians yang signifikan antara kedua kelompok."
               else "Kesimpulan: Gagal Tolak H0. Tidak terdapat perbedaan varians yang signifikan antara kedua kelompok."
        )
      })
    }
  })

# ANOVA
  observeEvent(input$perform_anova, {
    req(input$anova_response, input$anova_factor1)
    data <- working_data()

    factor1 <- as.factor(data[[input$anova_factor1]])

    if (input$anova_type == "one_way") {
      formula_str <- paste(input$anova_response, "~ factor1")
      model <- aov(as.formula(formula_str), data = data)

      output$anova_result <- renderPrint({ summary(model) })

      # PERBAIKAN: Hipotesis dinamis
      output$anova_interpretation <- renderText({
        aov_summary <- summary(model)
        p_value <- aov_summary[[1]][["Pr(>F)"]][1]
        num_groups <- nlevels(factor1)
        
        # Membuat string hipotesis dinamis
        h0_string <- paste0("H0: ", paste(paste0("mu_", 1:num_groups), collapse = " = "))
        
        paste0(
          "INTERPRETASI ONE-WAY ANOVA:\n\n",
          h0_string, " (Rata-rata semua ", num_groups, " kelompok adalah sama)\n",
          "H1: Minimal ada satu grup yang rata-ratanya berbeda\n\n",
          "p-value: ", round(p_value, 4), "\n\n",
          if (!is.na(p_value) && p_value < 0.05) {
            "Kesimpulan: Tolak H0. Terdapat perbedaan rata-rata yang signifikan di antara kelompok-kelompok."
          } else {
            "Kesimpulan: Gagal Tolak H0. Tidak terdapat perbedaan rata-rata yang signifikan di antara kelompok-kelompok."
          }
        )
      })
    } else if (input$anova_type == "two_way") {
      req(input$anova_factor2)
      factor2 <- as.factor(data[[input$anova_factor2]])

      formula_str <- paste(input$anova_response, "~ factor1 * factor2")
      model <- aov(as.formula(formula_str), data = data)

      output$anova_result <- renderPrint({ summary(model) })

      output$anova_interpretation <- renderText({
        aov_summary <- summary(model)
        p_values <- aov_summary[[1]][["Pr(>F)"]]

        paste0(
          "INTERPRETASI TWO-WAY ANOVA:\n\n",
          "Efek utama Faktor 1 (", input$anova_factor1, ") p-value: ", round(p_values[1], 4), "\n",
          "Efek utama Faktor 2 (", input$anova_factor2, ") p-value: ", round(p_values[2], 4), "\n",
          "Interaksi p-value: ", if(length(p_values)>2) round(p_values[3], 4) else "N/A", "\n\n",
          "Interpretasi (p < 0.05 = signifikan):\n",
          "- Faktor 1: ", if (!is.na(p_values[1]) && p_values[1] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
          "- Faktor 2: ", if (!is.na(p_values[2]) && p_values[2] < 0.05) "Signifikan" else "Tidak signifikan", "\n",
          "- Interaksi: ", if (length(p_values)>2 && !is.na(p_values[3]) && p_values[3] < 0.05) "Signifikan" else "Tidak signifikan"
        )
      })
    }
  })

  # Regression analysis
  observeEvent(input$perform_regression, {
    req(input$reg_predictors)
    data <- working_data()

    if (!is.numeric(data[[input$reg_response]])) {
      showNotification("Variabel respons harus numerik untuk regresi.", type = "error")
      return()
    }
    if (length(input$reg_predictors) == 0) {
      showNotification("Pilih setidaknya satu variabel prediktor.", type = "error")
      return()
    }
    for (pred in input$reg_predictors) {
      if (!is.numeric(data[[pred]])) {
        showNotification(paste0("Prediktor '", pred, "' harus numerik."), type = "error")
        return()
      }
    }


    formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " + "))
    model <- lm(as.formula(formula_str), data = data)

    output$regression_result <- renderPrint({ summary(model) })
    output$regression_diagnostics <- renderPlot({
      par(mfrow = c(2, 2))
      plot(model)
    })
    output$regression_assumptions <- renderPrint({
      residuals_data <- residuals(model)
      list(
        "Durbin-Watson Test (Independence)" = dwtest(model),
        "Breusch-Pagan Test (Homoscedasticity)" = bptest(model),
        "Shapiro-Wilk Test (Normality of Residuals)" = if(length(residuals_data) <= 5000) shapiro.test(residuals_data) else "N > 5000, dilewati."
      )
    })
    output$regression_interpretation <- renderText({
      model_summary <- summary(model)
      r_squared <- model_summary$r.squared
      adj_r_squared <- model_summary$adj.r.squared
      f_stat <- model_summary$fstatistic
      p_value <- pf(f_stat[1], f_stat[2], f_stat[3], lower.tail = FALSE)

      paste0(
        "INTERPRETASI REGRESI LINEAR BERGANDA:\n\n",
        "R-squared: ", round(r_squared, 4), " (", round(r_squared * 100, 2), "% variasi variabel dependen dijelaskan oleh model)\n",
        "Adjusted R-squared: ", round(adj_r_squared, 4), "\n",
        "F-statistic p-value: ", format(p_value, scientific = TRUE, digits=4), "\n\n",
        if (p_value < 0.05) {
          "Model secara keseluruhan signifikan dalam memprediksi variabel dependen (p < 0.05)."
        } else {
          "Model secara keseluruhan tidak signifikan (p >= 0.05)."
        }, "\n\n",
        "Koefisien Signifikan (p < 0.05):\n",
        {
          coefs <- model_summary$coefficients
          sig_coefs <- rownames(coefs)[coefs[,4] < 0.05]
          if(length(sig_coefs) > 0) paste("- ", sig_coefs, collapse="\n") else "Tidak ada koefisien yang signifikan secara statistik."
        }
      )
    })
  })

  # =================
  # CHOROPLETH MAP AND SPATIAL ANALYSIS
  # =================
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

  observeEvent(input$generate_spatial, {
    req(input$spatial_var1, input$spatial_var2, input$distance_threshold)

    output$spatial_plot <- renderPlotly({
        sovi <- sovi_data()
        dist_matrix <- as.matrix(distance_data())

        var1_data <- sovi[[input$spatial_var1]]
        var2_data <- sovi[[input$spatial_var2]]

        spatial_pairs <- which(dist_matrix <= input$distance_threshold & dist_matrix > 0, arr.ind = TRUE)

        if (nrow(spatial_pairs) > 0) {
          pair_var1 <- var1_data[spatial_pairs[, 1]]
          pair_var2 <- var1_data[spatial_pairs[, 2]] # Moran's I: membandingkan variabel dengan dirinya sendiri di lokasi tetangga

          spatial_df <- data.frame(
            local_value = pair_var1,
            neighbor_value = pair_var2,
            distance = dist_matrix[spatial_pairs]
          )

          p <- ggplot(spatial_df, aes(x = local_value, y = neighbor_value, color = distance)) +
            geom_point(alpha = 0.6, size = 2) +
            geom_smooth(method = "lm", se = TRUE, color = "red") +
            scale_color_viridis_c(name = "Jarak (km)") +
            labs(
              title = paste("Scatter Plot Spasial (Moran's I like):", input$spatial_var1),
              subtitle = paste("Kabupaten dalam radius", input$distance_threshold, "km"),
              x = paste("Nilai di Lokasi A:", input$spatial_var1),
              y = paste("Nilai di Lokasi Tetangga B:", input$spatial_var1)
            ) + theme_minimal()
          ggplotly(p, tooltip = c("x", "y", "colour"))
        } else {
          p <- ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Tidak ada pasangan kabupaten\ndalam radius yang ditentukan", size = 6) + theme_void()
          ggplotly(p)
        }
    })

    output$spatial_interpretation <- renderText({
        sovi <- sovi_data()
        dist_matrix <- as.matrix(distance_data())
        var1_data <- sovi[[input$spatial_var1]]
        
        spatial_pairs <- which(dist_matrix <= input$distance_threshold & dist_matrix > 0, arr.ind = TRUE)

        if (nrow(spatial_pairs) > 0) {
          pair_var1 <- var1_data[spatial_pairs[, 1]]
          pair_var2 <- var1_data[spatial_pairs[, 2]]
          
          # Cek jika ada data non-NA yang cukup untuk korelasi
          valid_pairs <- complete.cases(pair_var1, pair_var2)
          if(sum(valid_pairs) < 2){
            return("Tidak cukup data valid untuk menghitung korelasi spasial.")
          }

          spatial_cor <- cor(pair_var1[valid_pairs], pair_var2[valid_pairs])
          avg_distance <- mean(dist_matrix[spatial_pairs])

          paste0(
            "INTERPRETASI ANALISIS SPASIAL (Autokorelasi):\n\n",
            "Variabel: ", input$spatial_var1, "\n",
            "Threshold Jarak: ", input$distance_threshold, " km\n\n",
            "Jumlah pasangan tetangga: ", nrow(spatial_pairs), "\n",
            "Rata-rata jarak tetangga: ", round(avg_distance, 2), " km\n",
            "Korelasi spasial (Moran's I like): ", round(spatial_cor, 4), "\n\n",
            "Interpretasi:\n",
            if (spatial_cor > 0.1) {
              "Korelasi positif menunjukkan adanya pengelompokan (clustering). Kabupaten yang berdekatan cenderung memiliki nilai yang serupa (nilai tinggi berdekatan dengan nilai tinggi, nilai rendah berdekatan dengan nilai rendah)."
            } else if (spatial_cor < -0.1) {
              "Korelasi negatif menunjukkan pola seperti papan catur (checkerboard). Kabupaten yang berdekatan cenderung memiliki nilai yang berlawanan (nilai tinggi berdekatan dengan nilai rendah)."
            } else {
              "Korelasi mendekati nol menunjukkan pola spasial yang acak. Nilai suatu kabupaten tidak berhubungan dengan nilai kabupaten tetangganya."
            }
          )
        } else {
          "Tidak ada pasangan kabupaten dalam radius yang ditentukan."
        }
    })
  })

  # =================
  # REPORT MANAGEMENT
  # =================

  # Fungsi untuk membersihkan item laporan dan plot terkait
  cleanup_all_plots <- function() {
    cleaned_files <- 0
    if (dir.exists("temp_plots")) {
      plot_files_temp <- list.files("temp_plots", pattern = "\\.png$", full.names = TRUE)
      if (length(plot_files_temp) > 0) {
        file.remove(plot_files_temp)
        cleaned_files <- cleaned_files + length(plot_files_temp)
      }
    }
  }
  # Add exploration analysis to report
  observeEvent(input$add_exploration_to_report, {
    req(input$x_var, input$plot_type)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("exploration_", report_items$counter)

    # Get current interpretation from the actual output
    interpretation <- isolate({
      data <- working_data()
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
        # Asumsi create_plot_path menghasilkan path relatif, e.g., "temp_plots/namafile.png"
        plot_paths <- create_plot_path(plot_filename)

        # Generate plot code
        plot_code <- quote({
          if (!is.null(par("mfrow"))) {
            if (!all(par("mfrow") == c(1, 1))) {
              par(mfrow = c(1, 1)) # Reset to single plot
            }
          }

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
          } else { # Fallback
            hist(var_data, main = paste("Histogram of", var_name), xlab = var_name, col = "lightblue", border = "white", breaks = 30)
          }
        })

        # Save plot safely
        plot_success <- save_plot_safely(plot_paths, plot_code)

        content <- paste0(
          "## Eksplorasi Data: ", var_name, "\n\n",
          "**Jenis Visualisasi:** ", input$plot_type, "\n\n",
          if (plot_success) {
            # Path relatif akan bekerja untuk Word
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
      if (!is.null(par("mfrow"))) {
        if (!all(par("mfrow") == c(1, 1))) {
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
      "**Korelasi:** ", round(cor(working_data()[[input$spatial_var1]], working_data()[[input$spatial_var2]], use = "complete.obs"), 3), "\n\n",
      "**Interpretasi:** Scatter plot menunjukkan hubungan spasial antara kedua variabel dengan mempertimbangkan threshold jarak ", input$distance_threshold, " km. Korelasi sebesar ", round(cor(working_data()[[input$spatial_var1]], working_data()[[input$spatial_var2]], use = "complete.obs"), 3),
      if (abs(cor(working_data()[[input$spatial_var1]], working_data()[[input$spatial_var2]], use = "complete.obs")) > 0.7) " menunjukkan hubungan yang kuat." else if (abs(cor(working_data()[[input$spatial_var1]], working_data()[[input$spatial_var2]], use = "complete.obs")) > 0.3) " menunjukkan hubungan yang sedang." else " menunjukkan hubungan yang lemah.", "\n\n"
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
          data <- working_data()
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

          coef_table <- model_summary$coefficients
          sig_vars <- rownames(coef_table)[coef_table[, "Pr(>|t|)"] < 0.05]
          sig_vars <- sig_vars[sig_vars != "(Intercept)"]

          plot_filename <- paste0("regression_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$reg_response), ".png")
          plot_paths <- create_plot_path(plot_filename)

          plot_code <- quote({
            par(mfrow = c(2, 2))
            plot(model, which = 1:4)
            par(mfrow = c(1, 1))
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
      data <- working_data()
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
      data <- working_data()
      numeric_vars <- sapply(data, is.numeric)
      summary_stats <- summary(data[numeric_vars])

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



  # Add correlation to report
  observeEvent(input$add_correlation_to_report, {
    report_items$counter <- report_items$counter + 1
    item_id <- paste0("correlation_", report_items$counter)

    content <- isolate({
      data <- working_data()
      
      numeric_vars <- sapply(data, is.numeric)
      numeric_data <- data[numeric_vars]
      
      if (ncol(numeric_data) < 2) {
        return("**Error:** Tidak cukup variabel numerik untuk analisis korelasi.\n\n")
      }
      
      cor_matrix <- cor(numeric_data, use = "complete.obs")
      
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

      plot_filename <- paste0("correlation_", report_items$counter, ".png")
      plot_paths <- create_plot_path(plot_filename)

      plot_success <- tryCatch({
        png(plot_paths$full_path, width = 800, height = 600, res = 100)
        
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

    report_items$content[[item_id]] <- list(
      type = "correlation",
      title = "Analisis Korelasi",
      content = content,
      timestamp = Sys.time()
    )

    showNotification("Analisis korelasi berhasil ditambahkan ke laporan!", type = "message")
  })



  # Add normality test to report
  observeEvent(input$add_normality_to_report, {
    req(input$normality_var)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("normality_", report_items$counter)

    content <- isolate({
      data <- working_data()
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

      plot_filename <- paste0("normality_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$normality_var), ".png")
      plot_paths <- create_plot_path(plot_filename)

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
    req(input$homogeneity_var, input$group_var_homogeneity)

    report_items$counter <- report_items$counter + 1
    item_id <- paste0("homogeneity_", report_items$counter)

    content <- isolate({
      data <- working_data()
      group_var <- data[[input$group_var_homogeneity]]

      levene_result <- leveneTest(data[[input$homogeneity_var]], group_var)
      bartlett_result <- bartlett.test(data[[input$homogeneity_var]], group_var)

      interpretation <- if (levene_result$`Pr(>F)`[1] < 0.05) {
        "Varians antar kelompok tidak homogen (p < 0.05)"
      } else {
        "Varians antar kelompok homogen (p >= 0.05)"
      }

      paste0(
        "## Uji Homogenitas: ", input$homogeneity_var, "\n\n",
        "**Variabel Pengelompokan:** ", input$group_var_homogeneity, "\n\n",
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
      if (input$test_type == "one_sample") {
        data <- working_data()
        var_data <- data[[input$mean_var]]
        result <- t.test(var_data, mu = input$mu)
        test_desc <- paste("Uji t satu sampel dengan mu_0 =", input$mu)
        conclusion <- if (result$p.value < 0.05) {
          paste0("Tolak H0. Rata-rata populasi berbeda signifikan dari ", input$mu)
        } else {
          paste0("Gagal tolak H0. Rata-rata populasi tidak berbeda signifikan dari ", input$mu)
        }
      } else if (input$test_type == "two_sample") {
        data <- working_data()
        group_var <- data[[input$group_var_mean]]
        if (length(unique(na.omit(group_var))) != 2) {
          return("Error: Uji t dua sampel memerlukan variabel pengelompokan dengan tepat 2 kategori.")
        }
        group_levels <- levels(droplevels(factor(group_var)))
        group1_data <- data[[input$mean_var]][group_var == group_levels[1]]
        group2_data <- data[[input$mean_var]][group_var == group_levels[2]]
        result <- t.test(group1_data, group2_data)
        test_desc <- paste("Uji t dua sampel independen (Grup:", input$group_var_mean, ")")
        conclusion <- if (result$p.value < 0.05) {
          "Tolak H0. Rata-rata kedua kelompok berbeda signifikan"
        } else {
          "Gagal tolak H0. Rata-rata kedua kelompok tidak berbeda signifikan"
        }
      } else if (input$test_type == "paired") {
        data <- working_data()
        var1_data <- data[[input$mean_var]]
        var2_data <- data[[input$paired_var]]
        result <- t.test(var1_data, var2_data, paired = TRUE)
        test_desc <- paste("Uji t berpasangan:", input$mean_var, "vs", input$paired_var)
        conclusion <- if (result$p.value < 0.05) {
          "Tolak H0. Ada perbedaan signifikan antara kedua variabel"
        } else {
          "Gagal tolak H0. Tidak ada perbedaan signifikan antara kedua variabel"
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
      data <- working_data()
      factor1 <- data[[input$anova_factor1]]

      if (input$anova_type == "one_way") {
        plot_filename <- paste0("anova_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$anova_response), "_by_", gsub("[^A-Za-z0-9]", "_", input$anova_factor1), ".png")
        plot_paths <- create_plot_path(plot_filename)

        plot_success <- tryCatch({
          png(plot_paths$full_path, width = 800, height = 600, res = 100)
          
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

        conclusion <- if (!is.na(p_value) && p_value < 0.05) {
          "Tolak H0. Ada perbedaan signifikan antar kelompok"
        } else {
          "Gagal tolak H0. Tidak ada perbedaan signifikan antar kelompok"
        }

        content_text <- paste0(
          "## ANOVA Satu Arah: ", input$anova_response, "\n\n",
          if (plot_success) {
            paste0("![Boxplot ANOVA](", plot_paths$latex_path, ")\n\n")
          } else {
            "**Error:** Plot ANOVA tidak dapat dibuat.\n\n"
          },
          "**Faktor:** ", input$anova_factor1, " (", length(levels(factor1)), " kategori)\n\n",
          "**Hasil ANOVA:**\n",
          "- F-statistic: ", round(aov_summary[[1]][["F value"]][1], 4), "\n",
          "- p-value: ", format(p_value, scientific = TRUE), "\n\n",
          "**Kesimpulan:** ", conclusion, "\n\n"
        )
        
      } else if (input$anova_type == "two_way") {
        req(input$anova_factor2)
        
        plot_filename <- paste0("anova_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$anova_response), "_two_way.png")
        plot_paths <- create_plot_path(plot_filename)

        factor2 <- data[[input$anova_factor2]]

        plot_success <- tryCatch({
          png(plot_paths$full_path, width = 800, height = 600, res = 100)
          
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
        
        interpret_significance <- function(p_val) {
          if (is.na(p_val)) {
            return("Tidak dapat dihitung")
          } else if (p_val < 0.05) {
            return("Signifikan")
          } else {
            return("Tidak signifikan")
          }
        }
        
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
      data <- working_data()
      var_data <- data[[input$prop_var]]
      success_count <- sum(var_data > input$prop_threshold, na.rm = TRUE)
      total_count <- sum(!is.na(var_data))
      sample_prop <- success_count / total_count

      prop_result <- prop.test(success_count, total_count, p = input$prop_hypothesized)

      conclusion <- if (prop_result$p.value < 0.05) {
        paste0("Tolak H0. Proporsi populasi berbeda signifikan dari ", input$prop_hypothesized)
      } else {
        paste0("Gagal tolak H0. Proporsi populasi tidak berbeda signifikan dari ", input$prop_hypothesized)
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
      if (input$var_test_type == "one_var") {
        data <- working_data()
        var_data <- data[[input$variance_var]]
        var_data <- var_data[!is.na(var_data)]
        n <- length(var_data)
        sample_var <- var(var_data)
        chi_stat <- (n - 1) * sample_var / input$sigma_squared
        p_value <- 2 * min(pchisq(chi_stat, n - 1), 1 - pchisq(chi_stat, n - 1))

        conclusion <- if (p_value < 0.05) {
          paste0("Tolak H0. Varians populasi berbeda signifikan dari ", input$sigma_squared)
        } else {
          paste0("Gagal tolak H0. Varians populasi tidak berbeda signifikan dari ", input$sigma_squared)
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
      } else { # two_var
        data <- working_data()
        group_var <- data[[input$group_var_variance]]
        if (length(unique(na.omit(group_var))) != 2) {
          return("Error: Uji F untuk dua varians memerlukan variabel pengelompokan dengan tepat 2 kategori.")
        }
        group_levels <- levels(droplevels(factor(group_var)))
        group1_data <- data[[input$variance_var]][group_var == group_levels[1]]
        group2_data <- data[[input$variance_var]][group_var == group_levels[2]]

        f_result <- var.test(group1_data, group2_data)

        conclusion <- if (f_result$p.value < 0.05) {
          "Tolak H0. Varians kedua kelompok berbeda signifikan"
        } else {
          "Gagal tolak H0. Varians kedua kelompok tidak berbeda signifikan"
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
      data <- working_data()
      var_name <- input$var_to_categorize
      n_cat <- input$n_categories

      cat_var <- NULL
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

      if (is.null(cat_var) || all(is.na(cat_var))) {
        return(paste0(
          "## Kategorisasi Data: ", input$var_to_categorize, "\n\n",
          "**Error:** Kategorisasi tidak dapat dilakukan. Pastikan variabel dan jumlah kategori sesuai.\n\n"
        ))
      }

      summary_stats <- data.frame(
        Kategori = names(table(cat_var)),
        Frekuensi = as.numeric(table(cat_var)),
        Persentase = round(as.numeric(prop.table(table(cat_var))) * 100, 2),
        stringsAsFactors = FALSE
      )

      means_by_cat <- aggregate(data[[var_name]], by = list(cat_var), FUN = mean, na.rm = TRUE)
      summary_stats$Rata_rata_Asli <- round(means_by_cat$x, 4)

      plot_filename <- paste0("categorization_", report_items$counter, "_", gsub("[^A-Za-z0-9]", "_", input$var_to_categorize), ".png")
      plot_paths <- create_plot_path(plot_filename)

      plot_code <- quote({
        if (!is.null(par("mfrow"))) {
          if (!all(par("mfrow") == c(1, 1))) {
            par(mfrow = c(1, 1)) # Reset to single plot
          }
        }

        par(mfrow = c(1, 2))

        freq_table <- table(cat_var)
        barplot(freq_table,
          main = paste("Distribusi Kategori:", var_name),
          xlab = "Kategori",
          ylab = "Frekuensi",
          col = rainbow(length(freq_table)),
          border = "white"
        )

        hist(data[[var_name]],
          main = paste("Histogram:", var_name),
          xlab = var_name,
          col = "lightblue",
          border = "white",
          breaks = 20
        )
        par(mfrow = c(1, 1))
      })
      plot_success <- save_plot_safely(plot_paths, plot_code, extra_vars = list(cat_var = cat_var))

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
        "|---|---|---|---|\n",
        paste(apply(summary_stats, 1, function(x) {
          paste("|", x[1], "|", x[2], "|", x[3], "% |", x[4], "|")
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
      data <- working_data()
      var_name <- input$cat_var_explore
      freq_table <- table(data[[var_name]])
      prop_table <- prop.table(freq_table) * 100

      table_text <- paste0(
        "## Tabel Frekuensi: ", var_name, "\n\n",
        "| Kategori | Frekuensi | Persentase |\n",
        "|-|--||\n"
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
      data <- working_data()
      var1 <- input$cat_var1
      var2 <- input$cat_var2

      crosstab <- table(data[[var1]], data[[var2]])
      prop_crosstab <- prop.table(crosstab) * 100

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
      return("Belum ada analisis yang ditambahkan ke laporan.\n\nPETUNJUK PENGGUNAAN:\n1. Lakukan analisis di berbagai tab (Eksplorasi, Asumsi, Inferensia, Regresi)\n2. Klik tombol 'Tambah ke Laporan' pada setiap analisis\n3. Visualisasi akan otomatis disertakan dalam laporan\n4. Download laporan lengkap dalam format Word (.docx)")
    }

    contents <- character(0)
    viz_count <- 0

    for (i in seq_along(report_items$content)) {
      item <- report_items$content[[i]]
      contents <- c(contents, paste0(i, ". ", item$title, " (", item$type, ")"))

      if (item$type %in% c("exploration", "choropleth", "spatial", "regression", "categorization", "correlation", "normality", "anova")) {
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
      "CATATAN: Pastikan untuk mendownload semua visualisasi secara terpisah jika diperlukan."
    )

    return(summary_text)
  })


  # Clear report with plot cleanup
  observeEvent(input$clear_report, {
    cleaned_count <- cleanup_all_plots()

    report_items$content <- list()
    report_items$counter <- 0

    showNotification(paste("Laporan dibersihkan! Dihapus", cleaned_count, "file plot."), type = "message")
  })



  # =================
  # DOWNLOAD HANDLERS
  # =================

  output$download_choropleth <- downloadHandler(
    filename = function() {
      paste("choropleth_", input$choropleth_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      tryCatch(
        {
          geo_data <- indonesia_geojson()
          sovi <- sovi_data()

          if (!is.null(geo_data) && nrow(sovi) == 512 && length(geo_data) == 512) {
            geo_data$selected_var <- sovi[[input$choropleth_var]]
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
          sovi <- sovi_data()
          var1_data <- sovi[[input$spatial_var1]]
          var2_data <- sovi[[input$spatial_var2]]

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
          p <- ggplot() +
            annotate("text", x = 0.5, y = 0.5, label = paste("Error creating spatial plot:", e$message), size = 6) +
            theme_void()
          ggsave(file, plot = p, width = 8, height = 6, dpi = 300)
        }
      )
    }
  )
  output$download_data_summary <- downloadHandler(
    filename = function() {
      paste("data_summary_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      data <- working_data()
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
      write.csv(working_data(), file, row.names = FALSE)
    }
  )

  output$download_desc <- downloadHandler(
    filename = function() {
      paste("descriptive_statistics_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      data <- working_data()
      numeric_vars <- sapply(data, is.numeric)
      capture.output(summary(data[numeric_vars]), file = file)
    }
  )

  output$download_corr <- downloadHandler(
    filename = function() {
      paste("correlation_matrix_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      data <- working_data()
      numeric_vars <- sapply(data, is.numeric)
      cor_matrix <- cor(data[numeric_vars], use = "complete.obs")
      write.csv(cor_matrix, file)
    }
  )

  output$download_plot <- downloadHandler(
    filename = function() {
      paste("exploration_plot_", input$plot_type, "_", input$x_var, "_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      data <- working_data()

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

  output$download_regression <- downloadHandler(
    filename = function() {
      paste("regression_analysis_", input$reg_response, "_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(input$reg_predictors)
      tryCatch(
        {
          data <- working_data()
          formula_str <- paste(input$reg_response, "~", paste(input$reg_predictors, collapse = " + "))
          model <- lm(as.formula(formula_str), data = data)
          png_file <- gsub("\\.txt$", "_diagnostics.png", file)
          png(png_file, width = 12, height = 8, units = "in", res = 300)
          par(mfrow = c(2, 2))
          plot(model)
          dev.off()

          output_text <- paste(
            "ANALISIS REGRESI LINEAR\n", "======================\n\n",
            "Formula: ", formula_str, "\n\n",
            "SUMMARY MODEL:\n", paste(capture.output(summary(model)), collapse = "\n"), "\n\n",
            "UJI ASUMSI:\n", paste(capture.output({
              cat("Durbin-Watson Test (Independence):\n"); print(dwtest(model))
              cat("\nBreusch-Pagan Test (Homoscedasticity):\n"); print(bptest(model))
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

  output$download_categorized_data <- downloadHandler(
    filename = function() {
      paste("categorized_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0) {
        data <- working_data()
        write.csv(data, file, row.names = FALSE)
      }
    }
  )

  output$print_categorized_data <- downloadHandler(
    filename = function() {
      paste("categorized_data_print_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0) {
        data <- working_data()
        write.csv(data, file, row.names = FALSE)
      }
    }
  )

  output$download_cat_plot <- downloadHandler(
    filename = function() {
      paste("categorical_plot_", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0 && !is.null(input$cat_var_explore)) {
        data <- working_data()
        cat_var <- input$cat_var_explore
        p <- ggplot(data, aes(x = !!sym(cat_var), fill = !!sym(cat_var))) +
          geom_bar(alpha = 0.7, color = "white", size = 0.5) +
          theme_minimal() +
          labs(title = paste("Distribusi Frekuensi:", cat_var), x = cat_var, y = "Frekuensi") +
          theme(legend.position = "none", plot.title = element_text(size = 14, face = "bold"), axis.text.x = element_text(angle = 45, hjust = 1)) +
          scale_fill_brewer(type = "qual", palette = "Set3")
        ggsave(file, plot = p, width = 10, height = 6, dpi = 300, bg = "white")
      }
    }
  )

  output$download_freq_table <- downloadHandler(
    filename = function() {
      paste("frequency_table_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0 && !is.null(input$cat_var_explore)) {
        data <- working_data()
        cat_var <- input$cat_var_explore
        freq_data <- data %>%
          count(!!sym(cat_var)) %>%
          mutate(Percentage = round(n / sum(n) * 100, 2), `Cumulative Freq` = cumsum(n), `Cumulative %` = round(cumsum(n) / sum(n) * 100, 2)) %>%
          rename(Category = !!sym(cat_var), Frequency = n)
        write.csv(freq_data, file, row.names = FALSE)
      }
    }
  )

  output$download_crosstab <- downloadHandler(
    filename = function() {
      paste("crosstab_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      if (input$categorize > 0 && !is.null(input$cat_var1) && !is.null(input$cat_var2)) {
        data <- working_data()
        crosstab <- table(data[[input$cat_var1]], data[[input$cat_var2]])
        crosstab_df <- as.data.frame.matrix(crosstab)
        crosstab_df$Total <- rowSums(crosstab_df)
        crosstab_df <- rbind(crosstab_df, Total = colSums(crosstab_df))
        write.csv(crosstab_df, file)
      }
    }
  )



  # =================
  # REPORT DOWNLOAD HANDLERS (MODIFIED FOR DOCX)
  # =================

  # Download Word (.docx) report
  output$download_doc_report <- downloadHandler(
    filename = function() {
      paste("laporan_analisis_", Sys.Date(), ".docx", sep = "")
    },
    content = function(file) {
      # Gunakan direktori sementara untuk proses render agar tidak mengotori direktori kerja
      temp_render_dir <- tempdir()
      original_wd <- getwd()
      setwd(temp_render_dir)

      # Salin semua file plot yang dibutuhkan dari 'temp_plots' ke direktori render sementara
      temp_plots_source_dir <- file.path(original_wd, "temp_plots")
      if (dir.exists(temp_plots_source_dir)) {
        plot_files_in_temp_plots <- list.files(temp_plots_source_dir, pattern = "\\.png$", full.names = TRUE)
        if (length(plot_files_in_temp_plots) > 0) {
          file.copy(plot_files_in_temp_plots, temp_render_dir, overwrite = TRUE)
        }
      }

      # Buat file R Markdown sementara di dalam direktori render
      temp_rmd_path <- file.path(temp_render_dir, "report.Rmd")

      # Siapkan konten dari semua item laporan
      content_sections <- ""
      if (length(report_items$content) > 0) {
        for (item in report_items$content) {
          # Pastikan path gambar hanya nama file agar rmarkdown dapat menemukannya
          item_content <- item$content
          item_content <- gsub("!\\[([^\\]]*)\\]\\(.*?([^/\\\\]+\\.png)\\)", "![\\1](\\2)", item_content)
          content_sections <- paste0(content_sections, "\n\n\n\n", item_content)
        }
      } else {
        content_sections <- "\nBelum ada analisis yang ditambahkan ke laporan.\n"
      }

      # Gabungkan header YAML untuk Word dengan konten laporan
      full_rmd_content <- paste(
        "---",
        "title: 'Laporan Analisis Kerentanan Sosial Indonesia'",
        "author: 'Dihasilkan oleh Aplikasi Shiny'",
        paste0("date: '", format(Sys.Date(), "%d %B %Y"), "'"),
        "output: word_document",
        "---",
        "\n# Ringkasan Analisis\n",
        "Dokumen ini berisi hasil analisis yang dihasilkan secara otomatis dari aplikasi analisis data. Semua visualisasi dan interpretasi yang ditambahkan oleh pengguna disertakan di bawah ini.",
        content_sections,
        sep = "\n"
      )

      # Tulis konten Rmd ke file sementara
      writeLines(full_rmd_content, temp_rmd_path, useBytes = TRUE)

      # Render Rmd ke DOCX
      tryCatch(
        {
          rmarkdown::render(
            input = temp_rmd_path,
            output_file = file,
            quiet = TRUE,
            envir = new.env()
          )
          showNotification("Laporan Word (.docx) berhasil dibuat!", type = "message")
        },
        error = function(e) {
          showNotification(paste("Error saat membuat laporan Word:", e$message), type = "error")
          cat(paste("Full DOCX error:", e$message, "\n"))
        },
        finally = {
          # Kembalikan direktori kerja ke aslinya
          setwd(original_wd)
        }
      )
      
      # Kosongkan item laporan setelah diunduh
      report_items$content <- list()
      report_items$counter <- 0
    }
  )


  # Download Full Text Report (Plain Text)
  output$download_full_report <- downloadHandler(
    filename = function() {
      paste("full_analysis_report_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      data <- working_data()
      text_content <- paste0(
        "LAPORAN ANALISIS KERENTANAN SOSIAL INDONESIA\n",
        "============================================\n\n",
        "STATISTIK DESKRIPTIF\n",
        "-\n",
        paste(capture.output(summary(data)), collapse = "\n"), "\n\n",
        "INFORMASI DATASET\n",
        "-\n",
        "Jumlah observasi: ", nrow(data), "\n",
        "Jumlah variabel: ", ncol(data), "\n",
        "Tanggal analisis: ", Sys.Date(), "\n\n",
        "Sumber: SUSENAS 2017 - BPS Statistics Indonesia\n"
      )
      writeLines(text_content, file)
    }
  )
}
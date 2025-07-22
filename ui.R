library(shiny)
library(shinydashboard)
library(shinythemes)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(corrplot)
library(VIM)
library(gridExtra)
library(tseries)
library(leaflet)
library(sf)
library(geojsonio)

ui <- dashboardPage(
  skin= "black",
  dashboardHeader(title = "SIVASIDA", titleWidth=300),
  dashboardSidebar( width=300,
    sidebarMenu(
      menuItem("Beranda", tabName = "beranda"),
      menuItem("Upload Data", tabName = "upload_data"),
      menuItem("Manajemen Data", tabName = "manajemen",
               menuSubItem("Data Asli", tabName = "data_asli"),
               menuSubItem("Proses Kategorisasi", tabName = "proses_kategorisasi"),
               menuSubItem("Eksplorasi Kategorik", tabName = "eksplorasi_kategorik")
      ),
      menuItem("Eksplorasi Data", tabName = "eksplorasi",
               menuSubItem("Eksplorasi Umum", tabName = "eksplorasi"),
               menuSubItem("Peta Choropleth", tabName = "peta_choropleth"),
               menuSubItem("Analisis Spasial", tabName = "analisis_spasial")
      ),
      menuItem("Uji Asumsi Data", tabName = "asumsi"),
      menuItem("Statistik Inferensia", tabName = "inferensia",
               menuSubItem("Uji Rata-rata", tabName = "uji_rata"),
               menuSubItem("Uji Proporsi & Variance", tabName = "uji_proporsi"),
               menuSubItem("ANOVA", tabName = "anova")
      ),
      menuItem("Regresi Linear", tabName = "regresi"),
      menuItem("Laporan Gabungan", tabName = "download")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel="stylesheet", type="text/css", href="styles.css")
    ),
    tabItems(
      tabItem(tabName = "beranda",
              fluidRow(
                box(
                  title = "Profil Dashboard", status = "primary", solidHeader = TRUE, width = 12,
                  h3("Simple Vulnerability Analysis Shiny Dashboard Indonesia"),
                  br(),
                  h4("Deskripsi Data:"),
                  p("Dataset ini berisi informasi tentang kerentanan sosial di Indonesia berdasarkan Survei Sosial Ekonomi Nasional (SUSENAS) 2017 oleh BPS-Statistics Indonesia."),
                  p("Dashboard ini dibuat untuk memenuhi penilaian di Ujian Akhir Semester pada mata kuliah Komputasi Statistik"),
                  br(),
                  p("Oleh: "),
                  tags$table(
                    style= "width: 50%; border-collapse: collapse; border: 1px solid black",
                    tags$tr(
                      style="border: 1px solid black",
                      tags$th("Nama"),
                      tags$th("NIM"),
                      tags$th("Kelas"),
                      tags$th("No.")
                    ),
                    tags$tr(
                      style="border: 1px solid black",
                      tags$td("Narangga Khoirul Utama"),
                      tags$td("222313288"),
                      tags$td("2KS1"),
                      tags$td("30")
                    )
                  ),
                  br(),
                  p("Metadata asli dapat diakses melalui", a("https://www.sciencedirect.com/science/article/pii/S2352340921010180")),
                  br(),
                  h4("Informasi Dataset:"),
                  p("Dashboard ini mendukung upload data kustom atau menggunakan data default SOVI Indonesia."),
                  h5("Data Default:"),
                  p("URL Data SOVI:", a("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/sovi_data.csv")),
                  p("URL Matriks Penimbang Jarak:", a("https://raw.githubusercontent.com/bmlmcmc/naspaclust/main/data/distance.csv")),
                  br(),
                  h4("Variabel Default SOVI yang Tersedia:"),
                  tags$ul(
                    tags$li("DISTRICTCODE: Kode wilayah/kabupaten"),
                    tags$li("CHILDREN: Persentase populasi di bawah 5 tahun"),
                    tags$li("FEMALE: Persentase populasi perempuan"),
                    tags$li("ELDERLY: Persentase populasi 65 tahun ke atas"),
                    tags$li("FHEAD: Persentase rumah tangga dengan kepala keluarga perempuan"),
                    tags$li("FAMILYSIZE: Rata-rata jumlah anggota rumah tangga"),
                    tags$li("NOELECTRIC: Persentase rumah tangga tanpa listrik"),
                    tags$li("LOWEDU: Persentase populasi 15+ dengan pendidikan rendah"),
                    tags$li("GROWTH: Persentase perubahan populasi"),
                    tags$li("POVERTY: Persentase penduduk miskin"),
                    tags$li("ILLITERATE: Persentase populasi yang buta huruf"),
                    tags$li("NOTRAINING: Persentase rumah tangga tanpa pelatihan bencana"),
                    tags$li("DPRONE: Persentase rumah tangga di area rawan bencana"),
                    tags$li("RENTED: Persentase rumah tangga yang menyewa rumah"),
                    tags$li("NOSEWER: Persentase rumah tangga tanpa sistem drainase"),
                    tags$li("TAPWATER: Persentase rumah tangga dengan air keran"),
                    tags$li("POPULATION: Jumlah populasi")
                  ),
                  br(),
                  h4("Upload Data Kustom:"),
                  p("Anda dapat mengupload data CSV sendiri melalui tab 'Upload Data'. Data yang diupload akan menggantikan pilihan variabel di seluruh dashboard secara otomatis."),
                  br(),
                  h4("Informasi Data Default:"),
                  h5("Dataset utama (sovi_data.csv):"),
                  p("Memuat data kerentanan sosial diperoleh dari Survei Sosial Ekonomi Nasional (SUSENAS) tahun 2017 yang dilakukan oleh BPS-Statistik Indonesia"),
                  h5("Data matriks penimbang jarak (distance.csv):"),
                  p("Data dikompilasi dari SUSENAS 2017 dengan menggunakan bobot untuk estimasi berdasarkan multistage sampling. Matrix jarak dikonstruksi dari peta geografis Indonesia tahun 2013."),
                  br(),
                  p("Berdasarkan makalah/ artikel Revisiting Social VUlnerability Analysis in Indonesia yang dapat diakses pada url \"metadata asli \"")
                )
              )
      ),
      
      # Upload Data Tab
      tabItem(tabName = "upload_data",
              fluidRow(
                box(
                  title = "Upload Dataset", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Upload Data Utama (Kerentanan Sosial)"),
                  p("Upload file CSV dengan data kerentanan sosial. File harus memiliki header kolom."),
                  fileInput("file_main", "Pilih File CSV:",
                           accept = c(".csv")),
                  checkboxInput("header_main", "Header", TRUE),
                  checkboxInput("stringsAsFactors_main", "Strings as factors", FALSE),
                  radioButtons("sep_main", "Separator",
                              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                              selected = ","),
                  radioButtons("quote_main", "Quote",
                              choices = c(None = "", "Double Quote" = '"', "Single Quote" = "'"),
                              selected = '"'),
                  br(),
                  h4("Upload Data Jarak (Opsional)"),
                  p("Upload file CSV dengan matriks jarak antar wilayah untuk analisis spasial."),
                  fileInput("file_distance", "Pilih File CSV Jarak:",
                           accept = c(".csv")),
                  checkboxInput("header_distance", "Header", TRUE),
                  radioButtons("sep_distance", "Separator",
                              choices = c(Comma = ",", Semicolon = ";", Tab = "\t"),
                              selected = ","),
                  br(),
                  actionButton("use_default_data", "Gunakan Data Default", class = "btn-info"),
                  p("Klik tombol di atas untuk menggunakan data SUSENAS 2017 yang sudah tersedia.")
                )
              ),
              fluidRow(
                box(
                  title = "Preview Data yang Diupload", status = "info", solidHeader = TRUE, width = 12,
                  h5("Data Utama:"),
                  DT::dataTableOutput("uploaded_data_preview"),
                  br(),
                  h5("Data Jarak:"),
                  DT::dataTableOutput("uploaded_distance_preview")
                )
              )
      ),
      
      # Data Asli Tab
      tabItem(tabName = "data_asli",
              fluidRow(
                box(
                  title = "Data Asli - Informasi Dataset", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Dataset Kerentanan Sosial Indonesia"),
                  p("Dataset ini berisi informasi kerentanan sosial 512 kabupaten/kota di Indonesia berdasarkan SUSENAS 2017."),
                  br(),
                  h5("Ringkasan Data:"),
                  verbatimTextOutput("data_summary"),
                  br(),
                  downloadButton("download_data_summary", "Download Ringkasan Data", class = "btn-success"),
                  br(),
                  actionButton("add_data_summary_to_report", "Tambah Ringkasan ke Laporan", class = "btn-info")
                )
              ),
              fluidRow(
                box(
                  title = "Data Mentah", status = "info", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("raw_data_table"),
                  br(),
                  fluidRow(
                    column(6,
                           downloadButton("download_raw_data", "Download Data Mentah", class = "btn-success")
                    ),
                    column(6,
                           downloadButton("print_raw_data", "Cetak Data Mentah (CSV)", class = "btn-info")
                    )
                  )
                )
              )
      ),
      
      # Proses Kategorisasi Tab
      tabItem(tabName = "proses_kategorisasi",
              fluidRow(
                box(
                  title = "Kategorisasi Data Kontinu", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           selectInput("var_to_categorize", "Pilih Variabel:",
                                       choices = NULL),
                           numericInput("n_categories", "Jumlah Kategori:", value = 3, min = 2, max = 10),
                           radioButtons("method", "Metode Kategorisasi:",
                                        choices = list("Quantile" = "quantile", 
                                                       "Equal Width" = "equal_width",
                                                       "K-means" = "kmeans")),
                           
                           # Dynamic category names input (extended to 10)
                           conditionalPanel(
                             condition = "input.n_categories >= 2",
                             h5("Nama Kategori:"),
                             textInput("cat_name_1", "Kategori 1:", value = "Sangat Rendah"),
                             textInput("cat_name_2", "Kategori 2:", value = "Rendah")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 3",
                             textInput("cat_name_3", "Kategori 3:", value = "Sedang")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 4",
                             textInput("cat_name_4", "Kategori 4:", value = "Tinggi")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 5",
                             textInput("cat_name_5", "Kategori 5:", value = "Sangat Tinggi")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 6",
                             textInput("cat_name_6", "Kategori 6:", value = "Ekstrim")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 7",
                             textInput("cat_name_7", "Kategori 7:", value = "Kategori_7")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 8",
                             textInput("cat_name_8", "Kategori 8:", value = "Kategori_8")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 9",
                             textInput("cat_name_9", "Kategori 9:", value = "Kategori_9")
                           ),
                           conditionalPanel(
                             condition = "input.n_categories >= 10",
                             textInput("cat_name_10", "Kategori 10:", value = "Kategori_10")
                           ),
                           br(),
                           actionButton("categorize", "Kategorikan Data", class = "btn-primary"),
                           actionButton("add_categorization_to_report", "Tambah ke Laporan", class = "btn-info")
                    ),
                    column(6,
                           h4("Preview Data Terkategorisasi:"),
                           DT::dataTableOutput("categorized_preview")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Kategorisasi", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("categorization_interpretation")
                )
              ),
              fluidRow(
                box(
                  title = "Data Terkategorisasi Lengkap", status = "success", solidHeader = TRUE, width = 12,
                  DT::dataTableOutput("categorized_full_data"),
                  br(),
                  fluidRow(
                    column(6,
                           downloadButton("download_categorized_data", "Download Data Terkategorisasi", class = "btn-success")
                    ),
                    column(6,
                           downloadButton("print_categorized_data", "Cetak Data Terkategorisasi (CSV)", class = "btn-info")
                    )
                  )
                )
              )
      ),
      
      # Eksplorasi Kategorik Tab
      tabItem(tabName = "eksplorasi_kategorik",
              fluidRow(
                box(
                  title = "Kontrol Eksplorasi Kategorik", status = "info", solidHeader = TRUE, width = 12,
                  conditionalPanel(
                    condition = "input.categorize > 0",
                    fluidRow(
                      column(6,
                             h5("Pilihan Variabel:"),
                             selectInput("cat_var_explore", "Pilih Variabel Kategorik:",
                                         choices = NULL), # Will be updated dynamically
                             br(),
                             actionButton("add_categorization_to_report", "Tambah ke Laporan", class = "btn-info"),
                             br(), br(),
                             downloadButton("download_cat_plot", "Download Plot Kategorik", class = "btn-success")
                      ),
                      column(6,
                             plotlyOutput("categorical_plot", height = "400px")
                      )
                    )
                  ),
                  conditionalPanel(
                    condition = "input.categorize == 0",
                    div(style = "text-align: center; padding: 50px;",
                        h5("Silakan lakukan kategorisasi data terlebih dahulu"),
                        p("Pergi ke 'Proses Kategorisasi' dan buat kategori data terlebih dahulu"))
                  )
                )
              ),
              fluidRow(
                # Tabel Frekuensi dan Tabulasi Silang
                column(6,
                       box(
                         title = "Tabel Frekuensi", status = "info", solidHeader = TRUE, width = 12,
                         conditionalPanel(
                           condition = "input.categorize > 0",
                           DT::dataTableOutput("frequency_table"),
                           br(),
                           actionButton("add_freq_table_to_report", "Tambah ke Laporan", class = "btn-info"),
                           downloadButton("download_freq_table", "Download Tabel Frekuensi", class = "btn-success")
                         ),
                         conditionalPanel(
                           condition = "input.categorize == 0",
                           div(style = "text-align: center; padding: 30px;",
                               h5("Tidak ada data kategorik"),
                               p("Silakan kategorisasi data terlebih dahulu"))
                         )
                       )
                ),
                column(6,
                       box(
                         title = "Tabulasi Silang", status = "info", solidHeader = TRUE, width = 12,
                         conditionalPanel(
                           condition = "input.categorize > 0",
                           selectInput("cat_var1", "Variabel Kategorik 1:",
                                       choices = NULL), # Will be updated dynamically
                           selectInput("cat_var2", "Variabel Kategorik 2:",
                                       choices = NULL), # Will be updated dynamically
                           actionButton("create_crosstab", "Buat Tabulasi Silang", class = "btn-primary"),
                           br(), br(),
                           DT::dataTableOutput("crosstab_table"),
                           br(),
                           actionButton("add_crosstab_to_report", "Tambah ke Laporan", class = "btn-info"),
                           downloadButton("download_crosstab", "Download Tabulasi Silang", class = "btn-success")
                         ),
                         conditionalPanel(
                           condition = "input.categorize == 0",
                           div(style = "text-align: center; padding: 30px;",
                               h5("Tidak ada data kategorik"),
                               p("Silakan kategorisasi data terlebih dahulu"))
                         )
                       )
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Eksplorasi Kategorik", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("categorical_interpretation")
                )
              )
      ),
      
      # Eksplorasi Data Umum Tab
      tabItem(tabName = "eksplorasi",
              fluidRow(
                box(
                  title = "Statistik Deskriptif", status = "primary", solidHeader = TRUE, width = 6,
                  verbatimTextOutput("descriptive_stats"),
                  br(),
                  downloadButton("download_desc", "Download Statistik Deskriptif", class = "btn-success"),
                  br(),
                  actionButton("add_descriptive_to_report", "Tambah ke Laporan", class = "btn-info")
                ),
                box(
                  title = "Matriks Korelasi", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("correlation_plot"),
                  br(),
                  downloadButton("download_corr", "Download Korelasi", class = "btn-success"),
                  br(),
                  actionButton("add_correlation_to_report", "Tambah ke Laporan", class = "btn-info")
                )
              ),
              fluidRow(
                box(
                  title = "Visualisasi Data", status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("plot_type", "Jenis Plot:",
                                       choices = c("Histogram", "Boxplot", "Scatter Plot", "Density Plot")),
                           selectInput("x_var", "Variabel X:",
                                       choices = NULL),
                           conditionalPanel(
                             condition = "input.plot_type == 'Scatter Plot'",
                             selectInput("y_var", "Variabel Y:",
                                         choices = NULL)
                           )
                    ),
                    column(8,
                           plotlyOutput("data_plot", height = "400px"),
                           br(),
                           downloadButton("download_plot", "Download Plot", class = "btn-success"),
                           br(),
                           actionButton("add_exploration_to_report", "Tambah ke Laporan", class = "btn-info")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Eksplorasi Data", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("exploration_interpretation")
                )
              )
      ),
      
      # Peta Choropleth Tab
      tabItem(tabName = "peta_choropleth",
              fluidRow(
                box(
                  title = "Peta Choropleth Indonesia", status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3,
                           selectInput("choropleth_var", "Pilih Variabel untuk Peta:",
                                       choices = NULL),
                           br(),
                           actionButton("generate_choropleth", "Generate Peta", class = "btn-primary"),
                           br(), br(),
                           p("Saran:"), br(),
                           p("Untuk menyimpan peta, anda menggunakan tangkap layar")

                    ),
                    column(9,
                           leafletOutput("choropleth_map", height = "500px")
                    )
                  )
                )
              )
      ),
      
      # Analisis Spasial Tab
      tabItem(tabName = "analisis_spasial",
              fluidRow(
                box(
                  title = "Analisis Spasial", status = "info", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(6,
                           selectInput("spatial_var1", "Variabel Utama:",
                                       choices = NULL),
                           selectInput("spatial_var2", "Variabel Pembanding:",
                                       choices = NULL),
                           numericInput("distance_threshold", "Threshold Jarak (km):", 
                                        value = 100, min = 10, max = 1000, step = 10),
                           actionButton("generate_spatial", "Analisis Spasial", class = "btn-primary"),
                           br(), br(),
                           downloadButton("download_spatial", "Download Hasil", class = "btn-success"),
                           br(),
                           actionButton("add_spatial_to_report", "Tambah ke Laporan", class = "btn-info")
                    ),
                    column(6,
                           plotlyOutput("spatial_plot", height = "400px")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Analisis Spasial", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("spatial_interpretation")
                )
              )
      ),
      
      # Uji Asumsi Data Tab
      tabItem(tabName = "asumsi",
              fluidRow(
                box(
                  title = "Uji Normalitas", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("normality_var", "Pilih Variabel:",
                              choices = NULL),
                  actionButton("test_normality", "Uji Normalitas", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("normality_result"),
                  plotOutput("normality_plot"),
                  br(),
                  actionButton("add_normality_to_report", "Tambah ke Laporan", class = "btn-info")
                ),
                box(
                  title = "Uji Homogenitas", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("homogeneity_var", "Pilih Variabel:",
                              choices = NULL),
                  selectInput("group_var", "Pilih Variabel Pengelompokan:",
                              choices = NULL),
                  actionButton("test_homogeneity", "Uji Homogenitas", class = "btn-primary"),
                  br(), br(),
                  verbatimTextOutput("homogeneity_result"),
                  br(),
                  actionButton("add_homogeneity_to_report", "Tambah ke Laporan", class = "btn-info")
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Uji Asumsi", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("assumption_interpretation")
                )
              )
      ),
      
      # Uji Rata-rata Tab
      tabItem(tabName = "uji_rata",
              fluidRow(
                box(
                  title = "Uji Rata-rata", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("test_type", "Jenis Uji:",
                                       choices = c("One Sample t-test" = "one_sample",
                                                   "Two Sample t-test" = "two_sample",
                                                   "Paired t-test" = "paired")),
                           selectInput("mean_var", "Variabel:",
                                       choices = NULL),
                           conditionalPanel(
                             condition = "input.test_type == 'one_sample'",
                             numericInput("mu", "Nilai Hipotesis (μ):", value = 0)
                           ),
                           conditionalPanel(
                             condition = "input.test_type == 'two_sample'",
                             selectInput("group_var_mean", "Variabel Pengelompokan:",
                                         choices = NULL)
                           ),
                           conditionalPanel(
                             condition = "input.test_type == 'paired'",
                             selectInput("paired_var", "Variabel Kedua:",
                                         choices = NULL)
                           ),
                           actionButton("test_mean", "Uji Rata-rata", class = "btn-primary")
                    ),
                    column(8,
                           verbatimTextOutput("mean_test_result"),
                           br(),
                           actionButton("add_mean_test_to_report", "Tambah ke Laporan", class = "btn-info")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Uji Rata-rata", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("mean_interpretation")
                )
              )
      ),
      
      # Uji Proporsi & Variance Tab
      tabItem(tabName = "uji_proporsi",
              fluidRow(
                box(
                  title = "Uji Proporsi", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("prop_var", "Variabel:",
                              choices = NULL),
                  numericInput("prop_threshold", "Threshold:", value = 50),
                  numericInput("prop_hypothesized", "Proporsi Hipotesis:", value = 0.5, min = 0, max = 1, step = 0.01),
                  actionButton("test_proportion", "Uji Proporsi", class = "btn-primary"),
                  actionButton("add_proportion_to_report", "Tambah ke Laporan", class = "btn-info"),
                  br(), br(),
                  verbatimTextOutput("proportion_result")
                ),
                box(
                  title = "Uji Variance", status = "primary", solidHeader = TRUE, width = 6,
                  selectInput("var_test_type", "Jenis Uji:",
                              choices = c("One Sample Variance" = "one_var",
                                          "Two Sample Variance" = "two_var")),
                  selectInput("variance_var", "Variabel:",
                              choices = NULL),
                  conditionalPanel(
                    condition = "input.var_test_type == 'one_var'",
                    numericInput("sigma_squared", "Variance Hipotesis (σ²):", value = 1)
                  ),
                  conditionalPanel(
                    condition = "input.var_test_type == 'two_var'",
                    selectInput("group_var_variance", "Variabel Pengelompokan:",
                                choices = NULL)
                  ),
                  actionButton("test_variance", "Uji Variance", class = "btn-primary"),
                  actionButton("add_variance_to_report", "Tambah ke Laporan", class = "btn-info"),
                  br(), br(),
                  verbatimTextOutput("variance_result")
                )
              ),
              fluidRow(
                column(6,
                  box(
                    title = "Interpretasi Uji Proporsi", status = "warning", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("proportion_interpretation")
                  )
                ),
                column(6,
                  box(
                    title = "Interpretasi Uji Variance", status = "warning", solidHeader = TRUE, width = 12,
                    verbatimTextOutput("variance_interpretation")
                  )
                )
              )
      ),
      
      # ANOVA Tab
      tabItem(tabName = "anova",
              fluidRow(
                box(
                  title = "ANOVA", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("anova_type", "Jenis ANOVA:",
                                       choices = c("One-way ANOVA" = "one_way",
                                                   "Two-way ANOVA" = "two_way")),
                           selectInput("anova_response", "Variabel Response:",
                                       choices = NULL),
                           selectInput("anova_factor1", "Faktor 1:",
                                       choices = NULL),
                           conditionalPanel(
                             condition = "input.anova_type == 'two_way'",
                             selectInput("anova_factor2", "Faktor 2:",
                                         choices = NULL)
                           ),
                           actionButton("perform_anova", "Perform ANOVA", class = "btn-primary")
                    ),
                    column(8,
                           verbatimTextOutput("anova_result"),
                           br(),
                           actionButton("add_anova_to_report", "Tambah ke Laporan", class = "btn-info")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi ANOVA", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("anova_interpretation")
                )
              )
      ),
      
      # Regresi Linear Tab
      tabItem(tabName = "regresi",
              fluidRow(
                box(
                  title = "Regresi Linear Berganda", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(4,
                           selectInput("reg_response", "Variabel Response:",
                                       choices = NULL),
                           checkboxGroupInput("reg_predictors", "Variabel Predictor:",
                                              choices = NULL),
                           actionButton("perform_regression", "Perform Regression", class = "btn-primary")
                    ),
                    column(8,
                           verbatimTextOutput("regression_result")
                    )
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Diagnostic Plots", status = "info", solidHeader = TRUE, width = 12,
                  plotOutput("regression_diagnostics", height = "600px")
                )
              ),
              fluidRow(
                box(
                  title = "Uji Asumsi Regresi", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("regression_assumptions"),
                  br(),
                  downloadButton("download_regression", "Download Regresi", class = "btn-success"),
                  br(),
                  actionButton("add_regression_to_report", "Tambah ke Laporan", class = "btn-info")
                )
              ),
              fluidRow(
                box(
                  title = "Interpretasi Regresi", status = "warning", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("regression_interpretation")
                )
              )
      ),
      
      # Download Tab
      tabItem(tabName = "download",
              fluidRow(
                box(
                  title = "Laporan Gabungan", status = "primary", solidHeader = TRUE, width = 12,
                  h4("Manajemen Laporan Akhir"),
                  p("Laporan ini akan berisi analisis yang telah Anda tambahkan menggunakan tombol 'Tambah ke Laporan'."),
                  br(),
                  h5("Konten Laporan Saat Ini:"),
                  verbatimTextOutput("report_contents"),
                  br(),
                  fluidRow(
                    column(6,
                           h5("Aksi Laporan:"),
                           actionButton("clear_report", "Bersihkan Laporan", class = "btn-warning"),
                           br(), br(),
                           downloadButton("download_doc_report", "Download Laporan Gabungan (.doc)", class = "btn-success")
                    ),
                    column(6,
                           h5("Opsi Laporan:"),
                           checkboxInput("include_metadata", "Sertakan Metadata Dataset", value = TRUE),
                           checkboxInput("include_summary", "Sertakan Ringkasan Statistik", value = TRUE),
                           checkboxInput("include_code", "Sertakan Kode R", value = FALSE)
                    )
                  ),
                  br(),
                  p("Laporan akan dihasilkan dalam format Doc dengan semua hasil analisis dan interpretasi yang telah ditambahkan.")
                )
              )
      )
    )
  )
)
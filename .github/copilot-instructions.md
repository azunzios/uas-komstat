# R Shiny Social Vulnerability Analysis Dashboard

## Architecture Overview

This is a comprehensive R Shiny dashboard for analyzing social vulnerability in Indonesia using 2017 SUSENAS data. The app follows a modular structure with separated UI and server logic.

### Key Components
- **`ui.R`**: shinydashboard layout with sidebar navigation and tabItems
- **`server.R`**: Reactive server logic handling data processing and statistical analysis
- **`app.R`**: Main entry point sourcing ui.R and server.R
- **`data/`**: Contains `sovi_data.csv` (social vulnerability metrics), `distance.csv` (geographic distance matrix), and `indonesia_kabkota_simple.geojson` (district boundaries)

## Data Architecture

The dashboard centers around three primary datasets:
- **Social Vulnerability Data**: 17 variables per district (CHILDREN, FEMALE, ELDERLY, FHEAD, FAMILYSIZE, NOELECTRIC, LOWEDU, GROWTH, POVERTY, ILLITERATE, NOTRAINING, DPRONE, RENTED, NOSEWER, TAPWATER, POPULATION)
- **Distance Matrix**: 512x512 matrix of geographic distances between Indonesian districts
- **Geospatial Data**: GeoJSON with Indonesian district/regency boundaries for choropleth mapping

## Key Development Patterns

### Reactive Data Pattern
```r
# Always use reactive() for data transformations
categorized_data <- reactive({
  req(input$categorize)  # Require input before processing
  data <- sovi_data()
  # ... transformation logic
  return(data)
})
```

### Dynamic UI Updates
```r
# Update selectInput choices based on reactive data
observe({
  if (input$categorize > 0) {
    cat_vars <- names(data)[grepl("_cat$", names(data))]
    updateSelectInput(session, "cat_var_explore", choices = cat_vars)
  }
})
```

### Statistical Analysis Structure
- Each statistical test follows the pattern: `observeEvent(input$button) { ... }`
- Results are rendered to separate outputs for results and interpretations
- All statistical interpretations are in Indonesian language

### Data Categorization System
The app implements a sophisticated categorization system:
- **Methods**: quantile, equal_width, kmeans
- **Custom naming**: Users can name categories (e.g., "Rendah", "Sedang", "Tinggi")
- **Dynamic forms**: UI adapts based on number of categories (2-10 categories supported)
- **Enhanced UI**: Nested sidebar structure with separate sections for original and categorized data
- **Validation**: Comprehensive input validation with graceful error handling

## Navigation Structure

The dashboard uses nested `menuItem` structure with enhanced organization:
- **Beranda**: Metadata and variable descriptions
- **Manajemen Data**: 
  - Data Asli: Original dataset viewing and metadata
  - Data Kategorik: Data categorization with custom naming (supports up to 10 categories)
- **Eksplorasi Data**: 
  - Eksplorasi Umum: Descriptive stats, correlation, plots
  - Eksplorasi Kategorik: Enhanced categorical analysis with frequency tables, cross-tabulation, and multiple visualization options
  - Peta Choropleth: Interactive map visualization with variable selection and non-zoomable/draggable interface
  - Analisis Spasial: Spatial correlation analysis using distance matrix with customizable thresholds
- **Uji Asumsi Data**: Normality and homogeneity tests
- **Statistik Inferensia**: t-tests, proportion tests, ANOVA
- **Regresi Linear**: Multiple regression with diagnostics
- **Download Gabungan**: Comprehensive reporting

## Development Workflows

### Running the App
```r
# Method 1: Direct execution
shiny::runApp("app.R", port=8080, launch.browser=TRUE)

# Method 2: Using run.R
source("run.R")
```

### Adding New Statistical Tests
1. Add UI elements in the appropriate `tabItem` in `ui.R`
2. Create reactive logic in `server.R` using `observeEvent()`
3. Add interpretation output with Indonesian language explanations
4. Include download handler for results

### Data Processing Conventions
- Always use `req()` for input validation
- Use `!!sym()` for dynamic variable selection in dplyr/ggplot
- Handle missing data with `na.rm = TRUE`
- Return comprehensive interpretation strings for all statistical tests

## Integration Points

### Leaflet Integration
- Interactive choropleth maps with variable selection
- Non-zoomable and non-draggable interface as specified
- Color-coded districts based on selected social vulnerability variables
- Popup information showing district names and values

### Spatial Analysis Features
- Distance-based correlation analysis using 512x512 distance matrix
- Customizable distance thresholds for spatial relationships
- Scatter plots showing spatial correlations with distance color-coding
- Statistical interpretation of spatial patterns

### DT DataTable Pattern
```r
DT::datatable(data, options = list(scrollX = TRUE, pageLength = 10))
```

### Download Handlers
- CSV downloads: `write.csv(data, file, row.names = FALSE)`
- HTML reports: Custom HTML generation with inline CSS
- Plot downloads: `ggsave()` for static versions

## Styling and Theming

- Uses shinydashboard with "black" skin
- Custom CSS in `www/styles.css` with Inter font
- Dark theme with `#303030` backgrounds and `#101010` borders
- Color coding: primary (blue), warning (yellow), success (green), info (light blue)

## Critical Dependencies

Core packages that must be loaded in both ui.R and server.R:
```r
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(ggplot2)
library(dplyr)
library(corrplot)
library(tseries)  # For jarque.bera.test
library(leaflet)  # For interactive maps
library(sf)       # For spatial data handling
library(geojsonio) # For GeoJSON reading
```

Statistical analysis packages:
```r
library(car)      # Levene's test
library(lmtest)   # Durbin-Watson, Breusch-Pagan
library(nortest)  # Anderson-Darling test
library(moments)  # Additional statistical tests
library(tseries)  # Jarque-Bera test
```

Geospatial packages:
```r
library(leaflet)    # Interactive maps
library(sf)         # Spatial data framework
library(geojsonio)  # GeoJSON import/export
library(RColorBrewer) # Color palettes for maps
```

## Error Handling Patterns

### Reactive Data Validation
```r
# Always validate required inputs
categorized_data <- reactive({
  req(input$categorize)
  req(input$var_to_categorize)
  req(input$n_categories)
  req(input$method)
  # ... rest of function
})
```

### Safe Output Rendering
```r
# Wrap outputs in tryCatch for graceful error handling
output$some_output <- renderDataTable({
  tryCatch({
    # Main logic here
  }, error = function(e) {
    DT::datatable(data.frame(Message = "Data belum siap..."))
  })
})
```

### Missing Data Handling
- K-means clustering requires special handling for NA values
- Always use `na.rm = TRUE` for statistical functions
- Use `tseries::jarque.bera.test()` for normality testing

## Performance Considerations

- Use `req()` extensively to prevent premature reactive execution
- Implement conditional panels to avoid unnecessary computations
- Load data once using `reactive()` and reuse across outputs
- Use `isolate()` for non-reactive dependencies when needed

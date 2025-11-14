# EconomicDashboard
# Global Economic Indicators Dashboard

An interactive R Shiny dashboard for exploring and analyzing global economic indicators across multiple countries and time periods.

![Dashboard](https://img.shields.io/badge/R-Shiny-blue)
![Version](https://img.shields.io/badge/version-1.0.0-green)

##  Overview

This dashboard provides comprehensive visualization and analysis of key economic indicators including:

- **GDP (Gross Domestic Product)** - Total and per capita
- **Unemployment Rate** - Labor market health
- **Inflation Rate** - Price stability measures
- **Trade Balance** - Import/export dynamics

The application features interactive visualizations, dynamic filtering, and comparative analysis tools to help understand global economic trends and relationships.

##  Features

###  Four Main Sections

1. **Overview Tab**
   - Summary statistics with key metrics
   - GDP distribution by country
   - Regional economic comparison
   - Correlation analysis between indicators

2. **Trends Analysis Tab**
   - Time series visualization for any indicator
   - Year-over-year growth rate calculations
   - Multi-country trend comparison

3. **Country Comparison Tab**
   - Side-by-side indicator comparisons
   - GDP vs Unemployment/Inflation scatter plots
   - Comprehensive heatmap of standardized indicators

4. **Data Table Tab**
   - Interactive data explorer with search and filtering
   - Summary statistics
   - CSV export functionality

###  Interactive Controls

- **Country Selection** - Choose multiple countries for analysis
- **Year Range Slider** - Focus on specific time periods
- **Region Filter** - Filter by geographic region
- **Indicator Selection** - Switch between different economic metrics
- **Year Comparison** - Compare countries in a specific year

##  Getting Started

### Prerequisites

Ensure you have R (version 4.0.0 or higher) installed on your system.

### Required Packages

Install the required R packages:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "ggplot2",
  "dplyr",
  "plotly",
  "DT",
  "scales"
))
```

### Installation

1. Clone or download the repository
2. Ensure `app.r` is in your working directory
3. Install all required packages (see above)

### Running the Dashboard

**Option 1: RStudio**
```r
# Open app.r in RStudio and click "Run App" button
```

**Option 2: R Console**
```r
library(shiny)
runApp("path/to/app.r")
```

**Option 3: Command Line**
```bash
R -e "shiny::runApp('path/to/app.r')"
```

The dashboard will launch in your default web browser, typically at `http://127.0.0.1:XXXX`

##  Project Structure

```
.
├── app.r                 # Main application file
└── README.md            # This file
```

##  Data

The current version uses **simulated economic data** for demonstration purposes. The dataset includes:

- **15 countries** across 5 regions (Americas, Europe, Asia, South America, Oceania)
- **7 years** of data (2018-2024)
- **Realistic patterns** including COVID-19 economic impact (2020) and recent inflation trends

### Extending with Real Data

To use real economic data, replace the `create_economic_data()` function with:

```r
# Example: Load from CSV
economic_data <- read.csv("your_data.csv")

# Example: World Bank API
# library(wbstats)
# economic_data <- wb_data(
#   indicator = c("NY.GDP.MKTP.CD", "SL.UEM.TOTL.ZS"),
#   start_date = 2018,
#   end_date = 2024
# )
```

Required columns:
- `Country` - Country name
- `Year` - Year (numeric)
- `GDP_Billions` - GDP in billions USD
- `Unemployment_Rate` - Unemployment percentage
- `Inflation_Rate` - Inflation percentage
- `Trade_Balance` - Trade balance in billions USD
- `GDP_Per_Capita` - GDP per capita in USD
- `Region` - Geographic region

##  Customization

### Changing Color Schemes

Modify the `skin` parameter in `dashboardPage()`:
```r
ui <- dashboardPage(
  skin = "blue",  # Options: blue, black, purple, green, red, yellow
  ...
)
```

### Adding New Indicators

1. Add the indicator to your dataset
2. Update the `trend_indicator` selectInput in the UI
3. Add appropriate labeling in the server logic

### Modifying Visualizations

All plots use `ggplot2` and `plotly`. Customize by editing the respective `output$` functions in the server section.

##  Dashboard Sections Explained

### Value Boxes (Overview)
Display real-time statistics for the latest year in your filtered dataset:
- Average GDP across selected countries
- Average unemployment rate
- Average inflation rate
- Number of countries being analyzed

### Interactive Plots
All visualizations are powered by `plotly`, enabling:
- Hover tooltips with detailed information
- Zoom and pan functionality
- Click-to-hide legend items
- Download as static image

##  Troubleshooting

**Issue**: Dashboard won't launch
- Solution: Ensure all required packages are installed
- Check R version (4.0.0+)

**Issue**: Plots not displaying
- Solution: Check your data has no NA values in key columns
- Verify filtered data isn't empty

**Issue**: Slow performance
- Solution: Reduce the date range or number of selected countries
- Consider using data aggregation for large datasets

##  Author

**Javan Meshack**

##  License

This project is open source and available under the MIT License.

##  Contributing

Contributions, issues, and feature requests are welcome! Feel free to:

1. Fork the project
2. Create your feature branch (`git checkout -b feature/AmazingFeature`)
3. Commit your changes (`git commit -m 'Add some AmazingFeature'`)
4. Push to the branch (`git push origin feature/AmazingFeature`)
5. Open a Pull Request

##  Contact

For questions or suggestions, please open an issue in the repository.

##  Acknowledgments

- Built with [R Shiny](https://shiny.rstudio.com/)
- Visualizations powered by [ggplot2](https://ggplot2.tidyverse.org/) and [plotly](https://plotly.com/r/)
- Dashboard framework by [shinydashboard](https://rstudio.github.io/shinydashboard/)

---

**Note**: This dashboard currently uses simulated data for demonstration. For production use, integrate with real economic data sources such as the World Bank API, IMF data, or other official economic databases.

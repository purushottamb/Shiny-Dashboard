# Global Military Spending Dashboard

A Shiny dashboard for interactive exploration and analysis of global military expenditure data. The app provides dynamic visualizations and data summaries by country, region, and year, leveraging data from the Stockholm International Peace Research Institute (SIPRI).

https://drpuru.shinyapps.io/Military_Spending_Dashboard/
---

## Overview

This dashboard enables users to:

- Explore military spending across countries, regions, and time periods.
- Visualize spending trends, regional summaries, and correlations between different economic metrics.
- Interactively filter data by region, sub-region, country, metric, and year range.
- View summary statistics, interactive maps, time series plots, and data tables.

---

## Features

- **Home Page:** Introduction, project background, and researcher information.
- **Interactive Filters:** Select region, sub-region, country, metric (USD, % of GDP, per capita, % of government spending), and year range.
- **Summary Value Boxes:** Key statistics on total and average spending.
- **Visualizations:**
  - Geographic map of spending by country.
  - Time series trends for selected metrics.
  - Regional and sub-regional summary bar charts.
  - Government spending allocation by country.
  - Correlation plot between per capita spending and % of GDP.
- **Data Table:** Filtered, sortable, and searchable data table for detailed exploration.

---

## Data Source

- **Military expenditure data:** Sourced from [SIPRI Military Expenditure Database](https://www.sipri.org/databases/milex).
- Data file: `military_expenditure_data.csv`

---

## Getting Started

### Prerequisites

- R (version 4.0 or higher recommended)
- The following R packages:
  - `shiny`
  - `bs4Dash`
  - `tidyverse`
  - `plotly`
  - `ggplot2`
  - `DT`
  - `dplyr`
  - `countrycode`
  - `readxl`
  - `here`
  - `sf`
  - `leaflet`
  - `tidygeocoder`

### Running the App

1. Clone this repository.
2. Place the `military_expenditure_data.csv` file in the project directory.
3. Open `app.R` in RStudio or your preferred R IDE.
4. Run the app with:

shiny::runApp()



---

## Author

**Purushottam Bhandare, Ph.D.**  
Social and development researcher

---

## Acknowledgments

- Data: Stockholm International Peace Research Institute (SIPRI)
- Quote: Martin Luther King Jr.

---

## License

This project is for educational and research purposes. Please refer to SIPRI's data usage policy for data licensing.

---

## Contact

For questions or feedback, please contact the author or open an issue in this repository.

---

> "A nation that continues year after year to spend more money on military defense than on programs of social uplift is approaching spiritual doom."  
> - Martin Luther King Jr.

---

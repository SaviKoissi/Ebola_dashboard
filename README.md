# 🦠 Ebola Surveillance Dashboard

## Overview

The Ebola Surveillance Dashboard is an interactive Shiny application designed to support the exploration, visualization, and dissemination of Ebola surveillance data across multiple countries and reporting periods.

The platform integrates epidemiological records into a unified analytical environment, allowing users to:

* Explore disease activity across countries and time periods.
* Visualize spatial and temporal trends.
* Compare epidemiological indicators.
* Generate country-specific summaries.
* Download filtered datasets for further analysis.

---

## Features

### Explorer

Provides a high-level overview of surveillance indicators and outbreak activity.

### Spatiotemporal Map

Interactive geographic visualization of reported Ebola activity across countries and reporting periods.

### Epidemiological Trends

Time-series analysis of key indicators including:

* Confirmed cases
* Suspected cases
* Fatalities
* Recoveries

### Country Profiler

Country-specific summaries and epidemiological insights.

### Data Hub

Download filtered datasets for external analysis and reporting.

---

## Project Structure

```text
EbolaDashboard/
│
├── app.R
│
├── R/
│   ├── data_loader.R
│   ├── mod_dashboard.R
│   ├── mod_map.R
│   ├── mod_timeseries.R
│   ├── mod_country.R
│   └── mod_download.R
│
├── www/
│   ├── css/
│   │   └── style.css
│   │
│   ├── js/
│   │   └── custom.js
│   │
│   └── ASB.png
│
└── data/
    └── surveillance_data.*
```

---

## Required Packages

```r
install.packages(c(
  "shiny",
  "bslib",
  "dplyr",
  "leaflet",
  "plotly",
  "DT",
  "sf",
  "tidyr",
  "lubridate"
))
```

Additional package requirements may depend on the implementation of individual modules.

---

## Running the Application

From R:

```r
shiny::runApp()
```

or

```r
source("app.R")
```

---

## Data Requirements

The application expects a surveillance dataset containing at minimum:

| Variable        | Description           |
| --------------- | --------------------- |
| date            | Reporting date        |
| country         | Reporting country     |
| confirmed_cases | Confirmed Ebola cases |
| suspected_cases | Suspected Ebola cases |
| deaths          | Reported fatalities   |
| recoveries      | Reported recoveries   |

Additional variables can be incorporated as needed.

---
## One liner to run the code 

```R
shiny::runGitHub( repo = "Ebola_dashboard", username = "SaviKoissi" )
```

---

## Authors

Developed as part of the Ebola Surveillance and Epidemiological Analytics Project.

---

## License

This project is intended for research, surveillance, and decision-support purposes. Please ensure that all data-sharing agreements and ethical requirements are respected when deploying or distributing the application.

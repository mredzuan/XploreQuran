# XploreQuran 📖

[![Project Status: Active](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Lifecycle: experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

**XploreQuran** is an interactive R Shiny web application dedicated to performing Natural Language Processing (NLP) and Text Analytics on Quranic translations.

## ✨ Features

- **Dashboard Overview:** Instantly view summary statistics, word frequencies, and an interactive word cloud with zoom capabilities.
- **Term Frequency Analysis:** Explore word prevalence across different Surahs using dynamic, searchable data tables and faceted histograms.
- **Word Analysis & Co-occurrence:** Dive deep into specific words to see context and common bigram/trigram associations.
- **Sentiment Analysis:** Analyze the emotional tone of the text using popular lexicons (Bing, NRC, AFINN).
- **Topic Modeling:** Uncover hidden thematic structures and grouped topics across the Quran.
- **Network Visualization:** Visually explore word relationships and how terms co-occur within verses.
- **Advanced Text Processing:** Customize your analysis on the fly by toggling stop words, inputting custom words to ignore, and activating word stemming (root word normalization).
- **Dynamic Translations:** Comes with built-in translations, plus the ability to import and analyze custom translations directly from [tanzil.net](https://tanzil.net).
- **Modern UI:** Built with `bslib` featuring a responsive, dynamic layout and a seamless Light/Dark mode toggle.

## 🚀 Getting Started

### Prerequisites

You will need **R** and optionally **RStudio** installed on your machine. The app relies on several R packages, primarily:

- `shiny`, `bslib`, `bsicons` (UI & Framework)
- `dplyr`, `tidytext`, `stringr` (Data Manipulation & NLP)
- `ggplot2`, `plotly`, `wordcloud`, `igraph`, `visNetwork` (Visualizations)
- `topicmodels`, `SnowballC`, `stopwords` (Advanced Text Processing)

### Installation & Execution

1. **Clone the repository:**
   ```bash
   git clone https://github.com/mredzuan/XploreQuran.git
   cd XploreQuran
   ```
2. **Open the project in R/RStudio.**
3. **Run the App:**
   You can launch the application by running the following command in your R console:
   ```R
   shiny::runApp("inst/app")
   ```

   *(Or by simply opening `inst/app/app.R` in RStudio and clicking "Run App").*

## 📁 Project Structure

This project is structured as an R package to keep the codebase modular and clean:

- `R/`: Contains backend logic, data processing pipelines, and text analytic configurations.
- `inst/app/`: Contains the main Shiny application (`app.R`, `global.R`) and all modularized UI/Server components (e.g., `mod_overview.R`, `mod_sentiment.R`).
- `data/`: Built-in datasets and translation files.

## 🤝 Contributing

Contributions, issues, and feature requests are welcome! Feel free to check the issues page if you want to contribute.

## 📝 License

This project is licensed under the MIT License - see the LICENSE file for details.

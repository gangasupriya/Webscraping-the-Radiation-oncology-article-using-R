# Webscraping-the-Radiation-oncology-article-using-R

```
 Radiation Oncology Article Analysis (BioMed Central)

This R project collects and analyzes article metadata from the *Radiation Oncology* journal (BioMed Central), based on the year input by the user. It provides insights into:

- Monthly publication trends
- Top contributing authors
- Frequently used keywords

 Key Features:
- Web scraping using `rvest`
- Year selection via user input
- Metadata extraction: titles, authors, abstracts, keywords, and email
- Data visualization using `ggplot2`

 Technologies Used:
- R, rvest, xml2, dplyr, stringr, ggplot2

 How to Run:
1. Open `main_script.R` in RStudio
2. Run the script line by line
3. Enter the desired year when prompted (e.g., 2023)
4. The data will be saved as `Radiation_Oncology_Data.csv`

 Output Includes:
- Cleaned dataset
- Bar plots: monthly publications, top authors, top keywords
- Scatter plot: keyword distribution


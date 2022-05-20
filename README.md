# note-in-the-pocket
STATCOM project Spring 2022. We worked with the Raleigh nonprofit Note in the Pocket to analyze requests fulfilled by the organization from January 2019-September 2021. We examined three main themes: the trend of the number of requests over time, the lag times of fulfilling those requests, and the number of requests by categories such as School, Organization, and ZIP Code.

See final_report.pdf for the complete exploratory data analysis.

## Data Analysis Pipeline

1. Data: the three datasets used for this project is in the folder OrdersFilled/.
2. The files read_in_data.R, Data cleaning for 2019 and 2021.r, and data_preprocessing.R read in and clean the Orders Filled data, and store the intermediary results in the folder intermediary_data/.
3. The code for the exploratory data analysis included in the final report is included in the folder EDA/.
4. The request counts were modeled with warpDLM (King, 2021). The code implementing warpDLM is contained in the folder warpDLM-reproducible-code-main/. The code applying warpDLM to the requests data is in the folder orders_filled_forecasting/.
5. Finally, all the EDA was collected in the R Markdown final_report.Rmd and knitted into the pdf final_report.pdf.

## warpDLM citation

King, B., & Kowal, D. R. (2021). Warped Dynamic Linear Models for Time Series of Counts. https://doi.org/10.48550/ARXIV.2110.14790 

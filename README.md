# Forecasting mental health crises: lowering healthcare costs using prevention and predictive analytics
A final capstone project for the Data Analytics, Big Data, and Predictive Analytics program at Ryerson University

## Introduction
Annual health care expenditures in the United States currently exceed 3.35 trillion dollars, averaging $10 345 per capita (Yoon et al. 2014). These costs are associated with rapid growth in medical prices and an aging population, suggesting that unless there are dramatic shifts in medical practices, these expenditures could increase rapidly in the coming years. 
To understand these ballooning costs, several large scale epidemiological studies are being conducted to provide information on the health of United States’ citizens. One such study, the Behavioural Risk Factor Surveillance System (BRFSS), conducts surveys to collect uniform data on health risk behaviours, chronic diseases, access to health care, and the use of preventative health services in the United States.  This survey provides valuable information on behavioural patterns which, if coupled with current Big Data and Machine Learning techniques, may help to provide valuable insights into persons at risk of mental health crises. By targeting and understanding these populations, preventative health measures could be put into place to ultimately help lower healthcare costs in the United States.

This project uses a C4.5 classification algorithm to predict mental health crises in patients with an accuracy of 81% and an AUC of 83% and a low False Negative rate of just 3.8%. 

## Files in this repository

### FinalReport_CBobbie.pdf
  This is the final copy of the report includes a Literature Review on depression and anxiety in the United States, Data Preprocessing and Cleaning steps, the Results of this project, and Conclusions based on the results.
  
### Final_MentalHealth_Code.R
  The reproducible R code for this project, including the classification algorithm and all figures.

### Recommendation_system.R
  Created as a user-input program, this is meant to assist doctors in determining if their patients are at risk of developing mental health crises, based on a truncated decision tree from the results of this study. The decision tree can be found in Appendix 2 for the FinalReport.pdf.
  
### Data
  All data for this project can be found on the BRFSS website. The .xpt file was used in this report as it can be easily imported in R.
    https://www.cdc.gov/brfss/annual_data/annual_2016.html
    



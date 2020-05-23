# COVID_RiskFactor

At the time of completion of this project the world is in the throws of the COVID-19 pandemic. Over 300,000 people's deaths have been attributed to COVID-19, with the United States being responsible for nearly a third, as of May 23rd. In order to slow the spread of the virus, many states elected to issue stay-at-home orders to reduce transmission. Stay-at-home orders, among other interventions, have resulted in a "bending" of the case and death curves in the US. At this time, many states are beginning lifting these orders with a wait and see approach to determine if transmission has slowed to a point that the healthcare system can handle. 

While it is abundantly evident that COVID-19 has had a profound effect globally and in the US, it has also become clear that some areas were effected worse than others. What factors determine how quickly the virus grew in a given area remain to be totally understood. 

This purpose of this project is to firstly complete the Capstone requirement for the Harvard Edx Data Science Specialization, and also to explore an interesting dataset for a contemporary issue. These data that will be analyzed is publically available information related to socio-economic and health data from counties and how they have been effected by the ongoing COVID-19 pandemic. The data was cleaned and aggregated by John Davis [@JDruns](https://twitter.com/JDruns "John Davis Twitter"). Information detailing the source of the data, and how it was assembled can be found in John's [kaggle notebook link](https://www.kaggle.com/johnjdavisiv/us-counties-weather-health-covid19-data). The data has information on 2,896 counties in the United States (US), and their County-level health and socioeconomic data. SOme counties are not included in the dataset because of the sparsity of information availabel for these counties. 

**Sources for Data Analyzed**
1. New York Times - case and fatality data
2. 2016 CDC Social Vulnerablity Data
3. 2020 Community Health Rankings Data
  * The bulk of the socioecomic and health data comes from here, and their methodology and definitions for these metrics can be found [here](https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/policies-and-programs) 

These data will be used to visualize how COVID-19 has effected counties across the US, define an outcome that will be predicted with machine learning, and validate the model we generate. 

## Success Criteria

Since there is no specific success criteria defined for this project beyond demonstrating learning, the following criteria will also be added. Broadly speaking, success will be defined as being able to stratify counties based off an important COVID-19 related outcome, and being able to successfully predict that outcome from the available socio-economic and health data. 

1. Demonstrate understanding of Data Science for the purpose of the Capstone Project 
2. Define outcome to be predicted by Machine Learning Algorithm
3. Build and optomize model for predicting outcome
4. Identify factors that were important in driving accuracy of the model
5. Assess Accuracy of the model

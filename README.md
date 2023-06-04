# sock-market-visualization
Poznan University of Technology project for Data Visualization class under supervision of Dariusz Brzeziński. \
## Authors:
- Adam Korba
- Krzysztof Szała

## Data
Link to the dataset we used: https://www.kaggle.com/datasets/borismarjanovic/price-volume-data-for-all-us-stocks-etfs \
Sectors and market cap scraped from: https://disfold.com/united-states/companies/


## Live demo
Link to the demo: https://adamkorba.shinyapps.io/stock-market-visualization/ \
Deploy using:
```{r}
library(rsconnect)
rsconnect::deployApp('.')
```
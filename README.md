# Ballmer-Peaker frontend
This is the repostitory for the front end of our best model prize at [2017 Quantify Datathon](http://quantify.monashdatascience.com/) at Monash University.

## What does it do?
This app allows a potential Airbnb host to estimate monthly return based on the attibutes of the property. They can then compare it to median rent return in the suburb and make an informed decision about whether to seek long-term rent or become an Airbnb host.

## Under the hood
We mainly used data from Inside Airbnb to train our model. We scraped data from various sources listed below and engineered location-based features to further improve our model.

We then saved and deployed the model along with the scraped data. When user input their address and property attributes, our app will geocode the address, engineer the required features in our model and produce a prediction of expected occupancy rate. This is then converted to expected monthly return, which is what the user would really be interested in.

## Model 
Gradient boosted decision tree


## Data Sources
- Airbnb data from [Inside Airbnb](http://insideairbnb.com/)
- Top 100 Melbourne destinations, scraped from [Trip Adviser](www.tripadvisor.com.au)
- Median rental price data from [Realestate.com.au](https://www.realestate.com.au/buy)
- Location data of all tram/train stations from [Public Transport Victoria](https://www.ptv.vic.gov.au/) 

## External Services
- Google maps service for live geocoding of input address and for geocoding the TripAdvisor Locations

## Collaborators
[Ed Farrell](https://github.com/EdFarrellDS), Letian Wang, [Lindy WoodBurn](https://github.com/ProcessFit), Michael Rodd, Nick De Silva

## Deployment
Final version for the Datathon deployed [here](https://lwbayes91.shinyapps.io/ballmerpeak/).

I plan to rewrite the front end with D3 at some point. 

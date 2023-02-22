# NYC Restaurant Inspection App: Your Guide to Clean and Safe Eateries

+ **Project summary**: Our Shiny app provides an overview of restaurants in the five boroughs of New York City based on inspections conducted by the Department of Health and Mental Hygiene (DOHMH). The data used in the app can be found on the [NYC government data website](https://data.cityofnewyork.us/Health/DOHMH-New-York-City-Restaurant-Inspection-Results/43nn-pn8j). The app is designed for individuals who want to make an informed decision about where to eat based on hygiene ratings. It is divided into three parts: Introduction, Government Initiatives, and Results. The Introduction provides an overview of the restaurant scene in NYC, focusing on the number of restaurants by grade and a word cloud of common violation descriptions. The Government Initiatives section describes the various initiatives undertaken by the government to improve restaurant hygiene and displays the number of inspections carried out between 2019 and 2022. The Results section allows users to easily identify restaurants with all ratings from A to Z by borough, year, restaurant name, and violation. With our app, you can explore the city one meal at a time and make informed decisions about where to eat. To use the app, simply visit [*Everything Good and Healthy in NYC*](https://nyc-restaurants.shinyapps.io/inspections/). 

# Introduction

# Results

Created to fulfill the requirements of project 2 for the GR5243 course  (Spring 2023 term). The project guidelines can be found [here](doc/project2_desc.md).

+ **Project title**: Everything Good and Healthy in NYC
+ **Team #1**:
	+ Safira Raharjo
	+ Namira Suniaprita
	+ Ashwathi Nair
	+ Henry
	+ Haoyu He
	+ Xueyi Zhang

+ **Contribution statement**: Every member of the team spent time looking for meaningful data to present before we zeroed in on this one. The contribution of each member was equal with respect to deciding what visualizations would be most appropriate to convey the message we want to as well as creating the storyline of the app and presentation. We then divided the work into 3 subgroups of 2 members each. Xueyi and Ashwathi worked on the introduction tab wherein our focus was to provide our users a picture of the grades of restaurants in NYC and highlight the major problems identified through word clouds. Henry and Haoyu worked on presenting facts related to the inspections conducted. Namira and Safira worked on displaying the gist of the inspections through a map. Additionally, Safira worked on cleaning the data in global.R and optimizing the code, and Namira worked on designing the UI and consolidating code from different team members. Both also working together to deploy the app on Shiny.

Following [suggestions](http://nicercode.github.io/blog/2013-04-05-projects/) by [RICH FITZJOHN](http://nicercode.github.io/about/#Team) (@richfitz). This folder is orgarnized as follows.

```
proj/
├── app/
├── lib/
├── data/
├── doc/
└── output/
```

Please see each subfolder for a README file.

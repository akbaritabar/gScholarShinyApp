# Google Scholar Shiny Application
A brief application to work with google scholar and show coauthorship network

- In order to see coauthorship networks graph, copy google scholar profile of author __(example: "https://scholar.google.com/citations?user=zufgVroAAAAJ&hl=en")__ and put it in the text box provided in the application UI, you will see coauthorship network in seconds
- This shiny application uses scholar package in R to exctract data from google scholar
- As always, I owe most parts of the code and what I wrote to stackoverflow and google searches

## How to run the app in Rstudio
You need to have 'shiny' package installed in R, then install these packages as well, using: 

` install.packages(c("shinyjs", "graphics", "methods", "scholar", "igraph", "network", "stringdist", "networkD3"))` 


Then open app.R file inside Rstudio, now you should see a "Run App" button on upper left side of script editor (instead of usual "Run" button), click on "Run App" and you should have another window opened on the top of Rstudio editor, follow guides in app (like above) to see google scholar coauthorship network graph.

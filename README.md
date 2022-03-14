# shiny-app
R Shiny app built for visualizing data from multiple research studies investigating prototype footwear (see Prototype Footwear Testing repo).  However, since data from studies are proprietary, simulated data are used for this app.

DataSimulation.R
This code generates simulated data from proprietary research data.  The simulated dataset includes information on subject characteristics, footwear mechanical properties, biomechanical variables (i.e. joint angle data), and runner perception of footwear.

SimulatedFootwearData.xlsx
Excel file containing simulated data generated from DataSimulation.R code.

BigCushShinyApp.R
Code for RShiny app to visualize data.  Visulizations include:

Perception: Scatter plot perception rating on the y-axis for each shoe (x-axis).  Size of scatter point is proportional to the number of observations for each rating level.
Mechanical properties: Simple violin plots showing distribution of values for each mechanical property across all shoes.
Subject characteristics: Violin plots showing distribution of subject characteristics for males and females.
Biomechanics: Three lines plots (one per joint of hip, knee, and ankle) showing the values of each biomechanical variable in three dimensions (X, Y, Z) across all shoes for each subject.

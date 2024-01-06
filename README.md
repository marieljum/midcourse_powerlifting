# Exploration and Visualization of USAPL Powerlifting Data

### Executive Summary
This Shiny app will analyze the most updated USAPL powerlifting database. This database is the complete data from all competitions in the USAPL federation. As a powerlifter, it was important for me to understand the competition and to share this information with other lifters or to the general public. Analysis of the data will include comparisons of competitors and visualization of all data in graphs and maps. 

### Motivation
I chose to analyze the USAPL powerlifting database to show other powerlifters or the general public competitions in each state or region of the world, and to understand the powerlifters I am up against, as a competitor. 
Data Question
What types of competitions are available in each state? How has powerlifting changed in each state (did it grow, is it lacking numbers)? How does one competitor compare against another competitor? What is the max number for each lift per year or total in each state? How far am I to the closest meet location? 
	 	 	 	
### Minimum Viable Product
This app will have a data analysis page, (1) in which you can parse through all the states for all or any available years per competition. A list of competitors can show up, in which a person can check all, one person, or more than one (to compare) to see the competitor’s numbers in a line graph. (2) There will be a map, in which you can select the state and view the locations of meets in that state. (3) There will be small text boxes to define terminology and next meet date and location in that selected state. (4) A pie chart can show the percentage of types of meets in each state per year or total time. 

### Data Sources
I have a DataFrame of most competitions in the USAPL database (webscraped from their results site: https://usapl.liftingdatabase.com/competitions)

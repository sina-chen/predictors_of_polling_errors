# Instructions for cleaning polls and merging covariates on gender and ethnicity

**Important**: Run scripts in the outlined order of the UML diagram below.

It is assumed that the working directory is set to the root folder of this github repo.


## Detailed description of wikipedia covariate code

In the flowchart below the whole data genarating process is described. 
- Yellow boxes: raw input data
- Blue boxes: R script to transform data
- Green boxes: intermediate and final data sets

The programming flow in the top right corner shows how raw senate election results are transformed and joined with scraped polls from pollingreport.com.
**Important**: Every data set which contains scraped polls is excluded from this reposotory as pollingreport offers the data for a fee.

Polls and corresponding results are now joined with data from wikipedia containing contextual variables of the singel senate elections. This results in the file 'senate_wiki_merged.csv' containing senate polls from 1998 until today, enriched with information about the two major canidates (democratic/republican), the number of candidates on the ballot as well as if the incumbent was running for re-election.

In a final step, information about the gender and the race of the single candidates are joined, resulting in the data set 'senate_polls_merged.csv' which is ready for analysis. Information about gender and race are predicted via the [demographics API of clarifai](https://www.clarifai.com/models/demographics-image-recognition). 


![Alt text](Data_Science_Project.jpg?raw=true "Title")


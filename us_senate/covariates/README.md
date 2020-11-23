# Instructions for cleaning polls and merging covariates on gender and ethnicity

**Important**: Run scripts in the outlined order of the UML diagram below.

It is assumed that the working directory is set to the root folder of this github repo.


## Detailed description of wikipedia covariate code

In addition to merging the datasets into one and cleaning them, some manipulation is done.
Data cleaning includes the removal of elections where more than one candidate from the two major parties runs for elections and ends up in the first three places in the election. This restriction is included, as the primary interest lies in the two party competition between Republicans and Democrats. Moreover, elections with two candidates from the same party on the final ballot are extremely rare due to election laws in the USA (except e.g. California).


![Alt text](./Data_Sciene_Project.svg)
<img src="./Data_Sciene_Project.svg">


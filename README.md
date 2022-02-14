# Data and analyses to accompany the manuscript: "Larval thermal conditioning does not improve post-settlement thermal tolerance in the dominant reef-building coral, *Montipora capitata*"

### Authors: G. Alexander, J. Hancock, *A.S. Huffmyer, S.B. Matsuda

### Journal: Coral Reefs

*Corresponding author:   
AS Huffmyer (ashuffmyer@gmail.com)  

*SB Matsuda and AS Huffmyer contributed equally and are co-senior authors.*  

Experiment testing on the effects of temperature conditioning on *M. capitata* larval survivorship, settlement and performance under elevated temperature. 

This respository contains data and analyses for experimental temperature treatments, larval survival, larval settlement, and recruit survival. 

Repository contains .rmd files for analyses in R, data, output, and figure folders. 

All analyses run from R project.  

R scripts and analyses written by AS Huffmyer and SB Matsuda.  

# Description of repository    

**Scripts:**   
`Analysis.Rmd`: R Markdown script to analyze larval survival, larval settlement, and recruit survival.   
`Temperature_analysis.Rmd`: R Markdown script to analyze and plot temperature treatments.     

Scripts will output `.html` files if `knit` is executed from project directory.    

**Data Folder:**  
`surv_larvae.csv`: Data for larval survival. Data includes date of measurement (Date), day of experiment (Day), conical tank numer (Conical), treatment (Treatment; C=Cold; A=Ambient), number of larvae (Larvae), volume of sample (Volume; mL), sample replicate (Sample), and total volume of conical (ConicalVol; mL).  

`settlement.csv`: Data for larval settlement. Data includes tank number (Tank), treatment (Treatment), chamber number (Chamber), plug number (Plug), number of starting larvae (Starting Larvae), day of experiment (Day), total number of recruits (Total), number settled as aggregates (Aggregate), number settled as individuals (Individual), and notes (Notes).  

`surv_recruits.csv`: Data for recruit survival. Data includes plug number (Plug.ID), tank number (Tank), day of experiment (Days), total number of alive recruits (Total), number of surviving aggregate colonies (Aggregate), number of surviving individual colonies (Individual), recruit temperature treatment (Juv.Treatment), number of alive recruits (Success), number of recruits that have died (Failure). Note in the data and scripts we refer to "juvenile treatment", which is specified as "recruit treatment" in the manuscript.  

**Figures Folder**:  

Scripts will output figures into the figures folder.  

**Temperature Folder**:  

Folder containing temperature data frames for larval, settlement, and recruit exposure phases of the experiment.  

  




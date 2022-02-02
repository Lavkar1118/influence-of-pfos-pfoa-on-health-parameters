# influence-of-pfos-pfoa-on-health-parameters

## Background

#### Perfluoroalkyl substances are highly ubiquitous given their highly water resistant property and are used in the manufacturing of cookwares, capets, adhesives, etc. The International Agency for Research on Cancer classified them as “possible carcinogen to humans” (Group 2B) following which they were flushed out form the US industrial market between 2000 – 2002

#### However, these compounds are still lurking around in the environment and exposure to them can result in cancers, liver damage, weakened immune systems and low birth weight in exposed fetuses. I have used the NHANES data from 2015-2016 to recognise the levels of these compounds in blood and understand their relationship towards vital health parameters such as cholestrol, glycohemoglobin and BMI.


### Study Procedure:

* Statistical software R was used to
  * Data was cleaned and manipulated to produce summary statistics
  * Histograms and boxplots were constructed to understand distribution of demographic and primary dependent variables
  * Build both simple and multiple regression models to check for statistical significance of covirates. 

### Key Findings:

* Both PFOA and PFOS had a statistically significant positive association with Cholestrol as well the dempgraphic variables age and smoking status.
* Futher analysis was performed for just the PFOS variables.
* The adjusted model had the effect size of PFOS drop by 29%, attributable to the confounding by age variable.
* The adjusted R^2 value of the multiple linear regression model (0.04061) was greater than the adjusted R^2 of the simple linear regression model (0.01572)
* This provides evidence that our complex multiple regression model has a better fit and is more accurate in explaining the variability of Cholesterol caused by PFOS values and we need to account for confounders such as age and gender when studying a potential casual relationship.

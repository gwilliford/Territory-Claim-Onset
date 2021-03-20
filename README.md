# The Domestic Proximate Causes of Territorial Claims

R code and data for Chapter 3 of my dissertation: George W. Williford. 2021. "The Domestic Proximate Causes of Territorial Claims."

## Project Description

When do countries initiate territorial claims? Existing research focuses on the structural factors that make states more likely to contest particular pieces of territory, the normative justifications states can use to justify their claims, and a variety of dyadic factors. While these factors create the conditions under which a claim could emerge between two states, it does not explain why states issue claims when they do. I argue that territorial claims often emerge as the result of coalition changes within a state. In many cases, the decision to initiate a new claim reflects the differing incentives that new winning coalitions have to do so compared to the old coalition. Using a cure model, I model the probability that coalition changes will lead to claim onset, conditional on the structural factors that make some dyads susceptible to claims to begin with. I find that coalition changes are associated with an increased probability of claim onset.

## Methods Used

- Cure Models
- Survival Analysis

## Software Used

Analysis was conducted in R, version 4.01. The analysis was completed using my R package for estimating proportional hazards cure models with time-varying covariates. The package, tvcure, can be found [here](https://github.com/gwilliford/tvcure).

## File Descriptions

- ClaimOnsetAnalysis20210307.R - contains code to replicate the analysis in this chapter
- Ch3_Plots - contains code to recreate the plots in this chapter
- Ch3_Tables - creates the table of results used in this chapter
- terrstart.csv - contains data used in the analysis
- Chapter3.pdf - the manuscript for this chapter

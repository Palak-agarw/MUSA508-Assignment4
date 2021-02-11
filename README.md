# Improving efficiency of HCD tax credit program
### Palak Agarwal and Leah Shapiro 
#### MUSA 508 - Public Policy

The Department of Housing and Community Development (HCD) in Emil City seeks to launch a targeted campaign to encourage homeowners to take advantage of a $5,000 tax credit for home repairs. Typically, only 11% of the eligible homeowners they reach out to take the credit. This analysis attempts to improve the efficiency of HCD’s outreach efforts, minimizing outreach to homeowners who are unlikely to take the credit while maximizing outreach to homeowners who are likely to take the credit.
The method used to maximize this efficiency is by generating a matrix that tells us how likely is the user to accept the credit or not. To make these prediction many variables are used which are related to the likely of taking the credit or not.

Our analysis uses a binary logistic regression to estimate whether eligible homeowners are likely to take the home repair tax credit based on a number of features. our goal is to create a model that can accurately predict instances of when a homeowner will and will not take the credit. After engineering features to make our predictive model as accurate as possible, we then use a cost-benefit analysis to search for an optimal threshold to limit ‘costly’ errors, or those create the greatest cost to HCD while producing the least benefit to homeowners. Based on our understanding of the credit program, we constructed some stylized facts to inform the cost-benefit analysis.

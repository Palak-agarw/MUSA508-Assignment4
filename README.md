# Improving efficiency of HCD tax credit program
### Palak Agarwal and Leah Shapiro 
#### MUSA 508 - Public Policy

The Department of Housing and Community Development (HCD) in Emil City seeks to launch a targeted campaign to encourage homeowners to take advantage of a $5,000 tax credit for home repairs. Typically, only 11% of the eligible homeowners they reach out to take the credit. This analysis attempts to improve the efficiency of HCD’s outreach efforts, minimizing outreach to homeowners who are unlikely to take the credit while maximizing outreach to homeowners who are likely to take the credit.
The method used to maximize this efficiency is by generating a matrix that tells us how likely is the user to accept the credit or not. To make these prediction many variables are used which are related to the likely of taking the credit or not.

Our analysis uses a binary logistic regression to estimate whether eligible homeowners are likely to take the home repair tax credit based on a number of features. our goal is to create a model that can accurately predict instances of when a homeowner will and will not take the credit. After engineering features to make our predictive model as accurate as possible, we then use a cost-benefit analysis to search for an optimal threshold to limit ‘costly’ errors, or those create the greatest cost to HCD while producing the least benefit to homeowners. Based on our understanding of the credit program, we constructed some stylized facts to inform the cost-benefit analysis:

For each homeowner predicted to take the credit, HCD will allocate $2,850 for outreach (this figure includes staff and resources to facilitate mailers, phone calls, and information/counseling sessions at the HCD offices).
Given our new targeting algorithm, we assume 25% of contacted homeowners take the credit.
The credit costs $5,000 per homeowner which can be used toward home improvement.
Houses that transacted after taking the credit sold with a $10,000 premium, on average.
An additional benefit of the credit is that homes surrounding the repaired home see an aggregate premium of $56,000 on average, which HCD would like to consider as a benefit in the cost-benefit analysis.

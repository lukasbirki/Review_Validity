#http://www.cookbook-r.com/Statistical_analysis/Inter-rater_reliability/

library(irr)

data(diagnoses)
irr::kappa2(diagnoses[,1:2])

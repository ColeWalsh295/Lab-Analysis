# Lab epistemology survey analysis

The lab epistemology survey analyzed here was developed by [Hu *et al.*](https://journals.aps.org/prper/abstract/10.1103/PhysRevPhysEducRes.14.010121) to evaluate students’ epistemological views on measurements and validity of experimental results. We administered this open-response survey to one class at Cornell University where students were enrolled in lab sections with different pedagogical goals.

In `ChiSquare_marginal-independence.Rmd`, we analyze whether students' responses to the survey vary by the type of lab they were enrolled in. Because the survey was open-response, student responses were hand-coded and multiple codes could be applied to a single text entry. Codes were not mutually exclusive, then, meaning standard chi-square tests could not be applied. We instead tested multiple marginal independence and calculated a modified chi-square statistic through nonparametric bootstrapping using the [`MRCV` package in R](https://cran.r-project.org/web/packages/MRCV/).

In `LabEpistemology.Rmd`, we build graphs of coded student responses showing how often two codes are applied to the same set of text. `Unsupervised-clustering.ipynb` includes a very simple machine learning pipeline that uses kmeans clustering to group students' text responses into groups without regard for what codes we manually applied to the text.

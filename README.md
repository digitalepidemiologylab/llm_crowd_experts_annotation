# Use of large language models as a scalable approach to understanding public health discourse

This repository assesses the performance of LLMs (GPT versions 3.5 and 4, Mistral and Mixtral),  and Amazon Mturk workers in comparison with experts when annotating tweets for public perception on vaccines.

Since Twitter/X data cannot be freely accessible, only certain data is available under the folder 'data', including the tweets id with at least partial agreement among experts.

For visualising the main results of the analysis, including a Shiny application, please do the following steps:

1. Open the R project. 
2. Check that the working directory is "~/gpt_annotation". If not, change it to that path.
3. Open "scripts/main.R"
4. Source the code of the scripts with all data publicly available, indicated by "(public)" 

## Structure of this repository
**R project**: enables to have this repository as a portable, self-contained folder.
**Shiny app**: web application to visualise some of the results of the study.
**data**: folder with the publicly available data or aggregated data used in this study.
**scripts**: folder with the R and python scripts used in the study to produce the results. Some of the scripts cannot be run since those are linked to restricted data that is not available in the repository.
**outputs**: folder with the outputs produced by the scripts and included in the study.

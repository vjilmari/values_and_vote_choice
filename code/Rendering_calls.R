library(rmarkdown)
library(knitr)

# Render RMDs to .html and .md, and obtain .R

# Preparations 1

render(input = "code/preparations/1_Extract_parties_voted_in_ESS.Rmd",
       envir = new.env())

purl(input="code/preparations/1_Extract_parties_voted_in_ESS.Rmd",
     output="code/preparations/1_Extract_parties_voted_in_ESS.R",
     documentation = 2)

# Preparations 2


render(input = "code/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
       envir = new.env())


purl(input="code/preparations/2_Assign_ESS_party_numbers_to_CHES.Rmd",
     output="code/preparations/2_Assign_ESS_party_numbers_to_CHES.R",
     documentation = 2)

# Preparations 3

render(input = "code/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
       envir = new.env())


purl(input="code/preparations/3_merge_ESS_and_CHES_by_vote.Rmd",
     output="code/preparations/3_merge_ESS_and_CHES_by_vote.R",
     documentation = 2)

# Preparations 4

render(input = "code/preparations/4_Variable_transformations.Rmd",
       envir = new.env())

purl(input="code/preparations/4_Variable_transformations.Rmd",
     output="code/preparations/4_Variable_transformations.R",
     documentation = 2)

# Descriptive statistics

render(input = "code/analysis/Descriptive_statistics.Rmd",
       envir = new.env())

purl(input="code/analysis/Descriptive_statistics.Rmd",
     output="code/analysis/Descriptive_statistics.R",
     documentation = 2)

# Run value main effects (RQ1, RQ2, ERQ1)

render(input = "code/analysis/run_value_main_effects.Rmd",
       envir = new.env())

purl(input="code/analysis/run_value_main_effects.Rmd",
     output="code/analysis/run_value_main_effects.R",
     documentation = 2)

# Run salience of economic left-right moderation (RQ3)

render(input = "code/analysis/run_RQ3_ELR_salience.Rmd",
       envir = new.env())

purl(input="code/analysis/run_RQ3_ELR_salience.Rmd",
     output="code/analysis/run_RQ3_ELR_salience.R",
     documentation = 2)

# Run salience of GAL-TAN moderation (RQ4)

render(input = "code/analysis/run_RQ4_GALTAN_salience.Rmd",
       envir = new.env())

purl(input="code/analysis/run_RQ4_GALTAN_salience.Rmd",
     output="code/analysis/run_RQ4_GALTAN_salience.R",
     documentation = 2)

# Run education moderations (RQ5_ERQ5)

render(input = "code/analysis/run_RQ5_ERQ5_value_education_interactions.Rmd",
       envir = new.env())

purl(input="code/analysis/run_RQ5_ERQ5_value_education_interactions.Rmd",
     output="code/analysis/run_RQ5_ERQ5_value_education_interactions.R",
     documentation = 2)

# Run abstain to vote versus vote (ERQ3)

render(input = "code/analysis/run_ERQ3_voting.Rmd",
       envir = new.env())

purl(input="code/analysis/run_ERQ3_voting.Rmd",
     output="code/analysis/run_ERQ3_voting.R",
     documentation = 2)

# Run West Europe vs. post-communist moderations (ERQ4)

render(input = 
         "code/analysis/run_ERQ4_value_west_vs_post_communist_interactions.Rmd",
       envir = new.env())

purl(input="code/analysis/run_ERQ4_value_west_vs_post_communist_interactions.Rmd",
     output="code/analysis/run_ERQ4_value_west_vs_post_communist_interactions.R",
     documentation = 2)

# Run value main effects for college sub-sample

render(input = "code/analysis/run_value_main_effects_college.Rmd",
       envir = new.env())

purl(input="code/analysis/run_value_main_effects_college.Rmd",
     output="code/analysis/run_value_main_effects_college.R",
     documentation = 2)

# Run value main effects for non-college sub-sample

render(input = "code/analysis/run_value_main_effects_no_college.Rmd",
       envir = new.env())

purl(input="code/analysis/run_value_main_effects_no_college.Rmd",
     output="code/analysis/run_value_main_effects_no_college.R",
     documentation = 2)

# Run value main effects for West Europe sub-sample

render(input = "code/analysis/run_value_main_effects_west.Rmd",
       envir = new.env())

purl(input="code/analysis/run_value_main_effects_west.Rmd",
     output="code/analysis/run_value_main_effects_west.R",
     documentation = 2)

# Run value main effects for Post-communist Europe sub-sample

render(input = "code/analysis/run_value_main_effects_east.Rmd",
       envir = new.env())

purl(input="code/analysis/run_value_main_effects_east.Rmd",
     output="code/analysis/run_value_main_effects_east.R",
     documentation = 2)



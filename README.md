# USCensusIncomeAnalysis

The analysis was done according to the following steps : 

- First, preprocessing the data, compute the percentage of missing values (data_preprocessing.R)
- An univariate audit on all variables, with visualizations and statistics (univariate_audit.R)
- Reprocess the data, in order to, first, deal with the NAs and unknown values, and then improve the datasets by transforming a few variables (data_improved.R)
- Random forest algorithm, to see the value of the improved dataset compared to the basic one, and assess the important variables (random_forest.R)
- Then fit a logistic regression on improved dataset with the important variables (log_regression.R)

(Main script : analysis.R)

The most satisfying results were with the random forest, with an error rate of 4.68% on predicting the income on the test dataset.

The most challenging part was understanding the data and processing the large amount of it. Especially all the different forms of values that could or could not be NAs.
Like '?', or 'Do not know' or even understanding what the 'Not in universe' was referring to.
I finally treated the NAs by removing the variables containing them, as they did not seem really relevant.
To improve my work, I could've dealt with the NA's differently maybe.
At the end, the most important variables were : sex, age, major occupation code, major indus code, and capital gains.
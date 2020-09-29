# Professional Portfolio

Welcome! This repo contains examples of my code and a writing sample. Be sure to also check out my [website](https://www.seantnorton.net/) for my blog posts and project descriptions.

### Writing Sample

`dem_and_discontent.pdf` is the first paper in my dissertation, and is currently under peer review. In this paper, I use a novel dataset combining a large Russian housing renovation project and submunicipal electoral results to identify a mechanism behind the observed relationship between the presence of large cities and authoritarian failure.

### Code Samples

* `electoral_res_cleaning.R`: This is part of the writing sample project. I web-scraped the electoral results for this project from the Russian electoral commission's website, which provides results as inconsistently formatted Excel workbooks. This script turns that mess into machine-readable format.
* `icews_cleaning.R`: This for a project using the Integrated Crisis Warning System event dataset, which is far too large to hold in memory on a typical machine. I instead turned the dataset into a SQLite database, which this script queries for protest events and then cleans into a R dataframe. 
* `ru_elec_scraper.R`: A suite of functions to scrape polling-district level data from the Russian Central Election Commission's website. The end goal is to turn this into an R package.
* `topic_mod_tune.py`: As part of a paper on the Russian Internet Research Agency's use of Twitter to manufacture domestic consent on the Ukraine crisis, I used this script to tune a topic model on a corpus of 5 million Russian tweets, using distributed computing. I wrote up some preliminary results [on my blog](https://www.seantnorton.net/post/polmeth-2020/).
* `var_prelagged_chol.stan`: Stan code for a hierarchical Bayesian vector autoregression, designed to be used on regions nested within countries. In addition to using country-level random intercepts and slopes, I've written this to make use of Stan's new multi-threading features. This yields a dramatic increase in sampling time.
* `w2v_mod.py`: Also a part of the Russian twitter project, this is designed to tune a word2vec model on the 5 million tweet dataset. I likewise wrote up preliminary results from this model [on my blog](https://www.seantnorton.net/post/sanctionsw2v/). 

# Movie watching behavior in streaming services - R
----------------------------------------------------
Business Case: Determining movie watching behavior in streaming serivices. The average American subscribes to 4 streaming services
and watches 8 hours of content daily. 

Packages used: wordcloud, dplyr, tidyverse, tidytext, stringr, tidy, ggplot2, scales, textdata, gutenbergr, reshape2,
textshape, RColorBrewer, igraph, tm , quanteda.textmodels, quanteda

Technical Analysis
-------------------
1. Recorded survey answers using Otter.ai speech to text.
2. Loaded answers as dataframe
3. Removed stop words
4. Unnested tokens
5. Computed total frequency of words
6. Computed Zipf's law term frequency and rank
7. Executed tf-idf framework
8. Created wordcloud
9. Analyzed sentiments - nrc and bing lexicons
10. Conducted bigram & trigram analysis with tf-idf framework
11. Visualized bigram and trigram networks
12. Conducted Naive Bayes Classification analysis
13. Uploaded graph outputs to Shiny app

Insights derived
-----------------
- Develop a laptop friendly screening platform.
- Promote comedies and kids shows in the afternoon.
- Promote thriller, action and romantic drama movies in the evening.






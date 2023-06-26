# amazon-review-sentiment
sentiment analysis on amazon review 

Sentiment analysis is a powerful technique used to analyze and understand the opinions, emotions, and sentiments expressed in textual data. In this project, I focus on performing sentiment analysis on Amazon review data in the category of Pet Supplies and Grocery and Gourmet Food which consists of a subset of 2,098,325 and 1,143,860 reviews consequently spanning from May 1996 to October 2018. The data was sourced from the paper "Justifying recommendations using distantly-labeled reviews and fine-grained aspects" by Jianmo Ni, Jiacheng Li, and Julian McAuley, presented at the Empirical Methods in Natural Language Processing (EMNLP) conference in 2019.

The project methodology involves several steps. Firstly, I extract the necessary data from the JSON format. Next, the data is being clean by selecting only the review text, performing tokenization, and removing stop words to prepare it for sentiment analysis. Three different sentiment analysis methods are employed: Bing lexicon, NRC lexicon, and AFINN lexicon. Bing focuses on positive and negative sentiment polarity, NRC lexicon offers a broader range of emotions and sentiments, and AFINN provides sentiment scores for social media analysis.

To visualize the results, I utilize word clouds, bar charts and pie chart for the Bing and NRC lexicons, and scatter plots for the AFINN lexicon. These visualizations provide intuitive representations of sentiment distribution and help gain insights into the sentiment patterns within the dataset.

Through this project, I aim to gain a comprehensive understanding of sentiment analysis techniques and their application to real-world data. By analyzing the sentiments expressed in Pet Supplies and Grocery and Gourmet Food reviews, I can uncover valuable insights that can assist businesses in making informed decisions, improving customer satisfaction, and enhancing product offerings.

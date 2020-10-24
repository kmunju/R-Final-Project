Project Proposal 

Situation: Harmful chemical content will be identified by its level of toxicity in various cosmetic products categorized by its prominent usage and brand names. Some brands don’t fix, discontinue, or update their products even if their commercial products are reported and continuously be problematic for human health. 

Data: There are two different datasets from the same source that explain:  

1. the various cosmetics by its brand, harmful chemicals, and dates of reports (csv file)  

2. the list of harmful chemicals explained by its toxicity (PDF file) 

Both datasets were publicly available on the website of California Department of Public Health (CDPH) under California Safe Cosmetic Program (CSCP). We will download the cosmetics/harmful chemical csv file then clean the data, and group the data appropriately (using R) by different type of chemical and brand names. We will use this data to test our hypotheses and answer certain questions listed below. During this process, SQL and R will be cogently used to answer different types of questions. Then, by analyzing the data using R, we will be matching the harmful chemical list with chemicals toxicity level (PDF file) and create the visualization based on our conclusion.  

Question:  

What are the top 10 most reported type of chemical in cosmetic? 

What is the toxicity of commonly reported chemicals?  

Which primary category of cosmetics contain most chemical content reports? 

Which companies' cosmetics contain most chemical reports and have not yet fixed, removed, or discontinued the product? Which companies are responsive to public health concern? (e.g., Ranking top 10) 

Hypothesis:  

We believe that sun block products will contain more toxic chemical reports while baby products will have less toxic chemical reports due to its publicly known stereotype of each kind of cosmetics. 

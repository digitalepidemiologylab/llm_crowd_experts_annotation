# Import libraries
import pandas as pd
import numpy as np
from llama_index.llms import Ollama

# Getting mistral and dataset
llm = Ollama(model="mistral", temperature = 0.8)
df = pd.read_csv("tweets_for_annotation.csv").reset_index()
print(df.info())

print(len(df))

# Setting up the columns for the database with the sentiment
id_mistral = []
sent_mistral = []
text = []

# Prompts

prompt_0 = "Give me the sentiment regarding vaccination in the tweet text delimited by triple backticks. \ Use one of the following words: neutral, negative and positive. \ ```{TWEET_TEXT_HERE}```"
        

# Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_0.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mistral.append(response.text)
    id_mistral.append(df.iloc[i,1])
    print(i) # Check how many tweets are left

# Save the sentiment in a file and export it
df_mistral = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
        columns = ['id', 'text', 'sentiment_mistral'])

df_mistral['prompt'] = 0

print(df_mistral.info())
print(df_mistral.head())
print(len(df_mistral))

df_mistral.to_csv('mistral_sentiment_prompt0.csv')


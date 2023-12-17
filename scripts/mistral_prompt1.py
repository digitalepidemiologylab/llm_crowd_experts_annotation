# Import libraries
import pandas as pd
import numpy as np
from llama_index.llms import Ollama

# Getting mistral and dataset
llm = Ollama(model="mistral", temperature = 0.8)
df = pd.read_csv("tweets_for_annotation.csv").reset_index()
print(df.info())

print(len(df))

# Getting prompts
prompts = pd.read_csv('prompts_text.csv')

# Setting up the columns for the database with the sentiment
id_mistral = []
sent_mistral = []
text = []

# Prompts

prompt_1 = prompts.iloc[1,1]
        
# Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_1.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mistral.append(response.text)
    id_mistral.append(df.iloc[i,1])
    print(i) # Check how many tweets are left

# Save the sentiment in a file and export it
df_mistral = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
        columns = ['id', 'text', 'sentiment_mistral'])

df_mistral['prompt'] = 1

print(df_mistral.info())
print(df_mistral.head())
print(len(df_mistral))

df_mistral.to_csv('mistral_sentiment_prompt1.csv')


# Setting up the columns for the database with the sentiment
id_mistral = []
sent_mistral = []
text = []

# Prompts
prompt_3 = prompts.iloc[2,1]

# Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_3.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mistral.append(response.text)
    id_mistral.append(df.iloc[i,1])
    print(i) # Check how many tweets are left

# Save the sentiment in a file and export it
df_mistral3 = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
        columns = ['id', 'text', 'sentiment_mistral'])

df_mistral3['prompt'] = 3

print(df_mistral.info())
print(df_mistral.head())
print(len(df_mistral))

df_mistral3.to_csv('mistral_sentiment_prompt3.csv')



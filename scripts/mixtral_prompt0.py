# Import libraries
import pandas as pd
from llama_index.llms import Ollama

# Getting mistral and dataset
llm = Ollama(
        base_url = "http://iccluster133.iccluster.epfl.ch:12923",
        model="mixtral", 
        temperature = 0.8,
        request_timeout = 300.0
        )

df = pd.read_csv("tweets_for_annotation.csv").reset_index()
print(df.info())

print(len(df))

# Setting up the columns for the database with the sentiment
id_mixtral = []
sent_mixtral = []
text = []

# Prompts

prompt_0 = "Give me the sentiment regarding vaccination in the tweet text delimited by triple backticks. \ Use one of the following words: neutral, negative and positive. \ ```{TWEET_TEXT_HERE}```"
        

# Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_0.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt0") # Check how many tweets are left

# Save the sentiment in a file and export it
df_mixtral = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral['prompt'] = 0

print(df_mixtral.info())
print(df_mixtral.head())
print(len(df_mixtral))

df_mixtral.to_csv('mixtral_sentiment_prompt0.csv')


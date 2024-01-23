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

# Getting prompts 
prompts = pd.read_csv("prompts_text.csv")
prompt_0 = prompts.iloc[0,1]
prompt_1 = prompts.iloc[1,1]
prompt_3 = prompts.iloc[2,1]
prompt_4 = prompts.iloc[3,1]
prompt_5 = prompts.iloc[4,1]
prompt_6 = prompts.iloc[5,1]
prompt_7 = prompts.iloc[6,1]
prompt_8 = prompts.iloc[7,1]


# Setting up the columns for the database with the sentiment
id_mixtral = []
sent_mixtral = []
text = []

# Prompt 0    

## Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_0.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt0") # Check how many tweets are left

## Save the sentiment in a file and export it
df_mixtral_0 = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral_0['prompt'] = 0

print(df_mixtral_0.info())
print(df_mixtral_0.head())
print(len(df_mixtral_0))

df_mixtral_0.to_csv('mixtral_sentiment_prompt0.csv')

# Prompt 1     

## Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_1.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt1") # Check how many tweets are left

## Save the sentiment in a file and export it
df_mixtral_1 = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral_1['prompt'] = 1

print(df_mixtral_1.info())
print(df_mixtral_1.head())
print(len(df_mixtral_1))

df_mixtral_1.to_csv('mixtral_sentiment_prompt1.csv')

# Prompt 3     

## Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_3.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt3") # Check how many tweets are left

## Save the sentiment in a file and export it
df_mixtral_3 = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral_3['prompt'] = 3

print(df_mixtral_3.info())
print(df_mixtral_3.head())
print(len(df_mixtral_3))

# Prompt 4   

## Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_4.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mixtral.append(response)
    id_mixtral.append(df.iloc[i,1])
    print(i, "prompt4") # Check how many tweets are left

## Save the sentiment in a file and export it
df_mixtral_4 = pd.DataFrame(list(zip(id_mixtral, text, sent_mixtral)),
        columns = ['id', 'text', 'sentiment_mixtral'])

df_mixtral_4['prompt'] = 4

print(df_mixtral_4.info())
print(df_mixtral_4.head())
print(len(df_mixtral_4))

df_mixtral_4.to_csv('mixtral_sentiment_prompt4.csv')
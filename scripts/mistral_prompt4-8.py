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

prompt_4 = prompts.iloc[3,1]

prompt_5 = prompts.iloc[4,1]

prompt_6 = prompts.iloc[5,1]

prompt_7 = prompts.iloc[6,1]

prompt_8 = prompts.iloc[7,1]

# Prompt 5        
# Setting up the columns for the database with the sentiment
#id_mistral = []
#sent_mistral = []
#text = []

## Loop to get the sentiment from mistral

#for i in range(0, 999):
#    prompt_i = prompt_5.replace('TWEET_TEXT_HERE', df.iloc[i,2])
#    response = llm.complete(prompt_i)
#    text.append(df.iloc[i,2])
#    sent_mistral.append(response.text)
#    id_mistral.append(df.iloc[i,1])
#    print('Tweet #:', i, '  Prompt: 5') # Check how many tweets are left

## Save the sentiment in a file and export it
#df_mistral5 = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
#        columns = ['id', 'text', 'sentiment_mistral'])

#df_mistral5['prompt'] = 5

#df_mistral5.to_csv('mistral_sentiment_prompt5.csv')

#print(df_mistral5.info())
#print(df_mistral5.head())
#print('Length mistral5:', len(df_mistral5))

# Prompt 4
## Setting up the columns for the database with the sentiment
#id_mistral = []
#sent_mistral = []
#text = []

## Loop to get the sentiment from mistral

#for i in range(0, 999):
#    prompt_i = prompt_4.replace('TWEET_TEXT_HERE', df.iloc[i,2])
#    response = llm.complete(prompt_i)
#    text.append(df.iloc[i,2])
#    sent_mistral.append(response.text)
#    id_mistral.append(df.iloc[i,1])
#    print(i, 4) # Check how many tweets are left

## Save the sentiment in a file and export it
#df_mistral4 = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
#        columns = ['id', 'text', 'sentiment_mistral'])

#df_mistral4['prompt'] = 4

#df_mistral4.to_csv('mistral_sentiment_prompt4.csv')

#print(df_mistral4.info())
#print(df_mistral4.head())
#print('Length mistral4:', len(df_mistral4))


# Prompt 8
## Setting up the columns for the database with the sentiment
id_mistral = []
sent_mistral = []
text = []

## Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_8.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mistral.append(response.text)
    id_mistral.append(df.iloc[i,1])
    print(i, 8) # Check how many tweets are left


## Save the sentiment in a file and export it
df_mistral8 = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
        columns = ['id', 'text', 'sentiment_mistral'])

df_mistral8['prompt'] = 8

df_mistral8.to_csv('mistral_sentiment_prompt8.csv')

print(df_mistral8.info())
print(df_mistral8.head())
print('Length mistral8:', len(df_mistral8))

df_mistral8.to_csv('mistral_sentiment_prompt8.csv')

# Prompt 6
## Setting up the columns for the database with the sentiment
id_mistral = []
sent_mistral = []
text = []

## Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_6.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mistral.append(response.text)
    id_mistral.append(df.iloc[i,1])
    print(i, 6) # Check how many tweets are left


## Save the sentiment in a file and export it
df_mistral6 = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
        columns = ['id', 'text', 'sentiment_mistral'])

df_mistral6['prompt'] = 6

df_mistral6.to_csv('mistral_sentiment_prompt6.csv')

print(df_mistral6.info())
print(df_mistral6.head())
print('Length mistral6:', len(df_mistral6))


# Prompt 7
## Setting up the columns for the database with the sentiment
id_mistral = []
sent_mistral = []
text = []

## Loop to get the sentiment from mistral

for i in range(0, 999):
    prompt_i = prompt_7.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_mistral.append(response.text)
    id_mistral.append(df.iloc[i,1])
    print(i, 7) # Check how many tweets are left

## Save the sentiment in a file and export it
df_mistral7 = pd.DataFrame(list(zip(id_mistral, text, sent_mistral)),
        columns = ['id', 'text', 'sentiment_mistral'])

df_mistral7['prompt'] = 7

df_mistral7.to_csv('mistral_sentiment_prompt7.csv')

print(df_mistral7.info())
print(df_mistral7.head())
print('Length mistral7:', len(df_mistral7))




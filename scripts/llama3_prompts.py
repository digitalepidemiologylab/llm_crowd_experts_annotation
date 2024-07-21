# Import libraries
import pandas as pd
import numpy as np
from llama_index.llms.ollama import Ollama

# Getting llama3 and dataset
llm = Ollama(model="llama3", temperature = 0.8, request_timeout = 600)
df = pd.read_csv("tweets_for_annotation.csv").reset_index()
print(df.info())

print(len(df))

df_new = pd.read_csv("tweets_for_annotation_llama3.csv").reset_index()
print(len(df_new))
print(df_new.info())

# Getting prompts
prompts = pd.read_csv('prompts_text.csv')

prompt_0 = prompts.iloc[0,1]

prompt_1 = prompts.iloc[1,1]

prompt_3 = prompts.iloc[2,1]

prompt_4 = prompts.iloc[3,1]

prompt_5 = prompts.iloc[4,1]

prompt_6 = prompts.iloc[5,1]

prompt_7 = prompts.iloc[6,1]

prompt_8 = prompts.iloc[7,1]

# Prompt 0       
# Setting up the columns for the database with the sentiment
id_llama = []
sent_llama = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_0.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama.append(response.text)
    id_llama.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 0') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama30 = pd.DataFrame(list(zip(id_llama, text, sent_llama)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama30['prompt'] = 0

df_llama30.to_csv('llama3_sentiment_prompt0_2.csv')

print(df_llama30.info())
print(df_llama30.head())
print('Length llama30:', len(df_llama30))
  
# Prompt 1        
# Setting up the columns for the database with the sentiment
id_llama3 = []
sent_llama3 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_1.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama3.append(response.text)
    id_llama3.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 1') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama31 = pd.DataFrame(list(zip(id_llama3, text, sent_llama3)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama31['prompt'] = 1

df_llama31.to_csv('llama3_sentiment_prompt1_2.csv')

print(df_llama31.info())
print(df_llama31.head())
print('Length llama31:', len(df_llama31))

# Prompt 3       
# Setting up the columns for the database with the sentiment
id_llama3 = []
sent_llama3 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_3.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama3.append(response.text)
    id_llama3.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 3') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama33 = pd.DataFrame(list(zip(id_llama3, text, sent_llama3)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama33['prompt'] = 3

df_llama33.to_csv('llama3_sentiment_prompt3_2.csv')

print(df_llama33.info())
print(df_llama33.head())
print('Length llama33:', len(df_llama33))

# Prompt 4       
# Setting up the columns for the database with the sentiment
id_llama4 = []
sent_llama4 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_4.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama4.append(response.text)
    id_llama4.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 4') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama34 = pd.DataFrame(list(zip(id_llama4, text, sent_llama4)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama34['prompt'] = 4

df_llama34.to_csv('llama3_sentiment_prompt4_2.csv')

print(df_llama34.info())
print(df_llama34.head())
print('Length llama34:', len(df_llama34))

# Prompt 5        
# Setting up the columns for the database with the sentiment
id_llama5 = []
sent_llama5 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_5.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama5.append(response.text)
    id_llama5.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 5') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama35 = pd.DataFrame(list(zip(id_llama5, text, sent_llama5)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama35['prompt'] = 5

df_llama35.to_csv('llama3_sentiment_prompt5_2.csv')

print(df_llama35.info())
print(df_llama35.head())
print('Length llama35:', len(df_llama35))

# Prompt 6
## Setting up the columns for the database with the sentiment
id_llama6 = []
sent_llama6 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_6.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama6.append(response.text)
    id_llama6.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 6') # Check how many tweets are left


## Save the sentiment in a file and export it
df_llama36 = pd.DataFrame(list(zip(id_llama6, text, sent_llama6)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama36['prompt'] = 6

df_llama36.to_csv('llama3_sentiment_prompt6_2.csv')

print(df_llama36.info())
print(df_llama36.head())
print('Length llama36:', len(df_llama36))


# Prompt 7
## Setting up the columns for the database with the sentiment
id_llama7 = []
sent_llama7 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_7.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama7.append(response.text)
    id_llama7.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 7') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama37 = pd.DataFrame(list(zip(id_llama7, text, sent_llama7)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama37['prompt'] = 7

df_llama37.to_csv('llama3_sentiment_prompt7_2.csv')

print(df_llama37.info())
print(df_llama37.head())
print('Length llama37:', len(df_llama37))


# Prompt 8
## Setting up the columns for the database with the sentiment
id_llama8 = []
sent_llama8 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_8.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama8.append(response.text)
    id_llama8.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 8') # Check how many tweets are left


## Save the sentiment in a file and export it
df_llama38 = pd.DataFrame(list(zip(id_llama3, text, sent_llama3)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama38['prompt'] = 8

df_llama38.to_csv('llama3_sentiment_prompt8_2.csv')

print(df_llama38.info())
print(df_llama38.head())
print('Length llama38:', len(df_llama38))


######################
# Llama 3 70b code
import pandas as pd
import numpy as np
from llama_index.llms.ollama import Ollama


llm = Ollama(
    base_url = "http://iccluster051.iccluster.epfl.ch:11434",
    model="llama3:70b", 
    temperature = 0.8,
    request_timeout = 3600.0
    )

df = pd.read_csv("tweets_for_annotation.csv").reset_index()
print(df.info())

print(len(df))

# Getting prompts
prompts = pd.read_csv('prompts_text.csv')

prompt_0 = prompts.iloc[0,1]

prompt_1 = prompts.iloc[1,1]

prompt_3 = prompts.iloc[2,1]

prompt_4 = prompts.iloc[3,1]

prompt_5 = prompts.iloc[4,1]

prompt_6 = prompts.iloc[5,1]

prompt_7 = prompts.iloc[6,1]

prompt_8 = prompts.iloc[7,1]

# Prompt 0       
# Setting up the columns for the database with the sentiment
id_llama = []
sent_llama = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_0.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama.append(response.text)
    id_llama.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 0') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama30 = pd.DataFrame(list(zip(id_llama, text, sent_llama)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama30['prompt'] = 0

df_llama30.to_csv('llama3_70b_sentiment_prompt0.csv')

print(df_llama30.info())
print(df_llama30.head())
print('Length llama30:', len(df_llama30))

text_prompt0 = text

# Prompt 1        
# Setting up the columns for the database with the sentiment
id_llama31 = []
sent_llama31 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_1.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama31.append(response.text)
    id_llama31.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 1') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama31 = pd.DataFrame(list(zip(id_llama31, text, sent_llama31)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama31['prompt'] = 1

df_llama31.to_csv('llama3_70b_sentiment_prompt1.csv')

print(df_llama31.info())
print(df_llama31.head())
print('Length llama31:', len(df_llama31))

# Prompt 3       
# Setting up the columns for the database with the sentiment
id_llama33 = []
sent_llama33 = []
text33 = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_3.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text33.append(df.iloc[i,2])
    sent_llama33.append(response.text)
    id_llama33.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 3') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama33 = pd.DataFrame(list(zip(id_llama33, text33, sent_llama33)),
        columns = ['id', 'text', 'sentiment_llama3'])

df_llama33['prompt'] = 3

df_llama33.to_csv('llama3_70b_sentiment_prompt3.csv')

print(df_llama33.info())
print(df_llama33.head())
print('Length llama33:', len(df_llama33))

# Prompt 4       
# Setting up the columns for the database with the sentiment
id_llama4 = []
sent_llama4 = []
text = []

## Loop to get the sentiment from llama3
for i in range(732, 999):
    prompt_i = prompt_4.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama4.append(response.text)
    id_llama4.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 4') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama34 = pd.DataFrame(list(zip(id_llama4, text, sent_llama4)),
                          columns = ['id', 'text', 'sentiment_llama3'])

df_llama34['prompt'] = 4

df_llama34.to_csv('llama3_70b_sentiment_prompt4.csv')

print(df_llama34.info())
print(df_llama34.head())
print('Length llama34:', len(df_llama34))

# Prompt 5        
# Setting up the columns for the database with the sentiment
id_llama5 = []
sent_llama5 = []
text = []

## Loop to get the sentiment from llama3
# Start on 24/06 at 10.32 (estimated 49.5 h)
for i in range(999, 1000):
    prompt_i = prompt_5.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama5.append(response.text)
    id_llama5.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 5') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama35 = pd.DataFrame(list(zip(id_llama5, text, sent_llama5)),
                          columns = ['id', 'text', 'sentiment_llama3'])

df_llama35['prompt'] = 5

df_llama35.to_csv('llama3_70b_sentiment_prompt5.csv')

print(df_llama35.info())
print(df_llama35.head())
print('Length llama35:', len(df_llama35))

# Prompt 6
## Setting up the columns for the database with the sentiment
id_llama6 = []
sent_llama6 = []
text = []

## Loop to get the sentiment from llama3

for i in range(401, 999):
    prompt_i = prompt_6.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama6.append(response.text)
    id_llama6.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 6') # Check how many tweets are left


## Save the sentiment in a file and export it
df_llama36 = pd.DataFrame(list(zip(id_llama6, text, sent_llama6)),
                          columns = ['id', 'text', 'sentiment_llama3'])

df_llama36['prompt'] = 6

df_llama36.to_csv('llama3_70b_sentiment_prompt6.csv')

print(df_llama36.info())
print(df_llama36.head())
print('Length llama36:', len(df_llama36))


# Prompt 7
## Setting up the columns for the database with the sentiment
id_llama7 = []
sent_llama7 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_7.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama7.append(response.text)
    id_llama7.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 7') # Check how many tweets are left

## Save the sentiment in a file and export it
df_llama37 = pd.DataFrame(list(zip(id_llama7, text, sent_llama7)),
                          columns = ['id', 'text', 'sentiment_llama3'])

df_llama37['prompt'] = 7

df_llama37.to_csv('llama3_70b_sentiment_prompt7.csv')

print(df_llama37.info())
print(df_llama37.head())
print('Length llama37:', len(df_llama37))


# Prompt 8
## Setting up the columns for the database with the sentiment
id_llama8 = []
sent_llama8 = []
text = []

## Loop to get the sentiment from llama3

for i in range(0, 999):
    prompt_i = prompt_8.replace('TWEET_TEXT_HERE', df.iloc[i,2])
    response = llm.complete(prompt_i)
    text.append(df.iloc[i,2])
    sent_llama8.append(response.text)
    id_llama8.append(df.iloc[i,1])
    print('Tweet #:', i, '  Prompt: 8') # Check how many tweets are left


## Save the sentiment in a file and export it
df_llama38 = pd.DataFrame(list(zip(id_llama8, text, sent_llama8)),
                          columns = ['id', 'text', 'sentiment_llama3'])

df_llama38['prompt'] = 8

df_llama38.to_csv('llama3_70b_sentiment_prompt8.csv')

print(df_llama38.info())
print(df_llama38.head())
print('Length llama38:', len(df_llama38))
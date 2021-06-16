#!/usr/bin/env python
# coding: utf-8

# In[1]:


# load jsonl file into a dataframe
import jsonlines
import pandas as pd
favorite_tweets_list = []
with jsonlines.open('favorite-tweets.jsonl') as f:
    for line in f.iter():
        favorite_tweets_list.append(line)
favorite_tweets_df = pd.DataFrame(favorite_tweets_list)    


# In[2]:


# have a view of the dataframe
print(favorite_tweets_df.info())
print(favorite_tweets_df.describe())
print(favorite_tweets_df.head())


# In[3]:


# get a new dataframe from favorite_tweets_df with the columns I am interested in
# Text, UserName, CreatedAt
df = favorite_tweets_df[['Text', 'UserName', 'CreatedAt']]
import datetime
timeformat = lambda x: datetime.datetime.strptime(x,'%B %d, %Y at %I:%M%p')# format CreatedAt
df['CreatedAt'] = df['CreatedAt'].map(timeformat)
df.head()


# In[4]:


# Question1: Find the user who sent most tweets and count the number of the tokens that the users sent per day
# the user who sent most tweets
most_user = df.UserName.mode()[0]
print("The user who sent most tweets:", most_user)
Q1_df = df.loc[df['UserName'] == most_user]


# In[5]:


# count the number of the tokens of each tweets
import nltk
from nltk.tokenize import TweetTokenizer
tk = TweetTokenizer(preserve_case=False)
t = lambda x: len(tk.tokenize(x))
Q1_df['tokens_count'] = Q1_df['Text'].map(t)


# In[6]:


# sum the number of the tokens that the users sent per day
Q1_df = Q1_df.groupby(pd.Grouper(key='CreatedAt',freq='D')).agg({'tokens_count': 'sum'})
Q1_df.to_csv('Q1.csv',index_label=Q1_df.columns.name)


# In[7]:


print(Q1_df)


# In[8]:


# Question2: Separate df into two categories: positive, negative
from textblob import TextBlob
s = lambda x: TextBlob(x).sentiment.polarity >0
df['if_positive'] = df['Text'].map(s)


# In[9]:


# bar plot of the number of positive tweets and negative tweets per three months
import matplotlib
import matplotlib.pyplot as plt
Q2_df = df.groupby(pd.Grouper(key='CreatedAt',freq='3M')).agg({'if_positive': ['sum', 'count']})
Q2_df[('if_not_positive', 'sum')] = Q2_df[('if_positive', 'count')]-Q2_df[('if_positive', 'sum')]
Q2_df = Q2_df.drop(('if_positive', 'count'), 1)
Q2_df.plot(kind='bar',alpha=0.75, rot=90)
Q2_df.to_csv('Q2.csv',index_label=Q2_df.columns.name)


# In[10]:


# Question3: compare the sentiment between saladinahmed's tweets and OhNoSheTwitnt's tweets
Q3_sal_df = df.loc[df['UserName'] == 'saladinahmed']
Q3_Oh_df = df.loc[df['UserName'] == 'OhNoSheTwitnt']
print('Sentiment comparison between saladinahmed\'s tweets and OhNoSheTwitnt\'s tweets')
from tabulate import tabulate
print(tabulate([['saladinahmed', len(Q3_sal_df), len(Q3_sal_df.loc[Q3_sal_df['if_positive']==True])/len(Q3_sal_df)], ['OhNoSheTwitnt', len(Q3_Oh_df), len(Q3_Oh_df.loc[Q3_Oh_df['if_positive']==True])/len(Q3_Oh_df)]], headers=['UserName', 'Sum of tweets', 'Positive rating']))
Q3_df = {'UserName': ['saladinahmed', 'OhNoSheTwitnt'], 'Sum of tweets': [len(Q3_sal_df), len(Q3_Oh_df)], 'Positive rating': [len(Q3_sal_df.loc[Q3_sal_df['if_positive']==True])/len(Q3_sal_df), len(Q3_Oh_df.loc[Q3_Oh_df['if_positive']==True])/len(Q3_Oh_df)]}
Q3_df = pd.DataFrame.from_dict(Q3_df)
Q3_df.to_csv('Q3.csv',index_label=Q3_df.columns.name)


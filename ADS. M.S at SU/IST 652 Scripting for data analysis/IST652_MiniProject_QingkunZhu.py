#!/usr/bin/env python
# coding: utf-8

# In[1]:


# IST652_MiniProject
# Qingkun
# 2020/10/12

# Structured Data Processing:
'''
The main outline of your assignment is to write a program that will read in the data from a file, such as a .csv, .tsv, .txt, or  file saved from excel.  
This will be in a format that is structured with lines of data representing one type of unit (e.g. one donor in the donors file). 
Your program must represent the data using the Python data structures we learned.  
You may choose the overall structure to be one of the following:
    •Dictionaries, lists, or tuples
    •NumPy Arrays (this topic will be covered in class in Weeks 4 and 5)
    •pandas DataFrame (this topic will be covered in class on Weeks 6 and 7)
    •Or some combination of the above

'''
# read the Donors_Data.csv file
import csv
import os
import sys

os.chdir('E:/StudyMaterial/material-iShool/IST 652 Scripting for data analysis/MiniProject1')
infile = 'Donors_Data.csv'

# create new empty list
donorsList = []
# open the csv file and import each row as a dictionary
with open(infile, 'r') as csvfile:
    # the csv file reader returns a list of the csv items on each line
    donorReader = csv.reader(csvfile,  dialect='excel', delimiter=',')

    # from each line, a list of row items, put each element in a dictionary
    #   with a key representing the data
    for line in donorReader:
        #skip lines without data, specific for each file to catch non-data lines
        if line[0] == '' or line[0].startswith('Row Id') or line[0].startswith('*'):
            continue
        else:
            try:
                # create a dictionary for each donor
                donor = {}
                # create a list for the names of the columns
                columns_name = ['Row Id', 'Row Id.', 'zipconvert_2', 'zipconvert_3', 'zipconvert_4','zipconvert_5', 
                                'homeowner dummy', 'NUMCHLD', 'INCOME', 'gender dummy', 'WEALTH', 'HV', 
                                'Icmed', 'Icavg', 'IC15', 'NUMPROM', 'RAMNTALL', 'MAXRAMNT', 
                                'LASTGIFT', 'totalmonths', 'TIMELAG', 'AVGGIFT', 'TARGET_B', 'TARGET_D']
                # add each attribute of data under a key representing that column data
                for i in range(len(columns_name)):
                    donor[columns_name[i]] = line[i]

                # add this state to the list
                donorsList.append(donor)
            except IndexError:
                print ('Error: ', line)
# close the file
csvfile.close()
# print the number of the donors
print ('Read', len(donorsList), 'donors data') # 3120 donors

# print a few fields from first 30 rows of the donors read from the file
for donor in donorsList[:30]:
    print ('Row Id:', donor['Row Id'], ' zipconvert_2: ', donor['zipconvert_2'], ' NUMCHLD: ', donor['NUMCHLD'],  ' TIMELAG: ', donor['TIMELAG'], ' TARGET_D: ', donor['TARGET_D'])


# In[2]:


# select the following target attributes and create a dataframe for them:
# Row Id, Row Id., homeowner dummy, NUMCHLD, gender dummy, WEALTH, HV, Icmed, Icavg, IC15

# create the lists for the target attributes
Row_Id = []
Row_Id_dot = []
homeowner_dummy = []
NUMCHLD = []
gender_dummy = []
WEALTH = []
HV = []
Icmed = []
Icavg = []
IC15 = []
for donor in donorsList:
    Row_Id.append(donor['Row Id'])#string
    Row_Id_dot.append(donor['Row Id.'])#string
    homeowner_dummy.append(donor['homeowner dummy'])#factor(string)
    NUMCHLD.append(int(donor['NUMCHLD']))#int
    gender_dummy.append(donor['gender dummy'])#factor(string)
    WEALTH.append(donor['WEALTH'])#factor(string)
    HV.append(float(donor['HV']))#float, Average Home Value in potential donor's neighborhood   in $ hundreds
    Icmed.append(float(donor['Icmed']))#float, Median Family Income in potential donor's neighborhood in $ hundreds
    Icavg.append(float(donor['Icavg']))#float, Average Family Income in potential donor's neighborhood in hundreds
    IC15.append(float(donor['IC15']))#float, Percent earning less than 15K in potential donor's neighborhood
    
# preparation for data cleaning: duplicate data, wrong data, and missing values
# are there any duplicate values of Row Id or Row Id.?
print('Row Id has duplicate values: ', len(Row_Id) != len(set(Row_Id))) #False
print('Row Id. has duplicate values: ', len(Row_Id_dot) != len(set(Row_Id_dot))) #False
# there is no duplicate values of Row Id or Row Id.

# define a function for counting wrong data in a list based on the range of the list
def count_wrong(l, r):
    i = 0
    for element in l:
        if element not in r:
            i = i +1
    return i
# are there any wrong data of homeowner dummy: 1 = homeowner, 0 = not a homeowner
print('the number of wrong data of homeowner dummy: ', count_wrong(homeowner_dummy, ['1', '0'])) # no wrong data of homeowner dummy
# are there any wrong data of NUMCHLD: The Guinness World Records is 69
print('the number of wrong data of NUMCHLD: ', count_wrong(NUMCHLD, range(70))) # no wrong data of NUMCHLD
# are there any wrong data of gender dummy: 0 = Male, 1 = Female
print('the number of wrong data of gender dummy: ', count_wrong(gender_dummy, ['0', '1']))# no wrong data of gender dummy
# are there any wrong data of WEALTH: The segments are denoted 0-9, with 9 being the highest wealth group and zero being the lowest.
print('the number of wrong data of WEALTH: ', count_wrong(WEALTH, ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'])) # no wrong data of WEALTH
# check the outliers of HV, Icmed, Icavg, IC15
import numpy as np
import matplotlib.pyplot as plt
HV_array = np.array(HV)
fig1, ax1 = plt.subplots()
ax1.set_title('HV Box_plot')
ax1.boxplot(HV_array)# HV has a number of outliers on the high side, but they are not wrong data probably
Icmed_array = np.array(Icmed)
fig1, ax2 = plt.subplots()
ax2.set_title('Icmed Box_plot')
ax2.boxplot(Icmed_array)# Icmed has a number of outliers on the high side, but they are not wrong data probably
Icavg_array = np.array(Icavg)
fig1, ax3 = plt.subplots()
ax3.set_title('Icavg Box_plot')
ax3.boxplot(Icavg_array)# Icavg has a number of outliers on the high side, but they are not wrong data probably
IC15_array = np.array(IC15)
fig1, ax4 = plt.subplots()
ax4.set_title('IC15 Box_plot')
ax4.boxplot(IC15_array)# IC15 has a number of outliers on the high side, but they are not wrong data probably

# check missing values
print('Row Id, Row Id., homeowner dummy, NUMCHLD, gender dummy, WEALTH', 'HV', 'Icmed', 'Icavg', 'IC15')
print(len(Row_Id), len(Row_Id_dot), len(homeowner_dummy), len(NUMCHLD), len(gender_dummy), len(WEALTH), len(HV), len(Icmed), len(Icavg), len(IC15))
# all the number of the lists are 3120, so there is no missing values

# create a dataframe for the attributes
import pandas as pd
from pandas import DataFrame
donors_mini_df = DataFrame (Row_Id,columns=['Row_Id'])
donors_mini_df['Row Id.'] = Row_Id_dot
donors_mini_df['homeowner dummy'] = homeowner_dummy
donors_mini_df['NUMCHLD'] = NUMCHLD
donors_mini_df['gender dummy'] = gender_dummy
donors_mini_df['WEALTH'] = WEALTH
donors_mini_df['HV'] = HV
donors_mini_df['Icmed'] = Icmed
donors_mini_df['Icavg'] = Icavg
donors_mini_df['IC15'] = IC15
print(donors_mini_df.head())
print(donors_mini_df.shape)
print(donors_mini_df.dtypes)


# In[3]:


# Three questions:
# Question0.What is the correlation matrix of homeowner dummy, NUMCHLD, gender dummy, and WEALTH
# For this question, I use a function,corr from pandas directly
Q1_df = donors_mini_df[['homeowner dummy', 'NUMCHLD', 'gender dummy', 'WEALTH', 'HV', 'Icmed', 'Icavg', 'IC15']].apply(pd.to_numeric)
corrMatrix = Q1_df.corr(method ='pearson')
print(corrMatrix)
corrMatrix.to_csv("E:/StudyMaterial/material-iShool/IST 652 Scripting for data analysis/MiniProject1/CorrelationMatrix.csv")
# Correlation matrix visualization
import seaborn as sns
from scipy.stats import norm
cg = sns.clustermap(corrMatrix, cmap ="YlGnBu", linewidths = 0.2); 
plt.setp(cg.ax_heatmap.yaxis.get_majorticklabels(), rotation = 0) 
cg 


# In[4]:


# Question1.For different rank of Icmed in different gender, what is the Icavg of that type?
# Icmed: Median Family Income in potential donor's neighborhood in $ hundreds.
donors_mini_df['Icmed'].describe()# min,0 25%,278 50%,356 75%,465 max,1500
# define the Icmed ranks and the names of the ranks
Icmed_ranks = [x for x in range(0,1600,150)]
Icmed_names = ['0~150','151~300','301~450','451~600','601~750','751~900','901~1050','1051~1200','1201~1350','1351~1500']
# get the categories of Icmed
Icmed_categories = pd.cut(donors_mini_df['Icmed'], Icmed_ranks, labels=Icmed_names)
donors_mini_df['Icmed_categories'] = Icmed_categories
# multi-group by Icmed_categories and gender and return the mean of Icavg
Q1_grouped = donors_mini_df.groupby(['gender dummy', 'Icmed_categories'], as_index=False)['Icavg'].mean()
# pivot of the multi-grouped result
Q1_pivot = Q1_grouped.pivot(index='Icmed_categories', columns='gender dummy',values='Icavg')
# output the pivot as a csv file
p1 = pd.DataFrame(columns=Q1_pivot.columns, index=[Q1_pivot.index.name]).append(Q1_pivot)
p1.to_csv('E:/StudyMaterial/material-iShool/IST 652 Scripting for data analysis/MiniProject1/Q1_pivot.csv',index_label=Q1_pivot.columns.name)


# In[5]:


# Question2. what are the association rules between homeowner dummy, gender dummy, Icmed_categories
from mlxtend.frequent_patterns import apriori, association_rules
Q2_df = donors_mini_df[['homeowner dummy','gender dummy']].apply(pd.to_numeric)
# define a function to return the 0/1 for Icmed_categories
def Icmed_cat(x, low, high):
    if x > low and x <=high:
        return 1
    else:
        return 0
for i in range(len(Icmed_names)):
    Q2_df[Icmed_names[i]] = donors_mini_df.apply(lambda row: Icmed_cat(row['Icmed'], Icmed_ranks[i], Icmed_ranks[i+1]), axis=1)
# there is a SettingWithCopyWarning, I do not know how to fix it. But the result is right.
# get the association rules
freq_items = apriori(Q2_df, min_support=0.2, use_colnames=True, verbose=1)
rules = association_rules(freq_items, metric="confidence", min_threshold=0.6)
rules.to_csv('E:/StudyMaterial/material-iShool/IST 652 Scripting for data analysis/MiniProject1/Q2_association_rules.csv', index=False)


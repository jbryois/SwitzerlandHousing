#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sun Mar 10 13:45:25 2019

@author: julienbryios
"""

import requests
import bs4
import pandas as pd
import numpy as np
import os
import datetime

def parse_anibis():
    
    '''Function parsing the housing section of the Anibis website and 
    creates a data frame with the listings'''

    # Get date at which the webscrapping was done
    now = datetime.datetime.now()
    now_date = now.strftime("%d-%m-%Y")
    
    # URL to be webscrapped
    url_base = 'https://www.anibis.ch'
    url_end = '/fr/immobilier-immobilier-locations--410/advertlist.aspx'

    # Create a dictionary for the different variables of the listings
    appartmentDic = {'title':[],
                     'price':[],
                     'surface':[],
                     'rooms':[],
                     'object':[],
                     'location':[],
                     'canton':[],
                     'date':[]
                     }
    
    # While there is a next page, continue
    while url_end != '':
        
        url = url_base + url_end
        res = requests.get(url)
        res.raise_for_status()
        
        Soup = bs4.BeautifulSoup(res.text,features="lxml")
    
        for appartment in Soup.find_all('div', attrs={'class': 'listing-info'}):
            for d in appartment.descendants:
                if d.name == 'a' and d.get('class', '') == ['listing-title']:
                    title = d.text.strip()
                if d.name == 'div' and d.get('class', '') == ['listing-price']:
                    price = d.text.strip()
                if d.name == 'span' and d.get('class', '') == ['item-name']:
                    name = d.text.strip()
                if d.name == 'span' and d.get('class', '') == ['item-value']:
                    if name == 'Surface habitable:':
                        surface = d.text.strip()
                        name = ''
                    if name == 'Pièces:':
                        rooms = d.text.strip()
                        name = ''
                    if name == 'Objet:':
                        object_anibis = d.text.strip()
                        name = ''
                if d.name == 'li' and d.get('class', '') == ['horizontal-list-item', 'item-location']:
                    location = d.text.strip()
                if d.name == 'li' and d.get('class', '') == ['horizontal-list-item', 'item-category']:
                    canton = d.text.strip()
                if d.name == 'li' and d.get('class', '') == ['horizontal-list-item', 'item-date']:
                    date = d.text.strip() 
    
            #Add all informations to the dictionary
            appartmentDic['title'].append(title)
            appartmentDic['price'].append(price)
            appartmentDic['surface'].append(surface)
            appartmentDic['rooms'].append(rooms)
            appartmentDic['object'].append(object_anibis)
            appartmentDic['location'].append(location)
            appartmentDic['canton'].append(canton)
            appartmentDic['date'].append(date)
            
            #Reset all variables for the next listing
            title=price=surface=rooms=object_anibis=location=canton=date=''
    
        #Finds url for next page
        nextpage = Soup.find('a', attrs={'class': 'button icontext rooftop-right-thin icontext-right'})
        if(nextpage is None):
            url_end = ''
        else:
            url_end = nextpage.get('href')
            print(url_end)
    
    # Create data frame based on the dictionary
    d = pd.DataFrame.from_dict(appartmentDic)
    
    # Write data frame to directory and adds the date to the filename
    directory = '../Data/raw/'
    if not os.path.exists(directory):
        os.makedirs(directory)
    output = directory + 'anibis_price_' + now_date + '.txt'
    d.to_csv(output,sep='\t',index=False)
    return(d)
    
def is_number(s):
    '''tests if value is a number'''
    try:
        float(s)
        return True
    except ValueError:
        return False
    
def clean(d):
    '''Cleans a data frame by removing unecessary strings, removing listings with missing informations
    and setting price, surface and number of rooms to positive values'''
    
    print('Cleaning the data...')
    
    # Get date at which the webscrapping was done
    now = datetime.datetime.now()
    now_date = now.strftime("%d-%m-%Y")

    # Removes specific strings and white space in the price column
    d['price'].replace(inplace=True,regex=True,
     to_replace=r'''\/ Prix à discuter|Prix sur demande|Gratuit|CHF|\.–|\'| ''',value=r'')
    
    #Replace all empty strings by NaN
    d.replace('', np.nan,inplace=True)
    
    # Remove the m2 string from the surface column
    d.loc[:,'surface']= d['surface'].str.replace(' m2','')
    
    # Only keep listings where the price,surface, and rooms columns are numbers
    d = d[d['price'].apply(lambda x: is_number(x))]
    d = d[d['surface'].apply(lambda x: is_number(x))]
    d = d[d['rooms'].apply(lambda x: is_number(x))]

    # Sets the price,surface, rooms, to positive float
    d.loc[:,'price'] = d['price'].astype(float).abs()
    d.loc[:,'surface'] = d['surface'].astype(float).abs()
    d.loc[:,'rooms'] = d['rooms'].astype(float).abs()

    # Remove digits from location, set to lowercase and trim whitespace
    d.loc[:,'location'] = d['location'].str.replace('\d+', '').str.lower().str.strip()

    # Write data frame to directory, adds the date to the filename
    directory = '../Data/processed/'
    if not os.path.exists(directory):
        os.makedirs(directory)
    output = directory + 'anibis_price_' + now_date + '.txt'
    d.to_csv(output,sep='\t',index=False,na_rep='NA')
    
    print('Cleaning done!')
    return(d)
    
if __name__ == '__main__':
    
    d = parse_anibis()
    d = clean(d)
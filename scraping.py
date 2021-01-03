import os
import json
import requests
import logging
import logging.config
with open('logging_conf.json', 'r') as f:
   logConfDict = json.load(f)

logging.config.dictConfig(logConfDict)
logger = logging.getLogger('ScrapingLogger')

import pandas as pd
import numpy as np
import datetime as dt

# authorisation -------------------------------
with open('bearer', 'r') as f:
   bearer = f.readline()

headers = {#'Accept':'application/json', 
           #'Content-Type' : 'application/json', 
           'Authorization': "Bearer "+bearer}

# api settings ---------------------------------
base = 'https://api.twitter.com/'
search_tweets = '2/tweets/search/recent'
#user_tweets = '2/users/{}/tweets'.format(user_id)

params = {
   'query' : '(new resolution) -is:retweet',
   'tweet.fields' : 'id,text,lang,created_at,geo',
   'expansions' : 'geo.place_id',
   'start_time' : '2021-01-01T12:22:20Z', # 1 ssecond later that the data collected in the previous scraping
   'max_results' : 100
}

# calling a (multi-page) query ------------------------------
pages_to_search = 300
logger.debug('Searching up to {} pages with the follow paramters: {}'.format(
   pages_to_search, json.dumps(params, indent = 2)
))
pages_searched = 0
data = []
entire_history = []
while pages_searched < pages_to_search :
   logger.debug('Searching Page #{}'.format(pages_searched+1))
   resp = requests.get(base+search_tweets, params = params, headers = headers)
   resp.raise_for_status()

   entire = resp.json()
   data += entire['data']
   meta = entire['meta']
   logger.debug('Number of results returned: {}'.format(meta['result_count']))

   entire_history += [entire]
   if 'next_token' in meta.keys():
      params.update({'next_token' : meta['next_token']})
   else:
      print('Reached last page. Breaking loop.')
      break
   pages_searched += 1

logger.debug('Successfully searched {} pages and retrieved {} tweets'.format(
   len(entire_history), len(data)
))
# saving the retrived data -------------------------
utc_time = pd.Timestamp.utcnow().strftime('%Y%m%d%H%M%S')
fp = utc_time+'_data.json'
with open(fp, 'w') as f:
   logger.debug('Saving data to {}'.format(fp))
   f.writelines(str(data))

fp = utc_time+'_all.json'
with open(fp, 'w') as f:
   logger.debug('Saving data to {}'.format(fp))
   f.writelines(str(entire_history))



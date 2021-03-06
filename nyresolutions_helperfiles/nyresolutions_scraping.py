import os
os.chdir('..')
import json
import time
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
import copy

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
   'tweet.fields' : 'conversation_id,created_at,entities,geo,id,lang,public_metrics,possibly_sensitive,source,text',
   'expansions' : 'geo.place_id',
   'max_results' : 100
}

# calling a (multi-page) query ------------------------------
# pages_to_search = 300
# logger.debug('Searching up to {} pages with the follow paramters: {}'.format(
#    pages_to_search, json.dumps(params, indent = 2)
# ))
pages_searched = 0
data = []
entire_history = []
last_page = False
while not last_page :
   if (pages_searched % 350 == 0) and (pages_searched > 0):
      for i in range(16,0,-1):
         print('Sleeping: {} minutes left'.format(i))
         time.sleep(60)
   
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
      last_page = True
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
   json.dump(data, f, ensure_ascii=False)

fp = utc_time+'_all.json'
with open(fp, 'w') as f:
   logger.debug('Saving data to {}'.format(fp))
   json.dump(entire_history, f, ensure_ascii=False)

tidy_json = [None for i in range(len(data))]
for i in range(len(data)):
   print(i) if (i % 10000) == 0 else None
   d = {k : v for k, v in data[i].items() if k in 
      ['created_at','source','possibly_sensitive','text','id']}
   d.update(data[i]['public_metrics'])
   tidy_json[i] = copy.deepcopy(d)

fp = utc_time+'_tidy.json'
with open(fp, 'w') as f:
   logger.debug('Saving tidied data to {}'.format(fp))
   json.dump(tidy_json, f, ensure_ascii=False, )

json.dumps(tidy_json[0:3])
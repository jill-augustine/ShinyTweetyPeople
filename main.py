import os
import json
import requests
import logging
logging.getLogger()
logger = logging.getLogger()
logger.setLevel(logging.INFO)

with open('token', 'r') as f:
   token = f.readline()

base = 'https://api.spotify.com'
url = '/v1/browse/categories'

headers = {'Accept':'application/json', 
           'Content-Type' : 'application/json', 
           'Authorization': "Bearer "+token}
resp = requests.get(base+url, headers = headers)
resp.raise_for_status()
resp.json()

# Request a token
 
{
   "access_token": token,
   "token_type": "Bearer",
   "scope": "",
   "refresh_token": token
}



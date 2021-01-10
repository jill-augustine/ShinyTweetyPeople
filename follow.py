import os
import json
import time
import requests
from requests_oauthlib import OAuth1

import pandas as pd
import numpy as np
import datetime as dt

# authorisation -------------------------------
with open('bearer', 'r') as f:
   bearer = f.readline()

with open('app_key','r') as f:
    app_key = f.readline()

with open('app_secret', 'r') as f:
    app_secret = f.readline()

with open('oauth_token', 'r') as f:
    user_oauth_token = f.readline()

with open('oauth_token_secret', 'r') as f:
    user_oauth_token_secret = f.readline()

# auth = OAuth1('YOUR_APP_KEY', 'YOUR_APP_SECRET',
#               'USER_OAUTH_TOKEN', 'USER_OAUTH_TOKEN_SECRET')

auth = OAuth1(app_key, app_secret,
              user_oauth_token, user_oauth_token_secret)

url = 'https://api.twitter.com/1.1/friendships/create.json'

screen_names = []

for name in screen_names:
    params = {
    'screen_name' : name,
    'follow' : True
    }

    resp = requests.post(url, params = params, auth = auth)
    resp.raise_for_status()
    print(json.dumps(resp.json(), indent = 2))

    # ----------------------------
    url = 'https://hackernoon.com/200-black-women-in-tech-to-follow-on-twitter-2020-list-1uck31a9'
    resp = requests.get(url)
    resp.raise_for_status()



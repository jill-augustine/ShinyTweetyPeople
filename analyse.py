import json
import pandas as pd
import numpy as np
import glob

files = np.sort(glob.glob('*data.json'))
print('Number of JSON files found: {}'.format(len(files)))

with open('20210103141011_data.json', 'r') as f:
    data = eval(f.readline())

unnested = [dict(
    retweet_count = d['public_metrics']['retweet_count'],
    reply_count = d['public_metrics']['reply_count'],
    like_count = d['public_metrics']['like_count'],
    quote_count = d['public_metrics']['quote_count'],
    lang = d['lang'],
    source = d['source'],
    possibly_sensitive = d['possibly_sensitive'],
    text = d['text'],
    created_at = d['created_at'],
    ID = d['id'])
    for d in data]

df = pd.DataFrame.from_records(unnested)

assert df.duplicated().sum() == 0, 'Dataframe contained duplicates!'

df.to_json('20210103141011_tidy.json', orient = 'records')

[d for d in no_dups.geo.dropna().to_list() if len(d) > 1 ]

len(no_dups.geo.dropna().to_list())

print(df.head(2).to_json(orient = 'records', lines=False))

print('\n'.join([str(d) for d in data[:10]]))

with open('20210103141011_tidy.json', 'r') as f:
    one = f.readline()

two = [one[0]]
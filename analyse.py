import json
import pandas as pd
import numpy as np
import glob

files = np.sort(glob.glob('*data.json'))
print('Number of JSON files found: {}'.format(len(files)))

with open(files[-1], 'r') as f:
    data = eval(f.readline())

# df = pd.DataFrame.from_records(data)
# df = df.assign(created_at = pd.to_datetime(df.created_at))
# df.created_at.describe(datetime_is_numeric = True)

# ---------------------------
# Binding into 1 and saving unique tweets as text file.
# skipping the first scraped file because this included retweets

d = pd.DataFrame(columns = ['created_at', 'text', 'lang', 'id', 'geo'])
for i in range(1, len(files)):
    file = files[i]
    print(file)
    with open(file, 'r') as f:
        data = eval(f.readline())

    df = pd.DataFrame.from_records(data)
    # leave created_at as a str because export to json is then smoother
    d = d.append(df, ignore_index = True)


no_dups = (d.drop_duplicates(subset = [col for col in d if col != 'geo'])
            .reset_index(drop = True)
)

no_dups.to_json('all_joined.json', orient = 'records')

[d for d in no_dups.geo.dropna().to_list() if len(d) > 1 ]

len(no_dups.geo.dropna().to_list())
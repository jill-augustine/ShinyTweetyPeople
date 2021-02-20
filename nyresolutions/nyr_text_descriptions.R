#library(shiny)

social_links <- {'
  <a href="https://github.com/jill-augustine" class="social-links__entry" target="_blank">
  <i class="fa fa-github"></i>
  </a>\t
  <a href="https://in.linkedin.com/in/jillianaugustine" class="social-links__entry" target="_blank">
  <i class="fa fa-linkedin"></i>
  </a>\t
  <a href="https://twitter.com/jill_codes" class="social-links__entry" target="_blank">
  <i class="fa fa-twitter"></i>
  </a>
'}

desc_nyr_the_data <- {'
<div>
<h2>Tweet Data</h2>
<p>I scraped tweet data using the Twitter API whilst searching for the <strong>keywords "new" AND "resolution"</strong>. I did this in python because of the python dictionary functionality. The dataset contained the following columns:</p>
<ul>
    <li><code>possible_sensitive</code> - an indicator of whether the tweet is predicted to contain sensitive content</li>
    <li><code>text</code> - the text of the tweet</li>
    <li><code>created_at</code> - the UTC timestamp of the tweet</li>
    <li><code>lang</code> - the predicted language of the tweet</li>
    <li><code>source</code> - the device & origin of the tweet e.g. "Twitter for iPhone"</li>
    <li><code>lang</code> - the predicted language of the tweet</li>
    <li><code>retweet_count</code> - the number of times the tweet was retweeted</li>
    <li><code>reply_count</code> - the number of replies the tweet had</li>
    <li><code>like_count</code> - the number of times the tweet was liked</li>
    <li><code>quote_count</code> - the number of times the tweet was quoted</li>
    <li><code>ID</code> - the tweet\'s unique identifier</li>
</ul>
<p>I extracted information from the <code>text</code> column to create the following columns:</p>
<ul>
    <li><code>num_replying_to</code> - the number of Twitter accounts this tweet was replying to</li>
    <li><code>text_wo_replies_links</code> - the text of the tweet after removing all replies and links (but keeping hashtags). This was done to check that the length of the tweet was not longer that 280 characters. Most links and all replies do not count towards the 280-character limit.</li>
    <li><code>num_tagged</code> - the number of Twitter accounts tagged in the tweet</li>
    <li><code>num_hashtags</code> - the number of hashtags in the tweet. This information is directly available from the Twitter API but was easier to manipulate the data if I extracted it myself from <code>text</code>.</li>
    <li><code>text_length</code> - the number of code points in the text</li>
    <li><code>text_count</code> - the number of characters in the text</li>
</ul>
<h2>Emoji Data</h2>
<p>I downloaded the 13.1 version of <code>emoji-test.txt</code> from the unicode website <a href="https://www.unicode.org/Public/emoji/13.1/emoji-test.txt">here</a>. At the time of creation, this was the most recent set of 4590 available emojis (from September 2020). After skipping the intro rows, I split each line on <code>;</code>,<code>#</code> and the emoji number. This allowed me to create a dataframe containing the unicode emojis in one column and the text description of the emoji in another column.</p>
<p>I then added the following additional columns:</p>
<ul>
    <li>The ASCII form of the unicode emoji because this would be needed later for emoji recognition.</li>
    <li>An text identifier/tag for each emoji made up of the word <code>jaugur</code> followed by the underscore-separated emoji description.</li>
    <li>A url linking to the <a href="https://emojipedia.org/">Emojipedia</a> .png image of each emoji. I did this in case I wanted to use images of the emojis somewhere in the dashboard.</li>
</ul>
<h2>Timezone Data</h2>
<p>Data containing UTC Offset and the common name of the time region as take from <a href="https://greenwichmeantime.com/time-zone/definition/">greenwichmeantime.com</a>.</p>

</div>
'} 

desc_nyr_tweets_per_hour <- {'
<div>
<p>This plot shows the number of tweets per hour of the 24-hour period when the world moved into 2021. To make this plot I used an additional dataset which contained timezone data including the name of each timezone and the corresponding GMT offset (e.g. GMT+2).</p>

<p>First I selected only the rows of data from the 24-hour period of the transition into the new year, and then rounded the creation time of each tweet to the nearest hour. Then I grouped the data by hour and counted the number of tweets in each group. Finally, to each group I added the corresponding tick position and tick label information.</p>

<p>I had to reverse the order of the tick position information so that the later timestamp groups e.g <code>2021/01/01 10:00:00+00</code> were plotted on the left of the plot. Typically later timestamps are plotted to the right of earlier timestamps. I reversed the order so that plot would show the trend from each to west as is typically seen on a map with GMT in the centre.</p>

<p>I noticed three distinct peaks in the data and labeled these as static text in addition to the dynamic hovertext for all time regions.</p>
</div>
'}

desc_nyr_rank_emojis <- {'
<div>
<p>To create this plot I had to create a frequency table which counted the number of emojis present in the "text" field of the data. A challenge when handling this data was accurately recognising emojis in the text. To do this I used the <code>textclean::replace_emoji</code> function with a custom reference table (the <code>emoji_dt</code> argument). This custom reference table contained the ASCII code of over 4000 emojis and a corresponding identifier. I added a column to the main dataset called <code>text_with_emojis_replaced</code> which contained the original text but with reply tags and links removed, and with emojis replaced by their identifier. This new column was the data source for the frequency table.</p>

<p>I extracted all the emoji identifiers from the text, extracted the emoji descriptions from the identifiers and grouped by description. This process meant that emojis with different skin tones were grouped together. I counted how many emojis were in each group and joined this table to an emoji reference table. The final table is visble and searchable in the datatable on this page.</p>

<p>Given that there are many different emojis, I created a range slider so users can select which ranks of emojis they want to visualise in detail. Changing the values on the slider updates the data being fed to the plotly figure. It does not however affect the data being shown on the data table.</p>
</div>
'}

desc_nyr_rank_hashtags <- {'
<div>
<p>Similarly to the emoji rank plot, for this plot I had to create a frequency table to count the number of hashtags in the text. Although the hashtags were easily identifiable, having over 27,000 different hashtags in the text means that continuously updating the figure based on the current values of the range selector would cause the app to freeze. I therefore decided to change the plotting from a <code>reactive()</code> element to an <code>eventReactive()</code> element that refreshes only upon clicking a button. In addition, I restricted the data to plot to only hashtags that were used at least 10 times. This reduced the maximum number of hashtags to plot by over 23,000.</p>
</div>
'}

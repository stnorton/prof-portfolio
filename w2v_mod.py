## IMPORTS-----------------------------------------------------------------------------
#standard imports
import re  #regular expression
import numpy as np #arrays and math
import pandas as pd #dataframes

#gensim imports
import gensim #whole module
from gensim.models import Word2Vec


#nltk imports
from nltk.tokenize import TweetTokenizer # a tweet tokenizer from nltk.
tokenizer = TweetTokenizer() #instantiate

# Enable logging
import logging
logging.basicConfig(format='%(levelname)s : %(message)s', level=logging.INFO)

import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)

#import pickle, argument parser
import pickle
import argparse

##PARSE ARGS-----------------------------------------------------------------------------
#define arguments
parser = argparse.ArgumentParser()
parser.add_argument('--cores', required = True, type = int, help = 'n cores')
parser.add_argument('--data', required = True, type = str, help = 'Data path (full)')
parser.add_argument('--window', required = True, type = int, help = 'Window size for Word2Vec')
parser.add_argument('--min_count', required = True, type = int, help = 'min_count argument to Word2Vec')
parser.add_argument('--model_out', required = True, type = str, help = 'filename to save model as')

#parse
args = parser.parse_args()

#unpack arguments
cores = args.cores
dpath = args.data
win = args.window
min_c = args.min_count
outfile = args.model_out

##CLEAN DATA--------------------------------------------------------------------------------

#import the dataset and clean it
texts = pd.read_csv(dpath)

data = texts.values.tolist()

#remove all links and hashtags
def clean_tweet(tweets):
    '''Uses regular expressions to remove urls and hashtags'''
    for tweet in tweets:
        tweet = re.sub('http\S+\s*', '', tweet)  # remove URLs
        tweet = re.sub('#\S+', '', tweet)  # remove hashtags
    return tweet

data = [clean_tweet(text) for text in data]

tknzr = TweetTokenizer(preserve_case = False, strip_handles=True)

def tokenizer(tweets):
    texts_out = []
    for tweet in tweets:
        texts_out.append(tknzr.tokenize(tweet))
    return texts_out
        
tokens = tokenizer(data) #tokenize

#now stem
from nltk.stem.snowball import SnowballStemmer #import stemmer
stemmer = SnowballStemmer("russian") #instantiate, set to russian

stemmed_toks = [[stemmer.stem(word) for word in tweet] for tweet in tokens] #stem

##DICTIONARY SAVE------------------------------------------------------------------------

#function to flatten lists
flatten = lambda l: [item for sublist in l for item in sublist]

flat_toks = flatten(tokens)
flat_stoks = flatten(stemmed_toks)

#dump
pickle.dump(flat_toks, open('intact_tokens.p', 'wb'))
pickle.dump(flat_stoks, open('stem_tokens.p', 'wb'))

##MODEL---------------------------------------------------------------------------------

#initialize
model = Word2Vec(sg = 1, 
                 size = 200, 
                 min_count = min_c,
                 window = win,
                 seed = 1017,
                 workers = cores,
                 sample = 0.01,
                )

#build vocacb
print('Building vocab...')

model.build_vocab(stemmed_toks)

print('Training...')
model.train(stemmed_toks, total_examples=len(stemmed_toks), epochs=10)

print('Saving model...')
model.save(outfile)



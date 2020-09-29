##IMPORTS----------------------------------------------------------------------

import re  #regular expression
import numpy as np
import pandas as pd
from pprint import pprint #pretty printing

#gensim imports
import gensim #whole module
import gensim.corpora as corpora #convenience rename
from gensim.utils import simple_preprocess #import preprocessor
from gensim.models import CoherenceModel #model for coherence
from gensim.corpora.sharded_corpus import ShardedCorpus #convenience import
import string


#nltk imports
from nltk.tokenize import TweetTokenizer # a tweet tokenizer from nltk.
tokenizer = TweetTokenizer()
from nltk.stem.snowball import SnowballStemmer #import stemmer
stemmer = SnowballStemmer("russian") #instatiate stemmer to russian

# Enable logging
import logging
logging.basicConfig(format='%(levelname)s : %(message)s', level=logging.INFO)
logging.root.level = logging.INFO  

import warnings
warnings.filterwarnings("ignore",category=DeprecationWarning)

##command line arguments
import argparse

##PARSE ARGS--------------------------------------------------------------------
parser = argparse.ArgumentParser()
parser.add_argument('--k', required = True, type = int, help = 'N topics')
parser.add_argument('--cores', required = True , type = int, help = 'N cpus')
parser.add_argument('--filename', required = True, type = str, help = 'Data path')

args = parser.parse_args()

k = args.k
cores = args.cores
dfile = args.filename

print('Starting %s topic model' % k)

#import master stopwords list
with open('base_ru_stopwords.txt') as f:
    stop_words = f.readlines()

stop_words=[x.strip() for x in stop_words]

#read in data
texts = pd.read_csv(dfile)

#convert to list
data = texts.values.tolist()

#remove all links and hashtags
def clean_tweet(tweets):
    '''Uses regular expressions to remove urls and hashtags'''
    for tweet in tweets:
        tweet = re.sub('http\S+\s*', '', tweet)  # remove URLs
        tweet = re.sub('#\S+', '', tweet)  # remove hashtags
    return tweet

data = [clean_tweet(text) for text in data]

#print(data[:10]) #sanity check results

#instatiate tweet tokenizer
tknzr = TweetTokenizer(preserve_case = False, strip_handles=True)

def tokenizer(tweets):
    texts_out = []
    for tweet in tweets:
        texts_out.append(tknzr.tokenize(tweet))
    return texts_out

tokens = tokenizer(data) #tokenize

#remove stopwords

def remove_stopwords(texts):
    '''Loops over texts, preprocess, and then compares each word to stopwords using a list comprehension'''
    for text in texts:
        return[[word for word in simple_preprocess(str(doc)) if word not in stop_words] for doc in texts]

tokens = remove_stopwords(tokens)

#tokens[:5] #sanity check

#stem all tokens
stemmed_toks = [[stemmer.stem(word) for word in tweet] for tweet in tokens] #stem

#MODELING-------------------------------------------------
#dictionary
id2word = corpora.Dictionary(stemmed_toks)

#corpus
texts = stemmed_toks

#dtf
corpus = [id2word.doc2bow(text) for text in texts]

#run LDA
from datetime import datetime
now = datetime.now()
current_time = now.strftime("%H:%M:%S")
print("Model start =", current_time)

lda_model = gensim.models.LdaMulticore(corpus=corpus,
                                        id2word=id2word,
                                        num_topics=k,
                                        random_state=1017,
                                        chunksize=100,
                                        passes = 10,
                                        per_word_topics=True,
                                        workers=cores
                                      )
now = datetime.now()
current_time = now.strftime("%H:%M:%S")
print("Model end:", current_time)

FILE = str(k) + '_topic.model'

lda_model.save(FILE)

print('Finished %s topic model' % k)

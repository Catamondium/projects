#!/bin/envrun
# Based on nltk.org/howto/sentiment.html

"""
Classifiers for use in crawler,
to include polarity, subjectivity and (if not provided by nltk) name classification
"""

def __init__():
    download_dicts()

def download_dicts():
    """
    Download required corpuses & lexicons
    """
    import nltk
    nltk.download('vader_lexicon')
    nltk.download('subjectivity')

def subjectivity_classifier():
    from nltk.classify import NaiveBayesClassifier
    from nltk.corpus import subjectivity
    from nltk.sentiment import SentimentAnalyzer
    from nltk.sentiment.util import *
    """
    Initializes and trains categorical subjectivity analyzer
    """
    N_INSTANCES = 100

    subj_docs = [(sent, 'subj') for sent in subjectivity.sents(categories='subj')[:N_INSTANCES]]
    obj_docs = [(sent, 'obj') for sent in subjectivity.sents(categories='obj')[:N_INSTANCES]]

    train_subj_docs = subj_docs[:80]
    test_subj_docs = subj_docs[80:]
    train_obj_docs = obj_docs[:80]
    test_obj_docs = obj_docs[80:]
    training_docs = train_subj_docs + train_obj_docs
    testing_docs = test_subj_docs + test_obj_docs

    sent_analyzer = SentimentAnalyzer()
    all_words_neg = sent_analyzer.all_words([mark_negation(doc) for doc in training_docs])

    unigram_feats = sent_analyzer.unigram_word_feats(all_words_neg, min_freq=4)
    print(f"unigram feats: {len(unigram_feats)}")

    sent_analyzer.add_feat_extractor(extract_unigram_feats, unigrams=unigram_feats)

    training_set = sent_analyzer.apply_features(training_docs)
    test_set = sent_analyzer.apply_features(testing_docs)

    trainer = NaiveBayesClassifier.train
    classifier = sent_analyzer.train(trainer, training_set)
    for k,v in sorted(sent_analyzer.evaluate(test_set).items()):
        print(f"{k}: {v}")

    return sent_analyzer

# Pretrained polarity classifier
from nltk.sentiment.vader import SentimentIntensityAnalyzer

subj_analyzer = subjectivity_classifier()
polarity_analyzer = SentimentIntensityAnalyzer()

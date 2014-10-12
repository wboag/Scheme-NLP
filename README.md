Scheme-NLP
==========

Simple little NLP toolkit for an undergraduate class I'm in.

For a class assignment, I was told to find a library in Racket that I'd be interested in working with for a final semester project. I began searching for an NLP library for Scheme, and I learned of the <a href="https://www.academia.edu/1592758/The_Scheme_Natural_Language_Toolkit_SNLTK_">Scheme Natural Language Toolkit (SNLTK)</a>. Unfortunately, http://snltk.org is no longer active. The library can still be downloaded, but I could only get it working for r6rs, not Racket. Granted, I was probably trying to install it incorrectly, but it inspired me to write my own toolkit.

This project is a very small little toolkit that I'm making mostly for fun to use during my class. 

The goal is to make it 100% out of Racket (without any external dependencies) so that it can be used with projects without any installation beyond out-of-the-box Racket.


Currently, Scheme-NLP has:

    1) An n-gram language model. This model:
  
      - will come with out-of-the-box models (once I learn how to serialize objects).
      - the ability to train your own models.
      - can predict the probabilities of sentences based on ngram frequencies.
      - supports additive (Laplace) smoothing.
      - can generate random text using a Markov process.


    2) A word stemming module. This module:

        - implements Porter's stemming algorithm.
        - can stem a word down to its root. (ex. "training" -> "train")
        - does not need any training data.

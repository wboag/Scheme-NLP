Scheme-NLP
==========

Simple little NLP project for an undergraduate class I'm in.

For a class assignment, I was directed to find a library in Racket that I'd be interested in working with for a final semester project. I began searching for an NLP library for Scheme, and I learned of the <a href="https://www.academia.edu/1592758/The_Scheme_Natural_Language_Toolkit_SNLTK_">Scheme Natural Language Toolkit (SNLTK)</a>. Unfortunately, http://snltk.org is no longer active. The library can still be downloaded, but I could not find a version working for Scheme, only r6rs. I hardly know anything about the Scheme community (so I probably wasn't installing correctly) but I just couldn't get SNLTK to work for Racket.

This project is a very small little toolkit that I'm making mostly for fun to use during my class. 

The goal is to make it 100% out of Racket (without any external dependencies) so that single files can be copied and pasted for projects without any hassle of downloads.


Currently, Scheme-NLP has:

    1) An n-gram language model. This model:
  
      - will come with out-of-the-box models (once I learn how to serialize objects in Scheme)
      - the ability to train your own models.
      - can predict the probabilities of sentences based on ngram frequencies.
      - supports additive (Laplace) smoothing.
      - can generate random sequences of text using a Markov process.

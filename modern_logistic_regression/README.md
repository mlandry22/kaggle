Modern Logistic Regression
======

This is a separate collection of files that support the talk I gave to the Austin Machine Learning Meetup on 1/19/2015. 

These scripts are used directly (OK by the amusing license) from Kaggle forums, and provide a fantastic entry point for robust classification tasks.

* First version: easiest to start adapting
  * link: http://www.kaggle.com/c/criteo-display-ad-challenge/forums/t/10322/beat-the-benchmark-with-less-then-200mb-of-memory
  * techniques: online learning, logistic regression, hash trick, adaptive learning rate
* Second version: same plus multiclass output handling
  * link: http://www.kaggle.com/c/tradeshift-text-classification/forums/t/10537/beat-the-benchmark-with-less-than-400mb-of-memory
  * techniques: online learning, logistic regression, hash trick, adaptive learning rate, multiclass output
* Third version: FTRL-proximal
  * link: http://www.kaggle.com/c/avazu-ctr-prediction/forums/t/10927/beat-the-benchmark-with-less-than-1mb-of-memory
  * techniques: online learning, logistic regression, hash trick, FTRL-proximal (stochastic gradient descent, L1 & L2 regularization)

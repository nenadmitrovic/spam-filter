# spam-filter

This is spam filter application created in Clojure


The goal of this application is to classify email message as spam, ham or unknown. If you want to classify some email message, put its text in the text area and press Classify button. Once the classifier finished the task, you will see on the screen the category which the classifer has classified your email in. If you want to train classifier, after you type the text in the text area choose the category you want to put that text in and press Train button.

To train classifier, I've used SpamAssassin public mail corpus you can find on this adress https://spamassassin.apache.org/old/.

To decide whether a given email is spam, unknown or ham, we calculate probabilities. First we calculate the probability that a word is in particular category by dividing the number of times the word appears in a document in that category by the total number of documents in that category. This is called conditional probability. We can write that as  Pr(word | classification), probability of word for a given classification. Once we have this probability, we have to combine these individual probabilities to get the probability that an entire document belongs in a given category. For this task I used a naive Bayesian classifier.

Also, I implemented Fisher method that is named for Sir Ronald Aylmer Fisher who was a British statistician and geneticist. Unlike the naive Bayesian classifier which combines feature probabilities to get the probability of an entire document, Fisher method calculates the probability of a category for each feature in the document and combines these probabilities.

 
Installing Instructions

To run this application you will have to download it first, install MySQL if you already dont have it and install Leiningen. Database script you will find in db folder  within the root folder. To start application navigate to the root folder and type "lein run". After that open your browser navigate to the port you saw the application has started on and you can try spam filter.


Libraries

Ring - Ring is a Clojure web applications library inspired by Python's WSGI and Ruby's Rack. By abstracting the details of HTTP into a simple, unified API, Ring allows web applications to be constructed of modular components that can be shared among a variety of applications, web servers, and web frameworks. (https://github.com/ring-clojure/ring)

Compojure - Compojure is a small routing library for Ring that allows web applications to be composed of small, independent parts.(https://github.com/weavejester/compojure)


Literature

1. Clojure for the brave and true (https://www.braveclojure.com/)
2. Web development with Clojure
3. Programing Collective Intelligence
4. Clojure for Machine learning
5. Functional Programming Patterns in Scala and Clojure
6. ClojureDocs (https://clojuredocs.org/)



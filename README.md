# Classifying-Malicious-and-Amiable-Websites-using-R

In this project we have implemented The State-Of-the-Art Decision Tree Machine Learning Models such as Random Forest and Decision Tree to classify URLs as malicious or amiable. Implementation of Classification algorithms for discrete data as well as normal regression model is used in the project.

# Project Description

<img width="719" alt="Screenshot 2021-07-06 at 9 29 23 PM" src="https://user-images.githubusercontent.com/64043016/124632501-0717ca80-dea2-11eb-8d07-804052bd9c8f.png">

# Random Forest Algorithm

Random forest builds multiple decision trees (called the forest) and glues them together to get a more accurate and stable prediction. The forest it builds is a collection of Decision Trees, which are trained can also be used in unsupervised mode for assessing proximities among data points.

# Decision Treee Algorithm

Decision tree is a graph to represent choices and their results in form of a tree. The nodes in the graph represent an event or choice and the edges of the graph represent the decision rules or conditions. Decision tree model shows how a website is classified as malicious or amiable based on the network and application
layer characteristics in dataset. It classifies into type 0- amiable and type 1-malitious based on Characteristics like TCP, port URL length etc. and forms a decision tree for classification

# The implementation Digrams are Shown Below

<img width="538" alt="Screenshot 2021-07-06 at 9 29 43 PM" src="https://user-images.githubusercontent.com/64043016/124632541-126af600-dea2-11eb-8904-a883cfd36f8d.png">

<img width="1082" alt="Screenshot 2021-07-06 at 9 30 18 PM" src="https://user-images.githubusercontent.com/64043016/124632591-1d258b00-dea2-11eb-8cf9-c8908c8d818d.png">

<img width="673" alt="Screenshot 2021-07-06 at 9 38 19 PM" src="https://user-images.githubusercontent.com/64043016/124633102-94f3b580-dea2-11eb-9d22-c7894b816d28.png">

<img width="698" alt="Screenshot 2021-07-06 at 9 38 26 PM" src="https://user-images.githubusercontent.com/64043016/124633110-97eea600-dea2-11eb-950f-4dd709172e49.png">


# Conclusion
The numerical simulations and the corresponding statistical tests strongly suggest that the differences between the methods are significant. Therefore, the ranking of the methods as shown in Table II can be accepted as a correct and significant ranking in terms of prediction accuracy. Although all methods achieve fairly high prediction accuracy, Random Forest appears to be the most appropriate classification algorithm for this problem, followed by MLP. Moreover, Random Forest achieves high scores for both precision and recall, which not only indicates well-balanced and unbiased prediction results, but also provides credibility to the method’s ability to maximize the detection of malicious URLs within reasonable boundaries.

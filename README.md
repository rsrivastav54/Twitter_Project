# COP5615_Twitter_Project

## Team Members

- Rishabh Srivastav : UFID 7659-9488
- Ashish Sunny Abraham : UFID 6388-7782

## Outline of the project
- This project aims to design a Twitter Clone which performs all the functionalities of the twitter application  and a client tester/simulator to test the above used functionalities. The user should be able to add/register himself on the network, be able to tweet, subscribe and follow people so that the user is able to view tweets of other users as well, and also add the features for querying. Querying is handled in ways like extracting tweets which contain a specific hashtag or tweets that contain a specific @ mention to some user.

## Commands to run the program
- Compile all the .erl files in the zip folder (engine, user, test, server). 
- Open one erl shell and run the engine by typing “test:delegate({})” and then enter your IP address.
- Open a second erl shell and run the client simulation by typing “test:delegate({50,49,20})” where the first parameter represents the number of users you want to simulate for and the second parameter represents the maximum number of followers a user can have, which will be 1 less than total users at max. The third parameter represents the percentage of users you want to participate in the disconnect/reconnect functionality. 
- In order to test each functionality manually, the user can refer to the screenshots attached in the features section to see how to test for each feature manually apart from the simulation.

## Architecture
- The Twitter Engine acts as an interface between client and database which retrieves information on demand as specified by the user. The engine handles multiple API calls to perform tasks and queries entered by the user and interacts with the database (ETS tables) to return the expected values. The engine is responsible for registering a user, processing tweets, and query handling. The database stores client/user information, list of followers and subscribers of a client and their tweets and mentions. The client module is responsible for sending requests to the server to process tasks based on features demanded by the client. The client then in-turn receives a response back from the engine after every request.
 <img width="574" alt="Screenshot 2022-12-01 at 7 57 11 PM" src="https://user-images.githubusercontent.com/59756917/205191026-da84fb37-a8e6-4d5c-a5c7-8254e1965d64.png">

 
 ## Features implemented 
 
- Twitter Engine start: Starts the server so that the application can become on-line
- Register user: Allows user to register himself onto the social media clone without which the user can’t access any of the functionalities of the application.
<img width="650" alt="Screenshot 2022-12-01 at 7 58 07 PM" src="https://user-images.githubusercontent.com/59756917/205191113-08624041-518b-45f6-acb9-dee31e793cc4.png">

- Send tweet: Allows a user to express his/her thoughts and send tweets. Checks have been added to ensure a user who hasn’t registered himself cannot tweet unless he creates an account.
<img width="641" alt="Screenshot 2022-12-01 at 7 58 17 PM" src="https://user-images.githubusercontent.com/59756917/205191265-9f61efcf-7d50-472a-adda-78e01409b980.png">

- Subscribe to users: Provides users the ability to connect with other users whose tweets they find interesting and would want to follow them. By subscribing to a user, one can view all their tweets as part of their own tweet list.
-Retweet: Allows users to view tweets of specific interest to them. This has been incorporated with the subscriber model itself.
<img width="640" alt="Screenshot 2022-12-01 at 7 58 29 PM" src="https://user-images.githubusercontent.com/59756917/205191309-55d23ef5-9554-4446-9b60-178c4fe99a32.png">


- Querying:
  - Hashtags: Provides a list of tweets which contain a specific hashtag as entered by the user. Returns nothing if no such match is found.
  <img width="656" alt="Screenshot 2022-12-01 at 7 58 37 PM" src="https://user-images.githubusercontent.com/59756917/205191344-11e47deb-e670-4248-b35f-8f59504ae85c.png">

  - Mentions: Provides a list of tweets which contain mentions of other users as requested by some users. Checks have been added to verify if the mentioned user exists in the first place or not. Returns nothing if no such match is found.<img width="645" alt="Screenshot 2022-12-01 at 7 58 48 PM" src="https://user-images.githubusercontent.com/59756917/205191440-1d382713-e033-45a4-bf4c-4e2eea407c83.png">

  
  - Subscribe: Allows user to view tweets of another person he/she has subscribed to
  <img width="641" alt="Screenshot 2022-12-01 at 7 59 01 PM" src="https://user-images.githubusercontent.com/59756917/205191468-dfd626c7-5308-4594-b3cb-159f63bc7664.png">

  
## Implementation Details

- test.erl: This module contains code for simulation. Steps to execute functions from this file have been provided in steps to run the program.
- engine.erl: The code for the Twitter engine implementation, which is in charge of handling and implementing tweets, is located in this file. To manage subscriptions, tweets, searches, etc., the engine directly interfaces with the database (built using ETS tables). In order to convey the search queries and distribute the tweets to subscribers, it also directly interacts with clients via the API handler. The clients registry ETS table, which tracks numerous actors inside the server, maintains process states.
- user.erl: This file contains details about a particular client participant, which corresponds to a single twitter user. This includes its user Id data, which is saved as a string supplied to it upon program startup. The primary registry ETS table, which maintains track of different actors, performs the core logic of preserving process state.
- server.erl: To manually test for functions and features of the application, use this file and refer to features implemented section to see how to check for each feature. Start server by typing “server:start()” and then manually test the features implemented.

- Zipf distribution : In accordance with the specifications, we were required to model a Zipf distribution based on the subscriber count. By requesting an extra variable from the client, maxSubscribers, which specifies the most subscribers a client may have throughout the trial, the Zipf distribution was calculated. The user with the second-highest subscriber count had a total subscriber count of maxSubscribers/2, the user with the next-highest subscriber count had a total subscriber count of maxSubscribers/3, and so on. This was done by employing the equation noToSubscribe = round(Float.floor(totalSubscribers/(noOfClients-count+1))) - 1, where 'count' is the userId of a client, to determine the amount of peers a person should register to in order to ensure zipf distribution.

- Periods of live connection and disconnection for users : Disconnect clients, the third parameter used by the program to simulate times of live connection and disengagement, collects the proportion of clients who disconnect. The client's simulation console outputs the performance metrics at the conclusion if the disconnectClients> parameter is set to 0. If not, it outputs the statistics and keeps simulating recurrent live connection and disconnection times.
Every client is required to choose a random tweet from one of its subscribers and rebroadcast it to its own subscribers in order to manage retweets. As with the original Twitter, a "-RT" is added to the end of a retweet to distinguish it from regular tweets.
By data transmission to the server and showing the list of tweets acquired on the user end, all queries relating to tweets that were linked to, tweets with particular hashtags, and tweets in which the user is referenced (my mentions) were effectively handled. A person who is online will receive the tweets via live view in real time. In order to determine which user's live view is being updated, User ID is prefixed to this output.

## Observations and graphs for simulation
- Testing the simulation for 3 users with maximum of 2 followers each and disconnect and reconnect users parameter set to 0 to obtain statistics
- User Live View:
<img width="706" alt="Screenshot 2022-12-01 at 7 59 13 PM" src="https://user-images.githubusercontent.com/59756917/205191567-e7c23d78-7158-420e-a9b0-fa7dc32aefa4.png">
<img width="705" alt="Screenshot 2022-12-01 at 7 59 25 PM" src="https://user-images.githubusercontent.com/59756917/205191577-68d8e98f-4990-4a07-aebb-b53b9afc6406.png">

- Engine View:

<img width="712" alt="Screenshot 2022-12-01 at 7 59 37 PM" src="https://user-images.githubusercontent.com/59756917/205191637-669ceb03-c5b8-448b-984d-6d71d56c0731.png">
<img width="709" alt="Screenshot 2022-12-01 at 7 59 48 PM" src="https://user-images.githubusercontent.com/59756917/205191658-a1ce9f25-fb58-4702-8752-ae8e7185469b.png">

- User Live View for testing reconnect and disconnect of user nodes: Testing with 3 users with maximum of 2 followers each and having 50% disconnects and reconnects in between


<img width="653" alt="Screenshot 2022-12-01 at 9 11 46 PM" src="https://user-images.githubusercontent.com/59756917/205199163-12d0365d-d434-41ea-bf45-95db364d8f85.png">

<img width="656" alt="Screenshot 2022-12-01 at 9 11 33 PM" src="https://user-images.githubusercontent.com/59756917/205199098-4df904cf-48a6-4621-b80b-3ba7ac95d6c4.png">

- Engine View for testing reconnect and disconnect of user nodes: Testing with 3 users with maximum of 2 followers each and having 50% disconnects and reconnects in between

<img width="679" alt="Screenshot 2022-12-01 at 9 13 26 PM" src="https://user-images.githubusercontent.com/59756917/205199300-f23eeb93-b032-4c58-9aec-710977b8fd75.png">
<img width="675" alt="Screenshot 2022-12-01 at 9 13 43 PM" src="https://user-images.githubusercontent.com/59756917/205199314-8dc64e44-abd1-4487-8c07-6e30b5ae7ae6.png">


## Tabular Results and Graphs
<img width="754" alt="Screenshot 2022-12-01 at 4 16 38 PM" src="https://user-images.githubusercontent.com/59756917/205191720-e839757b-bf32-41f7-9591-994455057bb9.png">
<img width="774" alt="Screenshot 2022-12-01 at 8 00 01 PM" src="https://user-images.githubusercontent.com/59756917/205191735-079104e3-f305-4fff-a773-2ebe1799076c.png">
<img width="757" alt="Screenshot 2022-12-01 at 8 00 09 PM" src="https://user-images.githubusercontent.com/59756917/205191745-8a739887-e4d7-4f4e-8455-9c6497710b95.png">

## Largest Network we managed to test our application with:

- The largest number of users we managed to test with is 3600 and the results for that are:

  - The time to register 3600 users is 7442.84 ms
  - The time to send 10 tweets is 110990.34 ms
  - The time to Zipf subscribe 3200 users is 15225.70 ms
  - The time to query 3600 users is 13889.39 ms
  - The time to query 3600 hashtags is 6575.74 ms
  - The time to query 3600 mentions is 5946.99 ms
  - The time to 3600 random operations is 10498.63 ms

## Conclusion:
- The twitter clone application was simulated successfully using the erlang actor model. Simulation results were tabulated and plotted graphically to understand trends and patterns between various features and querying procedures. We plan to further scale this backend model by implementing the UI Design for it in the next project part.






# Erlang Project - Basic ideas
Description of Project - keywords:
- key value store
- sessions
- message
- users and their credentials (different data types)
- storage of data inside erlang code -> FIFO?

To do for next week:
- how can the code be structured? 
- how many different files?
- what is the content of each file?
- is it "easy" to link the Erlang code with a GUI to write messages? -> Java Script?

Open questions:
- how do we have different servers, if we have users who send messages to one server?
- how do we integrate the docker in this?

## Tasks : 
### Milena :
- Adapt the code for sending messages
- SessionId dans le backend
- link cowboy et erlang (API)

### Antoine : 
- Setup cowboy, rebar3, docker
- modification frontend : champ pour entrer nom et un champ pour envoyer un message et un champ pour lire les messages
- Link cowboy et react.

## Name
The Distributed User-session Handler

## short project outline
The "Distributed User-session Handler" project aims to create a distributed key-value store in Erlang for managing user session data on a website. It involves developing distributed Erlang nodes, a load balancer/web server, and a user-friendly client interface. The key objectives include efficient session management, data distribution, and fault tolerance. It will be considered to use React for the frontend and to explore the potential integration of Docker for consistent development environments and easy deployment. Overall, the goal is to leverage Erlang's distributed capabilities to create a scalable and fault-tolerant solution for handling user sessions.

## System Overview
- **Objective:** Build a distributed key-value store in Erlang to manage user session data (First Name, Last Name, Email, Address, etc.) for a website.
- **Features:** The system should support creating, updating, deleting, and retrieving user session data.

## System Components

### 1. Distributed Erlang Nodes
- **Purpose:** Each node represents an instance of your key-value store running on different machines or virtual environments.
- **Functionality:** These nodes store and manage session data. They communicate with each other to ensure data consistency and availability.

### 2. Load Balancer / Web Server
- **Purpose:** Acts as an intermediary between the client (web browser) and the Erlang nodes.
- **Functionality:** Distributes client requests to various Erlang nodes, ensuring load distribution and fault tolerance.

### 3. Client Interface
- **Purpose:** The front-end part of the website where users interact.
- **Functionality:** Sends session data requests (create, read, update, delete) to the web server.

## Processes in Erlang
1. **Session Management Processes:** Each session operation (create, read, update, delete) can be handled by separate Erlang processes. These processes manage the session data in the key-value store.
2. **Data Replication Processes:** To ensure fault tolerance, you might have processes dedicated to replicating data across different nodes.
3. **Load Distribution Processes:** These processes could be responsible for distributing the load evenly across the nodes, although this might also be handled by your web server/load balancer.

### Data Distribution and Fault Tolerance
- **Sharding:** Consider partitioning (sharding) session data across different nodes. Each shard handles a subset of the session data.
- **Replication:** Implement data replication strategies to ensure that if one node fails, the session data is still accessible from another node.

### Client-Server Communication
- **API Endpoints:** The web server should expose API endpoints for session data operations.
- **Erlang Communication:** Use Erlang's message passing capabilities for internal communication between nodes.

## Frontend with React
Suggestion : We could use React for the frontend.

Linking a React frontend with an Erlang backend involves setting up a communication channel between the two. Typically, this is done through a REST API or WebSocket connection implemented in Erlang. You don't necessarily need a specific IDE for this; Visual Studio Code (VSCode) can handle both React and Erlang development. Here's a step-by-step guide to set up this connection:

### Step 1: Setting Up Your Erlang Backend
1. **Develop the Backend:** Write your Erlang code to implement the key-value store and user session handling functionalities.
2. **Create API Endpoints:** Use an Erlang web server framework like Cowboy to create RESTful API endpoints. These endpoints will handle HTTP requests from your React frontend.

### Step 2: Developing the React Frontend
1. **Set Up React Environment:**
   - If you haven't already, install Node.js, which comes with npm (Node Package Manager).
   - Use the command `npx create-react-app my-app` in your terminal to create a new React application.
   - Navigate into your new React app directory (`cd my-app`) and start the development server with `npm start`.
2. **Develop the Frontend:** Build your React components to interact with the user, such as login forms, user profile pages, etc.

### Step 3: Linking Frontend with Backend
1. **HTTP Requests:** In your React components, use `fetch` or Axios to make HTTP requests to your Erlang backend's API endpoints. 
   - For instance, when a user logs in, send a POST request with the user's credentials to the Erlang backend.
2. **Handling Responses:** Process the responses from your Erlang backend in React to update the user interface accordingly.

### Step 4: Running Both Applications
1. **Run Erlang Backend:** Start your Erlang application so that it's listening for HTTP requests on a designated port.
2. **Run React Frontend:** Run your React application. It should now be able to communicate with your Erlang backend through API requests.

## Docker
Using Docker in your project to link a React frontend with an Erlang backend can be very beneficial. Docker provides a consistent and isolated environment for your applications, ensuring that they run the same regardless of where they are deployed. Here are some advantages and considerations for using Docker in your project:

### Advantages of Using Docker
1. **Consistent Development Environment:** Docker containers ensure that your development environment is consistent across all machines. This is especially useful if you’re working in a team, as it eliminates the "it works on my machine" problem.
2. **Easy Deployment and Scalability:** Docker makes it easier to deploy your application on any server that supports Docker. It also simplifies scaling your application by running multiple containers in a distributed environment.
3. **Isolation:** Containers are isolated from each other and the host system. This isolation can help in minimizing conflicts between different development environments and between your applications.
4. **Version Control and Rollbacks:** Docker allows you to version your container images, making it easier to roll back to a previous version if something goes wrong.
5. **Microservices Architecture:** If you decide to architect your application as microservices (with separate services for your frontend, backend, database, etc.), Docker is an ideal tool for managing these services.

### Implementing Docker in Your Project
1. **Dockerize the Erlang Backend:**
   - Create a `Dockerfile` for your Erlang backend application.
   - Specify the Erlang base image, copy your application code into the container, and set the necessary commands to run your Erlang application.
2. **Dockerize the React Frontend:**
   - Similarly, create a `Dockerfile` for your React application.
   - Use a Node.js base image, build your React application, and serve it using a server like Nginx or serve it in a Node.js environment.
3. **Use Docker Compose:**
   - Implement Docker Compose to manage multi-container Docker applications.
   - Define services, networks, and volumes for your frontend and backend in a `docker-compose.yml` file, allowing them to be spun up together in an interconnected manner.
4. **Local Development and Testing:**
   - Use Docker to run your applications locally. This ensures that if it works on your machine, it will work in any other environment where Docker is available.
5. **Deployment:**
   - Deploy your Docker containers to a cloud provider or your own server. This process becomes straightforward because Docker takes care of the environment setup.

### Considerations
- **Learning Curve:** If you're new to Docker, there's a learning curve involved. However, Docker's extensive documentation and community support can be very helpful.
- **Resource Usage:** Docker can be resource-intensive, especially when running multiple containers simultaneously. Ensure your development machine has sufficient resources.

## Implementation Steps
1. **Design the Data Model:** Define how session data will be structured and stored.
2. **Implement the Key-Value Store:** Code the basic functionalities in Erlang.
3. **Set Up the Distributed Environment:** Configure multiple Erlang nodes.
4. **Develop the Web Server Interface:** Create the intermediary web server with API endpoints.
5. **Integrate with the Front-End:** Ensure the client interface can communicate with the web server.
6. **Docker Integration:** Dockerize the backend and frontend, and set up with Docker Compose.
7. **Test the System:** Conduct thorough testing for functionality, load handling, and fault tolerance.

## Conclusion
Your idea leverages Erlang's distributed computing capabilities and is well-suited for managing session data in a concurrent and fault-tolerant manner. The key will be to carefully design the distribution and replication strategies to ensure the system can handle failures and maintain data consistency.

# Distributed Chatroom Application with Erlang and React
by Antoine Tissot and Milena Kovatsch

## Project Overview
- **Objective:** Develop a real-time chatroom application with user session management.
- **Features:** Real-time messaging, participant tracking, and session management.

## System Architecture
- **Erlang Backend:** Manages user sessions and chat messages, using Cowboy and WebSocket for real-time communication.
- **React Frontend:** Provides a dynamic user interface for chat interaction.
- **Docker Integration:** Ensures consistent deployment and scalability by containerizing the frontend and backend.

## Components and Functionality
### Erlang Backend
- Uses Cowboy for HTTP requests and WebSocket connections.
- Manages chat messages and user sessions.
- Distributes messages to all connected users using multicasting with gproc.

### React Frontend
- Handles user interactions for chat sessions.
- Manages real-time message display and user inputs.
- Communicates with the backend via WebSockets for seamless messaging.

### Docker
- Containerizes individual components for consistent deployment.
- Supports multiple frontends and ensures compatibility across environments.
- Simplifies build and run processes with `docker-compose up --build`.

## Implementation and Integration
- Backend and frontend are developed separately and integrated using WebSocket.
- Docker Compose manages the orchestration of backend and frontend services.
- Modular design allows for scalability and easy maintenance.

## Conclusion
This project effectively demonstrates the integration of Erlang, React, and Docker in creating a functional, real-time chatroom application. It highlights the strengths of each technology in handling real-time communication, user interface design, and deployment consistency. This project serves as a practical example of building scalable and interactive web applications using a combination of backend and frontend technologies within a Dockerized environment.

import React, { useState, useEffect } from 'react';
import './CreateSession.css';

function CreateSession() {
  const [username, setUsername] = useState('');
  const [message, setMessage] = useState('');
  const [messages, setMessages] = useState([]);
  const [userCreated, setUserCreated] = useState(false);

  // WebSocket connection
  const socket = new WebSocket('ws://localhost:8080/sessions');

  // Handle WebSocket events
  useEffect(() => {
    socket.addEventListener('open', (event) => {
      console.log('WebSocket connection opened:', event);
    });

    socket.addEventListener('message', (event) => {
      console.log('Recieved message', event);
      const data = JSON.parse(event.data);
      console.log('Parsed message data:', data);
      setMessages([...messages, { username: data.username, message: data.message }]);
    });

    socket.addEventListener('close', (event) => {
      console.log('WebSocket connection closed:', event);
    });

    return () => {
      // Clean up WebSocket on component unmount
      socket.close();
    };
  }, [messages]); // Include dependencies to prevent unnecessary reconnects

  // Function to create user and initiate WebSocket connection
  const handleCreateUser = () => {
    setUserCreated(true);
    socket.send(JSON.stringify({ type: 'user-created', username }));
  };

  // Function to send message via WebSocket
  const handleSendMessage = (event) => {
    event.preventDefault();
    socket.send(JSON.stringify({ type: 'send-message', username, message }));
    setMessage('');
  };

  return (
    <div>
      {!userCreated ? (
        <div>
          <input
            type="text"
            value={username}
            onChange={(e) => setUsername(e.target.value)}
            placeholder="Username"
          />
          <button onClick={handleCreateUser}>Create User</button>
        </div>
      ) : (
        <div>
          <form onSubmit={handleSendMessage}>
            <input
              type="text"
              value={message}
              onChange={(e) => setMessage(e.target.value)}
              placeholder="Write a message"
            />
            <button type="submit">Send Message</button>
          </form>
          <div className="message-box">
            {messages.map((msg, index) => (
              <p key={index}>
                <b>{msg.username}:</b> {msg.message}
              </p>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

export default CreateSession;

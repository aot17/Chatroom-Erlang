import React, { useState, useEffect } from 'react';
import { sendMessage, addMessageListener, removeMessageListener } from './WebSocketManager';
import './CreateSession.css';

function CreateSession() {
  const [username, setUsername] = useState('');
  const [message, setMessage] = useState('');
  const [messages, setMessages] = useState([]);
  const [userCreated, setUserCreated] = useState(false);

  useEffect(() => {
    const handleMessage = (event) => {
      try {
        const data = JSON.parse(event.data);
        setMessages(prevMessages => [...prevMessages, { username: data.username, message: data.message }]);
      } catch (error) {
        console.error('Error parsing message data:', error);
        // Handle non-JSON messages or log for debugging
        console.log('Received non-JSON message:', event.data);
      }
    };

    addMessageListener(handleMessage);

    // Clean up: Remove the message listener when the component unmounts
    return () => {
      removeMessageListener(handleMessage);
    };
  }, []);

  const handleCreateUser = () => {
    setUserCreated(true);
    sendMessage(JSON.stringify({ type: 'user-created', username }));
  };

  const handleSendMessage = (event) => {
    event.preventDefault();
    sendMessage(JSON.stringify({ type: 'send-message', username, message }));
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

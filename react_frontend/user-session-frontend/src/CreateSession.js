import React, { useState } from 'react';
import './CreateSession.css'; // Import CSS file for styling

function CreateSession() {
  const [username, setUsername] = useState('');
  const [message, setMessage] = useState('');
  const [messages, setMessages] = useState([]);
  const [userCreated, setUserCreated] = useState(false);

  const handleCreateUser = async () => {
    // Logic to create user in Erlang key-value store
    // For now, we just simulate user creation
    setUserCreated(true);
    // You need to replace this with a POST request to your Erlang backend
  };

  const handleSendMessage = async (event) => {
    event.preventDefault();
    // Logic to send message - this will be similar to what you already have
    setMessages([...messages, { username, message }]);
    setMessage(''); // Clear message input after sending
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
          <div className ="message-box">
            {messages.map((msg, index) => (
              <p key={index}><b>{msg.username}:</b> {msg.message}</p>
            ))}
          </div>
        </div>
      )}
    </div>
  );
}

export default CreateSession;

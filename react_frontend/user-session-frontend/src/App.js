// App.js
import React from 'react';
import './App.css';
import CreateSession from './CreateSession';

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <h1>Join the Chatroom</h1>
        <CreateSession />
      </header>
    </div>
  );
}

export default App;


// App.js
import React from 'react';
import './App.css';
import CreateSession from './CreateSession'; // Make sure this path is correct

function App() {
  return (
    <div className="App">
      <header className="App-header">
        <h1>Create a user</h1>
        <CreateSession />
      </header>
    </div>
  );
}

export default App;


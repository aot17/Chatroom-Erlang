// WebSocketManager.js
let socket = null;

export const initializeWebSocket = () => {
  socket = new WebSocket('ws://localhost:8080/sessions');

  socket.addEventListener('open', () => {
    console.log('WebSocket connection opened');
  });

  socket.addEventListener('close', () => {
    console.log('WebSocket connection closed');
  });
};

export const sendMessage = (message) => {
  if (socket && socket.readyState === WebSocket.OPEN) {
    socket.send(message);
  } else {
    console.error('WebSocket is not connected.');
  }
};

export const addMessageListener = (callback) => {
  if (socket) {
    socket.addEventListener('message', callback);
  }
};

export const removeMessageListener = (callback) => {
  if (socket) {
    socket.removeEventListener('message', callback);
  }
};

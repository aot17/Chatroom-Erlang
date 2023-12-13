# Use a node image
FROM node:latest

# Set the working directory in the container
WORKDIR /app

# Copy package.json and package-lock.json
COPY package.json package-lock.json ./

# Install dependencies
RUN npm install

# Copy the rest of the application
COPY . .

# Expose the port the app runs on. Default for React is 3000, but it's configurable
EXPOSE 3000

# Start the app using npm start
CMD ["npm", "start"]

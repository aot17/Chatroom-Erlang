# Use an official Erlang runtime as a parent image
FROM erlang:latest

# Set the working directory in the container
WORKDIR /usr/src/app

# Copy the current directory contents into the container
COPY . .

# Compile the Erlang application
RUN rebar3 compile

# Make port 8080 available to the world outside this container
EXPOSE 8080

# Run the application when the container launches
CMD ["rebar3", "shell"]


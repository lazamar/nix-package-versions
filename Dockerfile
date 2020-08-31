FROM haskell:8.6.5

RUN apt-get update && apt-get install -y curl bzip2 adduser tmux vim sqlite3

# Build project as root
COPY . /home/marcelo
WORKDIR /home/marcelo
RUN stack build

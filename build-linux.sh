# Mount current directory and build it in a linux box
docker run -v $(pwd):/mnt haskell:8.6.5 bash -c "cd /mnt && stack build"

# The problem with the command above is that every time it is run it will
# start the build process from scratch. When I built this last time I had
# a few OOM kills. To recover from that the script can be run in interactive
# mode so that the build can restart from where it stops from within the container.
#
# docker run -it -v $(pwd):/mnt haskell:8.6.5 bash

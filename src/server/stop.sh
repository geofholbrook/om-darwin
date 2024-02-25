PORT=$(lsof -t -i:32794 -sTCP:LISTEN)
echo process with pid $PORT is using port 32794\; killing it.
kill -9 $PORT
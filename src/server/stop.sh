PORT=$(lsof -t -i:32794 -sTCP:LISTEN)
if [ -z "$PORT" ]; then
  echo No process is using port 32794.
  exit 0
fi
echo process with pid $PORT is using port 32794\; killing it.
kill -9 $PORT
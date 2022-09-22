### Building and running

```bash
# Go into docker-compose folder
cd docker-compose
# Build all docker images
docker-compose build
# Run shinyproxy
docker-compose up shinyproxy
## or
docker-compose up -d shinyproxy
```

Visit: <http://localhost:8080>

To stop shinyproxy use `docker-compose down`.

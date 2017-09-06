# Glicko Ratings

Ratings system for a tiered league system based on [Glicko ratings](http://www.glicko.net/research/glicko.pdf)

## Building

To build the docker container, from the root of this repo, run.

```
make build
```

## Running

To run, run

```
docker run --rm -p 3838:3838 -v <data dir>:/srv/shiny-server/pingpong/data/ nathandemaria/glicko
```

where `<data dir>` is a directory containing `default.csv`, meeting the requirements for `load_results`.

If you just want to run, the container is up on [Docker Hub](https://hub.docker.com/r/nathandemaria/glicko/)

If you want to save logs, also add `-v <path to log dir>:/var/log/shiny-server`

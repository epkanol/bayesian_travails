1. Build the docker image via: `docker build -t epkanol-travails .`

2. Run the docker image via: `docker run --cpus=8 -p 3435:3435 epkanol-travails`
   The first port can be chosen freely, according to what is available on your local host.
   * All Rmd files in the `study` directory will be exposed on the port.
   * Note that the container requires a lot of memory to run - possibly more than 32 GiB. Make sure you have enough swap space on your computer.
   * It is recommended to run with at least 8 vCPUs. The number of cores and threads consumed by R is specified in the `ingest_data.R` file.

3. Models can be saved between invocations of the container, which saves a lot of compute time.
   Inside the docker environment, all models are created in the directory `/home/app/study/.cache`
   If this directory is mapped to a host directory, the models will be stored in that directory, and can be reused in the next container invocation.
   1. First create a suitable cache-directory on the host, and make it writable for the docker container:
      `mkdir -p ${PWD}/.dockercache && chmod a+rwx ${PWD}/.dockercache`
      The R scripts will create files here that is owned by uid 2000 (defined in the `RUN useradd` statement in the Dockerfile)
   2. Mount this directory to the container, as path `/home/app/.cache`
      `docker run --cpus=8 -p 3435:3435 --mount type=bind,source=${PWD}/.dockercache,target=/home/app/.cache epkanol-travails`

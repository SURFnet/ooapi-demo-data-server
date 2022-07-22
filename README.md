# OOAPI Demodata server

OOAPI Demodata server - just add water to get an working [OOAPI](https://openonderwijsapi.nl/) endpoint with realistically looking data. This is useful for applications that have to work with an OOAPI endpoint but that do not yet have access to a real endpoint implemented by an educational institution.

When started, OOAPI Demodata server generates random, but realistically looking data. All entities described in the the [OOAPI specification 4.0](https://open-education-api.github.io/specification/v4/docs.html) or [OOAPI specification 5.0](https://open-education-api.github.io/specification/v5/docs.html) get generated, including links between entities.

## Building and running
OOAPI Demodata server is implemented in Clojure. There are two ways of building and running it:

### Using Clojure and Java
Make sure you have [Clojure installed](https://clojure.org/guides/install_clojure) and a recent version of Java. You can then run the following command:

`clojure -X:depstar uberjar :jar ooapi-demo-data-server.jar`

This will generate a jar you can run using the following command:

`java -cp ooapi-demo-data-server.jar clojure.main -m ooapi-demo-data-server.core`

The server is now available on http://localhost:8080.

### Using Docker
Alternatively you can use Docker. Build a Docker image by running the following command:

`docker build . -t ooapi-demo-data-server:latest`

Then run using:

`docker run -p 8080:8080 ooapi-demo-data-server:latest`

## Features

### Choosing the OOAPI version
Use the environment variable `OOAPIVERSION` to choose which OOAPI version to support. Allowed values are `v4` and `v5`.

### Setting the seed
Each time the server runs, new random data is generated. To make runs reproducible you can set the seed user for random generation. Use the environment variable `SEED` to an integer. Reusing the same integer across runs should result in the same data being generated each time.

### Chaos mode
Chaos mode was added to how well applications can handle malfunctioning OOAPI endpoints. You can set chaos mode as active by setting the environment variable `CHAOS` to `true`. When chaos mode is active, requests will be handled using a randomly picked `mode`. The following modes are available:
- `normal`: The request is handled as it normally would.
- `empty`: Returns an empty response.
- `slow`: Requests are handled normally, but with a randomly chosen delay between 3 and 63 seconds.

Which modes are active can be chosen by setting the environment variable `CHAOS_MODES` to a comma-delimited string of modes. This defaults to `normal,empty,slow`.
#Stream-web

## This webserver is implemented based on [Streamly](https://github.com/composewell/streamly.git) using TCP sockets.

Stream-web exposes Modules StreamWeb and StreamWeb.Types and exposes function `startServer` which takes a portNumber and returns a stream of Tuple (socket, Request).


## Request Handler
First 2048 bytes will be read from socket and parse all the headers then with the value of Content-Length read the remaining data from socket and parse the body then convert it into Json then to ByteString.


## Response Handler
StreamWeb exposed three different functions to work with responses
* sendJson will take byte-stringified Json and sends the response 
* sendStatus will take status and sends the response with body as corresponding message for status
* sendWithStatus will take ByteString, status and headers and build response

**Maximum Size of headers is 2K and unless a GET request, without Content-Length request won't be processed**

# Supported Features
* Content-Length based request parser
* CORS support

#TODO
* Middlewares support (critical)
* Add new api support (critical)
* Chunk Transfer based request parser (non-critical)
* Accept Ranges (non-critical)


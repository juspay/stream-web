#Stream-web

## This webserver is implemented based on [Streamly](https://github.com/composewell/streamly.git) using TCP sockets.

Stream-web exposes Modules StreamWeb and StreamWeb.Types and exposes function `startServer` which takes a portNumber and returns a stream of Tuple (socket, Request).


**Maximum Size of headers is 2K and unless a GET request, without Content-Length request won't be processed**

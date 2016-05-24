# CrossCourse

CrossCourse is a chat server based on websockets. 

### Building the server

You'll need cabal & ghc installed. Once/if you have them installed

    git clone git@github.com:fhsjaagshs/crosscourse.git
    cd crosscourse
    cabal sandbox init
    cabal build
    
### Running the server

It's simple to run once compiled

    PORT=3000 dist/build/crosscourse/crosscourse
    
It takes the ENV variable `PORT` to determine which port it runs on.

### Communicating with JavaScript

First, initialize a websocket connection:

    var socket = new WebSocket('ws://localhost:3000/','crosscourse');
    
Then, send commands:

    function startedTyping(ws,uid) {
      var buffer = new ArrayBuffer(5);
      var uint8buffer = new Uint8Array(buffer,0,1);
      var int64buffer = new Int64Array(buffer,
      uint8buffer[0] = 1;
      int64buffer[0] = uid;
      ws.sendBytes(buffer);
    }
    
    function startedTyping(ws,uid) {
      var buffer = new ArrayBuffer(5);
      var uint8buffer = new Uint8Array(buffer,0,1);
      var int64buffer = new Int64Array(buffer,
      uint8buffer[0] = 2;
      int64buffer[0] = uid;
      ws.sendBytes(buffer);
    }
    
    startedTyping(socket,123); // user # 123 started typing
    stoppedTyping(socket,123); // user # 123 stopped typing


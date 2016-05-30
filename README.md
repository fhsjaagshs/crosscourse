# CrossCourse

CrossCourse is a sophisticated chat server based on WebSockets. It's also a library providing chat logic over anything you can model with a Haskell GHC `Handle`.

### Currently implemented

Most chat logic; you can create chat rooms, send messages, and get typing notifications.

### Not currently implemented

- Persistence
	- persistence mechanism TDB
- Read/delivered receipts & timestamps
- Push notifications †
- Message synchronization †
- Clustering †
- Miscellaneous tits & bits

The items with a dagger require persistence.

### Clients

While there are basic clients in Objective-C (and anticipated frontend JavaScript), you can also use the protocol described below directly.

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

### The Protocol

CrossCourse uses a binary protocol over WebSockets called `crosscourse`. You must declare this protocol when connecting:

    var socket = new WebSocket('ws://localhost:3000/','crosscourse');
 
**The protocol always uses big-endian values.** The following notation will be used for the rest of the documentation:

`*` 8-bit unsigned integer

`%` opcode, 8-bit unsigned integer

`#` 64-bit unsigned integer

`u` user, 128-bit UUID

`c` chat, 128-bit UUID

`b` a byte of raw binary data, often preceeded by a 64-bit unsigned integer

if a term is surrounded by brackets (`[`,`]`), it's deemed to be a variable number of terms of that type. You'll see this notation under chat creation & messaging.
   
    
#### Authentication

opcode: `0x0`

request format: `%u`

response format: `%u`


Send the server a UUID to authenticate your connection with. The server echoes it.

#### Typing

opcode (start typing): `0x1`

opcode (stop typing): `0x2`

request format: `%c`

response format: `%uc`


When you send a typing message, the server uses the connection's authenticated UUID. All other users in the designated chat room receive a response dictating that a given user started typing in a given chat.

#### Messaging

opcode: `0x3`

request format: `%c*#[b]`

response format: `%uc*#[b]`

Send a message. The other users in the chat will receive the response, never the sender. In the future, there will be read/delivered messages to make this less like a UDP datagram-style kind of thing.

#### Chat Creation

opcode: `0x4`

request format: `%c#[u]`

response format: `%c`

Once a chat has been created, all users (including the creator) will receive a notification that the chat has been created in the format of the response.


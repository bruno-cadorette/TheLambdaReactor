# TheLambdaReactor
FRP online multiplayer game using Haskell and Elm

This project is a proof of concept for using Functionnal Programming in a game engine.
The Lamba Reactor is using Functionnal Reactive Programming(FRP) to acheive its task.
FRP allow us to not worry about thread-safe issue and only focus on logic.
For communication, we use Socket.IO, which is a fast way to communicate between server and clients. 

If you wish to use this for another project, here is a little how-to
1- Create your event diagram as listenerExample :: [SocketListener ApiExample] in src/Backend/Chat.hs
2- using a function like serverEventNetwork, you receive an Event for each event. You can create your logic from there.
3- use reactimateSocket to use SocketIO command like emit and broadcast.

Pre-requesite
1-Stack http://docs.haskellstack.org/
2-Elm http://elm-lang.org/

Install
1- Clone the repo
2- sudo stack build
3- Launch executable

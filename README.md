

Setting and unsetting sockets

- 0.18 - the subscriptions chosen
- 0.19 - a specific function called in the update process


Lifecycle
- opening 
    - triggered by user selecting the port; or
    - attempt to reconnect after unexpected closing
    - exponential backoff 
- open (route messages to client)
- closed by server / network down
- closed by client (does not exist, client must deselect client)
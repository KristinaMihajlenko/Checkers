# Checkers

Checkers client\server application

### Requirements

[stack](https://docs.haskellstack.org/en/stable/README/) installed

### Installation

```sh
git clone https://github.com/KristinaMihajlenko/Checkers.git
cd Checkers
stack build
```

### Server usage

```sh
stack exec server <port>
```

### Client usage

```sh
stack exec client <ip> <port>
```
* `<ip>`        - server ip address
* `<port>`      - server port 

# Based on [elm-webpack-starter](https://github.com/simonh1000/elm-webpack-starter)

## Installation

Clone this repo into a new project folder and run install script.
(You will probably want to delete the .git/ directory and start version control afresh.)

With npm

```sh
$ git clone git@github.com:simonh1000/elm-webpack-starter.git new-project
$ cd new-project
$ npm install
```

## Developing

Start with Elm debug tool with either
```sh
$ npm start
or
$ npm start --nodebug
```

the `--nodebug` removes the Elm debug tool. This can become valuable when your model becomes very large.

Open http://localhost:3000 and start modifying the code in /src.  **Note** that this starter expects you have installed [elm-format globally](https://github.com/avh4/elm-format#installation-). 


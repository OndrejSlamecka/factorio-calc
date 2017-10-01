# Factorio Calculator

This web-based program calculates how many factories you need in
[Factorio](https://www.factorio.com/) to produce the given number of
items in the given time. The information and images were taken from [the
official Factorio Wiki](https://wiki.factorio.com/).


**[Go to calculator.](https://www.slamecka.cz/factorio-calc)**


## Development

The project uses [PureScript](http://www.purescript.org/) and
[purescript-thermite](https://github.com/paf31/purescript-thermite)
(a [React](https://facebook.github.io/react/) wrapper).

    npm install -g purescript pulp bower
    npm install
    bower install
    npm run watch # see package.json how this is implemented
    npm test # if you want to make sure nothing broke
    # open index.html in browser

This is my first PureScript and React project and I welcome all
suggestions for code improvements.
I am using 2 space indentation, 100 characters line width limit.

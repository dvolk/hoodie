# hoodie

A small, toy roguelike in Haskell

[Obligatory screenshot one](http://i.imgur.com/yVNtv9p.png)
[two](http://i.imgur.com/3oeY3hs.png)
[three](http://i.imgur.com/vPtUD7L.png)

## Instructions

### Controls

Move into enemies to attack them.

* `hjklyubn` - vi keys to move
* `q` - quit
* `p` - message log (`p` again to go back)
* `?` - help (`?` again to go back)
* `,` - pick up item
* `d` - drop an item
* `a` - apply an item
* `i` - display inventory
* `e` - wield an item
* `r` - restart the game
* `>` - go down a level (if on stairs down)
* `<` - go up a level (if on stairs up)

### Downloading

    $ mkdir ~/src
    $ cd ~/src
    $ git clone https://github.com/dvolk/hoodie

### Building

    $ cd hoodie
    $ cabal configure
    $ cabal build

You will also need to get the hfov library if you don't have it yet (you don't):

    $ git clone https://github.com/nornagon/hfov

It is installed in a rather similar way.

### Running

    $ ./dist/build/hoodie/hoodie

## Contributing

Feel free to do that

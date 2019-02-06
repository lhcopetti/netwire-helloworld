# netwire-helloworld

An application that consists of a single square that can be moved around, have its size increased or decreased and teleported to a previous location. This is the result of my first effort in learning functional reactive programming concepts using Haskell and the FRP library, netwire.

The project is based on the work done by Oliver Charles on his [excellent post](https://ocharles.org.uk/blog/posts/2013-08-01-getting-started-with-netwire-and-sdl.html) for beginner FRP development using netwire and SDL. Besides completing some of the exercises he proposed at end of his article, I also added some of my own requirements. In the end, that is what we have got:


* The square can be moved around using the arrow keys
* The movement command is "sticky", meaning that whatever action you used last will be repeated indefinitely until interrupted by a different command.
* The square upon reaching any of the bounds of the screen will bounce back in the opposite direction, disregarding the "sticky" behavior.
* The object size can be increased/decreased.
* The object may teleport. You can save the current position of the object by pressing one key, and restore the square to that exact location by pressing another. (this was a really tough one for me).



### Controls

```
Arrow keys: up/down and left/right movement
<Space>: stop the square movement
<M/P> : decrease/increase the square size
<A/S> : save/restore the square position
```

### Installation

To run this project, you will need to have the libraries for SDL installed on your system.

On Ubuntu, this can be as simple as:

```
sudo apt install libsdl-dev
```

At the time of this writing, this package actually installs `libsdl1.2-dev`:

![sudo apt install libsdl-dev][apt-install-libsdl-dev]


Or visit [their site](https://www.libsdl.org/download-2.0.php) and follow the instructions for your operating system.


### Build and Run

```
stack build
stack exec netwire-helloword-exe
```

### Demo






[apt-install-libsdl-dev]: https://github.com/lhcopetti/netwire-helloworld/raw/master/docs/imgs/apt-install-libsdl-dev.png "sudo apt install libsdl-dev"

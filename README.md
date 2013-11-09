Gramophone
==========

Gramophone is an music library manager and player. While there are lots of music
library managers out there, many of them open source, I have a couple of reasons
for wanting to write a new one:

*   My main goal writing Gramophone is just to learn some new things,
    primarily Haskell. I specifically wanted to write a "normal" user
    application that was a decent size/complexity project but didn't require
    anything advanced--in addition to learning Haskell, I wanted to get a good
    feel for what it's like as an "everyday" programming language.

    Reviewing database/SQL stuff and learning some web programming is a bonus.

*   Features Gramophone will have that other music library managers lack (to
    my knowledge):

    -   First-class handling of composers. In particular, classical albums
        often feature a specific composer, and I often want to listen to music
        by a specific composer, but the performer/artist of a track or album
        is equally important.

    -   Multiple artists---"Bela Fleck", "Bela Fleck & Edgar Meyer" and "Bela
        Fleck and the Flecktones" are different "artists" but all include
        "Bela Fleck". Gramophone should understand this.

Originally, Gramophone was going to be a GTK application, but I later decided
to use the Yesod web framework as my GUI. The impetus for that change was that
I decided it would be easier to use a Haskell-specific framework for my first
significant Haskell project. The added bonus is that you'll be able to install
Gramophone on an HTPC and then controll it remotely using another computer,
tablet or (possibly) phone.

Building
--------

The following packages are required to build Gramophone on Ubuntu 13.10:

* haskell-platform
* libsqlite3-dev
* libglib2.0-dev
* libgstreamer0.10-dev
* libgstreamer-plugins-base0.10-dev

Additionally, gtk2hs-buildtools must be installed via cabal.
# Jeff Abrahamson does dotfiles

## Dotfiles

These are my dotfiles.  I use them on linux systems, mostly ubuntu.
YMMV, but feedback is most welcome.  If you can help me make them work
for you, I'd be very happy to entertain suggestions or (better) pull
requests.  The best way to offer suggestions is often by filing
issues.  You can also just send me mail.  All my info is on my [github
page](https://github.com/JeffAbrahamson).

I was motivated by [holman]() and [ryanb]().  And yet some bits of
what they do doesn't sync with the way my brain works, or the way my
fingers work, or my tastes at the moment.  Probably I mostly
misunderstood them, for which I offer that engineer's or scientist's
apology that mixes with great respect with bull-headed determination
to do something else.  This works for me.  

I have a couple design criteria that are important to me.  My machine
should be fully functional without a network connection (except for
the absence of network).  Nothing in my config should be broken by
development in my git dotfiles: so no symlinks, only copies.  I should
be able to test without installing or updating.  And installing on a
new machine or updating an existing machine should be easy.


## Install

Run this:

```sh
git clone https://github.com/JeffAbrahamson/dotfiles.git dotfiles-JeffAbrahamson
cd dotfiles-JeffAbrahamson
script/test-dotfiles
script/bootstrap
```


## Components

The code is organized by topic.  The directory scripts/ is purely
administrative, everything else does about what you'd expect it to.  I
mostly strip the leading dot from file names so that I can see
everything easily when working on my dotfiles.  The install scripts
sort out those details.

Each topic directory has its own tiny README, in case what I think is
obvious isn't.


## Bugs

I'd like this to be clean and elegant and to mostly work for everyone.
That said, I don't often test as people who aren't me, and this is a
background utility project, so goodness knows if it will really work
for you out of the box.  I'm pretty sure it won't hurt, up to
overwriting some of your dotfiles if you do script/bootstrap.

If you see problems or opportunities for improvement, please [open an
issue](https://github.com/JeffAbrahamson/dotfiles/issues) on this
repository.


## Thanks

[Zack Holman](http://github.com/holman/) does a great job of marketing
git and github and his own projects.  Time and again he inspires me
with some bit of code or idea for automating something.

In addition, I looked at a bunch of dotfile repos on github when I
first started writing mine.

* [Zach Holman](http://github.com/holman/dotfiles)
* [Ryan Bates](http://github.com/ryanb/dotfiles)
* [bash-it](http://github.com/revans/bash-it)
* [Mathias Bynens](https://github.com/mathiasbynens/dotfiles)
* [ghar](http://github.com/philips/ghar)

And then, the dotfiles themselves are the result and cruft of endless
years of using linux and unix-like machines.

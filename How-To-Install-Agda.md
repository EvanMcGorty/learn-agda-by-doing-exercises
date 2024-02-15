# How To Install Agda

The standard way of installing Agda is to build it from its source, which is written in Haskell.

You can also try using a package manager like Homebrew, but make sure that the version of Agda available is not out of date, and can be used with this tutorial according to [README.md](README.md).

Otherwise, we will need GHC, the standard haskell compiler, and Cabal, a haskell package manager.

The latest versions of both of these should be installed with GHCup: <https://www.haskell.org/ghcup/>

## GHCup

When running the GHCup installation script, you will be prompted a few times. The default choices should be good enough for our purposes, but here is a brief overview of what should happen here:

1. Append/prepend the location of GHCup binaries to your PATH (so that we can use them to install Agda).
2. You don't need to install the haskell-language-server (unless you want to program in haskell).
3. Don't worry about anything relating to haskell's other big package manager, stack, since we won't be using it here.
4. Finally, GHCup will list some other dependencies that you may need to install.

If you have done some programming, chances are you already have all of the necessary dependencies, so it's worth just trying to run the installer. If this fails, then you may need to install any missing dependencies by hand before rerunning the GHCup installation script.

On macOS, you may be prompted to install the Xcode Command Line Tools, which should be sufficient. If you have an M1/M2 Mac, you may need to use Rosetta.

On linux you may need to install some packages with your system package manager of course.

See https://www.haskell.org/ghcup/install/ for more.

## GHC and Cabal

Now, open a *new* terminal window and simply enter:

```bash
ghcup install ghc recommended
ghcup set ghc recommended
```

and

```bash
ghcup install cabal recommended
ghcup set cabal recommended
```

To ensure that the recommended versions of ghc and cabal are installed.

## Agda

Finally, we can compile and install Agda with:

```bash
cabal update
cabal install Agda
```

This should take a while to finish.

Afterwards you can run:

```bash
agda --version
```

to verify that your installation was successful.

## IDE Support (VSCode)

To get the full Agda experience, you absolutely need proper IDE support.

As of writing this, the two viable options are VSCode and Emacs.

If you are not particularly drawn to Emacs, I would recommend just using [VSCode](https://code.visualstudio.com/), and installing the agda-mode extension.

In the agda-mode settings, there is an option to enable the Agda Language Server, which is entirely unnecessary and can prevent Agda from working with VSCode entirely, so it is probably worth keeping this off for now.

If you have any problems with Agda commands, see the troubleshooting section of the agda-mode extension.

## I ran into a problem or want to use Emacs

If anything goes wrong, you can check out the much more exhaustive official documentation for installing Agda: <https://agda.readthedocs.io/en/latest/getting-started/installation.html>

You can also try the installation instructions from another great Agda tutorial, which also covers Emacs usage: <https://plfa.github.io/GettingStarted/>

Note that you do not need to install the Agda stdlib for this tutorial.

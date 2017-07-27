# Prerequisites

The only tool you need to install yourself is [stack](https://www.haskellstack.org).


# The template application

We've tried to make it as easy as possible to get from zero to a running
application. For that purpose, the source repository comes with multiple
example applications. The one we're going to use here is the **template**
application.

    git clone https://github.com/wereHamster/nauva.git
    cd nauva
    ./bin/nauva start template/app

Eventually, a browser should open pointing to a local web server where the
application is running. I say eventually because it may take a while. If this
is your first time you run the `bin/nauva` script, it will download the GHC
compiler and all other dependencies that are required. This may easily take
more than 30 minutes.

If for some reason the browser doesn't open automatically, go to [http://localhost:8000](http://localhost:8000). Note that the server may be running one a different port
if port 8000 is already occupied. See in the terminal output which port the server
has picked.

# Your first changes

The page you see tells you which file you can change. Open the file, and make
some changes. Immediately after you save your changes, you'll see that the page
updates.

Or if you make a mistake such that the code doesn't cleanly compile, you'll see
the errors inside your browser.


# Next

[The markup language](/markup)

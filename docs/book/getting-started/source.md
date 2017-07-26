For best experience use macOS. Better support for other platforms (Linux, Windows)
will follow later.

First install [stack], then:

    git clone https://github.com/wereHamster/nauva.git
    cd nauva
    ./bin/nauva start template/app

A browser should automatically open. If not then open [http://localhost:8000](http://localhost:8000)
manually.

```hint
if port 8000 is already occupied the server will pick the next
free port. See in the output which port the server has picked.
```

Now edit the template app source file (product/template/app/dev/src/Main.hs),
save and observe how the UI instantly reloads to reflect your changes.
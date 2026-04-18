# Bornet

## How to run

Before running the server for the first time:

```
rm db.sqlite && sqlite3 db.sqlite < sql/schema.sql && sqlite3 db.sqlite < sql/data.sql && sqlite3 db.sqlite < sql/horario_inserts.sql  
```

Also download htmx and [hx-drag](https://github.com/AjaniBilby/hx-drag) into `/static`:

```
curl -L -O https://cdn.jsdelivr.net/npm/htmx.org@2.0.8/dist/htmx.min.js
curl -L -o hx-drag.js https://unpkg.com/hx-drag@2.0.0
```

Then:

```
cabal build bornet
cabal run bornet:exe:bornet
```

and navigate to `http://localhost:8000/`.


## Useful while developing

```
ormolu --mode inplace $(git ls-files '*.hs')
```


## Links


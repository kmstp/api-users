# Development

Ghcid

```
ghcid --command="stack repl searchengine:searchengine-web" --test=main
```

Generate Modules graph:
```
stack exec graphmod | tred | dot -Tpdf > docs/modules.pdf
```

# Users API

![screenshot](docs/users_api.png)

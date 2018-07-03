# Development

Ghcid

```
ghcid --command="stack repl searchengine:searchengine-web" --test=main
```

Generate Modules graph:
```
brew install graphviz
stack exec graphmod | tred | dot -Tpdf > docs/modules.pdf
```

# Users API

## GET /

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/html;charset=utf-8`

- No response body

## POST /api/v1/users

### Request:

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- ok (`application/json;charset=utf-8`, `application/json`):

    ```javascript
1
    ```

- not ok (`application/json;charset=utf-8`, `application/json`):

    ```javascript
2
    ```

## GET /api/v1/users/:userid

### Captures:

- *userid*: userid of the person to get

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## POST /graphql

### Request:

- Supported content types are:

    - `text/plain;charset=utf-8`

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `application/json;charset=utf-8`
    - `application/json`

- No response body

## GET /static

### Response:

- Status code 200
- Headers: []

- No response body

## GET /styles.css

### Response:

- Status code 200
- Headers: []

- Supported content types are:

    - `text/css;charset=utf-8`

- No response body

![screenshot](docs/users_api.png)

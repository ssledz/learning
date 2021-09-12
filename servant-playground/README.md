Resources
=========
* [tutorial](https://docs.servant.dev/en/stable/tutorial/Server.html)

## having sources

```
hasktags . ~/git/ghc/libraries ~/git/servant
```

## generating hie.yaml

```
cabal install implicit-hie
gen-hie > hie.yaml
```

## other

```
cabal list --installed
```

# Examples

## app1

```
curl http://localhost:8081/users | jq
```

```
[
 {
  "name": "Isaac Newton",
  "email": "isaac@newton.co.uk",
  "age": 372,
  "registration_date": "1683-03-01"
 },
 {
  "name": "Albert Einstein",
  "email": "ae@mc2.org",
  "age": 136,
  "registration_date": "1905-12-01"
 }
]
```

## app2

```
curl http://localhost:8081/albert | jq
```

```
{
  "name": "Albert Einstein",
  "email": "ae@mc2.org",
  "age": 136,
  "registration_date": "1905-12-01"
}
```

## app3

```
curl http://localhost:8081/position/1/2 | jq
```

```
{
  "yCoord": 2,
  "xCoord": 1
}
```

```
curl -X POST -d '{"clientName":"Alp Mestanogullari", "clientEmail" : "alp@foo.com", "clientAge": 25, "clientInterestedIn": ["haskell", "mathematics"]}' -H 'Accept: application/json' -H 'Content-type: application/json' http://localhost:8081/marketing | jq
```

```
{
  "to": "alp@foo.com",
  "body": "Hi Alp Mestanogullari,\n\nSince you've recently turned 25, have you checked out our latest haskell, mathematics products? Give us a visit!",
  "subject": "Hey Alp Mestanogullari, we miss you!",
  "from": "great@company.com"
}
```

## app4

```
curl http://localhost:8081/persons
```

```
[
  {
    "firstName": "Isaac",
    "lastName": "Newton"
  },
  {
    "firstName": "Albert",
    "lastName": "Einstein"
  }
]
```

```
curl -H 'Accept: text/html' http://localhost:8081/persons | tidy -i
```

```
  <table>
    <tr>
      <th>first name</th>
      <th>last name</th>
    </tr>
    <tr>
      <td>Isaac</td>
      <td>Newton</td>
    </tr>
    <tr>
      <td>Albert</td>
      <td>Einstein</td>
    </tr>
  </table>

```

## app5

```
curl http://localhost:8081/myfile.txt
```

## app6

```
curl http://localhost:8081/myfile.txt
```

## app7


## cli server

```
curl -X PUT -d '{"cwpName":"my supper wallet" }' \ 
  -H 'Accept: application/json' \
  -H 'Content-type: application/json' http://localhost:8081/wallet | jq 
```

```
cat tx.draft | curl -s -X POST -d @- \
  -H 'Content-type: application/json' \
  http://localhost:8081/wallet/800d8734-2621-4ba6-a6aa-192882002f58/signTx | jq
```
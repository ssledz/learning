## Credentials

~/.spring-boot-devtools.properties
```
facebook.client.clientId=
facebook.client.clientSecret=

github.client.clientId=
github.client.clientSecret=

keycloak.client.clientId=outh2-demo
keycloak.client.clientSecret=
```

## Running keycloka

```
cd keycloak-7.0.0/bin
./standalone.sh -b localhost -Djboss.socket.binding.port-offset=1000
```

## Resources
* [github](https://github.com/settings/developers)
* [facebook](https://developers.facebook.com/)
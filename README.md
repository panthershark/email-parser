# Email Parser

A library for validating and parsing email addresses respecting RFC 5321 and RFC 5322. It uses `elm/parser` under the covers. 

### Email Validation

```
import Email exposing (isValid)


isValid "hello@world.com" == True
```


### Email Parsing

```
import Email exposing (parse)


parse "hello@world.com" == Ok { local = "hello", domain = "world.com" } 
```

### Email Parsing

```
import Email exposing (toString)


toString { local = "hello", domain = "world.com" }  == "hello@world.com"
```
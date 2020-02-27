# SDUI : Server driven UI

```
runHello :: IO ()
runHello = 
  runServer 8072 "Hello App" $ forever $ do
    firstName <- askUI "First name:"
    lastName <- askUI "Last name:"
    showMsg $ "Hello, " <> firstName <> " " <> lastName
```

![HelloVasya](https://user-images.githubusercontent.com/25211514/75405693-5fb57b80-58dc-11ea-8f54-165beb5820b4.png)

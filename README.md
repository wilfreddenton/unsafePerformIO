# unsafePerformIO

```
docker build -t wilfreddenton/unsafe-perform-io:1.0.1 .

docker run -d -p 8080:8080 \
           --name unsafe-perform-io \
           --mount type=bind,source=/root/db,target=/db \
           wilfreddenton/unsafe-perform-io:1.0.1
```

## Resources

* [Stacking Your Monads](https://www.youtube.com/watch?v=pzouxmWiemg) - [code](https://github.com/benkolera/talk-stacking-your-monads)

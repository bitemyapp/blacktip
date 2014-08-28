# Blacktip

![](blacktip.jpg "Blacktip Shark")

Blacktip is a k-ordered unique id service and a clone of Boundary's Flake.

## Benchmarks

### Blacktip

![](bench.png "Benchmark results, Blacktip is faster than Flake")

### Flake

```
(flake@127.0.0.1)11> flake_harness:timed_generate(100000).
src/flake_harness.erl:33:<0.75.0>: generating ids: 0.272 s
```

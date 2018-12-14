# Skater

A framework for benchmarking shell-commands written in Haskell.

## Goals
The idea is to provide a simple, but precise, easy-to-use environment where it is possible to launch benchmarks and collect execution time.

## Building
`stack` tool is required. After you have install it in your system:
```
git clone https://github.com/fusiled/Skater
cd Skater
stack build
```
The library heavily leverages `Turtle` for spawning shell commands. The execution time is collected with the `criterion` package.

## Usage
```
stack exec Skater-exec <path-to-skater-context> <path-to-benchmark-file>
```
A basic test example can be launched with:
```
stack exec Skater-exec examples_json/default_context.json examples_json/benchmark_example.json
```

You can write you own JSON files by looking at the simple examples provided in the `examples_json` folder. If you want to add different type of `Benchmark` you can extend the `data Benchmark` inside `src/Skater.hs`

## TODO
- **Improve JSON parsing**. The JSON parsing is very simple. The idea is to use generics for making the system more flexible (Removing a Map from the source code in `Skater.hs`)
- Implement usage of `stderr` stream (a `Turtle` limitation?). It seems that with `Turtle` it is possible to use the `stdout` stream. It could be possible to try to hard-code in the command string the redirection to `/dev/stderr`, but it doesn't look very nice.
- Implement test-cases

### Post-Note

This is my first work written in Haskell. Please be patient if everything is not performed in the *Haskell way*. For any feedback, please contact me.
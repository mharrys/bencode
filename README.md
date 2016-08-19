# Bencode
Encoder and decoder for the BitTorrent protocol "Bencode".

# Run tests
This assumes sbt is installed and exist in your path.

    $ sbt
    ... text omitted
    > test

# Examples
Remember to ``import bencode._`` before running the examples. Run ``console''
once inside sbt.

## Encode

    scala> encode(BInt(42))
    => i42e

    scala> encode(BStr("foo"))
    => 3:foo

    scala> encode(BList(BInt(42), BStr("foo")))
    => li42e3:fooe

    scala> encode(BDict("foo" -> BInt(42)))
    => d3:fooi42ee

## Decode

    scala> decode("i42e")
    => Right(42)

    scala> decode("3:foo")
    => Right(foo)

    scala> decode("li42e3:fooe")
    => Right([42 foo])

    scala> decode("d3:fooi42ee")
    => Right({foo -> 42})

## Zipper
The decoded results can easily be explored with a zipper.

```scala
val ds = decode("li42eld3:food3:barl5:helloeeeei100ee").right.get
// => [42 [{foo -> {bar -> [hello]}}] 100]

val z = BValueZipper(ds, Nil)
// => BValueZipper([42 [{foo -> {bar -> [hello]}}] 100],List())

val result = for {
  z2 <- z.downList
  z3 <- z2.forward
  z4 <- z3.downList
  z5 <- z4.downDict("foo")
  z6 <- z5.downDict("bar")
  z7 <- z6.downList
  // replace the nested string "hello" with "world"
  z8 = z7.set(BStr("world"))
  z9 <- z8.up
  z10 <- z9.up
  // append new int to nested dictionary
  z11 <- z10.putDict("zot", BInt(1337))
} yield (z11)

// walk up from dictionary focus to root and print result
result match {
  case Some(z) =>
    println(z.root.focus)
  case None =>
    println("Failure")
}
// => [42 [{foo -> {bar -> [world] zot -> 1337}}] 100]
//                          ^^^^^  ^^^^^^^^^^^
//                            |         |
//                            |         +- new item
//                            + hello replaced
```

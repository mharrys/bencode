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
    => Right(BInt(42))

    scala> decode("3:foo")
    => Right(BStr(foo))

    scala> decode("li42e3:fooe")
    => Right(BList(List(BInt(42), BStr(foo))))

    scala> decode("d3:fooi42ee")
    => Right(BDict(Map(foo -> BInt(42))))

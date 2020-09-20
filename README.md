# minicheck

> A state-of-the-art Property-Based-Testing library written in less than 200 lines Scala.

## About

This is a research project to develop a state-of-the-art Property-Based-Testing library.
[minicheck](src/main/scala/minicheck.scala) is a product of this project, written in less than 200 lines Scala, but it has some advanced features:

  - *Integrated Shrinking*:
    A shrinking function is integrated with a value generator.
  - *Higher-Order Functions*:
    To generate and shrink higher-order functions is supported.

See the paper ([EN](https://github.com/MakeNowJust-Labo/minicheck/releases/download/20200916/minicheck-en-20200916.pdf), [JA](https://github.com/MakeNowJust-Labo/minicheck/releases/download/20200916/minicheck-ja-20200916.pdf)) for a detailed explanation.

**NOTE**: This library is for demonstrating the research result.
          If you want a production-ready Property-Based-Testing library, please see [Hariko](https://github.com/MakeNowJust-Labo/hariko).

## License

MIT License.

(C) 2020 TSUYUSATO "MakeNowJust" Kitsune

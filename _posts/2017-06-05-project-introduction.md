---
layout: post
title: Project Introduction
---

My name is Edvard Hübinette, and I have gotten the opportunity to work with open-source Haskell during the upcoming months thanks to [Summer of Haskell](https://summer.haskell.org/), an awesome initiative that funds a handful of students to work with Haskell over the summer. My project will focus on adapting the [streaming](https://hackage.haskell.org/package/streaming) library to use with a prototype version of GHC that supports linear types, in order to make it safer. 

This GHC extension is based on a [paper](https://github.com/tweag/linear-types/releases/download/v1.0/hlt.pdf) that successfully retrofits linear types into the compiler in a very neat way; we experimented with a different approach in my [bachelor thesis](https://github.com/m0ar/lollipop) which is why I am particularly excited to continue working with linear types in the wild, applying them to (hopefully) improve a well-used library.

I will be working together with a mentor from [Tweag I/O](https://www.tweag.io/), namely Arnaud Spiwack, who is also the developer of the GHC fork and co-author of the underlying paper. This blog will be used to document this project both for my own sake and the interested public (if there exists such a group, questions and hoorays would be much appreciated!), and the project itself can be found [here](https://github.com/m0ar/safe-streaming).

Cheers,
Edvard



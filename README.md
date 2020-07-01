# CombineExperiments
Experiments to understand how combine works.

## Summary of findings

### `showActivityIndicatorWhileWaiting` 
After talking to an Apple engineer at a WWDC lab he thought that using `sink` to show the activity indicator was preferable to using `handleEvent`.  `handleEvent` was mentioned as an answer on my stackoverflow answer. The Apple engineer said that `handleEvent` was mainly built for debugging.

Even if `showActivityIndicatorWhileWaiting` was rewritten using `handleEvent` there are still use cases where its useful to `sink` more than once to combine pipeline.

### `Future` semantics
Subscribing twice (using `sink`) to a `Future` will only result in the `Future` being called **once**.

### `URLSession.shared.dataTaskPublisher` 
Subscribing twice (using `sink`) to a `dataTaskPublisher` will result in the `dataTaskPublisher` being called **twice**. BAD!. Established empirically by seeing how many requests are made to a test webserver. Can be detected using `assertSinglePipeline`.

### `.flatMap`
Introducing `.flatMap` into a publisher chain that is subscribed to twice (using `sink`) results in the closure being called twice and results in two duplicate chains within the closure. `.assertSinglePipeline` will not recognise multiple subscriptions as the whole pipeline from the closure upwards is duplicated.

### `.share()`
Calling `share` will solve the multiple subscription issues, but need to ensure that it is called after any flatmaps and before `sink`.

## See also

* [Opensource independant implementation of Combine](https://github.com/broadwaylamb/OpenCombine)
* [Swift Playground explaining the concepts of the new Combine framework](https://github.com/AvdLee/CombineSwiftPlayground)
* [Combine debugging using operators in Swift](https://www.avanderlee.com/swift/combine-swift/)
* [Understanding Combine](http://www.apeth.com/UnderstandingCombine/)
* [RxSwift to Apple’s Combine Cheat Sheet](https://medium.com/gett-engineering/rxswift-to-apples-combine-cheat-sheet-e9ce32b14c5b)

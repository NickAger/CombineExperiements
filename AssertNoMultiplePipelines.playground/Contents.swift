import Combine
import Foundation

// DoNothing from:  http://www.apeth.com/UnderstandingCombine/operators/operatorscustom.html

struct DoNothing<Upstream: Publisher>: Publisher {
    typealias Output = Upstream.Output
    typealias Failure = Upstream.Failure
    let upstream: Upstream
    init(upstream: Upstream) {
        self.upstream = upstream
        print("Creating DoNothing")
    }
    // When subscribed to, subscribe my Inner _upstream_
    func receive<S>(subscriber: S)
        where S : Subscriber, S.Input == Output, S.Failure == Failure {
            self.upstream.subscribe(Inner(downstream:subscriber))
    }
    class Inner<S:Subscriber, Input>: Subscriber, Subscription
    where S.Failure == Failure, S.Input == Input { // !
        var downstream: S?
        var upstream: Subscription?
        init(downstream: S) {
            self.downstream = downstream
        }
        // satisfy the requirements of being a Subscriber
        // keep subscription, pass _self_ downstream
        func receive(subscription: Subscription) {
              self.upstream = subscription
              self.downstream?.receive(subscription: self)
          }
          // pass input downstream
          func receive(_ input: Input) -> Subscribers.Demand {
              return self.downstream?.receive(input) ?? .max(0)
          }
          // pass completion downstream
          func receive(completion: Subscribers.Completion<Failure>) {
              self.downstream?.receive(completion: completion)
              self.downstream = nil
              self.upstream = nil
          }
        
        // satisfy the requirements of being a Subscription
        // pass demand upstream
        func request(_ demand: Subscribers.Demand) {
            self.upstream?.request(demand)
        }
        // pass cancel upstream
        func cancel() {
            self.upstream?.cancel()
            self.upstream = nil
            self.downstream = nil
        }
    }
}

extension Publisher {
    func doNothing() -> DoNothing<Self> {
        print("In doNothing")
        return DoNothing(upstream:self)
    }
}

// ---

extension Publisher {
    func showActivityIndicatorWhileWaiting(message: String) -> AnyCancellable {
        let cancellable = sink(receiveCompletion: { _ in Swift.print("Hide activity indicator") }, receiveValue: { (_) in })
        Swift.print("Busy: \(message)")
        return cancellable
    }
}

enum ServerErrors: Error {
    case authenticationFailed
    case noConnection
    case timeout
}

func authenticate(username: String, password: String) -> Future<Bool, ServerErrors> {
    Future { promise in
        print("Calling server to authenticate")
        DispatchQueue.main.async {
            promise(.success(true))
        }
    }
}

func downloadUserInfo(username: String) -> Future<String, ServerErrors> {
    Future { promise in
        print("Downloading user info")
        DispatchQueue.main.async {
            promise(.success("decoded user data"))
        }
    }
}

func authenticateAndDownloadUserInfo(username: String, password: String) -> some Publisher {
    return authenticate(username: username, password: password).flatMap { (isAuthenticated) -> Future<String, ServerErrors> in
        guard isAuthenticated else {
            return Future {$0(.failure(.authenticationFailed)) }
        }
        return downloadUserInfo(username: username)
    }
}

let future = authenticateAndDownloadUserInfo(username: "stack", password: "overflow")
let cancellable2 = future.showActivityIndicatorWhileWaiting(message: "Please wait downloading")
let cancellable = future.sink(receiveCompletion: { (completion) in
    switch completion {
    case .finished:
        print("Completed without errors.")
    case .failure(let error):
        print("received error: '\(error)'")
    }
}) { (output) in
    print("received userInfo: '\(output)'")
}


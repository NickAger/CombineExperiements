//
//  AppDelegate.swift
//  TestDoNothing
//
//  Created by Nick Ager on 27/06/2020.
//  Copyright © 2020 Nick Ager. All rights reserved.
//

import UIKit
import Combine


@UIApplicationMain
class AppDelegate: UIResponder, UIApplicationDelegate {
    var cancellable2: AnyCancellable?
    var cancellable: AnyCancellable?
    
    func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplication.LaunchOptionsKey: Any]?) -> Bool {

        let future = authenticateAndDownloadUserInfo(username: "stack", password: "overflow").doNothing()
        // cancellable2 = future.showActivityIndicatorWhileWaiting(message: "Please wait downloading")
        cancellable = future.sink(receiveCompletion: { (completion) in
            switch completion {
            case .finished:
                print("Completed without errors.")
            case .failure(let error):
                print("received error: '\(error)'")
            }
        }) { (output) in
            print("received userInfo: '\(output)'")
        }
        
        return true
    }

    // MARK: UISceneSession Lifecycle

    func application(_ application: UIApplication, configurationForConnecting connectingSceneSession: UISceneSession, options: UIScene.ConnectionOptions) -> UISceneConfiguration {
        // Called when a new scene session is being created.
        // Use this method to select a configuration to create the new scene with.
        return UISceneConfiguration(name: "Default Configuration", sessionRole: connectingSceneSession.role)
    }

    func application(_ application: UIApplication, didDiscardSceneSessions sceneSessions: Set<UISceneSession>) {
        // Called when the user discards a scene session.
        // If any sessions were discarded while the application was not running, this will be called shortly after application:didFinishLaunchingWithOptions.
        // Use this method to release any resources that were specific to the discarded scenes, as they will not return.
    }


}



// based on DoNothing defined: http://www.apeth.com/UnderstandingCombine/operators/operatorscustom.html
struct AssertSinglePipeline<Upstream: Publisher>: Publisher {
    typealias Output = Upstream.Output
    typealias Failure = Upstream.Failure
    let upstream: Upstream
    let receivedSubscriptionCount = CountBox()
    init(upstream: Upstream) {
        self.upstream = upstream
    }
    // When subscribed to, subscribe my Inner _upstream_
    func receive<S>(subscriber: S)
        where S : Subscriber, S.Input == Output, S.Failure == Failure {
            receivedSubscriptionCount.increment()
            assert(receivedSubscriptionCount.value == 1, "Only expected one subscriber, try adding '.share()'")
            self.upstream.subscribe(Inner(downstream:subscriber))
    }
    
    // MARK: - allow for a count in a non-mutating struct
    final class CountBox: CustomStringConvertible {
        var value = 0
        func increment() { value += 1 }
        public var description: String { return "CountBox(\(value))" }
    }
    
    // TODO: consider using `PassthroughSubject` instead
    // MARK: -
    final class Inner<S:Subscriber, Input>: Subscriber, Subscription
    where S.Failure == Failure, S.Input == Input { // !
        var downstream: S?
        var upstream: Subscription?
        init(downstream: S) {
            self.downstream = downstream
        }
        
        // MARK: - Subscriber implementation
        func receive(subscription: Subscription) {
            self.upstream = subscription // keep subscription
            self.downstream?.receive(subscription: self) // pass _self_ downstream
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
        
        // MARK: - Subscription implementation
        func request(_ demand: Subscribers.Demand) {
            // pass demand upstream
            self.upstream?.request(demand)
        }
        
        func cancel() {
            // pass cancel upstream
            self.upstream?.cancel()
            self.upstream = nil
            self.downstream = nil
        }
    }
}

extension Publisher {
    func assertSinglePipeline() -> AssertSinglePipeline<Self> {
        return AssertSinglePipeline(upstream:self)
    }
}

// ---

//extension Publisher {
//
//}

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

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

//        let future = authenticateAndDownloadUserInfo(username: "stack", password: "overflow")/*.assertSinglePipeline()*/
//        let future = downloadUserInfo(username: "Nick")
        let future = authenticateAndDownloadUserInfo8080(username: "stack", password: "overflow")
//        let future = downloadUserInfo8080(username: "Nick")
        cancellable2 = future.showActivityIndicatorWhileWaiting(message: "Please wait downloading")
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
    let prefix: String
    let file: StaticString
    let line: UInt
    let receivedSubscriptionCount = CountBox()
    init(upstream: Upstream, prefix: String, file: StaticString, line: UInt) {
        Swift.print("AssertSinglePipeline.init()")
        self.upstream = upstream
        self.prefix = prefix
        self.file = file
        self.line = line
    }
    // When subscribed to, subscribe my Inner _upstream_
    func receive<S>(subscriber: S)
        where S : Subscriber, S.Input == Output, S.Failure == Failure {
            receivedSubscriptionCount.increment()
            Swift.print("AssertSinglePipeline.receive, \(receivedSubscriptionCount.value)")
            if receivedSubscriptionCount.value != 1 {
                let prefix = self.prefix.isEmpty ? "" : self.prefix + ": "
                fatalError("\(prefix)Only expected one subscriber, but count = \(receivedSubscriptionCount.value). Try adding '.share()' to pipeline", file: file, line: line)
            }

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
    func assertSinglePipeline(_ prefix: String = "",
                              file: StaticString = #file,
                              line: UInt = #line) -> AssertSinglePipeline<Self> {
        return AssertSinglePipeline(upstream:self, prefix: prefix, file: file, line: line)
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

func downloadUserInfo(username: String) -> AnyPublisher<String, ServerErrors> {
    Future { promise in
        print("Downloading user info")
        DispatchQueue.main.async {
            promise(.success("decoded user data"))
        }
    }/*.assertSinglePipeline()*/.eraseToAnyPublisher()
}

func authenticateAndDownloadUserInfo(username: String, password: String) -> AnyPublisher<String, ServerErrors> {
    return authenticate(username: username, password: password).flatMap { (isAuthenticated) -> AnyPublisher<String, ServerErrors> in
        let publisher: AnyPublisher<String, ServerErrors>
        if isAuthenticated {
            publisher = downloadUserInfo(username: username)
        } else {
            publisher = Fail(error: ServerErrors.authenticationFailed).assertSinglePipeline().eraseToAnyPublisher()
        }
        return publisher
    }.eraseToAnyPublisher()
}

func authenticateAndDownloadUserInfo8080(username: String, password: String) -> AnyPublisher<String, ServerErrors> {
    return authenticate(username: username, password: password).flatMap { (isAuthenticated) -> AnyPublisher<String, ServerErrors> in
        let publisher: AnyPublisher<String, ServerErrors>
        if isAuthenticated {
            publisher = downloadUserInfo8080(username: username)
        } else {
            publisher = Fail(error: ServerErrors.authenticationFailed).assertSinglePipeline().eraseToAnyPublisher()
        }
        return publisher
    }.eraseToAnyPublisher()
}


func downloadUserInfo8080 (username: String) -> AnyPublisher<String, ServerErrors> {
        return URLSession.shared.dataTaskPublisher(for: URL(string: "http://localhost:8080")!)
            .mapError { (error) -> ServerErrors in
                return .noConnection
        }
        .map({ (result: URLSession.DataTaskPublisher.Output) -> String in
            String(decoding: result.data, as: UTF8.self)
        }).assertSinglePipeline("downloadUserInfo8080").eraseToAnyPublisher()
}

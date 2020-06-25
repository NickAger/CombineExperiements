// From my Stackoverflow question:
// https://stackoverflow.com/questions/62469222/combine-future-block-called-multiple-times-when-using-flatmap-and-multiple-subsc

import Combine
import Foundation

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
let cancellable1 = future.sink(receiveCompletion: { (completion) in
    switch completion {
    case .finished:
        print("Completed without errors.")
    case .failure(let error):
        print("received error: '\(error)'")
    }
}) { (output) in
    print("received userInfo: '\(output)'")
}

/*
 Two: "Downloading user info" as I've subscribed twice.
 */

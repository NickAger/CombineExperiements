import Combine
import Foundation

extension Publisher {
    func showActivityIndicatorWhileWaitingOriginal(message: String) -> AnyCancellable {
        let cancellable = sink(receiveCompletion: { _ in Swift.print("Hide activity indicator") }, receiveValue: { (_) in })
        Swift.print("Busy: \(message)")
        return cancellable
    }
    // after talking to an Apple engineer at WWDC he thought that this method of showing the activity indicator using `sink` was
    // preferable to using `handleEvent`. He said that `handleEvent` was mainly built for debugging.
    func showActivityIndicatorWhileWaitingFromStackoverflowAnswer(message: String) -> some Publisher {
        return handleEvents(receiveSubscription: { _ in Swift.print("Busy: \(message)") }, receiveCompletion: { _ in Swift.print("Hide activity indicator") })
    }
}

enum ServerErrors: Error {
    case authenticationFailed
    case noConnection
    case timeout
}

func authenticate(username: String, password: String) -> /*Future<Bool, ServerErrors>*/ AnyPublisher<Bool, ServerErrors> {
    Deferred {
    Future { promise in
        print("Calling server to authenticate")
        DispatchQueue.main.async {
            promise(.success(true))
        }
        }
    }.eraseToAnyPublisher()
}

func downloadUserInfo(username: String) -> AnyPublisher<String, ServerErrors> {
    return URLSession.shared.dataTaskPublisher(for: URL(string: "http://localhost:8080")!)
         .mapError { (error) -> ServerErrors in
             return .noConnection
     }
     .map({ (result:URLSession.DataTaskPublisher.Output) -> String in
         let s = String(decoding: result.data, as: UTF8.self)
        print("downloaded string is:", s)
        return s
     })
     .eraseToAnyPublisher()
}

func authenticateAndDownloadUserInfo(username: String, password: String) -> some Publisher {
    let a = authenticate(username: username, password: password).flatMap { (isAuthenticated) -> AnyPublisher<String, ServerErrors> in
        guard isAuthenticated else {
            return Future {$0(.failure(.authenticationFailed)) }.eraseToAnyPublisher()
        }
        return downloadUserInfo(username: username)
    }/*.share()*/
    
    return a
}

let future = authenticateAndDownloadUserInfo(username: "stack", password: "overflow")/*.showActivityIndicatorWhileWaitingFromStackoverflowAnswer(message: "Please wait downloading")*/
//let future = authenticate(username: "stack", password: "overflow")/*.showActivityIndicatorWhileWaitingOriginal(message: "Please wait downloading")*/
 let cancellable2 = future.showActivityIndicatorWhileWaitingOriginal(message: "Please wait downloading")
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

print("completed")




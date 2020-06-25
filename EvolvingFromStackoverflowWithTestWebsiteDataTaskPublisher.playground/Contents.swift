import Combine
import Foundation

extension Publisher {
    // after talking to Apple engineer at WWDC he thought that this method of showing the activity indicator using `sink` was
    // preferable to using `handleEvent`. He said that `handleEvent` was mainly built for debugging.
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

func downloadUserInfo(username: String) -> AnyPublisher<String, ServerErrors> {
        return URLSession.shared.dataTaskPublisher(for: URL(string: "http://localhost:8080")!)
            .mapError { (error) -> ServerErrors in
                return .noConnection
        }
        .map({ (result: URLSession.DataTaskPublisher.Output) -> String in
            String(decoding: result.data, as: UTF8.self)
        })
        .eraseToAnyPublisher()
}

let future = downloadUserInfo(username: "publisher")/*.share()*/
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

print("completed")

// see two requests for content without Share.

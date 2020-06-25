import Combine
import Foundation

enum AppErrors: Error {
    case badConnection
    case someElseWentWrong
}

enum AppErrors2: Error {
    case error2
    case error3
}

enum TestEnum {
    case one
    case two
    case three
    case four
    case five
    case six
    
    static func create(val: Int) -> TestEnum {
        switch val {
        case 1:
            return .one
        case 2:
            return .two
        case 3:
            return .three
        case 4:
            return .four
        case 5:
            return .five
        case 6:
            return .six
        default:
            return .two
        }
    }
    
    var asString: String {
        switch self {
        case .one:
            return "1"
        case .two:
            return "2"
        case .three:
            return "3"
        case .four:
            return "4"
        case .five:
            return "5"
        case .six:
            return "6"
        }
    }

}

func task1() -> Future<Int, AppErrors> {
    return Future { promise in
        print("Perform operation...")
        // Do some work...

        // promise(.failure(.badConnection))
        promise(.success(3))
    }
}

func task2(val: Int) -> Future<TestEnum, AppErrors> {
    return Future { promise in
        print("Perform operation2...")
        promise(.success(TestEnum.create(val: val - 1)))
        // promise(.failure(.badConnection))
    }
}

func task3(anEnum: TestEnum) -> Future<String, AppErrors> {
    return Future<String, AppErrors> { promise in
        DispatchQueue.main.async {
            print("Perform operation3...")
            promise(.success("Hello world: " + anEnum.asString))

        }
    }
}

//func taskPublisher() -> AnyPublisher<Int, Never> {
//    return AnyPublisher { subscriber in
//        print("Perform operation...")
//        // Do some work...
//        subscriber.receive(Int(arc4random()))
//        subscriber.receive(completion: .finished)
//    }
//}


//func combineTask1Task2() -> AnyPublisher<TestEnum, AppErrors> {
//    return task1().flatMap { (val) -> Future<TestEnum, AppErrors> in
//        if val == 3 {
//            print("calling task2")
//            return task2(val: val)
//        }
//        print("creating a new Future to publish '.one'")
//        return Future<TestEnum, AppErrors> { $0(.success(.one)) }
//        }.eraseToAnyPublisher()
//}

func combineTask1Task2() -> Future<TestEnum, AppErrors> {
    let f = task1().flatMap { (val) -> Future<TestEnum, AppErrors> in
        if val == 3 {
            print("calling task3")
            return task2(val: val)
        }
        print("creating a new Future to publish '.one'")
        return Future<TestEnum, AppErrors> { $0(.success(.one)) }
    }
    return Future { promise in
        let kim = KeepInMemory()
        let completion = f.sink(receiveCompletion: { (completion) in
            if  case .failure(let error) = completion {
                promise(.failure(error))
            }
            kim.freeFromMemory()
        }) { (value) in
            promise(.success(value))
        }
        kim.keepObjectInMemory(otherObject: completion)
    }
}

//func combineTask1Task2Task3() -> some Publisher {
//    return combineTask1Task2().flatMap { (anEnum)/* -> Publishers.FlatMap<Future<String, AppErrors>, AnyPublisher<TestEnum, AppErrors>>*/ in
//        return task3(anEnum: anEnum)
//    }
//}

func combineTask1Task2Task3() -> Future<String, AppErrors> {
    let f = combineTask1Task2().flatMap { (anEnum)/* -> Publishers.FlatMap<Future<String, AppErrors>, AnyPublisher<TestEnum, AppErrors>>*/ in
        return task3(anEnum: anEnum)
    }
    return Future<String, AppErrors> { promise in
        let keepInMemory = KeepInMemory()
        let cancellable = f.sink(receiveCompletion: { (completion) in
            if  case .failure(let error) = completion {
                promise(.failure(error))
            }
            keepInMemory.freeFromMemory()
        }) { (value) in
            promise(.success(value))
        }
        // keepInMemory.keepObjectInMemory(otherObject: cancellable)
    }
}

final class KeepInMemory {
    var keepOurselvesInMemory : KeepInMemory?
    var keepAnotherObjectInMemory: Any?
    
    init(_ otherObject: Any? = nil) {
        keepOurselvesInMemory = self
        keepAnotherObjectInMemory = otherObject
    }
    
    func keepObjectInMemory(otherObject: Any) {
        keepAnotherObjectInMemory = otherObject
    }
    
    func freeFromMemory() {
        self.keepAnotherObjectInMemory = nil
        self.keepOurselvesInMemory = nil
    }
    deinit {
        print("KeepInMemory - deinit")
    }
}

autoreleasepool {
    let f = combineTask1Task2Task3()
//    let f = combineTask1Task2()
    print("Let's sink")
    f.sink(receiveCompletion: { (result) in
        print("result1 = \(result)")
    }, receiveValue: { (value) in
        print("value1 = \(value)")
    })
    
    print("Let's sink again")
    let keepInMemory = KeepInMemory()
    let cancellable = f.sink(receiveCompletion: { (result) in
        print("result2 = \(result)")
        keepInMemory.freeFromMemory()
    }) { (value) in
        print("value2 = \(value)")
    }
    keepInMemory.keepObjectInMemory(otherObject: cancellable)
    
    DispatchQueue.main.asyncAfter(deadline: .now() + 5) { [weak cancellable] in
       print("after time")
        if cancellable == nil {
            print("cancellable has been deallocated")
        } else {
            print("WARNING cancellable has not been deallocated")
        }
    }
}


//
//    let g = task3()
//
//    let cancellable = g.sink(receiveCompletion: { (completion) in
//        print("completion = \(completion)")
//        keepInMemory.freeFromMemory()
//    }) { (val) in
//        print("val = \(val)")
//    }
//
//    keepInMemory.keepObjectInMemory(otherObject: cancellable)
//
//    DispatchQueue.main.asyncAfter(deadline: .now() + 5) { [weak cancellable] in
//       print("after time")
//        if cancellable == nil {
//            print("cancellable has been deallocated")
//        } else {
//            print("WARNING cancellable has not been deallocated")
//        }
//    }
//}



//DispatchQueue.main.async {
//    print("cancellatble \(cancellatble)")
//}

//autoreleasepool {
//    class Test {
//        var eat = 20
//        deinit {
//            print("Test - deinit")
//        }
//    }
//
//    let test = Test()
//    print("test.eat = \(test.eat)")
//}

//let trigger = PassthroughSubject<Void, Error>()
//
//let future =  Future<String, Error> { promise in
//                promise(.success("Future Succeded"))
//            }
//
// trigger
// .flatMap { future }
// .sink(receiveCompletion: { completion in
//     print("completion received \(completion)")
// }, receiveValue: { val in
//     print("value received \(val)")
// })
//
//trigger.send(())
//trigger.send(completion: .finished)

//(0..<2).publisher
//.flatMap { _ in return (0..<5).publisher }
//.sink(receiveCompletion: { completion in
//  print("completion received \(completion)")
//}, receiveValue: { val in
//  print("value received \(val)")
//})

/// An "either" type around the possible values
public extension Subscribers {
    enum Event<Value, Failure: Error> {
        case value(Value)
        case complete(Subscribers.Completion<Failure>)
    }
}


let sequenceA = Publishers.Sequence<ClosedRange<Int>, Never>(sequence: 1...4)
let scanB = Publishers.Scan(upstream: sequenceA, initialResult: 10) { state, next in
    print("state = \(state), next = \(next)")
    return state + next
}
var receivedC = [Subscribers.Event<Int, Never>]()
let sinkC = Subscribers.Sink<Int, Never>(
 receiveCompletion: { receivedC.append(.complete($0)) },
 receiveValue: { receivedC.append(.value($0)) }
)
var receivedD = [Subscribers.Event<Int, Never>]()
let sinkD = Subscribers.Sink<Int, Never>(
 receiveCompletion: { receivedD.append(.complete($0)) },
 receiveValue: { receivedD.append(.value($0)) }
)

scanB.subscribe(sinkC)
scanB.subscribe(sinkD)

receivedC
receivedD


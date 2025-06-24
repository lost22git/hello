import Vapor

func routes(_ app: Application) throws {
    app.get { req async in
        "It works!"
    }

    app.get("hello") { req async -> String in
        "Hello, world!"
    }
    
    app.get("proxy") { req async throws -> ClientResponse in
        guard let target: String = try req.query.get(at: "target") else {
            throw Abort(.badRequest, reason: "Argument not found: `target`")
        }
        req.logger.info("Fetching \(target)")
        return try await req.client.get(URI(string: target))
    }

    app.on(.POST,"proxy", body: .collect(maxSize: "1mb")) { req async throws -> ClientResponse in
        guard let target: String = try req.query.get(at: "target") else {
            throw Abort(.badRequest, reason: "Argument not found: `target`")
        }
        req.logger.info("Fetching \(target)")
        let targetUrl = URI(string: target)
        return try await req.client.post(targetUrl) { creq in
            creq.headers = req.headers
            creq.headers.replaceOrAdd(name: .host, value: targetUrl.host!)
            creq.body = req.body.data
        }
    }
}

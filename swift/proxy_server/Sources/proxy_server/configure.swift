import Vapor

// configures your application
public func configure(_ app: Application) async throws {
    // uncomment to serve files from /Public folder
    // app.middleware.use(FileMiddleware(publicDirectory: app.directory.publicDirectory))

    app.http.client.configuration.timeout.connect = .seconds(30)
    app.http.client.configuration.timeout.read = .seconds(30)
    
    // register routes
    try routes(app)
}

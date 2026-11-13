// swift-tools-version: 6.0
import PackageDescription

let package = Package(
    name: "LEGOExplorer",
    platforms: [.macOS(.v14), .iOS(.v17)],
    products: [
        .executable(name: "LEGOExplorer", targets: ["LEGOExplorer"]),
        .executable(name: "explorer-tool", targets: ["explorer-tool"]),
    ],
    targets: [
        // Data layer: SQLite access, part classifier, Rebrickable table builder.
        .target(name: "ExplorerCore", path: "Sources/ExplorerCore"),
        // The SwiftUI app.
        .executableTarget(name: "LEGOExplorer", dependencies: ["ExplorerCore"], path: "Sources/LEGOExplorer"),
        // Command-line checks: classifier vs R, building the database from CSVs.
        .executableTarget(name: "explorer-tool", dependencies: ["ExplorerCore"], path: "Sources/explorer-tool"),
    ],
    swiftLanguageModes: [.v5]
)
